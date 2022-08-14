//===-- CapstoneGenInfo.cpp - Info Generation Module -----------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CapstoneGenInfo.h"
#include "CapstoneGenPrinter.h"
#include "CapstoneHelper.h"
#include "SequenceToOffsetTable.h"
#include "llvm/TableGen/Error.h"

using namespace llvm;

raw_ostream &operator<<(raw_ostream &OS, const EncodingAndInst &Value) {
  if (Value.EncodingDef != Value.Inst->TheDef)
    OS << Value.EncodingDef->getName() << ":";
  OS << Value.Inst->TheDef->getName();
  return OS;
}

void OperandInfo::addField(unsigned Base, unsigned Width, unsigned Offset) {
  Fields.push_back(EncodingField(Base, Width, Offset));
}
unsigned OperandInfo::numFields() const { return Fields.size(); }
OperandInfo::const_iterator OperandInfo::begin() const { return Fields.begin(); }
OperandInfo::const_iterator OperandInfo::end() const { return Fields.end(); }

////////////////////////////////////
//                                //
// CapstoneGenInfo Implementation //
//                                //
////////////////////////////////////

// Defaults preserved here for documentation, even though they aren't
// strictly necessary given the way that this is currently being called.
CapstoneGenInfo::CapstoneGenInfo(RecordKeeper &R, std::string PredicateNamespace,
                std::string GPrefix = "if (",
                std::string GPostfix = " == MCDisassembler::Fail)",
                std::string ROK = "MCDisassembler::Success",
                std::string RFail = "MCDisassembler::Fail",
                std::string L = "")
    : RK(R), CDP(R), SchedModels(CDP.getTargetInfo().getSchedModels()),
      Target(R), PredicateNamespace(std::move(PredicateNamespace)),
      GuardPrefix(std::move(GPrefix)), GuardPostfix(std::move(GPostfix)),
      ReturnOK(std::move(ROK)), ReturnFail(std::move(RFail)),
      Locals(std::move(L)) {
  Record *AsmWriter = Target.getAsmWriter();
  unsigned Variant = AsmWriter->getValueAsInt("Variant");

  // Get the instruction numbering.
  NumberedInstructions = Target.getInstructionsByEnumValue();

  for (unsigned i = 0, e = NumberedInstructions.size(); i != e; ++i) {
    const CodeGenInstruction *I = NumberedInstructions[i];
    if (!I->AsmString.empty() && I->TheDef->getName() != "PHI")
      Instructions.emplace_back(*I, i, Variant);
  }
}

// Emit the decoder state machine table.
void CapstoneGenInfo::emitTable(formatted_raw_ostream &OS, DecoderTable &Table,
                                unsigned Indentation, unsigned BitWidth,
                                StringRef Namespace) const {
  OS.indent(Indentation) << "static const uint8_t DecoderTable" << Namespace
                         << BitWidth << "[] = {\n";

  Indentation += 2;

  // FIXME: We may be able to use the NumToSkip values to recover
  // appropriate indentation levels.
  DecoderTable::const_iterator I = Table.begin();
  DecoderTable::const_iterator E = Table.end();
  while (I != E) {
    assert(I < E && "incomplete decode table entry!");

    uint64_t Pos = I - Table.begin();
    OS << "/* " << Pos << " */";
    OS.PadToColumn(12);

    switch (*I) {
    default:
      PrintFatalError("invalid decode table opcode");
    case MCD::OPC_ExtractField: {
      ++I;
      unsigned Start = *I++;
      unsigned Len = *I++;
      OS.indent(Indentation)
          << "MCD_OPC_ExtractField, " << Start << ", " << Len << ",  // Inst{";
      if (Len > 1)
        OS << (Start + Len - 1) << "-";
      OS << Start << "} ...\n";
      break;
    }
    case MCD::OPC_FilterValue: {
      ++I;
      OS.indent(Indentation) << "MCD_OPC_FilterValue, ";
      // The filter value is ULEB128 encoded.
      while (*I >= 128)
        OS << (unsigned)*I++ << ", ";
      OS << (unsigned)*I++ << ", ";

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_CheckField: {
      ++I;
      unsigned Start = *I++;
      unsigned Len = *I++;
      OS.indent(Indentation) << "MCD_OPC_CheckField, " << Start << ", " << Len
                             << ", "; // << Val << ", " << NumToSkip << ",\n";
      // ULEB128 encoded field value.
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";
      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_CheckPredicate: {
      ++I;
      OS.indent(Indentation) << "MCD_OPC_CheckPredicate, ";
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_Decode:
    case MCD::OPC_TryDecode: {
      bool IsTry = *I == MCD::OPC_TryDecode;
      ++I;
      // Extract the ULEB128 encoded Opcode to a buffer.
      uint8_t Buffer[16], *p = Buffer;
      while ((*p++ = *I++) >= 128)
        assert((p - Buffer) <= (ptrdiff_t)sizeof(Buffer) &&
               "ULEB128 value too large!");
      // Decode the Opcode value.
      unsigned Opc = decodeULEB128(Buffer);
      OS.indent(Indentation)
          << "MCD_OPC_" << (IsTry ? "Try" : "") << "Decode, ";
      for (p = Buffer; *p >= 128; ++p)
        OS << (unsigned)*p << ", ";
      OS << (unsigned)*p << ", ";

      // Decoder index.
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";

      if (!IsTry) {
        OS << "// Opcode: " << NumberedEncodings[Opc] << "\n";
        break;
      }

      // Fallthrough for OPC_TryDecode.

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;

      OS << "// Opcode: " << NumberedEncodings[Opc]
         << ", skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_SoftFail: {
      ++I;
      OS.indent(Indentation) << "MCD_OPC_SoftFail";
      // Positive mask
      uint64_t Value = 0;
      unsigned Shift = 0;
      do {
        OS << ", " << (unsigned)*I;
        Value += (*I & 0x7f) << Shift;
        Shift += 7;
      } while (*I++ >= 128);
      if (Value > 127) {
        OS << " /* 0x";
        OS.write_hex(Value);
        OS << " */";
      }
      // Negative mask
      Value = 0;
      Shift = 0;
      do {
        OS << ", " << (unsigned)*I;
        Value += (*I & 0x7f) << Shift;
        Shift += 7;
      } while (*I++ >= 128);
      if (Value > 127) {
        OS << " /* 0x";
        OS.write_hex(Value);
        OS << " */";
      }
      OS << ",\n";
      break;
    }
    case MCD::OPC_Fail: {
      ++I;
      OS.indent(Indentation) << "MCD_OPC_Fail,\n";
      break;
    }
    }
  }
  OS.indent(Indentation) << "0\n";

  Indentation -= 2;

  OS.indent(Indentation) << "};\n\n";
}

void CapstoneGenInfo::emitPredicateFunction(formatted_raw_ostream &OS,
                                            PredicateSet &Predicates,
                                            unsigned Indentation) const {
  // The predicate function is just a big switch statement based on the
  // input predicate index.
  OS.indent(Indentation) << "static bool getbool(uint64_t b)\n"
                            "{\n"
                            "\treturn b != 0;\n"
                            "}\n";
  OS.indent(Indentation) << "static bool checkDecoderPredicate(unsigned Idx, "
                         << "uint64_t Bits) {\n";
  Indentation += 2;
  if (!Predicates.empty()) {
    OS.indent(Indentation) << "switch (Idx) {\n";
    OS.indent(Indentation)
        << "default: llvm_unreachable(\"Invalid index!\");\n";
    unsigned Index = 0;
    for (const auto &Predicate : Predicates) {
      OS.indent(Indentation) << "case " << Index++ << ":\n";
      OS.indent(Indentation + 2) << "return getbool(" << Predicate << ");\n";
    }
    OS.indent(Indentation) << "}\n";
  } else {
    // No case statement to emit
    OS.indent(Indentation) << "llvm_unreachable(\"Invalid index!\");\n";
  }
  Indentation -= 2;
  OS.indent(Indentation) << "}\n\n";
}

void CapstoneGenInfo::emitDecoderFunction(formatted_raw_ostream &OS,
                                          DecoderSet &Decoders,
                                          unsigned Indentation) const {
  // The decoder function is just a big switch statement based on the
  // input decoder index.
  OS.indent(Indentation)
      << "#define DecodeToMCInst(fname, fieldname, InsnType) \\\n"
         "static DecodeStatus fname(DecodeStatus S, unsigned Idx, InsnType "
         "insn, MCInst *MI, \\\n"
         "                uint64_t Address, bool *Decoder) {\\\n";
  Indentation += 2;
  // TODO: When InsnType is large, using uint64_t limits all fields to 64 bits
  // It would be better for emitBinaryParser to use a 64-bit tmp whenever
  // possible but fall back to an InsnType-sized tmp for truly large fields.
  OS.indent(Indentation) << "InsnType tmp;\\\n";
  OS.indent(Indentation) << "switch (Idx) {\\\n";
  OS.indent(Indentation)
      << "default: llvm_unreachable(\"Invalid index!\");\\\n";
  unsigned Index = 0;
  for (const auto &Decoder : Decoders) {
    OS.indent(Indentation) << "case " << Index++ << ":\\\n";
    OS << Decoder;
    OS.indent(Indentation + 2) << "return S;\\\n";
  }
  OS.indent(Indentation) << "}\\\n";
  Indentation -= 2;
  OS.indent(Indentation) << "}\\\n\n";
}

std::vector<std::string>
CapstoneGenInfo::GetOperandInfo(const CodeGenInstruction &Inst) {
  std::vector<std::string> Result;

  for (auto &Op : Inst.Operands) {
    // Handle aggregate operands and normal operands the same way by expanding
    // either case into a list of operands for this op.
    std::vector<CGIOperandList::OperandInfo> OperandList;

    // This might be a multiple operand thing.  Targets like X86 have
    // registers in their multi-operand operands.  It may also be an anonymous
    // operand, which has a single operand, but no declared class for the
    // operand.
    DagInit *MIOI = Op.MIOperandInfo;

    if (!MIOI || MIOI->getNumArgs() == 0) {
      // Single, anonymous, operand.
      OperandList.push_back(Op);
    } else {
      for (unsigned j = 0, e = Op.MINumOperands; j != e; ++j) {
        OperandList.push_back(Op);

        auto *OpR = cast<DefInit>(MIOI->getArg(j))->getDef();
        OperandList.back().Rec = OpR;
      }
    }

    for (unsigned j = 0, e = OperandList.size(); j != e; ++j) {
      Record *OpR = OperandList[j].Rec;
      std::string Res;

      if (OpR->isSubClassOf("RegisterOperand"))
        OpR = OpR->getValueAsDef("RegClass");
      if (OpR->isSubClassOf("RegisterClass"))
        Res += std::string(OpR->getValueAsString("Namespace")) + "_" +
               OpR->getName().str() + "RegClassID, ";
      else if (OpR->isSubClassOf("PointerLikeRegClass"))
        Res += utostr(OpR->getValueAsInt("RegClassKind")) + ", ";
      else
        // -1 means the operand does not have a fixed register class.
        Res += "-1, ";

      // Fill in applicable flags.
      Res += "0";

      // Ptr value whose register class is resolved via callback.
      if (OpR->isSubClassOf("PointerLikeRegClass"))
        Res += "|(1<<MCOI_LookupPtrRegClass)";

      // Predicate operands.  Check to see if the original unexpanded operand
      // was of type PredicateOp.
      if (Op.Rec->isSubClassOf("PredicateOp"))
        Res += "|(1<<MCOI_Predicate)";

      // Optional def operands.  Check to see if the original unexpanded operand
      // was of type OptionalDefOperand.
      if (Op.Rec->isSubClassOf("OptionalDefOperand"))
        Res += "|(1<<MCOI_OptionalDef)";

      // Branch target operands.  Check to see if the original unexpanded
      // operand was of type BranchTargetOperand.
      if (Op.Rec->isSubClassOf("BranchTargetOperand"))
        Res += "|(1<<MCOI_BranchTarget)";

      // Fill in operand type.
      Res += ", ";
      assert(!Op.OperandType.empty() && "Invalid operand type.");
      // replace qualified name
      auto QualifiedName = std::string(Op.OperandType);
      if (QualifiedName.find("GENERIC_IMM") != std::string::npos) {
        QualifiedName = "MCOI_OPERAND_IMMEDIATE";
      }
      if (QualifiedName.find("MCOI") == std::string::npos) {
        QualifiedName = "MCOI_OPERAND_UNKNOWN";
      }
      auto pos = QualifiedName.find("::");
      if (pos != std::string::npos)
        QualifiedName =
            QualifiedName.substr(0, pos) + "_" + QualifiedName.substr(pos + 2);

      Res += QualifiedName;

      // Fill in constraint info.
      Res += ", ";

      const CGIOperandList::ConstraintInfo &Constraint = Op.Constraints[j];
      if (Constraint.isNone())
        Res += "0";
      else if (Constraint.isEarlyClobber())
        Res += "MCOI_EARLY_CLOBBER";
      else {
        assert(Constraint.isTied());
        Res += "MCOI_TIED_TO/*" + utostr(Constraint.getTiedOperand()) + "*/";
      }

      Result.push_back(Res);
    }
  }

  return Result;
}

void CapstoneGenInfo::EmitOperandInfo(raw_ostream &OS,
                                      OperandInfoMapTy &OperandInfoIDs) {
  // ID #0 is for no operand info.
  unsigned OperandListNum = 0;
  OperandInfoIDs[std::vector<std::string>()] = ++OperandListNum;

  OS << "\n";
  const CodeGenTarget &Target = CDP.getTargetInfo();
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue()) {
    std::vector<std::string> OperandInfo = GetOperandInfo(*Inst);
    unsigned &N = OperandInfoIDs[OperandInfo];
    if (N != 0)
      continue;

    N = ++OperandListNum;
    OS << "static const MCOperandInfo OperandInfo" << N << "[] = { ";
    for (const std::string &Info : OperandInfo)
      OS << "{ " << Info << " }, ";
    OS << "};\n";
  }
}

void CapstoneGenInfo::emitRecord(
    const CodeGenInstruction &Inst, unsigned Num, Record *InstrInfo,
    std::map<std::vector<Record *>, unsigned> &EmittedLists,
    const OperandInfoMapTy &OpInfo, raw_ostream &OS) {
  int MinOperands = 0;
  if (!Inst.Operands.empty())
    // Each logical operand can be multiple MI operands.
    MinOperands =
        Inst.Operands.back().MIOperandNo + Inst.Operands.back().MINumOperands;

  OS << "  { ";
  OS << /*Num << ",\t" <<*/ MinOperands << ",\t";
  /*<< Inst.Operands.NumDefs << ",\t"
  << Inst.TheDef->getValueAsInt("Size") << ",\t"
  << SchedModels.getSchedClassIdx(Inst) << ",\t0"; */
  std::vector<std::string> OperandInfo = GetOperandInfo(Inst);
  if (OperandInfo.empty())
    OS << "NULL";
  else
    OS << "OperandInfo" << OpInfo.find(OperandInfo)->second;

  OS << " },  // Inst #" << Num << " = " << Inst.TheDef->getName() << "\n";
}

// Emits disassembler code for instruction decoding.
void CapstoneGenInfo::run(raw_ostream &o) {

  formatted_raw_ostream OS(o);
  OS << "/* Capstone Disassembly Engine, http://www.capstone-engine.org */\n"
     << "/* By Phosphorus15 <phosphorus15@foxmail.com>, Year 2021 */\n"
     << "/* This generator is under https://github.com/rizinorg/llvm-capstone "
        "*/\n"
     << "/* Automatically generated file, do not edit! */\n\n";

  OS << "#include \"../../MCInst.h\"\n";
  OS << "#include \"../../LEB128.h\"\n";
  OS << "\n\n";

  DenseMap<Record *, unsigned> FeatureMap;

  Enumeration(OS, FeatureMap);

  OS << "#ifdef MIPS_GET_DISASSEMBLER\n"
     << "#undef MIPS_GET_DISASSEMBLER\n\n";

  emitFieldFromInstruction(OS);
  // emitInsertBits(OS);

  Target.reverseBitsForLittleEndianEncoding();

  // Parameterize the decoders based on namespace and instruction width.
  std::set<StringRef> HwModeNames;
  const auto &NumberedInstructions = Target.getInstructionsByEnumValue();
  NumberedEncodings.reserve(NumberedInstructions.size());
  DenseMap<Record *, unsigned> IndexOfInstruction;
  // First, collect all HwModes referenced by the target.
  for (const auto &NumberedInstruction : NumberedInstructions) {
    IndexOfInstruction[NumberedInstruction->TheDef] = NumberedEncodings.size();

    if (const RecordVal *RV =
            NumberedInstruction->TheDef->getValue("EncodingInfos")) {
      if (auto *DI = dyn_cast_or_null<DefInit>(RV->getValue())) {
        const CodeGenHwModes &HWM = Target.getHwModes();
        EncodingInfoByHwMode EBM(DI->getDef(), HWM);
        for (auto &KV : EBM)
          HwModeNames.insert(HWM.getMode(KV.first).Name);
      }
    }
  }

  // If HwModeNames is empty, add the empty string so we always have one HwMode.
  if (HwModeNames.empty())
    HwModeNames.insert("");

  for (const auto &NumberedInstruction : NumberedInstructions) {
    IndexOfInstruction[NumberedInstruction->TheDef] = NumberedEncodings.size();

    if (const RecordVal *RV =
            NumberedInstruction->TheDef->getValue("EncodingInfos")) {
      if (DefInit *DI = dyn_cast_or_null<DefInit>(RV->getValue())) {
        const CodeGenHwModes &HWM = Target.getHwModes();
        EncodingInfoByHwMode EBM(DI->getDef(), HWM);
        for (auto &KV : EBM) {
          NumberedEncodings.emplace_back(KV.second, NumberedInstruction,
                                         HWM.getMode(KV.first).Name);
          HwModeNames.insert(HWM.getMode(KV.first).Name);
        }
        continue;
      }
    }
    // This instruction is encoded the same on all HwModes. Emit it for all
    // HwModes.
    for (StringRef HwModeName : HwModeNames)
      NumberedEncodings.emplace_back(NumberedInstruction->TheDef,
                                     NumberedInstruction, HwModeName);
  }
  for (const auto &NumberedAlias :
       RK.getAllDerivedDefinitions("AdditionalEncoding"))
    NumberedEncodings.emplace_back(
        NumberedAlias,
        &Target.getInstruction(NumberedAlias->getValueAsDef("AliasOf")));

  std::map<std::pair<std::string, unsigned>, std::vector<EncodingIDAndOpcode>>
      OpcMap;
  std::map<unsigned, std::vector<OperandInfo>> Operands;

  for (unsigned i = 0; i < NumberedEncodings.size(); ++i) {
    const Record *EncodingDef = NumberedEncodings[i].EncodingDef;
    const CodeGenInstruction *Inst = NumberedEncodings[i].Inst;
    const Record *Def = Inst->TheDef;
    unsigned Size = EncodingDef->getValueAsInt("Size");
    if (Def->getValueAsString("Namespace") == "TargetOpcode" ||
        Def->getValueAsBit("isPseudo") ||
        Def->getValueAsBit("isAsmParserOnly") ||
        Def->getValueAsBit("isCodeGenOnly")) {
      NumEncodingsLackingDisasm++;
      continue;
    }

    if (i < NumberedInstructions.size())
      NumInstructions++;
    NumEncodings++;

    if (!Size)
      continue;

    if (populateInstruction(Target, *EncodingDef, *Inst, i, Operands)) {
      std::string DecoderNamespace =
          std::string(EncodingDef->getValueAsString("DecoderNamespace"));
      if (!NumberedEncodings[i].HwModeName.empty())
        DecoderNamespace +=
            std::string("_") + NumberedEncodings[i].HwModeName.str();
      OpcMap[std::make_pair(DecoderNamespace, Size)].emplace_back(
          i, IndexOfInstruction.find(Def)->second);
    } else {
      NumEncodingsOmitted++;
    }
  }

  DecoderTableInfo TableInfo;
  for (const auto &Opc : OpcMap) {
    // Emit the decoder for this namespace+width combination.
    ArrayRef<EncodingAndInst> NumberedEncodingsRef(NumberedEncodings.data(),
                                                   NumberedEncodings.size());
    FilterChooser FC(NumberedEncodingsRef, Opc.second, Operands,
                     8 * Opc.first.second, this);

    // The decode table is cleared for each top level decoder function. The
    // predicates and decoders themselves, however, are shared across all
    // decoders to give more opportunities for uniqueing.
    TableInfo.Table.clear();
    TableInfo.FixupStack.clear();
    TableInfo.Table.reserve(16384);
    TableInfo.FixupStack.emplace_back();
    FC.emitTableEntries(TableInfo);
    // Any NumToSkip fixups in the top level scope can resolve to the
    // OPC_Fail at the end of the table.
    assert(TableInfo.FixupStack.size() == 1 && "fixup stack phasing error!");
    // Resolve any NumToSkip fixups in the current scope.
    resolveTableFixups(TableInfo.Table, TableInfo.FixupStack.back(),
                       TableInfo.Table.size());
    TableInfo.FixupStack.clear();

    TableInfo.Table.push_back(MCD::OPC_Fail);

    // Print the table to the output stream.
    emitTable(OS, TableInfo.Table, 0, FC.getBitWidth(), Opc.first.first);
    OS.flush();
  }

  // Emit the predicate function.
  emitPredicateFunction(OS, TableInfo.Predicates, 0);

  // Emit the decoder function.
  emitDecoderFunction(OS, TableInfo.Decoders, 0);

  // Emit the main entry point for the decoder, decodeInstruction().
  emitDecodeInstruction(OS);

  OS << "\nFieldFromInstruction(fieldFromInstruction, uint32_t)\n"
     << "DecodeToMCInst(decodeToMCInst, fieldFromInstruction, uint32_t)\n"
     << "DecodeInstruction(decodeInstruction, fieldFromInstruction, "
        "decodeToMCInst, uint32_t)\n\n";

  OS << "#endif // MIPS_GET_DISASSEMBLER";

  runEnums(OS, Target, Target.getRegBank());

  runMCDesc(OS, Target, Target.getRegBank());

  // asm printer

  std::vector<std::vector<std::string>> TableDrivenOperandPrinters;
  unsigned BitsLeft = 0;
  unsigned AsmStrBits = 0;

  OS << "#ifdef GET_ASM_WRITER\n"
     << "#undef GET_ASM_WRITER\n\n";
  // boilerplate
  OS << "static void llvm_unreachable(const char * info) {}\n"
     << "static void assert(int val) {}\n";

  EmitGetMnemonic(OS, TableDrivenOperandPrinters, BitsLeft, AsmStrBits);
  EmitPrintInstruction(OS, TableDrivenOperandPrinters, BitsLeft, AsmStrBits);
  EmitGetRegisterName(OS);

  OS << "#endif";

  EmitPrintAliasInstruction(OS);

  OS << "#ifdef GET_INSTRINFO_MC_DESC\n";
  OS << "#undef GET_INSTRINFO_MC_DESC\n";

  CodeGenDAGPatterns CDP(RK);

  CodeGenTarget &Target = CDP.getTargetInfo();
  const std::string &TargetName = std::string(Target.getName());
  Record *InstrInfo = Target.getInstructionSet();

  // Keep track of all of the def lists we have emitted already.
  std::map<std::vector<Record *>, unsigned> EmittedLists;

  OperandInfoMapTy OperandInfoIDs;

  // Emit all of the operand info records.
  EmitOperandInfo(OS, OperandInfoIDs);

  // Emit all of the MCInstrDesc records in their ENUM ordering.
  //
  OS << "\nextern const MCInstrDesc " << TargetName << "Insts[] = {\n";

  SequenceToOffsetTable<std::string> InstrNames;
  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    // Keep a list of the instruction names.
    InstrNames.add(std::string(Inst->TheDef->getName()));
    // Emit the record into the table.
    emitRecord(*Inst, Num++, InstrInfo, EmittedLists, OperandInfoIDs, OS);
  }
  OS << "};\n\n";

  // Emit the array of instruction names.
  InstrNames.layout();
  InstrNames.emitStringLiteralDef(OS, Twine("extern const char ") + TargetName +
                                          "InstrNameData[]");

  OS << "extern const unsigned " << TargetName << "InstrNameIndices[] = {";
  Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    // Newline every eight entries.
    if (Num % 8 == 0)
      OS << "\n    ";
    OS << InstrNames.get(std::string(Inst->TheDef->getName())) << "U, ";
    ++Num;
  }
  OS << "\n};\n\n";

  OS << "#endif // GET_INSTRINFO_MC_DESC\n\n";
}

//
// Generate Enumerations
//

void CapstoneGenInfo::Enumeration(
    raw_ostream &OS, DenseMap<Record *, unsigned> &FeatureMap) const {
  // Get all records of class and sort
  std::vector<Record *> DefList =
      this->RK.getAllDerivedDefinitions("SubtargetFeature");
  llvm::sort(DefList, LessRecord());

  unsigned N = DefList.size();
  if (N == 0)
    return;
  if (N + 1 > MAX_SUBTARGET_FEATURES)
    PrintFatalError(
        "Too many subtarget features! Bump MAX_SUBTARGET_FEATURES.");

  // For each record
  for (unsigned i = 0; i < N; ++i) {
    // Next record
    Record *Def = DefList[i];

    // Get and emit name
    OS << "#define " << this->Target.getName() << "_" << Def->getName() << " "
       << i << "ULL\n";

    // Save the index for this feature.
    FeatureMap[Def] = i;
  }
}

void CapstoneGenInfo::runEnums(raw_ostream &OS, CodeGenTarget &Target,
                               CodeGenRegBank &Bank) {
  const auto &Registers = Bank.getRegisters();

  // Register enums are stored as uint16_t in the tables. Make sure we'll fit.
  assert(Registers.size() <= 0xffff && "Too many regs to fit in tables");

  StringRef Namespace = Registers.front().TheDef->getValueAsString("Namespace");

  OS << "\n#ifdef GET_REGINFO_ENUM\n";
  OS << "#undef GET_REGINFO_ENUM\n\n";

  for (const auto &Reg : Registers)
    OS << "#define " << Namespace << "_" << Reg.getName() << " "
       << Reg.EnumValue << "\n";
  assert(Registers.size() == Registers.back().EnumValue &&
         "Register enum value mismatch!");
  OS << "#define " << Namespace << "_"
     << "NUM_TARGET_REGS " << Registers.size() + 1 << "\n";
  OS << "\n";

  const auto &RegisterClasses = Bank.getRegClasses();
  if (!RegisterClasses.empty()) {

    // RegisterClass enums are stored as uint16_t in the tables.
    assert(RegisterClasses.size() <= 0xffff &&
           "Too many register classes to fit in tables");

    OS << "\n// Register classes\n\n";
    for (const auto &RC : RegisterClasses)
      OS << "#define " << Namespace << "_" << RC.getName() << "RegClassID"
         << " " << RC.EnumValue << "\n";
    OS << "\n";
  }
  OS << "#endif // GET_REGINFO_ENUM\n\n";

  // instruction info
  OS << "#ifdef GET_INSTRINFO_ENUM\n";
  OS << "#undef GET_INSTRINFO_ENUM\n";

  if (Namespace.empty())
    PrintFatalError("No instructions defined!");

  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue())
    OS << "#define " << Namespace << "_" << Inst->TheDef->getName() << "\t "
       << Num++ << "\n";

  OS << "#endif // GET_INSTRINFO_ENUM\n\n";

  OS << "#ifdef GET_REGINFO_EXTRA\n";
  OS << "#undef GET_REGINFO_EXTRA\n";

  const std::vector<Record *> &RegAltNameIndices =
      Target.getRegAltNameIndices();
  // If the only definition is the default NoRegAltName, we don't need to
  // emit anything.
  if (RegAltNameIndices.size() > 1) {
    OS << "\n// Register alternate name indices\n\n";
    OS << "enum {\n";
    for (unsigned i = 0, e = RegAltNameIndices.size(); i != e; ++i)
      OS << "  " << Namespace << "_" << RegAltNameIndices[i]->getName()
         << ",\t// " << i << "\n  ";
    OS << Namespace << "_"
       << "NUM_TARGET_REG_ALT_NAMES = " << RegAltNameIndices.size() << "\n";
    OS << "};\n";
  }

  auto &SubRegIndices = Bank.getSubRegIndices();
  if (!SubRegIndices.empty()) {
    OS << "\n// Subregister indices\n\n";
    OS << "enum {\n  NoSubRegister,\n";
    unsigned i = 0;
    for (const auto &Idx : SubRegIndices)
      OS << "  " << Namespace << "_" << Idx.getName() << ",\t// " << ++i
         << "\n  ";
    OS << Namespace << "_"
       << "NUM_TARGET_SUBREGS\n};\n";
  }

  OS << "#endif // GET_REGINFO_EXTRA\n\n";
}

//
// MC Descriptor methods
//

void CapstoneGenInfo::runMCDesc(raw_ostream &OS, CodeGenTarget &Target,
                                CodeGenRegBank &RegBank) {

  OS << "\n#ifdef GET_REGINFO_MC_DESC\n";
  OS << "#undef GET_REGINFO_MC_DESC\n\n";

  const auto &Regs = RegBank.getRegisters();

  //  auto &SubRegIndices = RegBank.getSubRegIndices();
  // The lists of sub-registers and super-registers go in the same array.  That
  // allows us to share suffixes.
  typedef std::vector<const CodeGenRegister *> RegVec;

  // Differentially encoded lists.
  SequenceToOffsetTable<DiffVec> DiffSeqs;
  SmallVector<DiffVec, 4> SubRegLists(Regs.size());
  SmallVector<DiffVec, 4> SuperRegLists(Regs.size());
  SmallVector<DiffVec, 4> RegUnitLists(Regs.size());
  SmallVector<unsigned, 4> RegUnitInitScale(Regs.size());

  // List of lane masks accompanying register unit sequences.
  SequenceToOffsetTable<MaskVec> LaneMaskSeqs;
  SmallVector<MaskVec, 4> RegUnitLaneMasks(Regs.size());

  // Keep track of sub-register names as well. These are not differentially
  // encoded.
  typedef SmallVector<const CodeGenSubRegIndex *, 4> SubRegIdxVec;
  SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> SubRegIdxSeqs;
  SmallVector<SubRegIdxVec, 4> SubRegIdxLists(Regs.size());

  SequenceToOffsetTable<std::string> RegStrings;

  // Precompute register lists for the SequenceToOffsetTable.
  unsigned i = 0;
  for (auto I = Regs.begin(), E = Regs.end(); I != E; ++I, ++i) {
    const auto &Reg = *I;
    RegStrings.add(std::string(Reg.getName()));

    // Compute the ordered sub-register list.
    SetVector<const CodeGenRegister *> SR;
    Reg.addSubRegsPreOrder(SR, RegBank);
    diffEncode(SubRegLists[i], Reg.EnumValue, SR.begin(), SR.end());
    DiffSeqs.add(SubRegLists[i]);

    // Compute the corresponding sub-register indexes.
    SubRegIdxVec &SRIs = SubRegIdxLists[i];
    for (const CodeGenRegister *S : SR)
      SRIs.push_back(Reg.getSubRegIndex(S));
    SubRegIdxSeqs.add(SRIs);

    // Super-registers are already computed.
    const RegVec &SuperRegList = Reg.getSuperRegs();
    diffEncode(SuperRegLists[i], Reg.EnumValue, SuperRegList.begin(),
               SuperRegList.end());
    DiffSeqs.add(SuperRegLists[i]);

    // Differentially encode the register unit list, seeded by register number.
    // First compute a scale factor that allows more diff-lists to be reused:
    //
    //   D0 -> (S0, S1)
    //   D1 -> (S2, S3)
    //
    // A scale factor of 2 allows D0 and D1 to share a diff-list. The initial
    // value for the differential decoder is the register number multiplied by
    // the scale.
    //
    // Check the neighboring registers for arithmetic progressions.
    unsigned ScaleA = ~0u, ScaleB = ~0u;
    SparseBitVector<> RUs = Reg.getNativeRegUnits();
    if (I != Regs.begin() &&
        std::prev(I)->getNativeRegUnits().count() == RUs.count())
      ScaleB = *RUs.begin() - *std::prev(I)->getNativeRegUnits().begin();
    if (std::next(I) != Regs.end() &&
        std::next(I)->getNativeRegUnits().count() == RUs.count())
      ScaleA = *std::next(I)->getNativeRegUnits().begin() - *RUs.begin();
    unsigned Scale = std::min(ScaleB, ScaleA);
    // Default the scale to 0 if it can't be encoded in 4 bits.
    if (Scale >= 16)
      Scale = 0;
    RegUnitInitScale[i] = Scale;
    DiffSeqs.add(diffEncode(RegUnitLists[i], Scale * Reg.EnumValue, RUs));

    const auto &RUMasks = Reg.getRegUnitLaneMasks();
    MaskVec &LaneMaskVec = RegUnitLaneMasks[i];
    assert(LaneMaskVec.empty());
    llvm::append_range(LaneMaskVec, RUMasks);
// Terminator mask should not be used inside of the list.
#ifndef NDEBUG
    for (LaneBitmask M : LaneMaskVec) {
      assert(!M.all() && "terminator mask should not be part of the list");
    }
#endif
    LaneMaskSeqs.add(LaneMaskVec);
  }

  // Compute the final layout of the sequence table.
  DiffSeqs.layout();
  LaneMaskSeqs.layout();
  SubRegIdxSeqs.layout();

  OS << "\n\n";

  const std::string &TargetName = std::string(Target.getName());

  // Emit the shared table of differential lists.
  OS << "static const MCPhysReg " << TargetName << "RegDiffLists[] = {\n";
  DiffSeqs.emit(OS, printDiff16);
  OS << "};\n\n";

  // Emit the shared table of regunit lane mask sequences.
  //  OS << "extern const LaneBitmask " << TargetName << "LaneMaskLists[] =
  //  {\n";
  //  LaneMaskSeqs.emit(OS, printMask, "LaneBitmask::getAll()");
  //  OS << "};\n\n";

  // Emit the table of sub-register indexes.
  OS << "static const uint16_t " << TargetName << "SubRegIdxLists[] = {\n";
  SubRegIdxSeqs.emit(OS, printSubRegIndex);
  OS << "};\n\n";

  // Emit the table of sub-register index sizes.
  //  OS << "extern const MCRegisterInfo::SubRegCoveredBits "
  //     << TargetName << "SubRegIdxRanges[] = {\n";
  //  OS << "  { " << (uint16_t)-1 << ", " << (uint16_t)-1 << " },\n";
  //  for (const auto &Idx : SubRegIndices) {
  //    OS << "  { " << Idx.Offset << ", " << Idx.Size << " },\t// "
  //       << Idx.getName() << "\n";
  //  }
  //  OS << "};\n\n";

  // Emit the string table.
  RegStrings.layout();
  RegStrings.emitStringLiteralDef(OS, Twine("static const char ") + TargetName +
                                          "RegStrings[]");

  OS << "static const MCRegisterDesc " << TargetName
     << "RegDesc[] = { // Descriptors\n";
  OS << "  { " << RegStrings.get("") << ", 0, 0, 0, 0, 0 },\n";

  // Emit the register descriptors now.
  i = 0;
  for (const auto &Reg : Regs) {
    OS << "  { " << RegStrings.get(std::string(Reg.getName())) << ", "
       << DiffSeqs.get(SubRegLists[i]) << ", " << DiffSeqs.get(SuperRegLists[i])
       << ", " << SubRegIdxSeqs.get(SubRegIdxLists[i]) << ", "
       << (DiffSeqs.get(RegUnitLists[i]) * 16 + RegUnitInitScale[i]) << ", "
       << LaneMaskSeqs.get(RegUnitLaneMasks[i]) << " },\n";
    ++i;
  }
  OS << "};\n\n"; // End of register descriptors...

  // Emit the table of register unit roots. Each regunit has one or two root
  // registers.
  //  OS << "static const MCPhysReg " << TargetName << "RegUnitRoots[][2] =
  //  {\n";
  //  for (unsigned i = 0, e = RegBank.getNumNativeRegUnits(); i != e; ++i) {
  //    ArrayRef<const CodeGenRegister*> Roots =
  //    RegBank.getRegUnit(i).getRoots();
  //    assert(!Roots.empty() && "All regunits must have a root register.");
  //    assert(Roots.size() <= 2 && "More than two roots not supported yet.");
  //    OS << "  { ";
  //    ListSeparator LS;
  //    for (const CodeGenRegister *R : Roots)
  //      OS << LS << getQualifiedName(R->TheDef);
  //    OS << " },\n";
  //  }
  //  OS << "};\n\n";

  const auto &RegisterClasses = RegBank.getRegClasses();

  SequenceToOffsetTable<std::string> RegClassStrings;

  // Emit the register enum value arrays for each RegisterClass
  for (const auto &RC : RegisterClasses) {
    ArrayRef<Record *> Order = RC.getOrder();

    // Give the register class a legal C name if it's anonymous.
    const std::string &Name = RC.getName();

    RegClassStrings.add(Name);

    StringRef Namespace = RC.Namespace;

    // Emit the register list now.
    OS << "  // " << Name << " Register Class...\n"
       << "  static const MCPhysReg " << Name << "[] = {\n    ";
    for (Record *Reg : Order) {
      OS << Namespace << "_" << Reg->getName().str() << ", ";
    }
    OS << "\n  };\n\n";

    OS << "  // " << Name << " Bit set.\n"
       << "  static const uint8_t " << Name << "Bits[] = {\n    ";
    BitVectorEmitter BVE;
    for (Record *Reg : Order) {
      BVE.add(Target.getRegBank().getReg(Reg)->EnumValue);
    }
    BVE.print(OS);
    OS << "\n  };\n\n";
  }
  OS << "// end of register classes misc\n\n";

  RegClassStrings.layout();
  RegClassStrings.emitStringLiteralDef(
      OS, Twine("static const char ") + TargetName + "RegClassStrings[]");

  OS << "static const MCRegisterClass " << TargetName
     << "MCRegisterClasses[] = {\n";

  for (const auto &RC : RegisterClasses) {
    assert(isInt<8>(RC.CopyCost) && "Copy cost too large.");
    OS << "  { " << RC.getName() << ", " << RC.getName() << "Bits, "
       << "sizeof(" << RC.getName() << "Bits)"
       << " },\n";
  }

  OS << "};\n\n";

  // FIXME we don't need this .. probably EmitRegMappingTables(OS, Regs, false);

  // Emit Reg encoding table
  //  OS << "extern const uint16_t " << TargetName;
  //  OS << "RegEncodingTable[] = {\n";
  //  // Add entry for NoRegister
  //  OS << "  0,\n";
  //  for (const auto &RE : Regs) {
  //    Record *Reg = RE.TheDef;
  //    BitsInit *BI = Reg->getValueAsBitsInit("HWEncoding");
  //    uint64_t Value = 0;
  //    for (unsigned b = 0, be = BI->getNumBits(); b != be; ++b) {
  //      if (BitInit *B = dyn_cast<BitInit>(BI->getBit(b)))
  //        Value |= (uint64_t)B->getValue() << b;
  //    }
  //    OS << "  " << Value << ",\n";
  //  }
  //  OS << "};\n";       // End of HW encoding table

  // MCRegisterInfo initialization routine.
  //  OS << "static inline void Init" << TargetName
  //     << "MCRegisterInfo(MCRegisterInfo *RI, unsigned RA, "
  //     << "unsigned DwarfFlavour = 0, unsigned EHFlavour = 0, unsigned PC = 0)
  //     "
  //        "{\n"
  //     << "  RI->InitMCRegisterInfo(" << TargetName << "RegDesc, "
  //     << Regs.size() + 1 << ", RA, PC, " << TargetName << "MCRegisterClasses,
  //     "
  //     << RegisterClasses.size() << ", " << TargetName << "RegUnitRoots, "
  //     << RegBank.getNumNativeRegUnits() << ", " << TargetName <<
  //     "RegDiffLists, "
  //     << TargetName << "LaneMaskLists, " << TargetName << "RegStrings, "
  //     << TargetName << "RegClassStrings, " << TargetName << "SubRegIdxLists,
  //     "
  //     << (std::distance(SubRegIndices.begin(), SubRegIndices.end()) + 1) <<
  //     ",\n"
  //     << TargetName << "SubRegIdxRanges, " << TargetName
  //     << "RegEncodingTable);\n\n";
  //
  //  //FIXME we don't need this.. probably EmitRegMapping(OS, Regs, false);
  //
  //  OS << "}\n\n";
  //
  //  OS << "} // end namespace llvm\n\n";
  OS << "#endif // GET_REGINFO_MC_DESC\n\n";
}

void CapstoneGenInfo::EmitPrintAliasInstruction(raw_ostream &O) {
  Record *AsmWriter = Target.getAsmWriter();

  O << "\n#ifdef PRINT_ALIAS_INSTR\n";
  O << "#undef PRINT_ALIAS_INSTR\n\n";

  //////////////////////////////
  // Gather information about aliases we need to print
  //////////////////////////////

  // Emit the method that prints the alias instruction.
  StringRef ClassName = AsmWriter->getValueAsString("AsmWriterClassName");
  unsigned Variant = AsmWriter->getValueAsInt("Variant");
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  std::vector<Record *> AllInstAliases =
      RK.getAllDerivedDefinitions("InstAlias");

  // Create a map from the qualified name to a list of potential matches.
  typedef std::set<std::pair<CodeGenInstAlias, int>, AliasPriorityComparator>
      AliasWithPriority;
  std::map<std::string, AliasWithPriority> AliasMap;
  for (Record *R : AllInstAliases) {
    int Priority = R->getValueAsInt("EmitPriority");
    if (Priority < 1)
      continue; // Aliases with priority 0 are never emitted.

    const DagInit *DI = R->getValueAsDag("ResultInst");
    AliasMap[getQualifiedName(DI->getOperatorAsDef(R->getLoc()))].insert(
        std::make_pair(CodeGenInstAlias(R, Target), Priority));
  }

  // A map of which conditions need to be met for each instruction operand
  // before it can be matched to the mnemonic.
  std::map<std::string, std::vector<IAPrinter>> IAPrinterMap;

  std::vector<std::pair<std::string, bool>> PrintMethods;

  // A list of MCOperandPredicates for all operands in use, and the reverse map
  std::vector<const Record *> MCOpPredicates;
  DenseMap<const Record *, unsigned> MCOpPredicateMap;

  for (auto &Aliases : AliasMap) {
    // Collection of instruction alias rules. May contain ambiguous rules.
    std::vector<IAPrinter> IAPs;

    for (auto &Alias : Aliases.second) {
      const CodeGenInstAlias &CGA = Alias.first;
      unsigned LastOpNo = CGA.ResultInstOperandIndex.size();
      std::string FlatInstAsmString =
          CodeGenInstruction::FlattenAsmStringVariants(
              CGA.ResultInst->AsmString, Variant);
      unsigned NumResultOps = CountNumOperands(FlatInstAsmString, Variant);

      std::string FlatAliasAsmString =
          CodeGenInstruction::FlattenAsmStringVariants(CGA.AsmString, Variant);
      UnescapeAliasString(FlatAliasAsmString);

      // Don't emit the alias if it has more operands than what it's aliasing.
      if (NumResultOps < CountNumOperands(FlatAliasAsmString, Variant))
        continue;

      StringRef Namespace = Target.getName();
      const auto &Registers = Target.getRegBank().getRegisters();
      StringRef NamespaceBrief =
          Registers.front().TheDef->getValueAsString("Namespace");
      unsigned NumMIOps = 0;
      for (auto &ResultInstOpnd : CGA.ResultInst->Operands)
        NumMIOps += ResultInstOpnd.MINumOperands;

      IAPrinter IAP(CGA.Result->getAsString(), FlatAliasAsmString, NumMIOps);

      bool CantHandle = false;

      unsigned MIOpNum = 0;
      for (unsigned i = 0, e = LastOpNo; i != e; ++i) {
        // Skip over tied operands as they're not part of an alias declaration.
        auto &Operands = CGA.ResultInst->Operands;
        while (true) {
          unsigned OpNum = Operands.getSubOperandNumber(MIOpNum).first;
          if (Operands[OpNum].MINumOperands == 1 &&
              Operands[OpNum].getTiedRegister() != -1) {
            // Tied operands of different RegisterClass should be explicit
            // within an instruction's syntax and so cannot be skipped.
            int TiedOpNum = Operands[OpNum].getTiedRegister();
            if (Operands[OpNum].Rec->getName() ==
                Operands[TiedOpNum].Rec->getName()) {
              ++MIOpNum;
              continue;
            }
          }
          break;
        }

        // Ignore unchecked result operands.
        while (IAP.getCondCount() < MIOpNum)
          IAP.addCond("AliasPatternCond_K_Ignore, 0");

        const CodeGenInstAlias::ResultOperand &RO = CGA.ResultOperands[i];

        switch (RO.Kind) {
        case CodeGenInstAlias::ResultOperand::K_Record: {
          const Record *Rec = RO.getRecord();
          StringRef ROName = RO.getName();
          int PrintMethodIdx = -1;

          // These two may have a PrintMethod, which we want to record (if it's
          // the first time we've seen it) and provide an index for the aliasing
          // code to use.
          if (Rec->isSubClassOf("RegisterOperand") ||
              Rec->isSubClassOf("Operand")) {
            StringRef PrintMethod = Rec->getValueAsString("PrintMethod");
            bool IsPCRel =
                Rec->getValueAsString("OperandType") == "OPERAND_PCREL";
            if (PrintMethod != "" && PrintMethod != "printOperand") {
              PrintMethodIdx = llvm::find_if(PrintMethods,
                                             [&](auto &X) {
                                               return X.first == PrintMethod;
                                             }) -
                               PrintMethods.begin();
              if (static_cast<unsigned>(PrintMethodIdx) == PrintMethods.size())
                PrintMethods.emplace_back(std::string(PrintMethod), IsPCRel);
            }
          }

          if (Rec->isSubClassOf("RegisterOperand"))
            Rec = Rec->getValueAsDef("RegClass");
          if (Rec->isSubClassOf("RegisterClass")) {
            if (!IAP.isOpMapped(ROName)) {
              IAP.addOperand(ROName, MIOpNum, PrintMethodIdx);
              Record *R = CGA.ResultOperands[i].getRecord();
              if (R->isSubClassOf("RegisterOperand"))
                R = R->getValueAsDef("RegClass");
              IAP.addCond(std::string(
                  formatv("AliasPatternCond_K_RegClass, {0}_{1}RegClassID",
                          NamespaceBrief, R->getName())));
            } else {
              IAP.addCond(std::string(formatv("AliasPatternCond_K_TiedReg, {0}",
                                              IAP.getOpIndex(ROName))));
            }
          } else {
            // Assume all printable operands are desired for now. This can be
            // overridden in the InstAlias instantiation if necessary.
            IAP.addOperand(ROName, MIOpNum, PrintMethodIdx);

            // There might be an additional predicate on the MCOperand
            unsigned Entry = MCOpPredicateMap[Rec];
            if (!Entry) {
              if (!Rec->isValueUnset("MCOperandPredicate")) {
                MCOpPredicates.push_back(Rec);
                Entry = MCOpPredicates.size();
                MCOpPredicateMap[Rec] = Entry;
              } else
                break; // No conditions on this operand at all
            }
            IAP.addCond(
                std::string(formatv("AliasPatternCond_K_Custom, {0}", Entry)));
          }
          break;
        }
        case CodeGenInstAlias::ResultOperand::K_Imm: {
          // Just because the alias has an immediate result, doesn't mean the
          // MCInst will. An MCExpr could be present, for example.
          auto Imm = CGA.ResultOperands[i].getImm();
          int32_t Imm32 = int32_t(Imm);
          if (Imm != Imm32)
            PrintFatalError("Matching an alias with an immediate out of the "
                            "range of int32_t is not supported");
          IAP.addCond(std::string(
              formatv("AliasPatternCond_K_Imm, (uint32_t){0}", Imm32)));
          break;
        }
        case CodeGenInstAlias::ResultOperand::K_Reg:
          // If this is zero_reg, something's playing tricks we're not
          // equipped to handle.
          if (!CGA.ResultOperands[i].getRegister()) {
            CantHandle = true;
            break;
          }

          StringRef Reg = CGA.ResultOperands[i].getRegister()->getName();
          IAP.addCond(std::string(
              formatv("AliasPatternCond_K_Reg, {0}_{1}", NamespaceBrief, Reg)));
          break;
        }

        MIOpNum += RO.getMINumOperands();
      }

      if (CantHandle)
        continue;

      std::vector<Record *> ReqFeatures;
      if (PassSubtarget) {
        // We only consider ReqFeatures predicates if PassSubtarget
        std::vector<Record *> RF =
            CGA.TheDef->getValueAsListOfDefs("Predicates");
        copy_if(RF, std::back_inserter(ReqFeatures), [](Record *R) {
          return R->getValueAsBit("AssemblerMatcherPredicate");
        });
      }

      for (auto I = ReqFeatures.cbegin(); I != ReqFeatures.cend(); I++) {
        Record *R = *I;
        const DagInit *D = R->getValueAsDag("AssemblerCondDag");
        std::string CombineType = D->getOperator()->getAsString();
        if (CombineType != "any_of" && CombineType != "all_of")
          PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
        if (D->getNumArgs() == 0)
          PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
        bool IsOr = CombineType == "any_of";

        for (auto *Arg : D->getArgs()) {
          bool IsNeg = false;
          if (auto *NotArg = dyn_cast<DagInit>(Arg)) {
            if (NotArg->getOperator()->getAsString() != "not" ||
                NotArg->getNumArgs() != 1)
              PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
            Arg = NotArg->getArg(0);
            IsNeg = true;
          }
          if (!isa<DefInit>(Arg) ||
              !cast<DefInit>(Arg)->getDef()->isSubClassOf("SubtargetFeature"))
            PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");

          IAP.addCond(std::string(formatv(
              "AliasPatternCond_K_{0}{1}Feature, {2}_{3}", IsOr ? "Or" : "",
              IsNeg ? "Neg" : "", Namespace, Arg->getAsString())));
        }
        // If an AssemblerPredicate with ors is used, note end of list should
        // these be combined.
        if (IsOr)
          IAP.addCond("AliasPatternCond_K_EndOrFeatures, 0");
      }

      IAPrinterMap[Aliases.first].push_back(std::move(IAP));
    }
  }

  //////////////////////////////
  // Write out the printAliasInstr function
  //////////////////////////////

  std::string Header;
  raw_string_ostream HeaderO(Header);

  HeaderO << "void printCustomAliasOperand(\n"
          << "         const MCInst *MI, unsigned OpIdx,\n"
          << "         unsigned PrintMethodIdx,\n"
          << "         SStream *OS);\n\n";

  HeaderO << "static char* printAliasInstr(MCInst *MI, "
          << "SStream *OS) {\n";

  std::string PatternsForOpcode;
  raw_string_ostream OpcodeO(PatternsForOpcode);

  unsigned PatternCount = 0;
  std::string Patterns;
  raw_string_ostream PatternO(Patterns);

  unsigned CondCount = 0;
  std::string Conds;
  raw_string_ostream CondO(Conds);

  // All flattened alias strings.
  std::map<std::string, uint32_t> AsmStringOffsets;
  std::vector<std::pair<uint32_t, std::string>> AsmStrings;
  size_t AsmStringsSize = 0;

  unsigned OpCodeCounts = 0;

  // Iterate over the opcodes in enum order so they are sorted by opcode for
  // binary search.
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    auto It = IAPrinterMap.find(getQualifiedName(Inst->TheDef));
    if (It == IAPrinterMap.end())
      continue;
    std::vector<IAPrinter> &IAPs = It->second;
    std::vector<IAPrinter *> UniqueIAPs;

    // Remove any ambiguous alias rules.
    for (auto &LHS : IAPs) {
      bool IsDup = false;
      for (const auto &RHS : IAPs) {
        if (&LHS != &RHS && LHS == RHS) {
          IsDup = true;
          break;
        }
      }

      if (!IsDup)
        UniqueIAPs.push_back(&LHS);
    }

    if (UniqueIAPs.empty())
      continue;

    unsigned PatternStart = PatternCount;

    // Insert the pattern start and opcode in the pattern list for debugging.
    PatternO << formatv("    // {0} - {1}\n", It->first, PatternStart);

    for (IAPrinter *IAP : UniqueIAPs) {
      // Start each condition list with a comment of the resulting pattern that
      // we're trying to match.
      unsigned CondStart = CondCount;
      CondO << formatv("    // {0} - {1}\n", IAP->getResult(), CondStart);
      for (const auto &Cond : IAP->getConds())
        CondO << "    {" << Cond << "},\n";
      CondCount += IAP->getCondCount();

      // After operands have been examined, re-encode the alias string with
      // escapes indicating how operands should be printed.
      uint32_t UnescapedSize = 0;
      std::string EncodedAsmString = IAP->formatAliasString(UnescapedSize);
      auto Insertion =
          AsmStringOffsets.insert({EncodedAsmString, AsmStringsSize});
      if (Insertion.second) {
        // If the string is new, add it to the vector.
        AsmStrings.push_back({AsmStringsSize, EncodedAsmString});
        AsmStringsSize += UnescapedSize + 1;
      }
      unsigned AsmStrOffset = Insertion.first->second;

      PatternO << formatv("    {{{0}, {1}, {2}, {3} },\n", AsmStrOffset,
                          CondStart, IAP->getNumMIOps(), IAP->getCondCount());
      ++PatternCount;
    }

    auto QualifiedName = It->first;
    auto pos = QualifiedName.find("::");
    QualifiedName =
        QualifiedName.substr(0, pos) + "_" + QualifiedName.substr(pos + 2);

    OpcodeO << formatv("    {{{0}, {1}, {2} },\n", QualifiedName, PatternStart,
                       PatternCount - PatternStart);
    OpCodeCounts++;
  }

  if (OpcodeO.str().empty()) {
    O << HeaderO.str();
    O << "  return false;\n";
    O << "}\n\n";
    O << "#endif // PRINT_ALIAS_INSTR\n";
    return;
  }
  // TODO this is now considered unnecessary
  MCOpPredicates.clear();

  // Forward declare the validation method if needed.
  if (!MCOpPredicates.empty())
    O << "static bool " << Target.getName() << ClassName
      << "ValidateMCOperand(MCOperand *MCOp,\n"
      << "                  unsigned PredicateIndex);\n";

  O << HeaderO.str();
  O.indent(2) << "static const PatternsForOpcode OpToPatterns[] = {\n";
  O << OpcodeO.str();
  O.indent(2) << "};\n\n";
  O.indent(2) << "static const AliasPattern Patterns[] = {\n";
  O << PatternO.str();
  O.indent(2) << "};\n\n";
  O.indent(2) << "static const AliasPatternCond Conds[] = {\n";
  O << CondO.str();
  O.indent(2) << "};\n\n";
  O.indent(2) << "static const char *AsmStrings[] = {\n";
  for (const auto &P : AsmStrings) {
    O.indent(4) << "/* " << P.first << " */ \"" << P.second << "\\0\"\n";
  }

  O.indent(2) << "};\n\n";

  O.indent(2) << "const char *AsmString = "
              << "MCInstPrinter_matchAliasPatterns(MI, OpToPatterns, Patterns, "
                 "Conds, AsmStrings, "
              << OpCodeCounts << ");\n";
  O.indent(2) << "if (!AsmString) return false;\n\n";

  // Code that prints the alias, replacing the operands with the ones from the
  // MCInst.
  O << "  char* tmpString = cs_strdup(AsmString);\n"
    << "\n"
    << "  unsigned I = 0;\n"
    << "  while (AsmString[I] != ' ' && AsmString[I] != '\\t' &&\n"
    << "         AsmString[I] != '$' && AsmString[I] != '\\0')\n"
    << "    ++I;\n"
    << "\n"
    << "  tmpString[I] = 0;\n"
    << "  SStream_concat0(OS, tmpString);\n"
    << "\n"
    << "  if (AsmString[I] != '\\0') {\n"
    << "    if (AsmString[I] == ' ' || AsmString[I] == '\\t') {\n"
    << "      SStream_concat0(OS, \"\\t\");\n"
    << "      ++I;\n"
    << "    }\n"
    << "    do {\n"
    << "      if (AsmString[I] == '$') {\n"
    << "        ++I;\n"
    << "        if (AsmString[I] == (char)0xff) {\n"
    << "          ++I;\n"
    << "          int OpIdx = AsmString[I++] - 1;\n"
    << "          int PrintMethodIdx = AsmString[I++] - 1;\n"
    << "          printCustomAliasOperand(MI, OpIdx, PrintMethodIdx, OS);\n"
    << "        } else\n"
    << "          printOperand(MI, ((unsigned)AsmString[I++]) - 1, OS);\n"
    << "      } else {\n"
    << "        SStream_concat1(OS, *(tmpString + (I++)));\n"
    << "      }\n"
    << "    } while (AsmString[I] != '\\0');\n"
    << "  }\n"
    << "\n"
    << "  return tmpString;\n";
  O << "}\n\n";

  //////////////////////////////
  // Write out the printCustomAliasOperand function
  //////////////////////////////

  O << "void printCustomAliasOperand(\n"
    << "         const MCInst *MI, unsigned OpIdx,\n"
    << "         unsigned PrintMethodIdx,\n"
    << "         SStream *OS) {\n";
  if (PrintMethods.empty())
    O << "  llvm_unreachable(\"Unknown PrintMethod kind\");\n";
  else {
    O << "  switch (PrintMethodIdx) {\n"
      << "  default:\n"
      << "    llvm_unreachable(\"Unknown PrintMethod kind\");\n"
      << "    break;\n";

    for (unsigned i = 0; i < PrintMethods.size(); ++i) {
      std::string BaseStr = PrintMethods[i].first;
      O << "// " << BaseStr << "\n";
      // this turns out to be harmful - don't use !
      //      if (BaseStr.find("printUImm") != std::string::npos)
      //        BaseStr = "printUnsignedImm";
      std::string Template = extractTemplate(BaseStr);
      O << "  case " << i << ":\n"
        << "    " << BaseStr << "(MI, "
        << "OpIdx, "
           /*<< (PassSubtarget ? "STI, " : "") <<*/ "OS"
        << Template << ");\n"
        << "    break;\n";
    }
    O << "  }\n";
  }
  O << "}\n\n";

  if (!MCOpPredicates.empty()) {
    O << "static bool " << Target.getName() << ClassName
      << "ValidateMCOperand(const MCOperand &MCOp,\n"
      << "                  const MCSubtargetInfo &STI,\n"
      << "                  unsigned PredicateIndex) {\n"
      << "  switch (PredicateIndex) {\n"
      << "  default:\n"
      << "    llvm_unreachable(\"Unknown MCOperandPredicate kind\");\n"
      << "    break;\n";

    for (unsigned i = 0; i < MCOpPredicates.size(); ++i) {
      StringRef MCOpPred =
          MCOpPredicates[i]->getValueAsString("MCOperandPredicate");
      O << "  case " << i + 1 << ": {\n"
        << MCOpPred.data() << "\n"
        << "    }\n";
    }
    O << "  }\n"
      << "}\n\n";
  }

  O << "#endif // PRINT_ALIAS_INSTR\n";
}

//
// Printer related methods
//

void CapstoneGenInfo::FindUniqueOperandCommands(
    std::vector<std::string> &UniqueOperandCommands,
    std::vector<std::vector<unsigned>> &InstIdxs,
    std::vector<unsigned> &InstOpsUsed, bool PassSubtarget) const {
  // This vector parallels UniqueOperandCommands, keeping track of which
  // instructions each case are used for.  It is a comma separated string of
  // enums.
  std::vector<std::string> InstrsForCase;
  InstrsForCase.resize(UniqueOperandCommands.size());
  InstOpsUsed.assign(UniqueOperandCommands.size(), 0);

  for (size_t i = 0, e = Instructions.size(); i != e; ++i) {
    const AsmWriterInst &Inst = Instructions[i];
    if (Inst.Operands.empty())
      continue; // Instruction already done.

    std::string Command =
        "    " + getCode(Inst.Operands[0], PassSubtarget) + "\n";

    // Check to see if we already have 'Command' in UniqueOperandCommands.
    // If not, add it.
    auto I = llvm::find(UniqueOperandCommands, Command);
    if (I != UniqueOperandCommands.end()) {
      size_t idx = I - UniqueOperandCommands.begin();
      InstrsForCase[idx] += ", ";
      InstrsForCase[idx] += Inst.CGI->TheDef->getName();
      InstIdxs[idx].push_back(i);
    } else {
      UniqueOperandCommands.push_back(std::move(Command));
      InstrsForCase.push_back(std::string(Inst.CGI->TheDef->getName()));
      InstIdxs.emplace_back();
      InstIdxs.back().push_back(i);

      // This command matches one operand so far.
      InstOpsUsed.push_back(1);
    }
  }

  // For each entry of UniqueOperandCommands, there is a set of instructions
  // that uses it.  If the next command of all instructions in the set are
  // identical, fold it into the command.
  for (size_t CommandIdx = 0, e = UniqueOperandCommands.size(); CommandIdx != e;
       ++CommandIdx) {

    const auto &Idxs = InstIdxs[CommandIdx];

    for (unsigned Op = 1;; ++Op) {
      // Find the first instruction in the set.
      const AsmWriterInst &FirstInst = Instructions[Idxs.front()];
      // If this instruction has no more operands, we isn't anything to merge
      // into this command.
      if (FirstInst.Operands.size() == Op)
        break;

      // Otherwise, scan to see if all of the other instructions in this command
      // set share the operand.
      if (any_of(drop_begin(Idxs), [&](unsigned Idx) {
            const AsmWriterInst &OtherInst = Instructions[Idx];
            return OtherInst.Operands.size() == Op ||
                   OtherInst.Operands[Op] != FirstInst.Operands[Op];
          }))
        break;

      // Okay, everything in this command set has the same next operand.  Add it
      // to UniqueOperandCommands and remember that it was consumed.
      std::string Command =
          "    " + getCode(FirstInst.Operands[Op], PassSubtarget) + "\n";

      UniqueOperandCommands[CommandIdx] += Command;
      InstOpsUsed[CommandIdx]++;
    }
  }

  // Prepend some of the instructions each case is used for onto the case val.
  for (unsigned i = 0, e = InstrsForCase.size(); i != e; ++i) {
    std::string Instrs = InstrsForCase[i];
    if (Instrs.size() > 70) {
      Instrs.erase(Instrs.begin() + 70, Instrs.end());
      Instrs += "...";
    }

    if (!Instrs.empty())
      UniqueOperandCommands[i] =
          "    // " + Instrs + "\n" + UniqueOperandCommands[i];
  }
}

void CapstoneGenInfo::EmitGetMnemonic(
    raw_ostream &O,
    std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
    unsigned &BitsLeft, unsigned &AsmStrBits) {
  Record *AsmWriter = Target.getAsmWriter();
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  O << "typedef struct MCMnemonic {\n"
    << "    const char *first;\n"
    << "    uint64_t second;\n"
    << "} MCMnemonic;\n"
    << "\n"
    << "static MCMnemonic createMnemonic(const char* first, uint64_t second) "
       "{\n"
    << "    MCMnemonic mnemonic;\n"
    << "    mnemonic.first = first;\n"
    << "    mnemonic.second = second;\n"
    << "    return mnemonic;\n"
    << "}\n\n";

  O << "/// getMnemonic - This method is automatically generated by "
       "tablegen\n"
       "/// from the instruction set description.\n"
       "MCMnemonic "
    << Target.getName() << "_getMnemonic(const MCInst *MI) {\n";

  // Build an aggregate string, and build a table of offsets into it.
  SequenceToOffsetTable<std::string> StringTable;

  /// OpcodeInfo - This encodes the index of the string to use for the first
  /// chunk of the output as well as indices used for operand printing.
  std::vector<uint64_t> OpcodeInfo(NumberedInstructions.size());
  const unsigned OpcodeInfoBits = 64;

  // Add all strings to the string table upfront so it can generate an optimized
  // representation.
  for (AsmWriterInst &AWI : Instructions) {
    if (AWI.Operands[0].OperandType == AsmWriterOperand::isLiteralTextOperand &&
        !AWI.Operands[0].Str.empty()) {
      std::string Str = AWI.Operands[0].Str;
      UnescapeString(Str);
      StringTable.add(Str);
    }
  }

  StringTable.layout();

  unsigned MaxStringIdx = 0;
  for (AsmWriterInst &AWI : Instructions) {
    unsigned Idx;
    if (AWI.Operands[0].OperandType != AsmWriterOperand::isLiteralTextOperand ||
        AWI.Operands[0].Str.empty()) {
      // Something handled by the asmwriter printer, but with no leading string.
      Idx = StringTable.get("");
    } else {
      std::string Str = AWI.Operands[0].Str;
      UnescapeString(Str);
      Idx = StringTable.get(Str);
      MaxStringIdx = std::max(MaxStringIdx, Idx);

      // Nuke the string from the operand list.  It is now handled!
      AWI.Operands.erase(AWI.Operands.begin());
    }

    // Bias offset by one since we want 0 as a sentinel.
    OpcodeInfo[AWI.CGIIndex] = Idx + 1;
  }

  // Figure out how many bits we used for the string index.
  AsmStrBits = Log2_32_Ceil(MaxStringIdx + 2);

  // To reduce code size, we compactify common instructions into a few bits
  // in the opcode-indexed table.
  BitsLeft = OpcodeInfoBits - AsmStrBits;

  while (true) {
    std::vector<std::string> UniqueOperandCommands;
    std::vector<std::vector<unsigned>> InstIdxs;
    std::vector<unsigned> NumInstOpsHandled;
    FindUniqueOperandCommands(UniqueOperandCommands, InstIdxs,
                              NumInstOpsHandled, PassSubtarget);

    // If we ran out of operands to print, we're done.
    if (UniqueOperandCommands.empty())
      break;

    // Compute the number of bits we need to represent these cases, this is
    // ceil(log2(numentries)).
    unsigned NumBits = Log2_32_Ceil(UniqueOperandCommands.size());

    // If we don't have enough bits for this operand, don't include it.
    if (NumBits > BitsLeft) {
      LLVM_DEBUG(errs() << "Not enough bits to densely encode " << NumBits
                        << " more bits\n");
      break;
    }

    // Otherwise, we can include this in the initial lookup table.  Add it in.
    for (size_t i = 0, e = InstIdxs.size(); i != e; ++i) {
      unsigned NumOps = NumInstOpsHandled[i];
      for (unsigned Idx : InstIdxs[i]) {
        OpcodeInfo[Instructions[Idx].CGIIndex] |=
            (uint64_t)i << (OpcodeInfoBits - BitsLeft);
        // Remove the info about this operand from the instruction.
        AsmWriterInst &Inst = Instructions[Idx];
        if (!Inst.Operands.empty()) {
          assert(NumOps <= Inst.Operands.size() &&
                 "Can't remove this many ops!");
          Inst.Operands.erase(Inst.Operands.begin(),
                              Inst.Operands.begin() + NumOps);
        }
      }
    }
    BitsLeft -= NumBits;

    // Remember the handlers for this set of operands.
    TableDrivenOperandPrinters.push_back(std::move(UniqueOperandCommands));
  }

  // Emit the string table itself.
  StringTable.emitStringLiteralDef(O, "  static const char AsmStrs[]");

  // Emit the lookup tables in pieces to minimize wasted bytes.
  unsigned BytesNeeded = ((OpcodeInfoBits - BitsLeft) + 7) / 8;
  unsigned Table = 0, Shift = 0;
  SmallString<128> BitsString;
  raw_svector_ostream BitsOS(BitsString);
  // If the total bits is more than 32-bits we need to use a 64-bit type.
  BitsOS << "  uint" << ((BitsLeft < (OpcodeInfoBits - 32)) ? 64 : 32)
         << "_t Bits = 0;\n";
  while (BytesNeeded != 0) {
    // Figure out how big this table section needs to be, but no bigger than 4.
    unsigned TableSize = std::min(1 << Log2_32(BytesNeeded), 4);
    BytesNeeded -= TableSize;
    TableSize *= 8; // Convert to bits;
    uint64_t Mask = (1ULL << TableSize) - 1;
    O << "  static const uint" << TableSize << "_t OpInfo" << Table
      << "[] = {\n";
    for (unsigned i = 0, e = NumberedInstructions.size(); i != e; ++i) {
      O << "    " << ((OpcodeInfo[i] >> Shift) & Mask) << "U,\t// "
        << NumberedInstructions[i]->TheDef->getName() << "\n";
    }
    O << "  };\n\n";
    // Emit string to combine the individual table lookups.
    BitsOS << "  Bits |= ";
    // If the total bits is more than 32-bits we need to use a 64-bit type.
    if (BitsLeft < (OpcodeInfoBits - 32))
      BitsOS << "(uint64_t)";
    BitsOS << "OpInfo" << Table << "[MCInst_getOpcode(MI)] << " << Shift
           << ";\n";
    // Prepare the shift for the next iteration and increment the table count.
    Shift += TableSize;
    ++Table;
  }

  O << "  // Emit the opcode for the instruction.\n";
  O << BitsString;

  // Return mnemonic string and bits.
  O << "  return createMnemonic(AsmStrs+(Bits & " << (1 << AsmStrBits) - 1
    << ")-1, Bits);\n\n";

  O << "}\n";
}

/// EmitPrintInstruction - Generate the code for the "printInstruction" method
/// implementation. Destroys all instances of AsmWriterInst information, by
/// clearing the Instructions vector.
void CapstoneGenInfo::EmitPrintInstruction(
    raw_ostream &O,
    std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
    unsigned &BitsLeft, unsigned &AsmStrBits) {
  const unsigned OpcodeInfoBits = 64;
  Record *AsmWriter = Target.getAsmWriter();
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  O << "/// printInstruction - This method is automatically generated by "
       "tablegen\n"
       "/// from the instruction set description.\n"
       "static void printInstruction(MCInst *MI, SStream *O) {\n";

  // Emit the initial tab character.
  // O << "  O << \"\\t\";\n\n";

  // Emit the starting string.
  O << "  MCMnemonic MnemonicInfo =" << Target.getName().str()
    << "_getMnemonic(MI);\n\n";
  O << "#ifndef CAPSTONE_DIET\n\n"
    << "  SStream_concat0(O, MnemonicInfo.first);\n"
    << "#endif\n\n";

  O << "  uint" << ((BitsLeft < (OpcodeInfoBits - 32)) ? 64 : 32)
    << "_t Bits = MnemonicInfo.second;\n"
    << "  assert(Bits != 0 && \"Cannot print this instruction.\");\n";

  // Output the table driven operand information.
  BitsLeft = OpcodeInfoBits - AsmStrBits;
  for (unsigned i = 0, e = TableDrivenOperandPrinters.size(); i != e; ++i) {
    std::vector<std::string> &Commands = TableDrivenOperandPrinters[i];

    // Compute the number of bits we need to represent these cases, this is
    // ceil(log2(numentries)).
    unsigned NumBits = Log2_32_Ceil(Commands.size());
    assert(NumBits <= BitsLeft && "consistency error");

    // Emit code to extract this field from Bits.
    O << "\n  // Fragment " << i << " encoded into " << NumBits << " bits for "
      << Commands.size() << " unique commands.\n";

    if (Commands.size() == 2) {
      // Emit two possibilitys with if/else.
      O << "  if ((Bits >> " << (OpcodeInfoBits - BitsLeft) << ") & "
        << ((1 << NumBits) - 1) << ") {\n"
        << Commands[1] << "  } else {\n"
        << Commands[0] << "  }\n\n";
    } else if (Commands.size() == 1) {
      // Emit a single possibility.
      O << Commands[0] << "\n\n";
    } else {
      O << "  switch ((Bits >> " << (OpcodeInfoBits - BitsLeft) << ") & "
        << ((1 << NumBits) - 1) << ") {\n"
        << "  default: llvm_unreachable(\"Invalid command number.\");\n";

      // Print out all the cases.
      for (unsigned j = 0, e = Commands.size(); j != e; ++j) {
        O << "  case " << j << ":\n";
        O << Commands[j];
        O << "    break;\n";
      }
      O << "  }\n\n";
    }
    BitsLeft -= NumBits;
  }

  // Okay, delete instructions with no operand info left.
  llvm::erase_if(Instructions,
                 [](AsmWriterInst &Inst) { return Inst.Operands.empty(); });

  // Because this is a vector, we want to emit from the end.  Reverse all of the
  // elements in the vector.
  std::reverse(Instructions.begin(), Instructions.end());

  // Now that we've emitted all of the operand info that fit into 64 bits, emit
  // information for those instructions that are left.  This is a less dense
  // encoding, but we expect the main 64-bit table to handle the majority of
  // instructions.
  if (!Instructions.empty()) {
    // Find the opcode # of inline asm.
    O << "  switch (MCInst_getOpcode(MI)) {\n";
    O << "  default: llvm_unreachable(\"Unexpected opcode.\");\n";
    while (!Instructions.empty())
      EmitInstructions(Instructions, O, PassSubtarget);

    O << "  }\n";
  }

  O << "}\n";
}

void CapstoneGenInfo::EmitGetRegisterName(raw_ostream &O) {
  const auto &Registers = Target.getRegBank().getRegisters();
  const std::vector<Record *> &AltNameIndices = Target.getRegAltNameIndices();
  bool hasAltNames = AltNameIndices.size() > 1;
  StringRef Namespace = Registers.front().TheDef->getValueAsString("Namespace");

  O << "\n\n/// getRegisterName - This method is automatically generated by "
       "tblgen\n"
       "/// from the register set description.  This returns the assembler "
       "name\n"
       "/// for the specified register.\n"
       "const char *";
  if (hasAltNames)
    O << "\ngetRegisterName(unsigned RegNo, unsigned AltIdx) {\n";
  else
    O << "getRegisterName(unsigned RegNo) {\n";
  O << "  assert(RegNo && RegNo < " << (Registers.size() + 1)
    << " && \"Invalid register number!\");\n"
    << "\n";

  if (hasAltNames) {
    for (const Record *R : AltNameIndices)
      emitRegisterNameString(O, R->getName(), Registers);
  } else
    emitRegisterNameString(O, "", Registers);

  if (hasAltNames) {
    O << "  switch(AltIdx) {\n"
      << "  default: llvm_unreachable(\"Invalid register alt name index!\");\n";
    for (const Record *R : AltNameIndices) {
      StringRef AltName = R->getName();
      O << "  case ";
      if (!Namespace.empty())
        O << Namespace << "_";
      O << AltName << ":\n";
      if (R->isValueUnset("FallbackRegAltNameIndex"))
        O << "    assert(*(AsmStrs" << AltName << "+RegAsmOffset" << AltName
          << "[RegNo-1]) &&\n"
          << "           \"Invalid alt name index for register!\");\n";
      else {
        O << "    if (!*(AsmStrs" << AltName << "+RegAsmOffset" << AltName
          << "[RegNo-1]))\n"
          << "      return getRegisterName(RegNo, ";
        if (!Namespace.empty())
          O << Namespace << "_";
        O << R->getValueAsDef("FallbackRegAltNameIndex")->getName() << ");\n";
      }
      O << "    return AsmStrs" << AltName << "+RegAsmOffset" << AltName
        << "[RegNo-1];\n";
    }
    O << "  }\n";
  } else {
    O << "  assert (*(AsmStrs+RegAsmOffset[RegNo-1]) &&\n"
      << "          \"Invalid alt name index for register!\");\n"
      << "  return AsmStrs+RegAsmOffset[RegNo-1];\n";
  }
  O << "}\n";
}
