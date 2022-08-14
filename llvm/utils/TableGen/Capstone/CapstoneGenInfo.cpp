//===-- CapstoneGenInfo.cpp - Info Generation Module -----------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CapstoneGenInfo.h"
#include "CapstoneGenMCDesc.h"
#include "CapstoneGenPrinter.h"
#include "CapstoneHelper.h"
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
