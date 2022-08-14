//===-- CapstoneHelper.cpp - Helper methods -------------------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../SequenceToOffsetTable.h"
#include "CapstoneGenInfo.h"
#include "CapstoneHelper.h"
#include "Types.h"
#include "llvm/TableGen/Error.h"

bool ValueSet(bit_value_t V) {
  return (V == BIT_TRUE || V == BIT_FALSE);
}

bool ValueNotSet(bit_value_t V) { return (V == BIT_UNSET); }

int Value(bit_value_t V) {
  return ValueNotSet(V) ? -1 : (V == BIT_FALSE ? 0 : 1);
}

bit_value_t bitFromBits(const BitsInit &bits, unsigned index) {
  if (BitInit *bit = dyn_cast<BitInit>(bits.getBit(index)))
    return bit->getValue() ? BIT_TRUE : BIT_FALSE;

  // The bit is uninitialized.
  return BIT_UNSET;
}

// Prints the bit value for each position.
void dumpBits(raw_ostream &o, const BitsInit &bits) {
  for (unsigned index = bits.getNumBits(); index > 0; --index) {
    switch (bitFromBits(bits, index - 1)) {
    case BIT_TRUE:
      o << "1";
      break;
    case BIT_FALSE:
      o << "0";
      break;
    case BIT_UNSET:
      o << "_";
      break;
    default:
      llvm_unreachable("unexpected return value from bitFromBits");
    }
  }
}

BitsInit &getBitsField(const Record &def, StringRef str) {
  BitsInit *bits = def.getValueAsBitsInit(str);
  return *bits;
}

void resolveTableFixups(DecoderTable &Table, const FixupList &Fixups,
                               uint32_t DestIdx) {
  // Any NumToSkip fixups in the current scope can resolve to the
  // current location.
  for (FixupList::const_reverse_iterator I = Fixups.rbegin(), E = Fixups.rend();
       I != E; ++I) {
    // Calculate the distance from the byte following the fixup entry byte
    // to the destination. The Target is calculated from after the 16-bit
    // NumToSkip entry itself, so subtract two  from the displacement here
    // to account for that.
    uint32_t FixupIdx = *I;
    uint32_t Delta = DestIdx - FixupIdx - 3;
    // Our NumToSkip entries are 24-bits. Make sure our table isn't too
    // big.
    assert(Delta < (1u << 24));
    Table[FixupIdx] = (uint8_t)Delta;
    Table[FixupIdx + 1] = (uint8_t)(Delta >> 8);
    Table[FixupIdx + 2] = (uint8_t)(Delta >> 16);
  }
}

std::string findOperandDecoderMethod(TypedInit *TI) {
  std::string Decoder;

  Record *Record = cast<DefInit>(TI)->getDef();

  RecordVal *DecoderString = Record->getValue("DecoderMethod");
  StringInit *String =
      DecoderString ? dyn_cast<StringInit>(DecoderString->getValue()) : nullptr;
  if (String) {
    Decoder = std::string(String->getValue());
    if (!Decoder.empty())
      return Decoder;
  }

  if (Record->isSubClassOf("RegisterOperand"))
    Record = Record->getValueAsDef("RegClass");

  if (Record->isSubClassOf("RegisterClass")) {
    Decoder = "Decode" + Record->getName().str() + "RegisterClass";
  } else if (Record->isSubClassOf("PointerLikeRegClass")) {
    Decoder = "DecodePointerLikeRegClass" +
              utostr(Record->getValueAsInt("RegClassKind"));
  }

  return Decoder;
}

bool populateInstruction(CodeGenTarget &Target, const Record &EncodingDef,
                    const CodeGenInstruction &CGI, unsigned Opc,
                    std::map<unsigned, std::vector<OperandInfo>> &Operands) {
  const Record &Def = *CGI.TheDef;
  // If all the bit positions are not specified; do not decode this instruction.
  // We are bound to fail!  For proper disassembly, the well-known encoding bits
  // of the instruction must be fully specified.

  BitsInit &Bits = getBitsField(EncodingDef, "Inst");
  if (Bits.allInComplete())
    return false;

  std::vector<OperandInfo> InsnOperands;

  // If the instruction has specified a custom decoding hook, use that instead
  // of trying to auto-generate the decoder.
  StringRef InstDecoder = EncodingDef.getValueAsString("DecoderMethod");
  if (InstDecoder != "") {
    bool HasCompleteInstDecoder =
        EncodingDef.getValueAsBit("hasCompleteDecoder");
    InsnOperands.push_back(
        OperandInfo(std::string(InstDecoder), HasCompleteInstDecoder));
    Operands[Opc] = InsnOperands;
    return true;
  }

  // Generate a description of the operand of the instruction that we know
  // how to decode automatically.
  // FIXME: We'll need to have a way to manually override this as needed.

  // Gather the outputs/inputs of the instruction, so we can find their
  // positions in the encoding.  This assumes for now that they appear in the
  // MCInst in the order that they're listed.
  std::vector<std::pair<Init *, StringRef>> InOutOperands;
  DagInit *Out = Def.getValueAsDag("OutOperandList");
  DagInit *In = Def.getValueAsDag("InOperandList");
  for (unsigned i = 0; i < Out->getNumArgs(); ++i)
    InOutOperands.push_back(
        std::make_pair(Out->getArg(i), Out->getArgNameStr(i)));
  for (unsigned i = 0; i < In->getNumArgs(); ++i)
    InOutOperands.push_back(
        std::make_pair(In->getArg(i), In->getArgNameStr(i)));

  // Search for tied operands, so that we can correctly instantiate
  // operands that are not explicitly represented in the encoding.
  std::map<std::string, std::string> TiedNames;
  for (unsigned i = 0; i < CGI.Operands.size(); ++i) {
    int tiedTo = CGI.Operands[i].getTiedRegister();
    if (tiedTo != -1) {
      std::pair<unsigned, unsigned> SO =
          CGI.Operands.getSubOperandNumber(tiedTo);
      TiedNames[std::string(InOutOperands[i].second)] =
          std::string(InOutOperands[SO.first].second);
      TiedNames[std::string(InOutOperands[SO.first].second)] =
          std::string(InOutOperands[i].second);
    }
  }

  std::map<std::string, std::vector<OperandInfo>> NumberedInsnOperands;
  std::set<std::string> NumberedInsnOperandsNoTie;
  if (Target.getInstructionSet()->getValueAsBit(
          "decodePositionallyEncodedOperands")) {
    const std::vector<RecordVal> &Vals = Def.getValues();
    unsigned NumberedOp = 0;

    std::set<unsigned> NamedOpIndices;
    if (Target.getInstructionSet()->getValueAsBit(
            "noNamedPositionallyEncodedOperands"))
      // Collect the set of operand indices that might correspond to named
      // operand, and skip these when assigning operands based on position.
      for (unsigned i = 0, e = Vals.size(); i != e; ++i) {
        unsigned OpIdx;
        if (!CGI.Operands.hasOperandNamed(Vals[i].getName(), OpIdx))
          continue;

        NamedOpIndices.insert(OpIdx);
      }

    for (unsigned i = 0, e = Vals.size(); i != e; ++i) {
      // Ignore fixed fields in the record, we're looking for values like:
      //    bits<5> RST = { ?, ?, ?, ?, ? };
      if (Vals[i].isNonconcreteOK() || Vals[i].getValue()->isComplete())
        continue;

      // Determine if Vals[i] actually contributes to the Inst encoding.
      unsigned bi = 0;
      for (; bi < Bits.getNumBits(); ++bi) {
        VarInit *Var = nullptr;
        VarBitInit *BI = dyn_cast<VarBitInit>(Bits.getBit(bi));
        if (BI)
          Var = dyn_cast<VarInit>(BI->getBitVar());
        else
          Var = dyn_cast<VarInit>(Bits.getBit(bi));

        if (Var && Var->getName() == Vals[i].getName())
          break;
      }

      if (bi == Bits.getNumBits())
        continue;

      // Skip variables that correspond to explicitly-named operands.
      unsigned OpIdx;
      if (CGI.Operands.hasOperandNamed(Vals[i].getName(), OpIdx))
        continue;

      // Get the bit range for this operand:
      unsigned bitStart = bi++, bitWidth = 1;
      for (; bi < Bits.getNumBits(); ++bi) {
        VarInit *Var = nullptr;
        VarBitInit *BI = dyn_cast<VarBitInit>(Bits.getBit(bi));
        if (BI)
          Var = dyn_cast<VarInit>(BI->getBitVar());
        else
          Var = dyn_cast<VarInit>(Bits.getBit(bi));

        if (!Var)
          break;

        if (Var->getName() != Vals[i].getName())
          break;

        ++bitWidth;
      }

      unsigned NumberOps = CGI.Operands.size();
      while (NumberedOp < NumberOps &&
             (CGI.Operands.isFlatOperandNotEmitted(NumberedOp) ||
              (!NamedOpIndices.empty() &&
               NamedOpIndices.count(
                   CGI.Operands.getSubOperandNumber(NumberedOp).first))))
        ++NumberedOp;

      OpIdx = NumberedOp++;

      // OpIdx now holds the ordered operand number of Vals[i].
      std::pair<unsigned, unsigned> SO =
          CGI.Operands.getSubOperandNumber(OpIdx);
      const std::string &Name = CGI.Operands[SO.first].Name;

      LLVM_DEBUG(dbgs() << "Numbered operand mapping for " << Def.getName()
                        << ": " << Name << "(" << SO.first << ", " << SO.second
                        << ") => " << Vals[i].getName() << "\n");

      std::string Decoder;
      Record *TypeRecord = CGI.Operands[SO.first].Rec;

      RecordVal *DecoderString = TypeRecord->getValue("DecoderMethod");
      StringInit *String = DecoderString
                               ? dyn_cast<StringInit>(DecoderString->getValue())
                               : nullptr;
      if (String && String->getValue() != "")
        Decoder = std::string(String->getValue());

      if (Decoder == "" && CGI.Operands[SO.first].MIOperandInfo &&
          CGI.Operands[SO.first].MIOperandInfo->getNumArgs()) {
        Init *Arg = CGI.Operands[SO.first].MIOperandInfo->getArg(SO.second);
        if (DefInit *DI = cast<DefInit>(Arg))
          TypeRecord = DI->getDef();
      }

      bool isReg = false;
      if (TypeRecord->isSubClassOf("RegisterOperand"))
        TypeRecord = TypeRecord->getValueAsDef("RegClass");
      if (TypeRecord->isSubClassOf("RegisterClass")) {
        Decoder = "Decode" + TypeRecord->getName().str() + "RegisterClass";
        isReg = true;
      } else if (TypeRecord->isSubClassOf("PointerLikeRegClass")) {
        Decoder = "DecodePointerLikeRegClass" +
                  utostr(TypeRecord->getValueAsInt("RegClassKind"));
        isReg = true;
      }

      DecoderString = TypeRecord->getValue("DecoderMethod");
      String = DecoderString ? dyn_cast<StringInit>(DecoderString->getValue())
                             : nullptr;
      if (!isReg && String && String->getValue() != "")
        Decoder = std::string(String->getValue());

      RecordVal *HasCompleteDecoderVal =
          TypeRecord->getValue("hasCompleteDecoder");
      BitInit *HasCompleteDecoderBit =
          HasCompleteDecoderVal
              ? dyn_cast<BitInit>(HasCompleteDecoderVal->getValue())
              : nullptr;
      bool HasCompleteDecoder =
          HasCompleteDecoderBit ? HasCompleteDecoderBit->getValue() : true;

      OperandInfo OpInfo(Decoder, HasCompleteDecoder);
      OpInfo.addField(bitStart, bitWidth, 0);

      NumberedInsnOperands[Name].push_back(OpInfo);

      // FIXME: For complex operands with custom decoders we can't handle tied
      // sub-operands automatically. Skip those here and assume that this is
      // fixed up elsewhere.
      if (CGI.Operands[SO.first].MIOperandInfo &&
          CGI.Operands[SO.first].MIOperandInfo->getNumArgs() > 1 && String &&
          String->getValue() != "")
        NumberedInsnOperandsNoTie.insert(Name);
    }
  }

  // For each operand, see if we can figure out where it is encoded.
  for (const auto &Op : InOutOperands) {
    if (!NumberedInsnOperands[std::string(Op.second)].empty()) {
      llvm::append_range(InsnOperands,
                         NumberedInsnOperands[std::string(Op.second)]);
      continue;
    }
    if (!NumberedInsnOperands[TiedNames[std::string(Op.second)]].empty()) {
      if (!NumberedInsnOperandsNoTie.count(TiedNames[std::string(Op.second)])) {
        // Figure out to which (sub)operand we're tied.
        unsigned i =
            CGI.Operands.getOperandNamed(TiedNames[std::string(Op.second)]);
        int tiedTo = CGI.Operands[i].getTiedRegister();
        if (tiedTo == -1) {
          i = CGI.Operands.getOperandNamed(Op.second);
          tiedTo = CGI.Operands[i].getTiedRegister();
        }

        if (tiedTo != -1) {
          std::pair<unsigned, unsigned> SO =
              CGI.Operands.getSubOperandNumber(tiedTo);

          InsnOperands.push_back(
              NumberedInsnOperands[TiedNames[std::string(Op.second)]]
                                  [SO.second]);
        }
      }
      continue;
    }

    TypedInit *TI = cast<TypedInit>(Op.first);

    // At this point, we can locate the decoder field, but we need to know how
    // to interpret it.  As a first step, require the target to provide
    // callbacks for decoding register classes.
    std::string Decoder = findOperandDecoderMethod(TI);
    Record *TypeRecord = cast<DefInit>(TI)->getDef();

    RecordVal *HasCompleteDecoderVal =
        TypeRecord->getValue("hasCompleteDecoder");
    BitInit *HasCompleteDecoderBit =
        HasCompleteDecoderVal
            ? dyn_cast<BitInit>(HasCompleteDecoderVal->getValue())
            : nullptr;
    bool HasCompleteDecoder =
        HasCompleteDecoderBit ? HasCompleteDecoderBit->getValue() : true;

    OperandInfo OpInfo(Decoder, HasCompleteDecoder);

    // Some bits of the operand may be required to be 1 depending on the
    // instruction's encoding. Collect those bits.
    if (const RecordVal *EncodedValue = EncodingDef.getValue(Op.second))
      if (const BitsInit *OpBits = dyn_cast<BitsInit>(EncodedValue->getValue()))
        for (unsigned I = 0; I < OpBits->getNumBits(); ++I)
          if (const BitInit *OpBit = dyn_cast<BitInit>(OpBits->getBit(I)))
            if (OpBit->getValue())
              OpInfo.InitValue |= 1ULL << I;

    unsigned Base = ~0U;
    unsigned Width = 0;
    unsigned Offset = 0;

    for (unsigned bi = 0; bi < Bits.getNumBits(); ++bi) {
      VarInit *Var = nullptr;
      VarBitInit *BI = dyn_cast<VarBitInit>(Bits.getBit(bi));
      if (BI)
        Var = dyn_cast<VarInit>(BI->getBitVar());
      else
        Var = dyn_cast<VarInit>(Bits.getBit(bi));

      if (!Var) {
        if (Base != ~0U) {
          OpInfo.addField(Base, Width, Offset);
          Base = ~0U;
          Width = 0;
          Offset = 0;
        }
        continue;
      }

      if (Var->getName() != Op.second &&
          Var->getName() != TiedNames[std::string(Op.second)]) {
        if (Base != ~0U) {
          OpInfo.addField(Base, Width, Offset);
          Base = ~0U;
          Width = 0;
          Offset = 0;
        }
        continue;
      }

      if (Base == ~0U) {
        Base = bi;
        Width = 1;
        Offset = BI ? BI->getBitNum() : 0;
      } else if (BI && BI->getBitNum() != Offset + Width) {
        OpInfo.addField(Base, Width, Offset);
        Base = bi;
        Width = 1;
        Offset = BI->getBitNum();
      } else {
        ++Width;
      }
    }

    if (Base != ~0U)
      OpInfo.addField(Base, Width, Offset);

    if (OpInfo.numFields() > 0)
      InsnOperands.push_back(OpInfo);
  }

  Operands[Opc] = InsnOperands;

#if 0
  LLVM_DEBUG({
      // Dumps the instruction encoding bits.
      dumpBits(errs(), Bits);

      errs() << '\n';

      // Dumps the list of operand info.
      for (unsigned i = 0, e = CGI.Operands.size(); i != e; ++i) {
        const CGIOperandList::OperandInfo &Info = CGI.Operands[i];
        const std::string &OperandName = Info.Name;
        const Record &OperandDef = *Info.Rec;

        errs() << "\t" << OperandName << " (" << OperandDef.getName() << ")\n";
      }
    });
#endif

  return true;
}

void emitFieldFromInstruction(formatted_raw_ostream &OS) {
  OS << "// Helper function for extracting fields from encoded instructions.\n"
        "#define FieldFromInstruction(fname, InsnType) \\\n"
        "static InsnType fname(InsnType insn, unsigned startBit, unsigned "
        "numBits) \\\n"
        "{ \\\n"
        "  InsnType fieldMask; \\\n"
        "  if (numBits == sizeof(InsnType)*8) \\\n"
        "    fieldMask = (InsnType)(-1LL); \\\n"
        "  else \\\n"
        "    fieldMask = (((InsnType)1 << numBits) - 1) << startBit; \\\n"
        "  return (insn & fieldMask) >> startBit; \\\n"
        "}\n\n";
}

// emitDecodeInstruction - Emit the templated helper function
// decodeInstruction().
void emitDecodeInstruction(formatted_raw_ostream &OS) {
  OS << "#define DecodeInstruction(fname, fieldname, decoder, InsnType) \\\n"
        "static DecodeStatus fname(const uint8_t DecodeTable[], MCInst *MI, "
        "\\\n"
        "           InsnType insn, uint64_t Address, MCRegisterInfo *MRI, int "
        "feature) \\\n"
        "{ \\\n"
        "  unsigned Start, Len, NumToSkip, PIdx, Opc, DecodeIdx; \\\n"
        "  InsnType Val, FieldValue, PositiveMask, NegativeMask; \\\n"
        "  bool Pred, Fail, DecodeComplete = true; \\\n"
        "  uint32_t ExpectedValue; \\\n"
        "  const uint8_t *Ptr = DecodeTable; \\\n"
        "  uint32_t CurFieldValue = 0; \\\n"
        "  DecodeStatus S = MCDisassembler_Success; \\\n"
        "  while (true) { \\\n"
        "    switch (*Ptr) { \\\n"
        "    default: \\\n"
        "      return MCDisassembler_Fail; \\\n"
        "    case MCD_OPC_ExtractField: { \\\n"
        "      Start = *++Ptr; \\\n"
        "      Len = *++Ptr; \\\n"
        "      ++Ptr; \\\n"
        "      CurFieldValue = fieldname(insn, Start, Len); \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_FilterValue: { \\\n"
        "      /* Decode the field value. */ \\\n"
        "      Val = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
        "      NumToSkip = *Ptr++; \\\n"
        "      NumToSkip |= (*Ptr++) << 8; \\\n"
        "      NumToSkip |= (*Ptr++) << 16; \\\n"
        "      /* Perform the filter operation. */ \\\n"
        "      if (Val != CurFieldValue) \\\n"
        "        Ptr += NumToSkip; \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_CheckField: { \\\n"
        "      Start = *++Ptr; \\\n"
        "      Len = *++Ptr; \\\n"
        "      FieldValue = fieldname(insn, Start, Len); \\\n"
        "      /* Decode the field value. */ \\\n"
        "      ExpectedValue = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
        "      NumToSkip = *Ptr++; \\\n"
        "      NumToSkip |= (*Ptr++) << 8; \\\n"
        "      NumToSkip |= (*Ptr++) << 16; \\\n"
        "      /* If the actual and expected values don't match, skip. */ \\\n"
        "      if (ExpectedValue != FieldValue) \\\n"
        "        Ptr += NumToSkip; \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_CheckPredicate: { \\\n"
        "      /* Decode the Predicate Index value. */ \\\n"
        "      PIdx = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
        "      NumToSkip = *Ptr++; \\\n"
        "      NumToSkip |= (*Ptr++) << 8; \\\n"
        "      NumToSkip |= (*Ptr++) << 16; \\\n"
        "      /* Check the predicate. */ \\\n"
        "      if (!(Pred = checkDecoderPredicate(PIdx, feature))) \\\n"
        "        Ptr += NumToSkip; \\\n"
        "\t\t/* printf(\"55 PIdx = %u, Pred = %u\\n\", PIdx, Pred); */ \\\n"
        "      (void)Pred; \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_Decode: { \\\n"
        "      /* Decode the Opcode value. */ \\\n"
        "      Opc = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      DecodeIdx = decodeULEB128(Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      MCInst_clear(MI); \\\n"
        "      MCInst_setOpcode(MI, Opc); \\\n"
        "      S = decoder(S, DecodeIdx, insn, MI, Address, &DecodeComplete); "
        "\\\n"
        "      /* assert(DecodeComplete); */ \\\n"
        "      return S; \\\n"
        "    } \\\n"
        "    case MCD_OPC_TryDecode: { \\\n"
        "      /* Decode the Opcode value. */ \\\n"
        "      Opc = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      DecodeIdx = decodeULEB128(Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
        "      NumToSkip = *Ptr++; \\\n"
        "      NumToSkip |= (*Ptr++) << 8; \\\n"
        "      NumToSkip |= (*Ptr++) << 16; \\\n"
        "      /* Perform the decode operation. */ \\\n"
        "      MCInst_setOpcode(MI, Opc); \\\n"
        "      S = decoder(S, DecodeIdx, insn, MI, Address, &DecodeComplete); "
        "\\\n"
        "      if (DecodeComplete) { \\\n"
        "        /* Decoding complete. */ \\\n"
        "        return S; \\\n"
        "      } else { \\\n"
        "        /* assert(S == MCDisassembler_Fail); */ \\\n"
        "        /* If the decoding was incomplete, skip. */ \\\n"
        "        Ptr += NumToSkip; \\\n"
        "        /* Reset decode status. This also drops a SoftFail status "
        "that could be */ \\\n"
        "        /* set before the decode attempt. */ \\\n"
        "        S = MCDisassembler_Success; \\\n"
        "      } \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_SoftFail: { \\\n"
        "      /* Decode the mask values. */ \\\n"
        "      PositiveMask = decodeULEB128(++Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      NegativeMask = decodeULEB128(Ptr, &Len); \\\n"
        "      Ptr += Len; \\\n"
        "      Fail = (insn & PositiveMask) || (~insn & NegativeMask); \\\n"
        "      if (Fail) \\\n"
        "        S = MCDisassembler_SoftFail; \\\n"
        "      break; \\\n"
        "    } \\\n"
        "    case MCD_OPC_Fail: { \\\n"
        "      return MCDisassembler_Fail; \\\n"
        "    } \\\n"
        "    } \\\n"
        "  } \\\n"
        "  /* llvm_unreachable(\"bogosity detected in disassembler state "
        "machine!\");*/  \\\n"
        "}\n";
}

DiffVec &diffEncode(DiffVec &V, unsigned InitVal, SparseBitVector<> List) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);

  for (uint16_t Cur : List) {
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

template <typename Iter>
DiffVec &diffEncode(DiffVec &V, unsigned InitVal, Iter Begin, Iter End) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);
  for (Iter I = Begin; I != End; ++I) {
    uint16_t Cur = (*I)->EnumValue;
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

void printDiff16(raw_ostream &OS, uint16_t Val) { OS << Val; }

void printSubRegIndex(raw_ostream &OS, const CodeGenSubRegIndex *Idx) {
  OS << Idx->EnumValue;
}

void printBitVectorAsHex(raw_ostream &OS, const BitVector &Bits,
                                unsigned Width) {
  assert(Width <= 32 && "Width too large");
  unsigned Digits = (Width + 3) / 4;
  for (unsigned i = 0, e = Bits.size(); i < e; i += Width) {
    unsigned Value = 0;
    for (unsigned j = 0; j != Width && i + j != e; ++j)
      Value |= Bits.test(i + j) << j;
    OS << format("0x%0*x, ", Digits, Value);
  }
}

void BitVectorEmitter::add(unsigned v) {
  if (v >= Values.size())
    Values.resize(((v / 8) + 1) * 8); // Round up to the next byte.
  Values[v] = true;
}

void BitVectorEmitter::print(raw_ostream &OS) {
  printBitVectorAsHex(OS, Values, 8);
}

void emitRegisterNameString(raw_ostream &O, StringRef AltName, const std::deque<CodeGenRegister> &Registers) {
  SequenceToOffsetTable<std::string> StringTable;
  SmallVector<std::string, 4> AsmNames(Registers.size());
  unsigned i = 0;
  for (const auto &Reg : Registers) {
    std::string &AsmName = AsmNames[i++];

    // "NoRegAltName" is special. We don't need to do a lookup for that,
    // as it's just a reference to the default register name.
    if (AltName == "" || AltName == "NoRegAltName") {
      AsmName = std::string(Reg.TheDef->getValueAsString("AsmName"));
      if (AsmName.empty())
        AsmName = std::string(Reg.getName());
    } else {
      // Make sure the register has an alternate name for this index.
      std::vector<Record *> AltNameList =
          Reg.TheDef->getValueAsListOfDefs("RegAltNameIndices");
      unsigned Idx = 0, e;
      for (e = AltNameList.size();
           Idx < e && (AltNameList[Idx]->getName() != AltName); ++Idx)
        ;
      // If the register has an alternate name for this index, use it.
      // Otherwise, leave it empty as an error flag.
      if (Idx < e) {
        std::vector<StringRef> AltNames =
            Reg.TheDef->getValueAsListOfStrings("AltNames");
        if (AltNames.size() <= Idx)
          PrintFatalError(Reg.TheDef->getLoc(),
                          "Register definition missing alt name for '" +
                              AltName + "'.");
        AsmName = std::string(AltNames[Idx]);
      }
    }
    StringTable.add(AsmName);
  }

  StringTable.layout();
  StringTable.emitStringLiteralDef(O, Twine("  static const char AsmStrs") +
                                          AltName + "[]");

  O << "  static const " << getMinimalTypeForRange(StringTable.size() - 1, 32)
    << " RegAsmOffset" << AltName << "[] = {";
  for (unsigned i = 0, e = Registers.size(); i != e; ++i) {
    if ((i % 14) == 0)
      O << "\n    ";
    O << StringTable.get(AsmNames[i]) << ", ";
  }
  O << "\n  };\n"
    << "\n";
}

std::string extractTemplate(std::string &Printer, std::string Op = "") {
  if (Printer.find('<') != std::string::npos) {
    size_t End = Printer.find('>');
    size_t Start = Printer.find('<');
    std::string TemplateVar = Printer.substr(Start + 1, End - Start - 1);

    while (auto Pos = TemplateVar.find("::"))
      if (Pos != std::string::npos)
        TemplateVar =
            TemplateVar.substr(0, Pos) + "_" + TemplateVar.substr(Pos + 2);
      else
        break;

    bool AllBlank = true;
    for (char C : TemplateVar) {
      if (!isspace(C))
        AllBlank = false;
    }

    auto TemplateVarAdd = std::string(", ") + TemplateVar;
    Printer = Printer.substr(0, Start);
    // Return a default `0` for null template
    if (AllBlank)
      return ", 0";
    // Try delegate
    if (TemplateVar == "int8_t" || TemplateVar == "int16_t" ||
        TemplateVar == "int32_t" || TemplateVar == "uint8_t" ||
        TemplateVar == "uint16_t" || TemplateVar == "uint32_t") {
      Printer += "32";
      return "";
    }
    if (TemplateVar == "int64_t" || TemplateVar == "uint64_t") {
      Printer += "64";
      return "";
    }
    return TemplateVarAdd;
  } // dummy patch for default value
  if (Printer.find("printPrefetchOp") != std::string::npos)
    return ", false";
  // dummy patch for default value - sparc
  if (Printer.find("printMemOperand") != std::string::npos && Op.empty())
    return ", \"\"";

  return "";
}

std::string getCode(const AsmWriterOperand &Op, bool PassSubtarget) {
  if (Op.OperandType == Op.isLiteralTextOperand) {
    return "SStream_concat0(O, \"" + Op.Str + "\");";
  }

  if (Op.OperandType == Op.isLiteralStatementOperand)
    return Op.Str;

  std::string StrBase = Op.Str;

  // quite a lot consequences!
  //
  // consequences
  //  if (Op.Str.find("printUImm") != std::string::npos)
  //    StrBase = "printUnsignedImm";

  auto Comment = std::string("/* ") + StrBase + " (+ " + Op.MiModifier + ") */";

  std::string Template =
      extractTemplate(StrBase, Op.MiModifier.empty() ? "" : Op.MiModifier);

  std::string Result = StrBase + Comment + "(MI";
  // FIXME is this correct ?
  //  if (Op.PCRel)
  //    Result += ", Address";
  if (Op.MIOpNo != ~0U)
    Result += ", " + utostr(Op.MIOpNo);
  //  if (PassSubtarget)
  //    Result += ", STI";
  Result += ", O";
  if (!Op.MiModifier.empty())
    Result += ", \"" + Op.MiModifier + '"';
  return Result + Template + ");";
}

void PrintCases(std::vector<std::pair<std::string, AsmWriterOperand>> &OpsToPrint,
           raw_ostream &O, bool PassSubtarget) {
  O << "    case " << OpsToPrint.back().first << ":";
  AsmWriterOperand TheOp = OpsToPrint.back().second;
  OpsToPrint.pop_back();

  // Check to see if any other operands are identical in this list, and if so,
  // emit a case label for them.
  for (unsigned i = OpsToPrint.size(); i != 0; --i)
    if (OpsToPrint[i - 1].second == TheOp) {
      O << "\n    case " << OpsToPrint[i - 1].first << ":";
      OpsToPrint.erase(OpsToPrint.begin() + i - 1);
    }

  // Finally, emit the code.
  O << "\n      " << getCode(TheOp, PassSubtarget);
  O << "\n      break;\n";
}

/// EmitInstructions - Emit the last instruction in the vector and any other
/// instructions that are suitably similar to it.
void EmitInstructions(std::vector<AsmWriterInst> &Insts, raw_ostream &O,
                             bool PassSubtarget) {
  AsmWriterInst FirstInst = Insts.back();
  Insts.pop_back();

  std::vector<AsmWriterInst> SimilarInsts;
  unsigned DifferingOperand = ~0;
  for (unsigned i = Insts.size(); i != 0; --i) {
    unsigned DiffOp = Insts[i - 1].MatchesAllButOneOp(FirstInst);
    if (DiffOp != ~1U) {
      if (DifferingOperand == ~0U) // First match!
        DifferingOperand = DiffOp;

      // If this differs in the same operand as the rest of the instructions in
      // this class, move it to the SimilarInsts list.
      if (DifferingOperand == DiffOp || DiffOp == ~0U) {
        SimilarInsts.push_back(Insts[i - 1]);
        Insts.erase(Insts.begin() + i - 1);
      }
    }
  }

  O << "  case " << FirstInst.CGI->Namespace << "_"
    << FirstInst.CGI->TheDef->getName() << ":\n";
  for (const AsmWriterInst &AWI : SimilarInsts)
    O << "  case " << AWI.CGI->Namespace << "_" << AWI.CGI->TheDef->getName()
      << ":\n";
  for (unsigned i = 0, e = FirstInst.Operands.size(); i != e; ++i) {
    if (i != DifferingOperand) {
      // If the operand is the same for all instructions, just print it.
      O << "    " << getCode(FirstInst.Operands[i], PassSubtarget);
    } else {
      // If this is the operand that varies between all of the instructions,
      // emit a switch for just this operand now.
      O << "    switch (MCInst_getOpcode(MI)) {\n";
      O << "    default: llvm_unreachable(\"Unexpected opcode.\");\n";
      std::vector<std::pair<std::string, AsmWriterOperand>> OpsToPrint;
      OpsToPrint.push_back(
          std::make_pair(FirstInst.CGI->Namespace.str() + "_" +
                             FirstInst.CGI->TheDef->getName().str(),
                         FirstInst.Operands[i]));

      for (const AsmWriterInst &AWI : SimilarInsts) {
        OpsToPrint.push_back(std::make_pair(
            AWI.CGI->Namespace.str() + "_" + AWI.CGI->TheDef->getName().str(),
            AWI.Operands[i]));
      }
      std::reverse(OpsToPrint.begin(), OpsToPrint.end());
      while (!OpsToPrint.empty())
        PrintCases(OpsToPrint, O, PassSubtarget);
      O << "    }";
    }
    O << "\n";
  }
  O << "    break;\n";
}

void UnescapeString(std::string &Str) {
  for (unsigned i = 0; i != Str.size(); ++i) {
    if (Str[i] == '\\' && i != Str.size() - 1) {
      switch (Str[i + 1]) {
      default:
        continue; // Don't execute the code after the switch.
      case 'a':
        Str[i] = '\a';
        break;
      case 'b':
        Str[i] = '\b';
        break;
      case 'e':
        Str[i] = 27;
        break;
      case 'f':
        Str[i] = '\f';
        break;
      case 'n':
        Str[i] = '\n';
        break;
      case 'r':
        Str[i] = '\r';
        break;
      case 't':
        Str[i] = '\t';
        break;
      case 'v':
        Str[i] = '\v';
        break;
      case '"':
        Str[i] = '\"';
        break;
      case '\'':
        Str[i] = '\'';
        break;
      case '\\':
        Str[i] = '\\';
        break;
      }
      // Nuke the second character.
      Str.erase(Str.begin() + i + 1);
    }
  }
}

/// UnescapeAliasString - Supports literal braces in InstAlias asm string which
/// are escaped with '\\' to avoid being interpreted as variants. Braces must
/// be unescaped before c++ code is generated as (e.g.):
///
///   AsmString = "foo \{$\x01\}";
///
/// causes non-standard escape character warnings.
void UnescapeAliasString(std::string &Str) {
  for (unsigned i = 0; i != Str.size(); ++i) {
    if (Str[i] == '\\' && i != Str.size() - 1) {
      switch (Str[i + 1]) {
      default:
        continue; // Don't execute the code after the switch.
      case '{':
        Str[i] = '{';
        break;
      case '}':
        Str[i] = '}';
        break;
      }
      // Nuke the second character.
      Str.erase(Str.begin() + i + 1);
    }
  }
}

unsigned CountNumOperands(StringRef AsmString, unsigned Variant) {
  return AsmString.count(' ') + AsmString.count('\t');
}

