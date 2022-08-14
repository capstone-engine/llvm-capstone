//===-- CapstoneFilterCooser.cpp - Capstone filter chooser methods -\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===--------------------------------------------------------------------------===//

#include "CapstoneHelper.h"
#include "CapstoneGenInfo.h"
#include "llvm/TableGen/Error.h"

void FilterChooser::insnWithID(insn_t &Insn, unsigned Opcode) const {
  BitsInit &Bits = getBitsField(*AllInstructions[Opcode].EncodingDef, "Inst");

  // We may have a SoftFail bitmask, which specifies a mask where an encoding
  // may differ from the value in "Inst" and yet still be valid, but the
  // disassembler should return SoftFail instead of Success.
  //
  // This is used for marking UNPREDICTABLE instructions in the ARM world.
  BitsInit *SFBits =
      AllInstructions[Opcode].EncodingDef->getValueAsBitsInit("SoftFail");

  for (unsigned i = 0; i < BitWidth; ++i) {
    if (SFBits && bitFromBits(*SFBits, i) == BIT_TRUE)
      Insn.push_back(BIT_UNSET);
    else
      Insn.push_back(bitFromBits(Bits, i));
  }
}

void FilterChooser::emitNameWithID(raw_ostream &OS, unsigned Opcode) const {
  const Record *EncodingDef = AllInstructions[Opcode].EncodingDef;
  const Record *InstDef = AllInstructions[Opcode].Inst->TheDef;
  if (EncodingDef != InstDef)
    OS << EncodingDef->getName() << ":";
  OS << InstDef->getName();
}

bool FilterChooser::PositionFiltered(unsigned i) const {
  return ValueSet(FilterBitValues[i]);
}

// Populates the field of the insn given the start position and the number of
// consecutive bits to scan for.
//
// Returns false if and on the first uninitialized bit value encountered.
// Returns true, otherwise.
bool FilterChooser::fieldFromInsn(uint64_t &Field, insn_t &Insn,
                                  unsigned StartBit, unsigned NumBits) const {
  Field = 0;

  for (unsigned i = 0; i < NumBits; ++i) {
    if (Insn[StartBit + i] == BIT_UNSET)
      return false;

    if (Insn[StartBit + i] == BIT_TRUE)
      Field = Field | (1ULL << i);
  }

  return true;
}

/// dumpFilterArray - dumpFilterArray prints out debugging info for the given
/// filter array as a series of chars.
void FilterChooser::dumpFilterArray(
    raw_ostream &o, const std::vector<bit_value_t> &filter) const {
  for (unsigned bitIndex = BitWidth; bitIndex > 0; bitIndex--) {
    switch (filter[bitIndex - 1]) {
    case BIT_UNFILTERED:
      o << ".";
      break;
    case BIT_UNSET:
      o << "_";
      break;
    case BIT_TRUE:
      o << "1";
      break;
    case BIT_FALSE:
      o << "0";
      break;
    }
  }
}

/// dumpStack - dumpStack traverses the filter chooser chain and calls
/// dumpFilterArray on each filter chooser up to the top level one.
void FilterChooser::dumpStack(raw_ostream &o, const char *prefix) const {
  const FilterChooser *current = this;

  while (current) {
    o << prefix;
    dumpFilterArray(o, current->FilterBitValues);
    o << '\n';
    current = current->Parent;
  }
}

// Calculates the island(s) needed to decode the instruction.
// This returns a list of undecoded bits of an instructions, for example,
// Inst{20} = 1 && Inst{3-0} == 0b1111 represents two islands of yet-to-be
// decoded bits in order to verify that the instruction matches the Opcode.
unsigned FilterChooser::getIslands(std::vector<unsigned> &StartBits,
                                   std::vector<unsigned> &EndBits,
                                   std::vector<uint64_t> &FieldVals,
                                   const insn_t &Insn) const {
  unsigned Num, BitNo;
  Num = BitNo = 0;

  uint64_t FieldVal = 0;

  // 0: Init
  // 1: Water (the bit value does not affect decoding)
  // 2: Island (well-known bit value needed for decoding)
  int State = 0;

  for (unsigned i = 0; i < BitWidth; ++i) {
    int64_t Val = Value(Insn[i]);
    bool Filtered = PositionFiltered(i);
    switch (State) {
    default:
      llvm_unreachable("Unreachable code!");
    case 0:
    case 1:
      if (Filtered || Val == -1)
        State = 1; // Still in Water
      else {
        State = 2; // Into the Island
        BitNo = 0;
        StartBits.push_back(i);
        FieldVal = Val;
      }
      break;
    case 2:
      if (Filtered || Val == -1) {
        State = 1; // Into the Water
        EndBits.push_back(i - 1);
        FieldVals.push_back(FieldVal);
        ++Num;
      } else {
        State = 2; // Still in Island
        ++BitNo;
        FieldVal = FieldVal | Val << BitNo;
      }
      break;
    }
  }
  // If we are still in Island after the loop, do some housekeeping.
  if (State == 2) {
    EndBits.push_back(BitWidth - 1);
    FieldVals.push_back(FieldVal);
    ++Num;
  }

  assert(StartBits.size() == Num && EndBits.size() == Num &&
         FieldVals.size() == Num);
  return Num;
}

void FilterChooser::emitBinaryParser(raw_ostream &o, unsigned &Indentation,
                                     const OperandInfo &OpInfo,
                                     bool &OpHasCompleteDecoder) const {
  const std::string &Decoder = OpInfo.Decoder;

  bool UseInsertBits = OpInfo.numFields() != 1 || OpInfo.InitValue != 0;

  if (UseInsertBits) {
    o.indent(Indentation) << "tmp = 0x";
    o.write_hex(OpInfo.InitValue);
    o << ";\\\n";
  }

  for (const EncodingField &EF : OpInfo) {
    o.indent(Indentation);
    if (UseInsertBits)
      o << "tmp |= ";
    else
      o << "tmp = ";
    o << "fieldname(insn, " << EF.Base << ", " << EF.Width << ')';
    if (UseInsertBits)
      o << " << " << EF.Offset;
    else if (EF.Offset != 0)
      o << " << " << EF.Offset;
    o << ";\\\n";
  }

  if (Decoder != "") {
    // Notable code here : try to catch function calls with template,
    // and relocate the template params' position
    if (Decoder.find('<') != std::string::npos) {
      // we decided the params should be replaced
      size_t end = Decoder.find('>');
      size_t start = Decoder.find('<');
      OpHasCompleteDecoder = OpInfo.HasCompleteDecoder;
      o.indent(Indentation)
          << Emitter->GuardPrefix << Decoder.substr(0, start)
          << "(MI, tmp, Address, Decoder, "
          << Decoder.substr(start + 1, end - start - 1) << ")"
          << Emitter->GuardPostfix << " { "
          << (OpHasCompleteDecoder ? "" : "*Decoder = false; ")
          << "return MCDisassembler_Fail; }\\\n";
    } else {
      OpHasCompleteDecoder = OpInfo.HasCompleteDecoder;
      o.indent(Indentation)
          << Emitter->GuardPrefix << Decoder << "(MI, tmp, Address, Decoder)"
          << Emitter->GuardPostfix << " { "
          << (OpHasCompleteDecoder ? "" : "*Decoder = false; ")
          << "return MCDisassembler_Fail; }\\\n";
    }
  } else {
    OpHasCompleteDecoder = true;
    o.indent(Indentation) << "MCOperand_CreateImm0(MI, tmp);\\\n";
  }
}

void FilterChooser::emitDecoder(raw_ostream &OS, unsigned Indentation,
                                unsigned Opc, bool &HasCompleteDecoder) const {
  HasCompleteDecoder = true;

  for (const auto &Op : Operands.find(Opc)->second) {
    // If a custom instruction decoder was specified, use that.
    if (Op.numFields() == 0 && !Op.Decoder.empty()) {
      HasCompleteDecoder = Op.HasCompleteDecoder;
      auto Decoder = Op.Decoder;
      if (Decoder.find('<') != std::string::npos) {
        // we decided the params should be replaced
        size_t end = Decoder.find('>');
        size_t start = Decoder.find('<');
        OS.indent(Indentation)
            << Emitter->GuardPrefix << Decoder.substr(0, start)
            << "(MI, tmp, Address, Decoder, "
            << Decoder.substr(start + 1, end - start - 1) << ")"
            << Emitter->GuardPostfix << " { "
            << (HasCompleteDecoder ? "" : "*Decoder = false; ")
            << "return MCDisassembler_Fail; }\\\n";
      } else {
        OS.indent(Indentation)
            << Emitter->GuardPrefix << (Op.Decoder)
            << "(MI, insn, Address, Decoder)" << Emitter->GuardPostfix << " { "
            << (HasCompleteDecoder ? "" : "*Decoder = false; ")
            << "return MCDisassembler_Fail; }\\\n";
      }
      break;
    }

    bool OpHasCompleteDecoder;
    emitBinaryParser(OS, Indentation, Op, OpHasCompleteDecoder);
    if (!OpHasCompleteDecoder)
      HasCompleteDecoder = false;
  }
}

unsigned FilterChooser::getDecoderIndex(DecoderSet &Decoders, unsigned Opc,
                                        bool &HasCompleteDecoder) const {
  // Build up the predicate string.
  SmallString<256> Decoder;
  // FIXME: emitDecoder() function can take a buffer directly rather than
  // a stream.
  raw_svector_ostream S(Decoder);
  unsigned I = 4;
  emitDecoder(S, I, Opc, HasCompleteDecoder);

  // Using the full decoder string as the key value here is a bit
  // heavyweight, but is effective. If the string comparisons become a
  // performance concern, we can implement a mangling of the predicate
  // data easily enough with a map back to the actual string. That's
  // overkill for now, though.

  // Make sure the predicate is in the table.
  Decoders.insert(CachedHashString(Decoder));
  // Now figure out the index for when we write out the table.
  DecoderSet::const_iterator P = find(Decoders, Decoder.str());
  return (unsigned)(P - Decoders.begin());
}

bool FilterChooser::emitPredicateMatch(raw_ostream &o, unsigned &Indentation,
                                       unsigned Opc) const {
  ListInit *Predicates =
      AllInstructions[Opc].EncodingDef->getValueAsListInit("Predicates");
  bool IsFirstEmission = true;
  for (unsigned i = 0; i < Predicates->size(); ++i) {
    Record *Pred = Predicates->getElementAsRecord(i);
    if (!Pred->getValue("AssemblerMatcherPredicate"))
      continue;

    if (!isa<DagInit>(Pred->getValue("AssemblerCondDag")->getValue()))
      continue;

    const DagInit *D = Pred->getValueAsDag("AssemblerCondDag");
    std::string CombineType = D->getOperator()->getAsString();
    if (CombineType != "any_of" && CombineType != "all_of")
      PrintFatalError(Pred->getLoc(), "Invalid AssemblerCondDag!");
    if (D->getNumArgs() == 0)
      PrintFatalError(Pred->getLoc(), "Invalid AssemblerCondDag!");
    bool IsOr = CombineType == "any_of";

    if (!IsFirstEmission)
      o << " && ";

    if (IsOr)
      o << "(";

    ListSeparator LS(IsOr ? " || " : " && ");
    for (auto *Arg : D->getArgs()) {
      o << LS;
      bool Require = true;
      if (auto *NotArg = dyn_cast<DagInit>(Arg)) {
        if (NotArg->getOperator()->getAsString() != "not" ||
            NotArg->getNumArgs() != 1)
          PrintFatalError(Pred->getLoc(), "Invalid AssemblerCondDag!");
        Arg = NotArg->getArg(0);
        Require = false;
      }
      if (!isa<DefInit>(Arg) ||
          !cast<DefInit>(Arg)->getDef()->isSubClassOf("SubtargetFeature"))
        PrintFatalError(Pred->getLoc(), "Invalid AssemblerCondDag!");
      // Capstone needs to know if this predicate is required or rejected
      o << "checkFeatureRequired(Bits, " << Emitter->PredicateNamespace << "_"
        << Arg->getAsString() << ", " << Require << ")";
    }

    if (IsOr)
      o << ")";

    IsFirstEmission = false;
  }
  return !Predicates->empty();
}

bool FilterChooser::doesOpcodeNeedPredicate(unsigned Opc) const {
  ListInit *Predicates =
      AllInstructions[Opc].EncodingDef->getValueAsListInit("Predicates");
  for (unsigned i = 0; i < Predicates->size(); ++i) {
    Record *Pred = Predicates->getElementAsRecord(i);
    if (!Pred->getValue("AssemblerMatcherPredicate"))
      continue;

    if (isa<DagInit>(Pred->getValue("AssemblerCondDag")->getValue()))
      return true;
  }
  return false;
}

unsigned FilterChooser::getPredicateIndex(DecoderTableInfo &TableInfo,
                                          StringRef Predicate) const {
  // Using the full predicate string as the key value here is a bit
  // heavyweight, but is effective. If the string comparisons become a
  // performance concern, we can implement a mangling of the predicate
  // data easily enough with a map back to the actual string. That's
  // overkill for now, though.

  // Make sure the predicate is in the table.
  TableInfo.Predicates.insert(CachedHashString(Predicate));
  // Now figure out the index for when we write out the table.
  PredicateSet::const_iterator P = find(TableInfo.Predicates, Predicate);
  return (unsigned)(P - TableInfo.Predicates.begin());
}

void FilterChooser::emitPredicateTableEntry(DecoderTableInfo &TableInfo,
                                            unsigned Opc) const {
  if (!doesOpcodeNeedPredicate(Opc))
    return;

  // Build up the predicate string.
  SmallString<256> Predicate;
  // FIXME: emitPredicateMatch() functions can take a buffer directly rather
  // than a stream.
  raw_svector_ostream PS(Predicate);
  unsigned I = 0;
  emitPredicateMatch(PS, I, Opc);

  // Figure out the index into the predicate table for the predicate just
  // computed.
  unsigned PIdx = getPredicateIndex(TableInfo, PS.str());
  SmallString<16> PBytes;
  raw_svector_ostream S(PBytes);
  encodeULEB128(PIdx, S);

  TableInfo.Table.push_back(MCD::OPC_CheckPredicate);
  // Predicate index
  for (unsigned i = 0, e = PBytes.size(); i != e; ++i)
    TableInfo.Table.push_back(PBytes[i]);
  // Push location for NumToSkip backpatching.
  TableInfo.FixupStack.back().push_back(TableInfo.Table.size());
  TableInfo.Table.push_back(0);
  TableInfo.Table.push_back(0);
  TableInfo.Table.push_back(0);
}

void FilterChooser::emitSoftFailTableEntry(DecoderTableInfo &TableInfo,
                                           unsigned Opc) const {
  BitsInit *SFBits =
      AllInstructions[Opc].EncodingDef->getValueAsBitsInit("SoftFail");
  if (!SFBits)
    return;
  BitsInit *InstBits =
      AllInstructions[Opc].EncodingDef->getValueAsBitsInit("Inst");

  APInt PositiveMask(BitWidth, 0ULL);
  APInt NegativeMask(BitWidth, 0ULL);
  for (unsigned i = 0; i < BitWidth; ++i) {
    bit_value_t B = bitFromBits(*SFBits, i);
    bit_value_t IB = bitFromBits(*InstBits, i);

    if (B != BIT_TRUE)
      continue;

    switch (IB) {
    case BIT_FALSE:
      // The bit is meant to be false, so emit a check to see if it is true.
      PositiveMask.setBit(i);
      break;
    case BIT_TRUE:
      // The bit is meant to be true, so emit a check to see if it is false.
      NegativeMask.setBit(i);
      break;
    default:
      // The bit is not set; this must be an error!
      errs() << "SoftFail Conflict: bit SoftFail{" << i << "} in "
             << AllInstructions[Opc] << " is set but Inst{" << i
             << "} is unset!\n"
             << "  - You can only mark a bit as SoftFail if it is fully defined"
             << " (1/0 - not '?') in Inst\n";
      return;
    }
  }

  bool NeedPositiveMask = PositiveMask.getBoolValue();
  bool NeedNegativeMask = NegativeMask.getBoolValue();

  if (!NeedPositiveMask && !NeedNegativeMask)
    return;

  TableInfo.Table.push_back(MCD::OPC_SoftFail);

  SmallString<16> MaskBytes;
  raw_svector_ostream S(MaskBytes);
  if (NeedPositiveMask) {
    encodeULEB128(PositiveMask.getZExtValue(), S);
    for (unsigned i = 0, e = MaskBytes.size(); i != e; ++i)
      TableInfo.Table.push_back(MaskBytes[i]);
  } else
    TableInfo.Table.push_back(0);
  if (NeedNegativeMask) {
    MaskBytes.clear();
    encodeULEB128(NegativeMask.getZExtValue(), S);
    for (unsigned i = 0, e = MaskBytes.size(); i != e; ++i)
      TableInfo.Table.push_back(MaskBytes[i]);
  } else
    TableInfo.Table.push_back(0);
}

// Emits table entries to decode the singleton.
void FilterChooser::emitSingletonTableEntry(DecoderTableInfo &TableInfo,
                                            EncodingIDAndOpcode Opc) const {
  std::vector<unsigned> StartBits;
  std::vector<unsigned> EndBits;
  std::vector<uint64_t> FieldVals;
  insn_t Insn;
  insnWithID(Insn, Opc.EncodingID);

  // Look for islands of undecoded bits of the singleton.
  getIslands(StartBits, EndBits, FieldVals, Insn);

  unsigned Size = StartBits.size();

  // Emit the predicate table entry if one is needed.
  emitPredicateTableEntry(TableInfo, Opc.EncodingID);

  // Check any additional encoding fields needed.
  for (unsigned I = Size; I != 0; --I) {
    unsigned NumBits = EndBits[I - 1] - StartBits[I - 1] + 1;
    TableInfo.Table.push_back(MCD::OPC_CheckField);
    TableInfo.Table.push_back(StartBits[I - 1]);
    TableInfo.Table.push_back(NumBits);
    uint8_t Buffer[16], *p;
    encodeULEB128(FieldVals[I - 1], Buffer);
    for (p = Buffer; *p >= 128; ++p)
      TableInfo.Table.push_back(*p);
    TableInfo.Table.push_back(*p);
    // Push location for NumToSkip backpatching.
    TableInfo.FixupStack.back().push_back(TableInfo.Table.size());
    // The fixup is always 24-bits, so go ahead and allocate the space
    // in the table so all our relative position calculations work OK even
    // before we fully resolve the real value here.
    TableInfo.Table.push_back(0);
    TableInfo.Table.push_back(0);
    TableInfo.Table.push_back(0);
  }

  // Check for soft failure of the match.
  emitSoftFailTableEntry(TableInfo, Opc.EncodingID);

  bool HasCompleteDecoder;
  unsigned DIdx =
      getDecoderIndex(TableInfo.Decoders, Opc.EncodingID, HasCompleteDecoder);

  // Produce OPC_Decode or OPC_TryDecode opcode based on the information
  // whether the instruction decoder is complete or not. If it is complete
  // then it handles all possible values of remaining variable/unfiltered bits
  // and for any value can determine if the bitpattern is a valid instruction
  // or not. This means OPC_Decode will be the final step in the decoding
  // process. If it is not complete, then the Fail return code from the
  // decoder method indicates that additional processing should be done to see
  // if there is any other instruction that also matches the bitpattern and
  // can decode it.
  TableInfo.Table.push_back(HasCompleteDecoder ? MCD::OPC_Decode
                                               : MCD::OPC_TryDecode);
  NumEncodingsSupported++;
  uint8_t Buffer[16], *p;
  encodeULEB128(Opc.Opcode, Buffer);
  for (p = Buffer; *p >= 128; ++p)
    TableInfo.Table.push_back(*p);
  TableInfo.Table.push_back(*p);

  SmallString<16> Bytes;
  raw_svector_ostream S(Bytes);
  encodeULEB128(DIdx, S);

  // Decoder index
  for (unsigned i = 0, e = Bytes.size(); i != e; ++i)
    TableInfo.Table.push_back(Bytes[i]);

  if (!HasCompleteDecoder) {
    // Push location for NumToSkip backpatching.
    TableInfo.FixupStack.back().push_back(TableInfo.Table.size());
    // Allocate the space for the fixup.
    TableInfo.Table.push_back(0);
    TableInfo.Table.push_back(0);
    TableInfo.Table.push_back(0);
  }
}

// Emits table entries to decode the singleton, and then to decode the rest.
void FilterChooser::emitSingletonTableEntry(DecoderTableInfo &TableInfo,
                                            const Filter &Best) const {
  EncodingIDAndOpcode Opc = Best.getSingletonOpc();

  // complex singletons need predicate checks from the first singleton
  // to refer forward to the variable filterchooser that follows.
  TableInfo.FixupStack.emplace_back();

  emitSingletonTableEntry(TableInfo, Opc);

  resolveTableFixups(TableInfo.Table, TableInfo.FixupStack.back(),
                     TableInfo.Table.size());
  TableInfo.FixupStack.pop_back();

  Best.getVariableFC().emitTableEntries(TableInfo);
}

// Assign a single filter and run with it.  Top level API client can initialize
// with a single filter to start the filtering process.
void FilterChooser::runSingleFilter(unsigned startBit, unsigned numBit,
                                    bool mixed) {
  Filters.clear();
  Filters.emplace_back(*this, startBit, numBit, true);
  BestIndex = 0; // Sole Filter instance to choose from.
  bestFilter().recurse();
}

// reportRegion is a helper function for filterProcessor to mark a region as
// eligible for use as a filter region.
void FilterChooser::reportRegion(bitAttr_t RA, unsigned StartBit,
                                 unsigned BitIndex, bool AllowMixed) {
  if (RA == ATTR_MIXED && AllowMixed)
    Filters.emplace_back(*this, StartBit, BitIndex - StartBit, true);
  else if (RA == ATTR_ALL_SET && !AllowMixed)
    Filters.emplace_back(*this, StartBit, BitIndex - StartBit, false);
}

// FilterProcessor scans the well-known encoding bits of the instructions and
// builds up a list of candidate filters.  It chooses the best filter and
// recursively descends down the decoding tree.
bool FilterChooser::filterProcessor(bool AllowMixed, bool Greedy) {
  Filters.clear();
  BestIndex = -1;
  unsigned numInstructions = Opcodes.size();

  assert(numInstructions && "Filter created with no instructions");

  // No further filtering is necessary.
  if (numInstructions == 1)
    return true;

  // Heuristics.  See also doFilter()'s "Heuristics" comment when num of
  // instructions is 3.
  if (AllowMixed && !Greedy) {
    assert(numInstructions == 3);

    for (unsigned i = 0; i < Opcodes.size(); ++i) {
      std::vector<unsigned> StartBits;
      std::vector<unsigned> EndBits;
      std::vector<uint64_t> FieldVals;
      insn_t Insn;

      insnWithID(Insn, Opcodes[i].EncodingID);

      // Look for islands of undecoded bits of any instruction.
      if (getIslands(StartBits, EndBits, FieldVals, Insn) > 0) {
        // Found an instruction with island(s).  Now just assign a filter.
        runSingleFilter(StartBits[0], EndBits[0] - StartBits[0] + 1, true);
        return true;
      }
    }
  }

  unsigned BitIndex;

  // We maintain BIT_WIDTH copies of the bitAttrs automaton.
  // The automaton consumes the corresponding bit from each
  // instruction.
  //
  //   Input symbols: 0, 1, and _ (unset).
  //   States:        NONE, FILTERED, ALL_SET, ALL_UNSET, and MIXED.
  //   Initial state: NONE.
  //
  // (NONE) ------- [01] -> (ALL_SET)
  // (NONE) ------- _ ----> (ALL_UNSET)
  // (ALL_SET) ---- [01] -> (ALL_SET)
  // (ALL_SET) ---- _ ----> (MIXED)
  // (ALL_UNSET) -- [01] -> (MIXED)
  // (ALL_UNSET) -- _ ----> (ALL_UNSET)
  // (MIXED) ------ . ----> (MIXED)
  // (FILTERED)---- . ----> (FILTERED)

  std::vector<bitAttr_t> bitAttrs;

  // FILTERED bit positions provide no entropy and are not worthy of pursuing.
  // Filter::recurse() set either BIT_TRUE or BIT_FALSE for each position.
  for (BitIndex = 0; BitIndex < BitWidth; ++BitIndex)
    if (FilterBitValues[BitIndex] == BIT_TRUE ||
        FilterBitValues[BitIndex] == BIT_FALSE)
      bitAttrs.push_back(ATTR_FILTERED);
    else
      bitAttrs.push_back(ATTR_NONE);

  for (unsigned InsnIndex = 0; InsnIndex < numInstructions; ++InsnIndex) {
    insn_t insn;

    insnWithID(insn, Opcodes[InsnIndex].EncodingID);

    for (BitIndex = 0; BitIndex < BitWidth; ++BitIndex) {
      switch (bitAttrs[BitIndex]) {
      case ATTR_NONE:
        if (insn[BitIndex] == BIT_UNSET)
          bitAttrs[BitIndex] = ATTR_ALL_UNSET;
        else
          bitAttrs[BitIndex] = ATTR_ALL_SET;
        break;
      case ATTR_ALL_SET:
        if (insn[BitIndex] == BIT_UNSET)
          bitAttrs[BitIndex] = ATTR_MIXED;
        break;
      case ATTR_ALL_UNSET:
        if (insn[BitIndex] != BIT_UNSET)
          bitAttrs[BitIndex] = ATTR_MIXED;
        break;
      case ATTR_MIXED:
      case ATTR_FILTERED:
        break;
      }
    }
  }

  // The regionAttr automaton consumes the bitAttrs automatons' state,
  // lowest-to-highest.
  //
  //   Input symbols: F(iltered), (all_)S(et), (all_)U(nset), M(ixed)
  //   States:        NONE, ALL_SET, MIXED
  //   Initial state: NONE
  //
  // (NONE) ----- F --> (NONE)
  // (NONE) ----- S --> (ALL_SET)     ; and set region start
  // (NONE) ----- U --> (NONE)
  // (NONE) ----- M --> (MIXED)       ; and set region start
  // (ALL_SET) -- F --> (NONE)        ; and report an ALL_SET region
  // (ALL_SET) -- S --> (ALL_SET)
  // (ALL_SET) -- U --> (NONE)        ; and report an ALL_SET region
  // (ALL_SET) -- M --> (MIXED)       ; and report an ALL_SET region
  // (MIXED) ---- F --> (NONE)        ; and report a MIXED region
  // (MIXED) ---- S --> (ALL_SET)     ; and report a MIXED region
  // (MIXED) ---- U --> (NONE)        ; and report a MIXED region
  // (MIXED) ---- M --> (MIXED)

  bitAttr_t RA = ATTR_NONE;
  unsigned StartBit = 0;

  for (BitIndex = 0; BitIndex < BitWidth; ++BitIndex) {
    bitAttr_t bitAttr = bitAttrs[BitIndex];

    assert(bitAttr != ATTR_NONE && "Bit without attributes");

    switch (RA) {
    case ATTR_NONE:
      switch (bitAttr) {
      case ATTR_FILTERED:
        break;
      case ATTR_ALL_SET:
        StartBit = BitIndex;
        RA = ATTR_ALL_SET;
        break;
      case ATTR_ALL_UNSET:
        break;
      case ATTR_MIXED:
        StartBit = BitIndex;
        RA = ATTR_MIXED;
        break;
      default:
        llvm_unreachable("Unexpected bitAttr!");
      }
      break;
    case ATTR_ALL_SET:
      switch (bitAttr) {
      case ATTR_FILTERED:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        RA = ATTR_NONE;
        break;
      case ATTR_ALL_SET:
        break;
      case ATTR_ALL_UNSET:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        RA = ATTR_NONE;
        break;
      case ATTR_MIXED:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        StartBit = BitIndex;
        RA = ATTR_MIXED;
        break;
      default:
        llvm_unreachable("Unexpected bitAttr!");
      }
      break;
    case ATTR_MIXED:
      switch (bitAttr) {
      case ATTR_FILTERED:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        StartBit = BitIndex;
        RA = ATTR_NONE;
        break;
      case ATTR_ALL_SET:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        StartBit = BitIndex;
        RA = ATTR_ALL_SET;
        break;
      case ATTR_ALL_UNSET:
        reportRegion(RA, StartBit, BitIndex, AllowMixed);
        RA = ATTR_NONE;
        break;
      case ATTR_MIXED:
        break;
      default:
        llvm_unreachable("Unexpected bitAttr!");
      }
      break;
    case ATTR_ALL_UNSET:
      llvm_unreachable("regionAttr state machine has no ATTR_UNSET state");
    case ATTR_FILTERED:
      llvm_unreachable("regionAttr state machine has no ATTR_FILTERED state");
    }
  }

  // At the end, if we're still in ALL_SET or MIXED states, report a region
  switch (RA) {
  case ATTR_NONE:
    break;
  case ATTR_FILTERED:
    break;
  case ATTR_ALL_SET:
    reportRegion(RA, StartBit, BitIndex, AllowMixed);
    break;
  case ATTR_ALL_UNSET:
    break;
  case ATTR_MIXED:
    reportRegion(RA, StartBit, BitIndex, AllowMixed);
    break;
  }

  // We have finished with the filter processings.  Now it's time to choose
  // the best performing filter.
  BestIndex = 0;
  bool AllUseless = true;
  unsigned BestScore = 0;

  for (unsigned i = 0, e = Filters.size(); i != e; ++i) {
    unsigned Usefulness = Filters[i].usefulness();

    if (Usefulness)
      AllUseless = false;

    if (Usefulness > BestScore) {
      BestIndex = i;
      BestScore = Usefulness;
    }
  }

  if (!AllUseless)
    bestFilter().recurse();

  return !AllUseless;
} // end of FilterChooser::filterProcessor(bool)

// Decides on the best configuration of filter(s) to use in order to decode
// the instructions.  A conflict of instructions may occur, in which case we
// dump the conflict set to the standard error.
void FilterChooser::doFilter() {
  unsigned Num = Opcodes.size();
  assert(Num && "FilterChooser created with no instructions");

  // Try regions of consecutive known bit values first.
  if (filterProcessor(false))
    return;

  // Then regions of mixed bits (both known and unitialized bit values allowed).
  if (filterProcessor(true))
    return;

  // Heuristics to cope with conflict set {t2CMPrs, t2SUBSrr, t2SUBSrs} where
  // no single instruction for the maximum ATTR_MIXED region Inst{14-4} has a
  // well-known encoding pattern.  In such case, we backtrack and scan for the
  // the very first consecutive ATTR_ALL_SET region and assign a filter to it.
  if (Num == 3 && filterProcessor(true, false))
    return;

  // If we come to here, the instruction decoding has failed.
  // Set the BestIndex to -1 to indicate so.
  BestIndex = -1;
}

// emitTableEntries - Emit state machine entries to decode our share of
// instructions.
void FilterChooser::emitTableEntries(DecoderTableInfo &TableInfo) const {
  if (Opcodes.size() == 1) {
    // There is only one instruction in the set, which is great!
    // Call emitSingletonDecoder() to see whether there are any remaining
    // encodings bits.
    emitSingletonTableEntry(TableInfo, Opcodes[0]);
    return;
  }

  // Choose the best filter to do the decodings!
  if (BestIndex != -1) {
    const Filter &Best = Filters[BestIndex];
    if (Best.getNumFiltered() == 1)
      emitSingletonTableEntry(TableInfo, Best);
    else
      Best.emitTableEntry(TableInfo);
    return;
  }

  // We don't know how to decode these instructions!  Dump the
  // conflict set and bail.

  // Print out useful conflict information for postmortem analysis.
  errs() << "Decoding Conflict:\n";

  dumpStack(errs(), "\t\t");

  for (unsigned i = 0; i < Opcodes.size(); ++i) {
    errs() << '\t';
    emitNameWithID(errs(), Opcodes[i].EncodingID);
    errs() << " ";
    dumpBits(errs(),
             getBitsField(*AllInstructions[Opcodes[i].EncodingID].EncodingDef,
                          "Inst"));
    errs() << '\n';
  }
}

