//===---------------- DecoderEmitter.cpp - Decoder Generator --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// It contains the tablegen backend that emits the decoder functions for
// targets with fixed/variable length instruction set.
//
//===----------------------------------------------------------------------===//

#include "CodeGenHwModes.h"
#include "CodeGenInstruction.h"
#include "CodeGenTarget.h"
#include "DecoderEmitterTypes.h"
#include "InfoByHwMode.h"
#include "Printer.h"
#include "VarLenCodeEmitterGen.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/CachedHashString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/LEB128.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "decoder-emitter"

namespace {

STATISTIC(NumEncodings, "Number of encodings considered");
STATISTIC(NumEncodingsLackingDisasm, "Number of encodings without disassembler info");
STATISTIC(NumInstructions, "Number of instructions considered");
STATISTIC(NumEncodingsSupported, "Number of encodings supported");
STATISTIC(NumEncodingsOmitted, "Number of encodings omitted");

class DecoderEmitter {
  RecordKeeper &RK;
  const PrinterLLVM &PI;
  std::vector<EncodingAndInst> NumberedEncodings;

  ArrayRef<const CodeGenInstruction *> NumberedInstructions;
  DenseMap<Record *, unsigned> IndexOfInstruction;
  // The opcode map.
  // It maps the DecoderNamespace (e.g. ARM Thumb16) and instruction sizes
  // to the encodings which belong into this subset.
  std::map<std::pair<std::string, unsigned>, std::vector<EncodingIDAndOpcode>>
      OpcMap;
  std::map<unsigned, std::vector<OperandInfo>> Operands;
  std::vector<unsigned> InstrLen;

public:
  // Defaults preserved here for documentation, even though they aren't
  // strictly necessary given the way that this is currently being called.
  DecoderEmitter(RecordKeeper &R, const PrinterLLVM &PI)
      : RK(R), PI(PI), NumberedInstructions(nullptr), Target(R) {}
  void retrieveHwModeEncodings();
  unsigned fillOpcMap(bool const IsVarLenInst);

  // run - Output the code emitter
  void run();

private:
  CodeGenTarget Target;
};

} // end anonymous namespace

static bool ValueSet(bit_value_t V) {
  return (V == BIT_TRUE || V == BIT_FALSE);
}

static bool ValueNotSet(bit_value_t V) {
  return (V == BIT_UNSET);
}

static int Value(bit_value_t V) {
  return ValueNotSet(V) ? -1 : (V == BIT_FALSE ? 0 : 1);
}

static bit_value_t bitFromBits(const BitsInit &bits, unsigned index) {
  if (BitInit *bit = dyn_cast<BitInit>(bits.getBit(index)))
    return bit->getValue() ? BIT_TRUE : BIT_FALSE;

  // The bit is uninitialized.
  return BIT_UNSET;
}

// Prints the bit value for each position.
static void dumpBits(raw_ostream &o, const BitsInit &bits) {
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

static BitsInit &getBitsField(const Record &def, StringRef str) {
  const RecordVal *RV = def.getValue(str);
  if (BitsInit *Bits = dyn_cast<BitsInit>(RV->getValue()))
    return *Bits;

  // variable length instruction
  VarLenInst VLI = VarLenInst(cast<DagInit>(RV->getValue()), RV);
  SmallVector<Init *, 16> Bits;

  for (auto &SI : VLI) {
    if (const BitsInit *BI = dyn_cast<BitsInit>(SI.Value)) {
      for (unsigned Idx = 0U; Idx < BI->getNumBits(); ++Idx) {
        Bits.push_back(BI->getBit(Idx));
      }
    } else if (const BitInit *BI = dyn_cast<BitInit>(SI.Value)) {
      Bits.push_back(const_cast<BitInit *>(BI));
    } else {
      for (unsigned Idx = 0U; Idx < SI.BitWidth; ++Idx)
        Bits.push_back(UnsetInit::get(def.getRecords()));
    }
  }

  return *BitsInit::get(def.getRecords(), Bits);
}

// Representation of the instruction to work on.
typedef std::vector<bit_value_t> insn_t;

namespace {

static const uint64_t NO_FIXED_SEGMENTS_SENTINEL = -1ULL;

class FilterChooser;

/// Filter - Filter works with FilterChooser to produce the decoding tree for
/// the ISA.
///
/// It is useful to think of a Filter as governing the switch stmts of the
/// decoding tree in a certain level.  Each case stmt delegates to an inferior
/// FilterChooser to decide what further decoding logic to employ, or in another
/// words, what other remaining bits to look at.  The FilterChooser eventually
/// chooses a best Filter to do its job.
///
/// This recursive scheme ends when the number of Opcodes assigned to the
/// FilterChooser becomes 1 or if there is a conflict.  A conflict happens when
/// the Filter/FilterChooser combo does not know how to distinguish among the
/// Opcodes assigned.
///
/// An example of a conflict is
///
/// Conflict:
///                     111101000.00........00010000....
///                     111101000.00........0001........
///                     1111010...00........0001........
///                     1111010...00....................
///                     1111010.........................
///                     1111............................
///                     ................................
///     VST4q8a         111101000_00________00010000____
///     VST4q8b         111101000_00________00010000____
///
/// The Debug output shows the path that the decoding tree follows to reach the
/// the conclusion that there is a conflict.  VST4q8a is a vst4 to double-spaced
/// even registers, while VST4q8b is a vst4 to double-spaced odd registers.
///
/// The encoding info in the .td files does not specify this meta information,
/// which could have been used by the decoder to resolve the conflict.  The
/// decoder could try to decode the even/odd register numbering and assign to
/// VST4q8a or VST4q8b, but for the time being, the decoder chooses the "a"
/// version and return the Opcode since the two have the same Asm format string.
class Filter {
protected:
  const FilterChooser *Owner;// points to the FilterChooser who owns this filter
  unsigned StartBit; // the starting bit position
  unsigned NumBits; // number of bits to filter
  bool Mixed; // a mixed region contains both set and unset bits

  // Map of well-known segment value to the set of uid's with that value.
  std::map<uint64_t, std::vector<EncodingIDAndOpcode>>
      FilteredInstructions;

  // Set of uid's with non-constant segment values.
  std::vector<EncodingIDAndOpcode> VariableInstructions;

  // Map of well-known segment value to its delegate.
  std::map<uint64_t, std::unique_ptr<const FilterChooser>> FilterChooserMap;

  // Number of instructions which fall under FilteredInstructions category.
  unsigned NumFiltered;

  // Keeps track of the last opcode in the filtered bucket.
  EncodingIDAndOpcode LastOpcFiltered;

public:
  Filter(Filter &&f);
  Filter(FilterChooser &owner, unsigned startBit, unsigned numBits, bool mixed);

  ~Filter() = default;

  unsigned getNumFiltered() const { return NumFiltered; }

  EncodingIDAndOpcode getSingletonOpc() const {
    assert(NumFiltered == 1);
    return LastOpcFiltered;
  }

  // Return the filter chooser for the group of instructions without constant
  // segment values.
  const FilterChooser &getVariableFC() const {
    assert(NumFiltered == 1);
    assert(FilterChooserMap.size() == 1);
    return *(FilterChooserMap.find(NO_FIXED_SEGMENTS_SENTINEL)->second);
  }

  // Divides the decoding task into sub tasks and delegates them to the
  // inferior FilterChooser's.
  //
  // A special case arises when there's only one entry in the filtered
  // instructions.  In order to unambiguously decode the singleton, we need to
  // match the remaining undecoded encoding bits against the singleton.
  void recurse();

  // Emit table entries to decode instructions given a segment or segments of
  // bits.
  void emitTableEntry(DecoderTableInfo &TableInfo) const;

  // Returns the number of fanout produced by the filter.  More fanout implies
  // the filter distinguishes more categories of instructions.
  unsigned usefulness() const;
}; // end class Filter

} // end anonymous namespace

/// FilterChooser - FilterChooser chooses the best filter among a set of Filters
/// in order to perform the decoding of instructions at the current level.
///
/// Decoding proceeds from the top down.  Based on the well-known encoding bits
/// of instructions available, FilterChooser builds up the possible Filters that
/// can further the task of decoding by distinguishing among the remaining
/// candidate instructions.
///
/// Once a filter has been chosen, it is called upon to divide the decoding task
/// into sub-tasks and delegates them to its inferior FilterChoosers for further
/// processings.
///
/// It is useful to think of a Filter as governing the switch stmts of the
/// decoding tree.  And each case is delegated to an inferior FilterChooser to
/// decide what further remaining bits to look at.
namespace {

class FilterChooser {
protected:
  friend class Filter;

  // The Printer to emit source code from.
  const PrinterLLVM &PI;

  // Vector of codegen instructions to choose our filter.
  ArrayRef<EncodingAndInst> AllInstructions;

  // Vector of uid's for this filter chooser to work on.
  // The first member of the pair is the opcode id being decoded, the second
  // is the opcode id that should be emitted.
  const std::vector<EncodingIDAndOpcode> &Opcodes;

  // Lookup table for the operand decoding of instructions.
  const std::map<unsigned, std::vector<OperandInfo>> &Operands;

  // Vector of candidate filters.
  std::vector<Filter> Filters;

  // Array of bit values passed down from our parent.
  // Set to all BIT_UNFILTERED's for Parent == NULL.
  std::vector<bit_value_t> FilterBitValues;

  // Links to the FilterChooser above us in the decoding tree.
  const FilterChooser *Parent;

  // Index of the best filter from Filters.
  int BestIndex;

  // Width of instructions
  unsigned BitWidth;

public:
  FilterChooser(const PrinterLLVM &PI, ArrayRef<EncodingAndInst> Insts,
                const std::vector<EncodingIDAndOpcode> &IDs,
                const std::map<unsigned, std::vector<OperandInfo>> &Ops,
                unsigned BW)
      : PI(PI), AllInstructions(Insts), Opcodes(IDs), Operands(Ops),
        FilterBitValues(BW, BIT_UNFILTERED), Parent(nullptr), BestIndex(-1),
        BitWidth(BW) {
    doFilter();
  }

  FilterChooser(const PrinterLLVM &PI, ArrayRef<EncodingAndInst> Insts,
                const std::vector<EncodingIDAndOpcode> &IDs,
                const std::map<unsigned, std::vector<OperandInfo>> &Ops,
                const std::vector<bit_value_t> &ParentFilterBitValues,
                const FilterChooser &Parent)
      : PI(Parent.PI), AllInstructions(Insts), Opcodes(IDs), Operands(Ops),
        FilterBitValues(ParentFilterBitValues), Parent(&Parent), BestIndex(-1),
        BitWidth(Parent.BitWidth) {
    doFilter();
  }

  FilterChooser(const FilterChooser &) = delete;
  void operator=(const FilterChooser &) = delete;

  unsigned getBitWidth() const { return BitWidth; }

protected:
  // Populates the insn given the uid.
  void insnWithID(insn_t &Insn, unsigned Opcode) const;

  // Emit the name of the encoding/instruction pair.
  void emitNameWithID(raw_ostream &ErrOS, unsigned Opcode) const;

  // Populates the field of the insn given the start position and the number
  // of consecutive bits to scan for.
  //
  // Returns false if there exists any uninitialized bit value in the range.
  // Returns true, otherwise.
  bool fieldFromInsn(uint64_t &Field, insn_t &Insn, unsigned StartBit,
                     unsigned NumBits) const;

  /// dumpFilterArray - dumpFilterArray prints out debugging info for the
  /// given filter array as a series of chars.
  void dumpFilterArray(raw_ostream &ErrOS,
                       const std::vector<bit_value_t> &Filter) const;

  /// dumpStack - dumpStack traverses the filter chooser chain and calls
  /// dumpFilterArray on each filter chooser up to the top level one.
  void dumpStack(raw_ostream &ErrOS, const char *Prefix) const;

  Filter &bestFilter() {
    assert(BestIndex != -1 && "BestIndex not set");
    return Filters[BestIndex];
  }

  bool positionFiltered(unsigned I) const {
    return ValueSet(FilterBitValues[I]);
  }

  // Calculates the island(s) needed to decode the instruction.
  // This returns a lit of undecoded bits of an instructions, for example,
  // Inst{20} = 1 && Inst{3-0} == 0b1111 represents two islands of yet-to-be
  // decoded bits in order to verify that the instruction matches the Opcode.
  unsigned getIslands(std::vector<unsigned> &StartBits,
                      std::vector<unsigned> &EndBits,
                      std::vector<uint64_t> &FieldVals,
                      const insn_t &Insn) const;

  bool doesOpcodeNeedPredicate(unsigned Opc) const;
  unsigned getPredicateIndex(DecoderTableInfo &TableInfo, StringRef P) const;
  void emitPredicateTableEntry(DecoderTableInfo &TableInfo, unsigned Opc) const;

  void emitSoftFailTableEntry(DecoderTableInfo &TableInfo, unsigned Opc) const;

  // Emits table entries to decode the singleton.
  void emitSingletonTableEntry(DecoderTableInfo &TableInfo,
                               EncodingIDAndOpcode Opc) const;

  // Emits code to decode the singleton, and then to decode the rest.
  void emitSingletonTableEntry(DecoderTableInfo &TableInfo,
                               const Filter &Best) const;

  void emitDecoder(raw_ostream &DecoderOS, unsigned Opc,
                   bool &HasCompleteDecoder) const;
  unsigned getDecoderIndex(DecoderSet &Decoders, unsigned Opc,
                           bool &HasCompleteDecoder) const;

  // Assign a single filter and run with it.
  void runSingleFilter(unsigned StartBit, unsigned NumBit, bool Mixed);

  // reportRegion is a helper function for filterProcessor to mark a region as
  // eligible for use as a filter region.
  void reportRegion(bitAttr_t RA, unsigned StartBit, unsigned BitIndex,
                    bool AllowMixed);

  // FilterProcessor scans the well-known encoding bits of the instructions
  // and builds up a list of candidate filters.  It chooses the best filter
  // and recursively descends down the decoding tree.
  bool filterProcessor(bool AllowMixed, bool Greedy = true);

  // Decides on the best configuration of filter(s) to use in order to decode
  // the instructions.  A conflict of instructions may occur, in which case we
  // dump the conflict set to the standard error.
  void doFilter();

public:
  // emitTableEntries - Emit state machine entries to decode our share of
  // instructions.
  void emitTableEntries(DecoderTableInfo &TableInfo) const;
};

} // end anonymous namespace

///////////////////////////
//                       //
// Filter Implementation //
//                       //
///////////////////////////

Filter::Filter(Filter &&f)
  : Owner(f.Owner), StartBit(f.StartBit), NumBits(f.NumBits), Mixed(f.Mixed),
    FilteredInstructions(std::move(f.FilteredInstructions)),
    VariableInstructions(std::move(f.VariableInstructions)),
    FilterChooserMap(std::move(f.FilterChooserMap)), NumFiltered(f.NumFiltered),
    LastOpcFiltered(f.LastOpcFiltered) {
}

Filter::Filter(FilterChooser &owner, unsigned startBit, unsigned numBits,
               bool mixed)
  : Owner(&owner), StartBit(startBit), NumBits(numBits), Mixed(mixed) {
  assert(StartBit + NumBits - 1 < Owner->BitWidth);

  NumFiltered = 0;
  LastOpcFiltered = {0, 0};

  for (unsigned i = 0, e = Owner->Opcodes.size(); i != e; ++i) {
    insn_t Insn;

    // Populates the insn given the uid.
    Owner->insnWithID(Insn, Owner->Opcodes[i].EncodingID);

    uint64_t Field;
    // Scans the segment for possibly well-specified encoding bits.
    bool ok = Owner->fieldFromInsn(Field, Insn, StartBit, NumBits);

    if (ok) {
      // The encoding bits are well-known.  Lets add the uid of the
      // instruction into the bucket keyed off the constant field value.
      LastOpcFiltered = Owner->Opcodes[i];
      FilteredInstructions[Field].push_back(LastOpcFiltered);
      ++NumFiltered;
    } else {
      // Some of the encoding bit(s) are unspecified.  This contributes to
      // one additional member of "Variable" instructions.
      VariableInstructions.push_back(Owner->Opcodes[i]);
    }
  }

  assert((FilteredInstructions.size() + VariableInstructions.size() > 0)
         && "Filter returns no instruction categories");
}

// Divides the decoding task into sub tasks and delegates them to the
// inferior FilterChooser's.
//
// A special case arises when there's only one entry in the filtered
// instructions.  In order to unambiguously decode the singleton, we need to
// match the remaining undecoded encoding bits against the singleton.
void Filter::recurse() {
  // Starts by inheriting our parent filter chooser's filter bit values.
  std::vector<bit_value_t> BitValueArray(Owner->FilterBitValues);

  if (!VariableInstructions.empty()) {
    // Conservatively marks each segment position as BIT_UNSET.
    for (unsigned bitIndex = 0; bitIndex < NumBits; ++bitIndex)
      BitValueArray[StartBit + bitIndex] = BIT_UNSET;

    // Delegates to an inferior filter chooser for further processing on this
    // group of instructions whose segment values are variable.
    FilterChooserMap.insert(std::make_pair(
        NO_FIXED_SEGMENTS_SENTINEL,
        std::make_unique<FilterChooser>(Owner->PI, Owner->AllInstructions,
                                        VariableInstructions, Owner->Operands,
                                        BitValueArray, *Owner)));
  }

  // No need to recurse for a singleton filtered instruction.
  // See also Filter::emit*().
  if (getNumFiltered() == 1) {
    assert(FilterChooserMap.size() == 1);
    return;
  }

  // Otherwise, create sub choosers.
  for (const auto &InstSubset: FilteredInstructions) {

    // Marks all the segment positions with either BIT_TRUE or BIT_FALSE.
    for (unsigned bitIndex = 0; bitIndex < NumBits; ++bitIndex) {
      if (InstSubset.first & (1ULL << bitIndex))
        BitValueArray[StartBit + bitIndex] = BIT_TRUE;
      else
        BitValueArray[StartBit + bitIndex] = BIT_FALSE;
    }

    // Delegates to an inferior filter chooser for further processing on this
    // category of instructions.
    FilterChooserMap.insert(std::make_pair(
        InstSubset.first, std::make_unique<FilterChooser>(
                          Owner->PI, Owner->AllInstructions, InstSubset.second,
                          Owner->Operands, BitValueArray, *Owner)));
  }
}

static void resolveTableFixups(DecoderTable &Table, const FixupList &Fixups,
                               uint32_t DestIdx) {
  // Any NumToSkip fixups in the current scope can resolve to the
  // current location.
  for (FixupList::const_reverse_iterator I = Fixups.rbegin(),
                                         E = Fixups.rend();
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

// Emit table entries to decode instructions given a segment or segments
// of bits.
void Filter::emitTableEntry(DecoderTableInfo &TableInfo) const {
  TableInfo.Table.push_back(MCD::OPC_ExtractField);
  TableInfo.Table.push_back(StartBit);
  TableInfo.Table.push_back(NumBits);

  // A new filter entry begins a new scope for fixup resolution.
  TableInfo.FixupStack.emplace_back();

  DecoderTable &Table = TableInfo.Table;

  size_t PrevFilter = 0;
  bool HasFallthrough = false;
  for (auto &Filter : FilterChooserMap) {
    // Field value -1 implies a non-empty set of variable instructions.
    // See also recurse().
    if (Filter.first == NO_FIXED_SEGMENTS_SENTINEL) {
      HasFallthrough = true;

      // Each scope should always have at least one filter value to check
      // for.
      assert(PrevFilter != 0 && "empty filter set!");
      FixupList &CurScope = TableInfo.FixupStack.back();
      // Resolve any NumToSkip fixups in the current scope.
      resolveTableFixups(Table, CurScope, Table.size());
      CurScope.clear();
      PrevFilter = 0;  // Don't re-process the filter's fallthrough.
    } else {
      Table.push_back(MCD::OPC_FilterValue);
      // Encode and emit the value to filter against.
      uint8_t Buffer[16];
      unsigned Len = encodeULEB128(Filter.first, Buffer);
      Table.insert(Table.end(), Buffer, Buffer + Len);
      // Reserve space for the NumToSkip entry. We'll backpatch the value
      // later.
      PrevFilter = Table.size();
      Table.push_back(0);
      Table.push_back(0);
      Table.push_back(0);
    }

    // We arrive at a category of instructions with the same segment value.
    // Now delegate to the sub filter chooser for further decodings.
    // The case may fallthrough, which happens if the remaining well-known
    // encoding bits do not match exactly.
    Filter.second->emitTableEntries(TableInfo);

    // Now that we've emitted the body of the handler, update the NumToSkip
    // of the filter itself to be able to skip forward when false. Subtract
    // two as to account for the width of the NumToSkip field itself.
    if (PrevFilter) {
      uint32_t NumToSkip = Table.size() - PrevFilter - 3;
      assert(NumToSkip < (1u << 24) && "disassembler decoding table too large!");
      Table[PrevFilter] = (uint8_t)NumToSkip;
      Table[PrevFilter + 1] = (uint8_t)(NumToSkip >> 8);
      Table[PrevFilter + 2] = (uint8_t)(NumToSkip >> 16);
    }
  }

  // Any remaining unresolved fixups bubble up to the parent fixup scope.
  assert(TableInfo.FixupStack.size() > 1 && "fixup stack underflow!");
  FixupScopeList::iterator Source = TableInfo.FixupStack.end() - 1;
  FixupScopeList::iterator Dest = Source - 1;
  llvm::append_range(*Dest, *Source);
  TableInfo.FixupStack.pop_back();

  // If there is no fallthrough, then the final filter should get fixed
  // up according to the enclosing scope rather than the current position.
  if (!HasFallthrough)
    TableInfo.FixupStack.back().push_back(PrevFilter);
}

// Returns the number of fanout produced by the filter.  More fanout implies
// the filter distinguishes more categories of instructions.
unsigned Filter::usefulness() const {
  if (!VariableInstructions.empty())
    return FilteredInstructions.size();
  else
    return FilteredInstructions.size() + 1;
}

//////////////////////////////////
//                              //
// Filterchooser Implementation //
//                              //
//////////////////////////////////

// Populates the insn given the uid.
void FilterChooser::insnWithID(insn_t &Insn, unsigned Opcode) const {
  BitsInit const &Bits =
      getBitsField(*AllInstructions[Opcode].EncodingDef, "Inst");
  Insn.resize(BitWidth > Bits.getNumBits() ? BitWidth : Bits.getNumBits(),
              BIT_UNSET);
  // We may have a SoftFail bitmask, which specifies a mask where an encoding
  // may differ from the value in "Inst" and yet still be valid, but the
  // disassembler should return SoftFail instead of Success.
  //
  // This is used for marking UNPREDICTABLE instructions in the ARM world.
  const RecordVal *RV =
      AllInstructions[Opcode].EncodingDef->getValue("SoftFail");
  const BitsInit *SFBits = RV ? dyn_cast<BitsInit>(RV->getValue()) : nullptr;
  for (unsigned I = 0; I < Bits.getNumBits(); ++I) {
    if (SFBits && bitFromBits(*SFBits, I) == BIT_TRUE)
      Insn[I] = BIT_UNSET;
    else
      Insn[I] = bitFromBits(Bits, I);
  }
}

// Emit the name of the encoding/instruction pair.
void FilterChooser::emitNameWithID(raw_ostream &ErrOS, unsigned Opcode) const {
  const Record *EncodingDef = AllInstructions[Opcode].EncodingDef;
  const Record *InstDef = AllInstructions[Opcode].Inst->TheDef;
  if (EncodingDef != InstDef)
    ErrOS << EncodingDef->getName() << ":";
  ErrOS << InstDef->getName();
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
void FilterChooser::dumpFilterArray(raw_ostream &o,
                                 const std::vector<bit_value_t> &filter) const {
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
    bool Filtered = positionFiltered(i);
    switch (State) {
    default: llvm_unreachable("Unreachable code!");
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

void FilterChooser::emitDecoder(raw_ostream &DecoderOS, unsigned Opc,
                                bool &HasCompleteDecoder) const {
  HasCompleteDecoder = true;

  for (const auto &Op : Operands.find(Opc)->second) {
    // If a custom instruction decoder was specified, use that.
    if (Op.numFields() == 0 && !Op.Decoder.empty()) {
      HasCompleteDecoder = Op.HasCompleteDecoder;
      PI.decoderEmitterEmitOpDecoder(DecoderOS, Op);
      break;
    }

    PI.decoderEmitterEmitOpBinaryParser(DecoderOS, Op);

    // If a custom decoder was set the flag decides otherwise its true.
    HasCompleteDecoder = Op.Decoder != "" ? Op.HasCompleteDecoder : true;
  }
}

unsigned FilterChooser::getDecoderIndex(DecoderSet &Decoders,
                                        unsigned Opc,
                                        bool &HasCompleteDecoder) const {
  // Build up the predicate string.
  SmallString<256> Decoder;
  raw_svector_ostream S(Decoder);
  emitDecoder(S, Opc, HasCompleteDecoder);

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
  raw_svector_ostream PS(Predicate);
  const ListInit *Predicates =
      AllInstructions[Opc].EncodingDef->getValueAsListInit("Predicates");
  PI.decoderEmitterEmitPredicateMatch(PS, Predicates, Opc);

  // Figure out the index into the predicate table for the predicate just
  // computed.
  unsigned const PIdx = getPredicateIndex(TableInfo, PS.str());
  SmallString<16> PBytes;
  raw_svector_ostream S(PBytes);
  encodeULEB128(PIdx, S);

  TableInfo.Table.push_back(MCD::OPC_CheckPredicate);
  // Predicate index
  for (unsigned I = 0, E = PBytes.size(); I != E; ++I)
    TableInfo.Table.push_back(PBytes[I]);
  // Push location for NumToSkip backpatching.
  TableInfo.FixupStack.back().push_back(TableInfo.Table.size());
  TableInfo.Table.push_back(0);
  TableInfo.Table.push_back(0);
  TableInfo.Table.push_back(0);
}

void FilterChooser::emitSoftFailTableEntry(DecoderTableInfo &TableInfo,
                                           unsigned Opc) const {
  const RecordVal *RV = AllInstructions[Opc].EncodingDef->getValue("SoftFail");
  BitsInit *SFBits = RV ? dyn_cast<BitsInit>(RV->getValue()) : nullptr;

  if (!SFBits) return;
  BitsInit *InstBits =
      AllInstructions[Opc].EncodingDef->getValueAsBitsInit("Inst");

  APInt PositiveMask(BitWidth, 0ULL);
  APInt NegativeMask(BitWidth, 0ULL);
  for (unsigned i = 0; i < BitWidth; ++i) {
    bit_value_t B = bitFromBits(*SFBits, i);
    bit_value_t IB = bitFromBits(*InstBits, i);

    if (B != BIT_TRUE) continue;

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
    unsigned NumBits = EndBits[I-1] - StartBits[I-1] + 1;
    TableInfo.Table.push_back(MCD::OPC_CheckField);
    TableInfo.Table.push_back(StartBits[I-1]);
    TableInfo.Table.push_back(NumBits);
    uint8_t Buffer[16], *p;
    encodeULEB128(FieldVals[I-1], Buffer);
    for (p = Buffer; *p >= 128 ; ++p)
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
  TableInfo.Table.push_back(HasCompleteDecoder ? MCD::OPC_Decode :
      MCD::OPC_TryDecode);
  NumEncodingsSupported++;
  uint8_t Buffer[16], *p;
  encodeULEB128(Opc.Opcode, Buffer);
  for (p = Buffer; *p >= 128 ; ++p)
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

    for (auto Opcode : Opcodes) {
      std::vector<unsigned> StartBits;
      std::vector<unsigned> EndBits;
      std::vector<uint64_t> FieldVals;
      insn_t Insn;

      insnWithID(Insn, Opcode.EncodingID);

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

  for (auto Opcode : Opcodes) {
    errs() << '\t';
    emitNameWithID(errs(), Opcode.EncodingID);
    errs() << " ";
    dumpBits(
        errs(),
        getBitsField(*AllInstructions[Opcode.EncodingID].EncodingDef, "Inst"));
    errs() << '\n';
  }
}

static std::string findOperandDecoderMethod(Record *Record) {
  std::string Decoder;

  RecordVal *DecoderString = Record->getValue("DecoderMethod");
  StringInit *String = DecoderString ?
    dyn_cast<StringInit>(DecoderString->getValue()) : nullptr;
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

OperandInfo getOpInfo(Record *TypeRecord) {
  std::string Decoder = findOperandDecoderMethod(TypeRecord);

  RecordVal *HasCompleteDecoderVal = TypeRecord->getValue("hasCompleteDecoder");
  BitInit *HasCompleteDecoderBit =
      HasCompleteDecoderVal
          ? dyn_cast<BitInit>(HasCompleteDecoderVal->getValue())
          : nullptr;
  bool HasCompleteDecoder =
      HasCompleteDecoderBit ? HasCompleteDecoderBit->getValue() : true;

  return OperandInfo(Decoder, HasCompleteDecoder);
}

void parseVarLenInstOperand(const Record &Def,
                            std::vector<OperandInfo> &Operands,
                            const CodeGenInstruction &CGI) {

  const RecordVal *RV = Def.getValue("Inst");
  VarLenInst VLI(cast<DagInit>(RV->getValue()), RV);
  SmallVector<int> TiedTo;

  for (unsigned Idx = 0; Idx < CGI.Operands.size(); ++Idx) {
    auto &Op = CGI.Operands[Idx];
    if (Op.MIOperandInfo && Op.MIOperandInfo->getNumArgs() > 0)
      for (auto *Arg : Op.MIOperandInfo->getArgs())
        Operands.push_back(getOpInfo(cast<DefInit>(Arg)->getDef()));
    else
      Operands.push_back(getOpInfo(Op.Rec));

    int TiedReg = Op.getTiedRegister();
    TiedTo.push_back(-1);
    if (TiedReg != -1) {
      TiedTo[Idx] = TiedReg;
      TiedTo[TiedReg] = Idx;
    }
  }

  unsigned CurrBitPos = 0;
  for (auto &EncodingSegment : VLI) {
    unsigned Offset = 0;
    StringRef OpName;

    if (const StringInit *SI = dyn_cast<StringInit>(EncodingSegment.Value)) {
      OpName = SI->getValue();
    } else if (const DagInit *DI = dyn_cast<DagInit>(EncodingSegment.Value)) {
      OpName = cast<StringInit>(DI->getArg(0))->getValue();
      Offset = cast<IntInit>(DI->getArg(2))->getValue();
    }

    if (!OpName.empty()) {
      auto OpSubOpPair =
          const_cast<CodeGenInstruction &>(CGI).Operands.ParseOperandName(
              OpName);
      unsigned OpIdx = CGI.Operands.getFlattenedOperandNumber(OpSubOpPair);
      Operands[OpIdx].addField(CurrBitPos, EncodingSegment.BitWidth, Offset);
      if (!EncodingSegment.CustomDecoder.empty())
        Operands[OpIdx].Decoder = EncodingSegment.CustomDecoder.str();

      int TiedReg = TiedTo[OpSubOpPair.first];
      if (TiedReg != -1) {
        unsigned OpIdx = CGI.Operands.getFlattenedOperandNumber(
            std::make_pair(TiedReg, OpSubOpPair.second));
        Operands[OpIdx].addField(CurrBitPos, EncodingSegment.BitWidth, Offset);
      }
    }

    CurrBitPos += EncodingSegment.BitWidth;
  }
}

static void debugDumpRecord(const Record &Rec) {
  // Dump the record, so we can see what's going on...
  std::string E;
  raw_string_ostream S(E);
  S << "Dumping record for previous error:\n";
  S << Rec;
  PrintNote(E);
}

/// For an operand field named OpName: populate OpInfo.InitValue with the
/// constant-valued bit values, and OpInfo.Fields with the ranges of bits to
/// insert from the decoded instruction.
static void addOneOperandFields(const Record &EncodingDef, const BitsInit &Bits,
                                std::map<std::string, std::string> &TiedNames,
                                StringRef OpName, OperandInfo &OpInfo) {
  // Some bits of the operand may be required to be 1 depending on the
  // instruction's encoding. Collect those bits.
  if (const RecordVal *EncodedValue = EncodingDef.getValue(OpName))
    if (const BitsInit *OpBits = dyn_cast<BitsInit>(EncodedValue->getValue()))
      for (unsigned I = 0; I < OpBits->getNumBits(); ++I)
        if (const BitInit *OpBit = dyn_cast<BitInit>(OpBits->getBit(I)))
          if (OpBit->getValue())
            OpInfo.InitValue |= 1ULL << I;

  for (unsigned I = 0, J = 0; I != Bits.getNumBits(); I = J) {
    VarInit *Var;
    unsigned Offset = 0;
    for (; J != Bits.getNumBits(); ++J) {
      VarBitInit *BJ = dyn_cast<VarBitInit>(Bits.getBit(J));
      if (BJ) {
        Var = dyn_cast<VarInit>(BJ->getBitVar());
        if (I == J)
          Offset = BJ->getBitNum();
        else if (BJ->getBitNum() != Offset + J - I)
          break;
      } else {
        Var = dyn_cast<VarInit>(Bits.getBit(J));
      }
      if (!Var || (Var->getName() != OpName &&
                   Var->getName() != TiedNames[std::string(OpName)]))
        break;
    }
    if (I == J)
      ++J;
    else
      OpInfo.addField(I, J - I, Offset);
  }
}

static unsigned
populateInstruction(CodeGenTarget &Target, const Record &EncodingDef,
                    const CodeGenInstruction &CGI, unsigned Opc,
                    std::map<unsigned, std::vector<OperandInfo>> &Operands,
                    bool IsVarLenInst) {
  const Record &Def = *CGI.TheDef;
  // If all the bit positions are not specified; do not decode this instruction.
  // We are bound to fail!  For proper disassembly, the well-known encoding bits
  // of the instruction must be fully specified.

  BitsInit &Bits = getBitsField(EncodingDef, "Inst");
  if (Bits.allInComplete())
    return 0;

  std::vector<OperandInfo> InsnOperands;

  // If the instruction has specified a custom decoding hook, use that instead
  // of trying to auto-generate the decoder.
  StringRef InstDecoder = EncodingDef.getValueAsString("DecoderMethod");
  if (InstDecoder != "") {
    bool HasCompleteInstDecoder = EncodingDef.getValueAsBit("hasCompleteDecoder");
    InsnOperands.push_back(
        OperandInfo(std::string(InstDecoder), HasCompleteInstDecoder));
    Operands[Opc] = InsnOperands;
    return Bits.getNumBits();
  }

  // Generate a description of the operand of the instruction that we know
  // how to decode automatically.
  // FIXME: We'll need to have a way to manually override this as needed.

  // Gather the outputs/inputs of the instruction, so we can find their
  // positions in the encoding.  This assumes for now that they appear in the
  // MCInst in the order that they're listed.
  std::vector<std::pair<Init*, StringRef>> InOutOperands;
  DagInit *Out  = Def.getValueAsDag("OutOperandList");
  DagInit *In  = Def.getValueAsDag("InOperandList");
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
    auto &Op = CGI.Operands[i];
    for (unsigned j = 0; j < Op.Constraints.size(); ++j) {
      const CGIOperandList::ConstraintInfo &CI = Op.Constraints[j];
      if (CI.isTied()) {
        int tiedTo = CI.getTiedOperand();
        std::pair<unsigned, unsigned> SO =
            CGI.Operands.getSubOperandNumber(tiedTo);
        std::string TiedName = CGI.Operands[SO.first].SubOpNames[SO.second];
        if (TiedName.empty())
          TiedName = CGI.Operands[SO.first].Name;
        std::string MyName = Op.SubOpNames[j];
        if (MyName.empty())
          MyName = Op.Name;

        TiedNames[MyName] = TiedName;
        TiedNames[TiedName] = MyName;
      }
    }
  }

  if (IsVarLenInst) {
    parseVarLenInstOperand(EncodingDef, InsnOperands, CGI);
  } else {
    // For each operand, see if we can figure out where it is encoded.
    for (const auto &Op : InOutOperands) {
      Init *OpInit = Op.first;
      StringRef OpName = Op.second;

      // We're ready to find the instruction encoding locations for this operand.

      // First, find the operand type ("OpInit"), and sub-op names
      // ("SubArgDag") if present.
      DagInit *SubArgDag = dyn_cast<DagInit>(OpInit);
      if (SubArgDag)
        OpInit = SubArgDag->getOperator();
      Record *OpTypeRec = cast<DefInit>(OpInit)->getDef();
      // Lookup the sub-operands from the operand type record (note that only
      // Operand subclasses have MIOperandInfo, see CodeGenInstruction.cpp).
      DagInit *SubOps = OpTypeRec->isSubClassOf("Operand")
                            ? OpTypeRec->getValueAsDag("MIOperandInfo")
                            : nullptr;

      // Lookup the decoder method and construct a new OperandInfo to hold our result.
      OperandInfo OpInfo = getOpInfo(OpTypeRec);

      // If we have named sub-operands...
      if (SubArgDag) {
        // Then there should not be a custom decoder specified on the top-level
        // type.
        if (!OpInfo.Decoder.empty()) {
          PrintError(EncodingDef.getLoc(),
                     "DecoderEmitter: operand \"" + OpName + "\" has type \"" +
                         OpInit->getAsString() +
                         "\" with a custom DecoderMethod, but also named "
                         "sub-operands.");
          continue;
        }

        // Decode each of the sub-ops separately.
        assert(SubOps && SubArgDag->getNumArgs() == SubOps->getNumArgs());
        for (unsigned i = 0; i < SubOps->getNumArgs(); ++i) {
          StringRef SubOpName = SubArgDag->getArgNameStr(i);
          OperandInfo SubOpInfo =
              getOpInfo(cast<DefInit>(SubOps->getArg(i))->getDef());

          addOneOperandFields(EncodingDef, Bits, TiedNames, SubOpName,
                              SubOpInfo);
          InsnOperands.push_back(SubOpInfo);
        }
        continue;
      }

      // Otherwise, if we have an operand with sub-operands, but they aren't
      // named...
      if (SubOps && OpInfo.Decoder.empty()) {
        // If it's a single sub-operand, and no custom decoder, use the decoder
        // from the one sub-operand.
        if (SubOps->getNumArgs() == 1)
          OpInfo = getOpInfo(cast<DefInit>(SubOps->getArg(0))->getDef());

        // If we have multiple sub-ops, there'd better have a custom
        // decoder. (Otherwise we don't know how to populate them properly...)
        if (SubOps->getNumArgs() > 1) {
          PrintError(EncodingDef.getLoc(),
                     "DecoderEmitter: operand \"" + OpName +
                         "\" uses MIOperandInfo with multiple ops, but doesn't "
                         "have a custom decoder!");
          debugDumpRecord(EncodingDef);
          continue;
        }
      }

      addOneOperandFields(EncodingDef, Bits, TiedNames, OpName, OpInfo);
      // FIXME: it should be an error not to find a definition for a given
      // operand, rather than just failing to add it to the resulting
      // instruction! (This is a longstanding bug, which will be addressed in an
      // upcoming change.)
      if (OpInfo.numFields() > 0)
        InsnOperands.push_back(OpInfo);
    }
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

  return Bits.getNumBits();
}

/// Encodings of instructions might differ if the Hardware Mode is different as
/// well. Here we add all possible encodings.
void DecoderEmitter::retrieveHwModeEncodings() {
  std::set<StringRef> HwModeNames;

  // First, collect all HwModes referenced by the target.
  for (const auto &NumberedInstruction : NumberedInstructions) {
    IndexOfInstruction[NumberedInstruction->TheDef] = NumberedEncodings.size();

    if (const RecordVal *RV =
            NumberedInstruction->TheDef->getValue("EncodingInfos")) {
      if (auto *DI = dyn_cast_or_null<DefInit>(RV->getValue())) {
        const CodeGenHwModes &HWM = Target.getHwModes();
        EncodingInfoByHwMode const EBM(DI->getDef(), HWM);
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
}

/// Fills the Opcode Map with the encodings of the different decoder spaces.
/// It returns the maximum length of all variable length instructions.
/// Or 0 if no variable length is in the set (\p IsVarLenInst = false)
unsigned DecoderEmitter::fillOpcMap(bool const IsVarLenInst) {
  unsigned MaxInstLen = 0;
  for (unsigned I = 0; I < NumberedEncodings.size(); ++I) {
    const Record *EncodingDef = NumberedEncodings[I].EncodingDef;
    const CodeGenInstruction *Inst = NumberedEncodings[I].Inst;
    const Record *Def = Inst->TheDef;
    unsigned const Size = EncodingDef->getValueAsInt("Size");
    if (Def->getValueAsString("Namespace") == "TargetOpcode" ||
        Def->getValueAsBit("isPseudo") ||
        Def->getValueAsBit("isAsmParserOnly") ||
        Def->getValueAsBit("isCodeGenOnly")) {
      NumEncodingsLackingDisasm++;
      continue;
    }

    if (I < NumberedInstructions.size())
      NumInstructions++;
    NumEncodings++;

    if (!Size && !IsVarLenInst)
      continue;

    if (IsVarLenInst)
      InstrLen.resize(NumberedInstructions.size(), 0);

    if (unsigned const Len = populateInstruction(Target, *EncodingDef, *Inst, I,
                                                 Operands, IsVarLenInst)) {
      if (IsVarLenInst) {
        MaxInstLen = std::max(MaxInstLen, Len);
        InstrLen[I] = Len;
      }
      std::string DecoderNamespace =
          std::string(EncodingDef->getValueAsString("DecoderNamespace"));
      if (!NumberedEncodings[I].HwModeName.empty())
        DecoderNamespace +=
            std::string("_") + NumberedEncodings[I].HwModeName.str();
      OpcMap[std::make_pair(DecoderNamespace, Size)].emplace_back(
          I, IndexOfInstruction.find(Def)->second);
    } else {
      NumEncodingsOmitted++;
    }
  }
  return MaxInstLen;
}

// Emits disassembler code for instruction decoding.
void DecoderEmitter::run() {
  PI.decoderEmitterEmitSourceFileHeader();
  PI.decoderEmitterEmitIncludes();
  PI.emitNamespace("llvm", true);
  PI.decoderEmitterEmitFieldFromInstruction();
  PI.decoderEmitterEmitInsertBits();
  PI.decoderEmitterEmitCheck();

  Target.reverseBitsForLittleEndianEncoding();

  // Parameterize the decoders based on namespace and instruction width.
  NumberedInstructions = Target.getInstructionsByEnumValue();
  NumberedEncodings.reserve(NumberedInstructions.size());

  retrieveHwModeEncodings();

  for (const auto &NumberedAlias :
       RK.getAllDerivedDefinitions("AdditionalEncoding"))
    NumberedEncodings.emplace_back(
        NumberedAlias,
        &Target.getInstruction(NumberedAlias->getValueAsDef("AliasOf")));

  bool const IsVarLenInst =
      any_of(NumberedInstructions, [](const CodeGenInstruction *CGI) {
        RecordVal *RV = CGI->TheDef->getValue("Inst");
        return RV && isa<DagInit>(RV->getValue());
      });

  unsigned const MaxInstLen = fillOpcMap(IsVarLenInst);

  // Build the state machine for instruction decoding.
  DecoderTableInfo TableInfo;
  for (const auto &Opc : OpcMap) {
    // Emit the decoder for this namespace+width combination.
    ArrayRef<EncodingAndInst> const NumberedEncodingsRef(
        NumberedEncodings.data(), NumberedEncodings.size());
    FilterChooser const FC(PI, NumberedEncodingsRef, Opc.second, Operands,
                           IsVarLenInst ? MaxInstLen : 8 * Opc.first.second);

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
    PI.decoderEmitterEmitTable(TableInfo.Table, FC.getBitWidth(), Opc.first.first,
                 NumberedEncodings);
    PI.flushOS();
  }

  // For variable instruction, we emit a instruction length table
  // to let the decoder know how long the instructions are.
  // You can see example usage in M68k's disassembler.
  if (IsVarLenInst)
    PI.decoderEmitterEmitInstrLenTable(InstrLen);
  // Emit the predicate function.
  PI.decoderEmitterEmitPredicateFunction(TableInfo.Predicates, 0);

  // Emit the decoder function.
  PI.decoderEmitterEmitDecoderFunction(TableInfo.Decoders, 0);

  // Emit the main entry point for the decoder, decodeInstruction().
  PI.decoderEmitterEmitDecodeInstruction(IsVarLenInst);
  PI.emitNamespace("llvm", false);
}

static void setPrinterParameters(CodeGenTarget &Target, PrinterLanguage PL,
                                 std::string &PredicateNamespace,
                                 std::string &GPrefix, std::string &GPostfix,
                                 std::string &ROK, std::string &RFail,
                                 std::string &L) {
  // ARM and Thumb have a CHECK() macro to deal with DecodeStatuses.
  if (Target.getName() == "ARM" || Target.getName() == "Thumb" ||
      Target.getName() == "AArch64" || Target.getName() == "ARM64") {
    PredicateNamespace = std::string(Target.getName());

    if (PredicateNamespace == "Thumb")
      PredicateNamespace = "ARM";

    switch (PL) {
    default:
      PrintFatalNote("DecoderEmitter does not support the given output language.");
    case llvm::PRINTER_LANG_CPP:
      GPrefix = "if (!Check(S, ";
      L = "  MCDisassembler::DecodeStatus S = "
          "MCDisassembler::Success;\n(void)S;";
      break;
    case llvm::PRINTER_LANG_CAPSTONE_C:
      GPrefix = "if (!Check(&S, ";
      L = "  MCDisassembler_DecodeStatus S = "
          "MCDisassembler_Success;\n(void)S;";
      break;
    }
    GPostfix = "))";
  } else {
    PredicateNamespace = Target.getName().str();
    GPrefix = "if (";
    L = "";

    switch (PL) {
    default:
      PrintFatalNote("DecoderEmitter does not support the given output language.");
    case llvm::PRINTER_LANG_CPP:
      GPostfix = " == MCDisassembler::Fail)";
      break;
    case llvm::PRINTER_LANG_CAPSTONE_C:
      GPostfix = " == MCDisassembler_Fail)";
      break;
    }
  }

  ROK = "S";

  switch (PL) {
  default:
    PrintFatalNote("DecoderEmitter does not support the given output language.");
  case llvm::PRINTER_LANG_CPP:
    RFail = "MCDisassembler::Fail";
    break;
  case llvm::PRINTER_LANG_CAPSTONE_C:
    RFail = "MCDisassembler_Fail";
    break;
  }
}

namespace llvm {

void EmitDecoder(RecordKeeper &RK, raw_ostream &OS, CodeGenTarget &Target) {
  formatted_raw_ostream FOS(OS);
  PrinterLLVM *PI;
  std::string PredicateNamespace;
  std::string GPrefix;
  std::string GPostfix;
  std::string ROK;
  std::string RFail;
  std::string L;
  PrinterLanguage const PL = PrinterLLVM::getLanguage();

  setPrinterParameters(Target, PL, PredicateNamespace, GPrefix, GPostfix,
                       ROK, RFail, L);

  if (PL == PRINTER_LANG_CPP) {
    PI = new PrinterLLVM(FOS, PredicateNamespace,
                         GPrefix, GPostfix, ROK, RFail,
                         L, Target.getName().str());
  } else if (PL == PRINTER_LANG_CAPSTONE_C) {
    PI = new PrinterCapstone(FOS, PredicateNamespace,
                         GPrefix, GPostfix, ROK, RFail,
                         L, Target.getName().str());
  } else {
    PrintFatalNote("DecoderEmitter does not support the given output language.");
  }
  DecoderEmitter(RK, *PI).run();
  delete PI;
}

} // end namespace llvm
