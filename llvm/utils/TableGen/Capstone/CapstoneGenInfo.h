//===-- CapstoneGenInfo.h - Info Generation Module -----------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Created by Phosphorus15 on 2021/5/14.
//

#ifndef LLVM_UTILS_TABLEGEN_CAPSTONEGENINFO_H
#define LLVM_UTILS_TABLEGEN_CAPSTONEGENINFO_H

#include "../AsmWriterInst.h"
#include "../CodeGenDAGPatterns.h"
#include "../CodeGenSchedule.h"
#include "CodeGenInstruction.h"
#include "llvm/ADT/CachedHashString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/LEB128.h"

using namespace llvm;

#define DEBUG_TYPE "decoder-emitter"

STATISTIC(NumEncodings, "Number of encodings considered");
STATISTIC(NumEncodingsLackingDisasm,
          "Number of encodings without disassembler info");
STATISTIC(NumInstructions, "Number of instructions considered");
STATISTIC(NumEncodingsSupported, "Number of encodings supported");
STATISTIC(NumEncodingsOmitted, "Number of encodings omitted");

namespace {

struct EncodingField {
  unsigned Base, Width, Offset;
  EncodingField(unsigned B, unsigned W, unsigned O)
      : Base(B), Width(W), Offset(O) {}
};

struct OperandInfo {
  std::vector<EncodingField> Fields;
  std::string Decoder;
  bool HasCompleteDecoder;
  uint64_t InitValue;

  OperandInfo(std::string D, bool HCD)
      : Decoder(std::move(D)), HasCompleteDecoder(HCD), InitValue(0) {}

  void addField(unsigned Base, unsigned Width, unsigned Offset);

  unsigned numFields() const;

  typedef std::vector<EncodingField>::const_iterator const_iterator;

  const_iterator begin() const;
  const_iterator end() const;
};

typedef std::vector<uint8_t> DecoderTable;
typedef uint32_t DecoderFixup;
typedef std::vector<DecoderFixup> FixupList;
typedef std::vector<FixupList> FixupScopeList;
typedef SmallSetVector<CachedHashString, 16> PredicateSet;
typedef SmallSetVector<CachedHashString, 16> DecoderSet;
struct DecoderTableInfo {
  DecoderTable Table;
  FixupScopeList FixupStack;
  PredicateSet Predicates;
  DecoderSet Decoders;
};

struct EncodingAndInst {
  const Record *EncodingDef;
  const CodeGenInstruction *Inst;
  StringRef HwModeName;

  EncodingAndInst(const Record *EncodingDef, const CodeGenInstruction *Inst,
                  StringRef HwModeName = "")
      : EncodingDef(EncodingDef), Inst(Inst), HwModeName(HwModeName) {}
};

raw_ostream &operator<<(raw_ostream &OS, const EncodingAndInst &Value);

struct EncodingIDAndOpcode {
  unsigned EncodingID;
  unsigned Opcode;

  EncodingIDAndOpcode() : EncodingID(0), Opcode(0) {}
  EncodingIDAndOpcode(unsigned EncodingID, unsigned Opcode)
      : EncodingID(EncodingID), Opcode(Opcode) {}
};

const unsigned MAX_SUBTARGET_WORDS = 4;
const unsigned MAX_SUBTARGET_FEATURES = MAX_SUBTARGET_WORDS * 64;

class CapstoneGenInfo {
  typedef std::map<std::vector<std::string>, unsigned> OperandInfoMapTy;
  RecordKeeper &RK;
  CodeGenDAGPatterns CDP;
  std::vector<EncodingAndInst> NumberedEncodings;
  ArrayRef<const CodeGenInstruction *> NumberedInstructions;
  std::vector<llvm::AsmWriterInst> Instructions;
  const CodeGenSchedModels &SchedModels;

public:
  CapstoneGenInfo(RecordKeeper &R, std::string PredicateNamespace,
                  std::string GPrefix,
                  std::string GPostfix,
                  std::string ROK,
                  std::string RFail,
                  std::string L);

  // Emit the decoder state machine table.
  void emitTable(formatted_raw_ostream &o, DecoderTable &Table,
                 unsigned Indentation, unsigned BitWidth,
                 StringRef Namespace) const;
  void emitPredicateFunction(formatted_raw_ostream &OS,
                             PredicateSet &Predicates,
                             unsigned Indentation) const;
  void emitDecoderFunction(formatted_raw_ostream &OS, DecoderSet &Decoders,
                           unsigned Indentation) const;
  void Enumeration(raw_ostream &OS,
                   DenseMap<Record *, unsigned> &FeatureMap) const;
  void runEnums(raw_ostream &OS, CodeGenTarget &Target, CodeGenRegBank &Bank);
  void runMCDesc(raw_ostream &OS, CodeGenTarget &Target,
                 CodeGenRegBank &RegBank);

  void EmitGetMnemonic(
      raw_ostream &o,
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits);
  void EmitPrintInstruction(
      raw_ostream &o,
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits);
  void EmitGetRegisterName(raw_ostream &o);
  void EmitPrintAliasInstruction(raw_ostream &O);

  void FindUniqueOperandCommands(std::vector<std::string> &UOC,
                                 std::vector<std::vector<unsigned>> &InstIdxs,
                                 std::vector<unsigned> &InstOpsUsed,
                                 bool PassSubtarget) const;

  // run - Output the code emitter
  void run(raw_ostream &o);

private:
  CodeGenTarget Target;

public:
  std::string PredicateNamespace;
  std::string GuardPrefix, GuardPostfix;
  std::string ReturnOK, ReturnFail;
  std::string Locals;
  void EmitOperandInfo(raw_ostream &OS, OperandInfoMapTy &OperandInfoIDs);
  std::vector<std::string> GetOperandInfo(const CodeGenInstruction &Inst);
  void emitRecord(const CodeGenInstruction &Inst, unsigned Num,
                  Record *InstrInfo,
                  std::map<std::vector<Record *>, unsigned> &EmittedLists,
                  const OperandInfoMapTy &OpInfo, raw_ostream &OS);
};

} // end anonymous namespace

// The set (BIT_TRUE, BIT_FALSE, BIT_UNSET) represents a ternary logic system
// for a bit value.
//
// BIT_UNFILTERED is used as the init value for a filter position.  It is used
// only for filter processings.
typedef enum {
  BIT_TRUE,      // '1'
  BIT_FALSE,     // '0'
  BIT_UNSET,     // '?'
  BIT_UNFILTERED // unfiltered
} bit_value_t;
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
  const FilterChooser
      *Owner;        // points to the FilterChooser who owns this filter
  unsigned StartBit; // the starting bit position
  unsigned NumBits;  // number of bits to filter
  bool Mixed;        // a mixed region contains both set and unset bits

  // Map of well-known segment value to the set of uid's with that value.
  std::map<uint64_t, std::vector<EncodingIDAndOpcode>> FilteredInstructions;

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

// These are states of our finite state machines used in FilterChooser's
// filterProcessor() which produces the filter candidates to use.
typedef enum {
  ATTR_NONE,
  ATTR_FILTERED,
  ATTR_ALL_SET,
  ATTR_ALL_UNSET,
  ATTR_MIXED
} bitAttr_t;

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

  // Vector of codegen instructions to choose our filter.
  ArrayRef<EncodingAndInst> AllInstructions;

  // Vector of uid's for this filter chooser to work on.
  // The first member of the pair is the opcode id being decoded, the second is
  // the opcode id that should be emitted.
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

  // Parent emitter
  const CapstoneGenInfo *Emitter;

public:
  FilterChooser(ArrayRef<EncodingAndInst> Insts,
                const std::vector<EncodingIDAndOpcode> &IDs,
                const std::map<unsigned, std::vector<OperandInfo>> &Ops,
                unsigned BW, const CapstoneGenInfo *E)
      : AllInstructions(Insts), Opcodes(IDs), Operands(Ops),
        FilterBitValues(BW, BIT_UNFILTERED), Parent(nullptr), BestIndex(-1),
        BitWidth(BW), Emitter(E) {
    doFilter();
  }

  FilterChooser(ArrayRef<EncodingAndInst> Insts,
                const std::vector<EncodingIDAndOpcode> &IDs,
                const std::map<unsigned, std::vector<OperandInfo>> &Ops,
                const std::vector<bit_value_t> &ParentFilterBitValues,
                const FilterChooser &parent)
      : AllInstructions(Insts), Opcodes(IDs), Operands(Ops),
        FilterBitValues(ParentFilterBitValues), Parent(&parent), BestIndex(-1),
        BitWidth(parent.BitWidth), Emitter(parent.Emitter) {
    doFilter();
  }

  FilterChooser(const FilterChooser &) = delete;
  void operator=(const FilterChooser &) = delete;

  unsigned getBitWidth() const { return BitWidth; }

protected:
  // Populates the insn given the uid.
  void insnWithID(insn_t &Insn, unsigned Opcode) const;

  // Emit the name of the encoding/instruction pair.
  void emitNameWithID(raw_ostream &OS, unsigned Opcode) const;

  // Populates the field of the insn given the start position and the number of
  // consecutive bits to scan for.
  //
  // Returns false if there exists any uninitialized bit value in the range.
  // Returns true, otherwise.
  bool fieldFromInsn(uint64_t &Field, insn_t &Insn, unsigned StartBit,
                     unsigned NumBits) const;

  /// dumpFilterArray - dumpFilterArray prints out debugging info for the given
  /// filter array as a series of chars.
  void dumpFilterArray(raw_ostream &o,
                       const std::vector<bit_value_t> &filter) const;

  /// dumpStack - dumpStack traverses the filter chooser chain and calls
  /// dumpFilterArray on each filter chooser up to the top level one.
  void dumpStack(raw_ostream &o, const char *prefix) const;

  Filter &bestFilter() {
    assert(BestIndex != -1 && "BestIndex not set");
    return Filters[BestIndex];
  }

  bool PositionFiltered(unsigned i) const;

  // Calculates the island(s) needed to decode the instruction.
  // This returns a lit of undecoded bits of an instructions, for example,
  // Inst{20} = 1 && Inst{3-0} == 0b1111 represents two islands of yet-to-be
  // decoded bits in order to verify that the instruction matches the Opcode.
  unsigned getIslands(std::vector<unsigned> &StartBits,
                      std::vector<unsigned> &EndBits,
                      std::vector<uint64_t> &FieldVals,
                      const insn_t &Insn) const;

  // Emits code to check the Predicates member of an instruction are true.
  // Returns true if predicate matches were emitted, false otherwise.
  bool emitPredicateMatch(raw_ostream &o, unsigned &Indentation,
                          unsigned Opc) const;

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

  void emitBinaryParser(raw_ostream &o, unsigned &Indentation,
                        const OperandInfo &OpInfo,
                        bool &OpHasCompleteDecoder) const;

  void emitDecoder(raw_ostream &OS, unsigned Indentation, unsigned Opc,
                   bool &HasCompleteDecoder) const;
  unsigned getDecoderIndex(DecoderSet &Decoders, unsigned Opc,
                           bool &HasCompleteDecoder) const;

  // Assign a single filter and run with it.
  void runSingleFilter(unsigned startBit, unsigned numBit, bool mixed);

  // reportRegion is a helper function for filterProcessor to mark a region as
  // eligible for use as a filter region.
  void reportRegion(bitAttr_t RA, unsigned StartBit, unsigned BitIndex,
                    bool AllowMixed);

  // FilterProcessor scans the well-known encoding bits of the instructions and
  // builds up a list of candidate filters.  It chooses the best filter and
  // recursively descends down the decoding tree.
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

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEGENINFO_H
