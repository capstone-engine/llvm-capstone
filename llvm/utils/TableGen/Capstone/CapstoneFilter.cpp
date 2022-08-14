//===-- CapstoneFilter.cpp - Capstone filter methods ----------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CapstoneHelper.h"
#include "CapstoneGenInfo.h"

Filter::Filter(Filter &&f)
    : Owner(f.Owner), StartBit(f.StartBit), NumBits(f.NumBits), Mixed(f.Mixed),
      FilteredInstructions(std::move(f.FilteredInstructions)),
      VariableInstructions(std::move(f.VariableInstructions)),
      FilterChooserMap(std::move(f.FilterChooserMap)),
      NumFiltered(f.NumFiltered), LastOpcFiltered(f.LastOpcFiltered) {}

Filter::Filter(FilterChooser &owner, unsigned startBit, unsigned numBits, bool mixed)
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

  assert((FilteredInstructions.size() + VariableInstructions.size() > 0) &&
         "Filter returns no instruction categories");
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
        std::make_unique<FilterChooser>(Owner->AllInstructions,
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
  for (const auto &Inst : FilteredInstructions) {

    // Marks all the segment positions with either BIT_TRUE or BIT_FALSE.
    for (unsigned bitIndex = 0; bitIndex < NumBits; ++bitIndex) {
      if (Inst.first & (1ULL << bitIndex))
        BitValueArray[StartBit + bitIndex] = BIT_TRUE;
      else
        BitValueArray[StartBit + bitIndex] = BIT_FALSE;
    }

    // Delegates to an inferior filter chooser for further processing on this
    // category of instructions.
    FilterChooserMap.insert(std::make_pair(
        Inst.first, std::make_unique<FilterChooser>(
                        Owner->AllInstructions, Inst.second, Owner->Operands,
                        BitValueArray, *Owner)));
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
      PrevFilter = 0; // Don't re-process the filter's fallthrough.
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
      assert(NumToSkip < (1u << 24) &&
             "disassembler decoding table too large!");
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
  return FilteredInstructions.size() + 1;
}

