//===------- DecoderEmitterTypes.h - Decoder Generator ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_DECODEREMITTERTYPES_H
#define LLVM_UTILS_TABLEGEN_DECODEREMITTERTYPES_H

#include "CodeGenInstruction.h"
#include "CodeGenTarget.h"
#include "InfoByHwMode.h"
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

//////////////////////
// Structs and Types
//////////////////////

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

  void addField(unsigned Base, unsigned Width, unsigned Offset) {
    Fields.push_back(EncodingField(Base, Width, Offset));
  }

  unsigned numFields() const { return Fields.size(); }

  typedef std::vector<EncodingField>::const_iterator const_iterator;

  const_iterator begin() const { return Fields.begin(); }
  const_iterator end() const { return Fields.end(); }
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

struct EncodingIDAndOpcode {
  unsigned EncodingID;
  unsigned Opcode;

  EncodingIDAndOpcode() : EncodingID(0), Opcode(0) {}
  EncodingIDAndOpcode(unsigned EncodingID, unsigned Opcode)
      : EncodingID(EncodingID), Opcode(Opcode) {}
};

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

// These are states of our finite state machines used in FilterChooser's
// filterProcessor() which produces the filter candidates to use.
typedef enum {
  ATTR_NONE,
  ATTR_FILTERED,
  ATTR_ALL_SET,
  ATTR_ALL_UNSET,
  ATTR_MIXED
} bitAttr_t;

inline raw_ostream &operator<<(raw_ostream &OS, const EncodingAndInst &Value) {
  if (Value.EncodingDef != Value.Inst->TheDef)
    OS << Value.EncodingDef->getName() << ":";
  OS << Value.Inst->TheDef->getName();
  return OS;
}

#endif // LLVM_UTILS_TABLEGEN_DECODEREMITTERTYPES_H
