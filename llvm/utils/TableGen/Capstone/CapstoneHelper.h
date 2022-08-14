//===-- CapstoneHelper.h - Helper methods ---------------------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_CAPSTONEHELPER_H
#define LLVM_UTILS_TABLEGEN_CAPSTONEHELPER_H

#include "CapstoneGenInfo.h"

typedef SmallVector<uint16_t, 4> DiffVec;
typedef SmallVector<LaneBitmask, 4> MaskVec;

class BitVectorEmitter {
  BitVector Values;

public:
  void add(unsigned v);
  void print(raw_ostream &OS);
};

bool ValueSet(bit_value_t V);
bool ValueNotSet(bit_value_t V);
int Value(bit_value_t V);
bit_value_t bitFromBits(const BitsInit &bits, unsigned index);
void dumpBits(raw_ostream &o, const BitsInit &bits);
BitsInit &getBitsField(const Record &def, StringRef str);
void resolveTableFixups(DecoderTable &Table, const FixupList &Fixups, uint32_t DestIdx);
std::string findOperandDecoderMethod(TypedInit *TI);
bool populateInstruction(CodeGenTarget &Target, const Record &EncodingDef, const CodeGenInstruction &CGI, unsigned Opc, std::map<unsigned, std::vector<OperandInfo>> &Operands);
void emitFieldFromInstruction(formatted_raw_ostream &OS);
void emitDecodeInstruction(formatted_raw_ostream &OS);
void printDiff16(raw_ostream &OS, uint16_t Val);
DiffVec &diffEncode(DiffVec &V, unsigned InitVal, SparseBitVector<> List);
template <typename Iter>
DiffVec &diffEncode(DiffVec &V, unsigned InitVal, Iter Begin, Iter End);
void printSubRegIndex(raw_ostream &OS, const CodeGenSubRegIndex *Idx);
void printBitVectorAsHex(raw_ostream &OS, const BitVector &Bits, unsigned Width);

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEHELPER_H
