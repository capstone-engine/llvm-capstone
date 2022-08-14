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

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEHELPER_H
