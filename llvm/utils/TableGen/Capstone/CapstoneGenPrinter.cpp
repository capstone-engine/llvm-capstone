//===-- CapstoneGenPrinter.cpp - Printer Generation Module ----\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CapstoneGenPrinter.h"


void IAPrinter::addOperand(StringRef Op, int OpIdx, int PrintMethodIdx = -1) {
  assert(OpIdx >= 0 && OpIdx < 0xFE && "Idx out of range");
  assert(PrintMethodIdx >= -1 && PrintMethodIdx < 0xFF && "Idx out of range");
  OpMap[Op] = std::make_pair(OpIdx, PrintMethodIdx);
}

std::pair<StringRef, StringRef::iterator> IAPrinter::parseName(StringRef::iterator Start, StringRef::iterator End) {
  StringRef::iterator I = Start;
  StringRef::iterator Next;
  if (*I == '{') {
    // ${some_name}
    Start = ++I;
    while (I != End && *I != '}')
      ++I;
    Next = I;
    // eat the final '}'
    if (Next != End)
      ++Next;
  } else {
    // $name, just eat the usual suspects.
    while (I != End && (isAlnum(*I) || *I == '_'))
      ++I;
    Next = I;
  }

  return std::make_pair(StringRef(Start, I - Start), Next);
}

std::string IAPrinter::formatAliasString(uint32_t &UnescapedSize) {
  // Directly mangle mapped operands into the string. Each operand is
  // identified by a '$' sign followed by a byte identifying the number of the
  // operand. We add one to the index to avoid zero bytes.
  StringRef ASM(AsmString);
  std::string OutString;
  raw_string_ostream OS(OutString);
  for (StringRef::iterator I = ASM.begin(), E = ASM.end(); I != E;) {
    OS << *I;
    ++UnescapedSize;
    if (*I == '$') {
      StringRef Name;
      std::tie(Name, I) = parseName(++I, E);
      assert(isOpMapped(Name) && "Unmapped operand!");

      int OpIndex, PrintIndex;
      std::tie(OpIndex, PrintIndex) = getOpData(Name);
      if (PrintIndex == -1) {
        // Can use the default printOperand route.
        OS << format("\\x%02X", (unsigned char)OpIndex + 1);
        ++UnescapedSize;
      } else {
        // 3 bytes if a PrintMethod is needed: 0xFF, the MCInst operand
        // number, and which of our pre-detected Methods to call.
        OS << format("\\xFF\\x%02X\\x%02X", OpIndex + 1, PrintIndex + 1);
        UnescapedSize += 3;
      }
    } else {
      ++I;
    }
  }

  OS.flush();
  return OutString;
}

bool IAPrinter::operator==(const IAPrinter &RHS) const {
  if (NumMIOps != RHS.NumMIOps)
    return false;
  if (Conds.size() != RHS.Conds.size())
    return false;

  unsigned Idx = 0;
  for (const auto &str : Conds)
    if (str != RHS.Conds[Idx++])
      return false;

  return true;
}
