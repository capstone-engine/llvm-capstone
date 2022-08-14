//===-- CapstoneGenPrinter.h - Printer Generation Module ------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// Created by Phosphorus15 on 2021/5/14.
// Assembly Printer Module
//

#ifndef LLVM_UTILS_TABLEGEN_CAPSTONEGENPRINTER_H
#define LLVM_UTILS_TABLEGEN_CAPSTONEGENPRINTER_H

#include "../AsmWriterInst.h"
#include "../Types.h"
#include "CodeGenInstruction.h"
#include "CapstoneGenInfo.h"

#include "llvm/ADT/StringExtras.h"
#include <llvm/Support/FormatVariadic.h>

#include "llvm/TableGen/Record.h"


using namespace llvm;

namespace {

// IAPrinter - Holds information about an InstAlias. Two InstAliases match if
// they both have the same conditionals. In which case, we cannot print out the
// alias for that pattern.
class IAPrinter {
  std::map<StringRef, std::pair<int, int>> OpMap;

  std::vector<std::string> Conds;

  std::string Result;
  std::string AsmString;

  unsigned NumMIOps;

public:
  IAPrinter(std::string R, std::string AS, unsigned NumMIOps)
      : Result(std::move(R)), AsmString(std::move(AS)), NumMIOps(NumMIOps) {}

  void addCond(std::string C) { Conds.push_back(std::move(C)); }
  ArrayRef<std::string> getConds() const { return Conds; }
  size_t getCondCount() const { return Conds.size(); }

  void addOperand(StringRef Op, int OpIdx, int PrintMethodIdx = -1) {
    assert(OpIdx >= 0 && OpIdx < 0xFE && "Idx out of range");
    assert(PrintMethodIdx >= -1 && PrintMethodIdx < 0xFF && "Idx out of range");
    OpMap[Op] = std::make_pair(OpIdx, PrintMethodIdx);
  }

  unsigned getNumMIOps() { return NumMIOps; }

  StringRef getResult() { return Result; }

  bool isOpMapped(StringRef Op) { return OpMap.find(Op) != OpMap.end(); }
  int getOpIndex(StringRef Op) { return OpMap[Op].first; }
  std::pair<int, int> &getOpData(StringRef Op) { return OpMap[Op]; }

  std::pair<StringRef, StringRef::iterator> parseName(StringRef::iterator Start,
                                                      StringRef::iterator End) {
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

  std::string formatAliasString(uint32_t &UnescapedSize) {
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

  bool operator==(const IAPrinter &RHS) const {
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
};

} // end anonymous namespace

namespace {

struct AliasPriorityComparator {
  typedef std::pair<CodeGenInstAlias, int> ValueType;
  bool operator()(const ValueType &LHS, const ValueType &RHS) const {
    if (LHS.second == RHS.second) {
      // We don't actually care about the order, but for consistency it
      // shouldn't depend on pointer comparisons.
      return LessRecordByID()(LHS.first.TheDef, RHS.first.TheDef);
    }

    // Aliases with larger priorities should be considered first.
    return LHS.second > RHS.second;
  }
};

} // end anonymous namespace

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEGENPRINTER_H
