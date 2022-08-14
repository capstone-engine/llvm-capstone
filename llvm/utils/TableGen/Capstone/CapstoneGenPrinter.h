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
#include "CapstoneGenInfo.h"

#include "CodeGenInstruction.h"


#include "llvm/ADT/StringExtras.h"
#include <llvm/Support/FormatVariadic.h>

#include "llvm/TableGen/Record.h"


using namespace llvm;

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

  void addOperand(StringRef Op, int OpIdx, int PrintMethodIdx);

  unsigned getNumMIOps() { return NumMIOps; }

  StringRef getResult() { return Result; }

  bool isOpMapped(StringRef Op) { return OpMap.find(Op) != OpMap.end(); }
  int getOpIndex(StringRef Op) { return OpMap[Op].first; }
  std::pair<int, int> &getOpData(StringRef Op) { return OpMap[Op]; }

  std::pair<StringRef, StringRef::iterator> parseName(StringRef::iterator Start,
                                                      StringRef::iterator End);

  std::string formatAliasString(uint32_t &UnescapedSize);

  bool operator==(const IAPrinter &RHS) const;
};

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

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEGENPRINTER_H
