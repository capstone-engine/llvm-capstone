//===-- SearchableTableEmitter.h - Generate efficiently searchable tables --==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_SEARCHABLETABLESTYPES_H
#define LLVM_UTILS_TABLEGEN_SEARCHABLETABLESTYPES_H

#include "CodeGenIntrinsics.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include <algorithm>
#include <set>
#include <string>
#include <vector>

using namespace llvm;

int getAsInt(Init *B);

struct GenericEnum {
  using Entry = std::pair<StringRef, int64_t>;

  std::string Name;
  Record *Class = nullptr;
  std::string PreprocessorGuard;
  std::vector<std::unique_ptr<Entry>> Entries;
  DenseMap<Record *, Entry *> EntryMap;
};

struct GenericField {
  std::string Name;
  RecTy *RecType = nullptr;
  bool IsCode = false;
  bool IsIntrinsic = false;
  bool IsInstruction = false;
  GenericEnum *Enum = nullptr;

  GenericField(StringRef Name) : Name(std::string(Name)) {}
};

struct SearchIndex {
  std::string Name;
  SMLoc Loc; // Source location of PrimaryKey or Key field definition.
  SmallVector<GenericField, 1> Fields;
  bool EarlyOut = false;
};

struct GenericTable {
  std::string Name;
  ArrayRef<SMLoc> Locs; // Source locations from the Record instance.
  std::string PreprocessorGuard;
  std::string CppTypeName;
  SmallVector<GenericField, 2> Fields;
  std::vector<Record *> Entries;

  std::unique_ptr<SearchIndex> PrimaryKey;
  SmallVector<std::unique_ptr<SearchIndex>, 2> Indices;

  const GenericField *getFieldByName(StringRef Name) const {
    for (const auto &Field : Fields) {
      if (Name == Field.Name)
        return &Field;
    }
    return nullptr;
  }
};

enum TypeContext {
  TypeInStaticStruct,
  TypeInTempStruct,
  TypeInArgument,
};

#endif // LLVM_UTILS_TABLEGEN_SEARCHABLETABLESTYPES_H