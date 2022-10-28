//===---- InstrInfoEmitterTypes.h - Instruction Set Desc. ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_INSTRINFOEMITTERTYPES_H
#define LLVM_UTILS_TABLEGEN_INSTRINFOEMITTERTYPES_H

#include "CodeGenDAGPatterns.h"
#include "CodeGenInstruction.h"
#include "CodeGenSchedule.h"
#include "CodeGenTarget.h"
#include "PredicateExpander.h"
#include "SequenceToOffsetTable.h"
#include "SubtargetFeatureInfo.h"
#include "TableGenBackends.h"
#include "Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <cassert>
#include <cstdint>
#include <iterator>
#include <map>
#include <string>
#include <utility>
#include <vector>

typedef std::map<std::vector<std::string>, unsigned> OperandInfoMapTy;

/// The keys of this map are maps which have OpName enum values as their keys
/// and instruction operand indices as their values.  The values of this map
/// are lists of instruction names.
typedef std::map<std::map<unsigned, unsigned>, std::vector<std::string>>
    OpNameMapTy;
typedef std::map<std::string, unsigned>::iterator StrUintMapIter;

#endif // LLVM_UTILS_TABLEGEN_INSTRINFOEMITTERTYPES_H
