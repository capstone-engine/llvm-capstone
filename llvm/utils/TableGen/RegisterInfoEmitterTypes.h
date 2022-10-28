//===- RegisterInfoEmitterTypes.h - Register Emitter Types -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===-----------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_REGISTERINFOEMITTERTYPES_H
#define LLVM_UTILS_TABLEGEN_REGISTERINFOEMITTERTYPES_H

#include "CodeGenRegisters.h"
#include "CodeGenTarget.h"
#include "SequenceToOffsetTable.h"
#include "Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SparseBitVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MachineValueType.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/SetTheory.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <iterator>
#include <set>
#include <string>
#include <vector>

using namespace llvm;

// Compress the sub-reg index lists.
typedef std::vector<const CodeGenSubRegIndex *> IdxList;

// Keep track of sub-register names as well. These are not differentially
// encoded.
typedef SmallVector<const CodeGenSubRegIndex *, 4> SubRegIdxVec;

// Differentially encoded register and regunit lists allow for better
// compression on regular register banks. The sequence is computed from the
// differential list as:
//
//   out[0] = InitVal;
//   out[n+1] = out[n] + diff[n]; // n = 0, 1, ...
//
// The initial value depends on the specific list. The list is terminated by a
// 0 differential which means we can't encode repeated elements.
typedef SmallVector<uint16_t, 4> DiffVec;
typedef SmallVector<LaneBitmask, 4> MaskVec;

// The lists of sub-registers and super-registers go in the same array.  That
// allows us to share suffixes.
typedef std::vector<const CodeGenRegister *> RegVec;

using DwarfRegNumsMapPair = std::pair<Record*, std::vector<int64_t>>;
using DwarfRegNumsVecTy = std::vector<DwarfRegNumsMapPair>;

#endif // LLVM_UTILS_TABLEGEN_REGISTERINFOEMITTERTYPES_H
