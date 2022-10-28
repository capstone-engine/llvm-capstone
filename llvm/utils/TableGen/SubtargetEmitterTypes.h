//===- SubtargetEmitterTypes.h - Generate subtarget enumerations types ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_SUBTARGETEMITTERTYPES_H
#define LLVM_UTILS_TABLEGEN_SUBTARGETEMITTERTYPES_H

#include "CodeGenSchedule.h"
#include "CodeGenTarget.h"
#include "PredicateExpander.h"
#include "PrinterTypes.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/MC/MCSchedule.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <map>
#include <string>
#include <vector>

using namespace llvm;

// Each processor has a SchedClassDesc table with an entry for each SchedClass.
// The SchedClassDesc table indexes into a global write resource table, write
// latency table, and read advance table.
typedef struct SchedClassTablesStruct {
  std::vector<std::vector<MCSchedClassDesc>> ProcSchedClasses;
  std::vector<MCWriteProcResEntry> WriteProcResources;
  std::vector<MCWriteLatencyEntry> WriteLatencies;
  std::vector<std::string> WriterNames;
  std::vector<MCReadAdvanceEntry> ReadAdvanceEntries;

  // Reserve an invalid entry at index 0
  SchedClassTablesStruct() {
    ProcSchedClasses.resize(1);
    WriteProcResources.resize(1);
    WriteLatencies.resize(1);
    WriterNames.push_back("InvalidWrite");
    ReadAdvanceEntries.resize(1);
  }
} SchedClassTablesT;

#endif // LLVM_UTILS_TABLEGEN_SUBTARGETEMITTERTYPES_H
