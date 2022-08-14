//===------------ FixedLenDecoderEmitter.cpp - Decoder Generator ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// It contains the tablegen backend that emits the decoder functions for
// targets with fixed length instruction set.
//
//===----------------------------------------------------------------------===//

#include "CodeGenInstruction.h"
#include "CodeGenTarget.h"
#include "InfoByHwMode.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/CachedHashString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCCodeView.h"
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

#include "Capstone/CapstoneGenInfo.h"
#include "Capstone/CapstoneGenMapper.h"

namespace llvm {

void EmitCapstone(RecordKeeper &RK, raw_ostream &OS) {
  CapstoneGenInfo(RK, CodeGenTarget(RK).getName().str(), "if (",
                  " == MCDisassembler_Fail)", "S", "MCDisassembler_Fail",
                  "  DecodeStatus S = "
                  "MCDisassembler_Success;\n(void)S;")
      .run(OS);
}

void EmitCapstoneMapper(RecordKeeper &RK, raw_ostream &OS) {
  CapstoneGenMapper(RK).run(OS);
}

} // end namespace llvm
