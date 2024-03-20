//===------------- PrinterTypes.h - Printer Interface -----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_PRINTERTYPES_H
#define LLVM_UTILS_TABLEGEN_PRINTERTYPES_H

#include "llvm/ADT/BitVector.h"

namespace llvm {

enum PrinterLanguage {
  PRINTER_LANG_CPP,
  PRINTER_LANG_CAPSTONE_C,
};

} // end namespace llvm

#endif // LLVM_UTILS_TABLEGEN_PRINTERTYPES_H
