//===- llvm/TableGen/TableGenBackend.h - Backend utilities ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Useful utilities for TableGen backends.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TABLEGEN_TABLEGENBACKEND_H
#define LLVM_TABLEGEN_TABLEGENBACKEND_H

#include "Printer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include <memory>

namespace llvm {

class RecordKeeper;
class raw_ostream;

namespace TableGen::Emitter {
std::unique_ptr<PrinterLLVM> PI;
using FnT = void (*)(RecordKeeper &Records, raw_ostream &OS);

struct OptCreatorT {
  static void *call();
};

extern ManagedStatic<cl::opt<FnT>, OptCreatorT> Action;

struct Opt {
  Opt(StringRef Name, FnT CB, StringRef Desc, bool ByDefault = false) {
    if (ByDefault)
      Action->setInitialValue(CB);
    Action->getParser().addLiteralOption(Name, CB, Desc);
  }
};

template <class EmitterC> class OptClass : Opt {
  static void run(RecordKeeper &RK, raw_ostream &OS) {
  EmitterC E = EmitterC(RK);

  CodeGenTarget CGTarget(RK);
  PrinterLanguage const PL = PrinterLLVM::getLanguage();

  formatted_raw_ostream FOS(OS);
  if (PL == PRINTER_LANG_CPP) {
    E.PI = new PrinterLLVM(FOS, CGTarget.getName().str());
  } else if (PL == PRINTER_LANG_CAPSTONE_C) {
    E.PI = new PrinterCapstone(FOS, CGTarget.getName().str());
  } else {
    llvm_unreachable("AsmWriterEmitter does not support the given output language.");
  }
  E.run(OS);
}

public:
  OptClass(StringRef Name, StringRef Desc) : Opt(Name, run, Desc) {}
};

} // namespace TableGen::Emitter

/// emitSourceFileHeader - Output an LLVM style file header to the specified
/// raw_ostream.
void emitSourceFileHeader(StringRef Desc, raw_ostream &OS);

} // End llvm namespace

#endif
