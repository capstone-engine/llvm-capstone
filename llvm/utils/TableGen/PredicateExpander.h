//===--------------------- PredicateExpander.h ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// Functionalities used by the Tablegen backends to expand machine predicates.
///
/// See file llvm/Target/TargetInstrPredicate.td for a full list and description
/// of all the supported MCInstPredicate classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_PREDICATEEXPANDER_H
#define LLVM_UTILS_TABLEGEN_PREDICATEEXPANDER_H

#include "llvm/ADT/StringRef.h"
#include <vector>

namespace llvm {

// Forward declarations.
class STIPredicateFunction;
class OpcodeGroup;
class raw_ostream;
class Record;
class PredicateExpanderLLVM;
class PredicateExpanderCapstone;

class PredicateExpander {

  friend PredicateExpanderLLVM;
  friend PredicateExpanderCapstone;

  bool EmitCallsByRef;
  bool NegatePredicate;
  bool ExpandForMC;
  unsigned IndentLevel;
  StringRef TargetName;
  // STI only
  bool ExpandDefinition;
  StringRef ClassPrefix;

  PredicateExpander(const PredicateExpander &) = delete;
  PredicateExpander &operator=(const PredicateExpander &) = delete;

private:
  virtual void expandHeader(raw_ostream &OS, const STIPredicateFunction &Fn) = 0;
  virtual void expandPrologue(raw_ostream &OS, const STIPredicateFunction &Fn) = 0;
  virtual void expandOpcodeGroup(raw_ostream &OS, const OpcodeGroup &Group,
                                 bool ShouldUpdateOpcodeMask) = 0;
  virtual void expandBody(raw_ostream &OS, const STIPredicateFunction &Fn) = 0;
  virtual void expandEpilogue(raw_ostream &OS, const STIPredicateFunction &Fn) = 0;

public:
  PredicateExpander(StringRef Target)
      : EmitCallsByRef(true), NegatePredicate(false), ExpandForMC(false),
        IndentLevel(1U), TargetName(Target), ExpandDefinition(false) {}
  virtual ~PredicateExpander() {}
  bool isByRef() const { return EmitCallsByRef; }
  bool shouldNegate() const { return NegatePredicate; }
  bool shouldExpandForMC() const { return ExpandForMC; }
  unsigned getIndentLevel() const { return IndentLevel; }
  StringRef getTargetName() const { return TargetName; }

  void setByRef(bool Value) { EmitCallsByRef = Value; }
  void flipNegatePredicate() { NegatePredicate = !NegatePredicate; }
  void setNegatePredicate(bool Value) { NegatePredicate = Value; }
  void setExpandForMC(bool Value) { ExpandForMC = Value; }
  void setIndentLevel(unsigned Level) { IndentLevel = Level; }
  void increaseIndentLevel() { ++IndentLevel; }
  void decreaseIndentLevel() { --IndentLevel; }

  using RecVec = std::vector<Record *>;
  virtual void expandTrue(raw_ostream &OS) = 0;
  virtual void expandFalse(raw_ostream &OS) = 0;
  virtual void expandCheckImmOperand(raw_ostream &OS, int OpIndex, int ImmVal,
                                     StringRef FunctionMapper) = 0;
  virtual void expandCheckImmOperand(raw_ostream &OS, int OpIndex,
                                     StringRef ImmVal,
                                     StringRef FunctionMapperer) = 0;
  virtual void expandCheckImmOperandSimple(raw_ostream &OS, int OpIndex,
                                           StringRef FunctionMapper) = 0;
  virtual void expandCheckRegOperand(raw_ostream &OS, int OpIndex,
                                     const Record *Reg,
                                     StringRef FunctionMapper) = 0;
  virtual void expandCheckRegOperandSimple(raw_ostream &OS, int OpIndex,
                                           StringRef FunctionMapper) = 0;
  virtual void expandCheckSameRegOperand(raw_ostream &OS, int First,
                                         int Second) = 0;
  virtual void expandCheckNumOperands(raw_ostream &OS, int NumOps) = 0;
  virtual void expandCheckOpcode(raw_ostream &OS, const Record *Inst) = 0;

  virtual void expandCheckPseudo(raw_ostream &OS, const RecVec &Opcodes) = 0;
  virtual void expandCheckOpcode(raw_ostream &OS, const RecVec &Opcodes) = 0;
  virtual void expandPredicateSequence(raw_ostream &OS, const RecVec &Sequence,
                                       bool IsCheckAll) = 0;
  virtual void expandTIIFunctionCall(raw_ostream &OS, StringRef MethodName) = 0;
  virtual void expandCheckIsRegOperand(raw_ostream &OS, int OpIndex) = 0;
  virtual void expandCheckIsImmOperand(raw_ostream &OS, int OpIndex) = 0;
  virtual void expandCheckInvalidRegOperand(raw_ostream &OS, int OpIndex) = 0;
  virtual void expandCheckFunctionPredicate(raw_ostream &OS, StringRef MCInstFn,
                                            StringRef MachineInstrFn) = 0;
  virtual void expandCheckFunctionPredicateWithTII(raw_ostream &OS,
                                                   StringRef MCInstFn,
                                                   StringRef MachineInstrFn,
                                                   StringRef TIIPtr) = 0;
  virtual void expandCheckNonPortable(raw_ostream &OS, StringRef CodeBlock) = 0;
  virtual void expandPredicate(raw_ostream &OS, const Record *Rec) = 0;
  virtual void expandReturnStatement(raw_ostream &OS, const Record *Rec) = 0;
  virtual void expandOpcodeSwitchCase(raw_ostream &OS, const Record *Rec) = 0;
  virtual void expandOpcodeSwitchStatement(raw_ostream &OS, const RecVec &Cases,
                                           const Record *Default) = 0;
  virtual void expandStatement(raw_ostream &OS, const Record *Rec) = 0;

  // STI only
  bool shouldExpandDefinition() const { return ExpandDefinition; }
  StringRef getClassPrefix() const { return ClassPrefix; }
  void setClassPrefix(StringRef S) { ClassPrefix = S; }
  void setExpandDefinition(bool Value) { ExpandDefinition = Value; }

  virtual void expandSTIPredicate(raw_ostream &OS,
                                  const STIPredicateFunction &Fn) = 0;
};

class PredicateExpanderLLVM : public PredicateExpander {
  using PredicateExpander::PredicateExpander;

  void expandHeader(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandPrologue(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandOpcodeGroup(raw_ostream &OS, const OpcodeGroup &Group,
                         bool ShouldUpdateOpcodeMask) override;
  void expandBody(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandEpilogue(raw_ostream &OS, const STIPredicateFunction &Fn) override;

  void expandTrue(raw_ostream &OS) override;
  void expandFalse(raw_ostream &OS) override;
  void expandCheckImmOperand(raw_ostream &OS, int OpIndex, int ImmVal,
                             StringRef FunctionMapper) override;
  void expandCheckImmOperand(raw_ostream &OS, int OpIndex, StringRef ImmVal,
                             StringRef FunctionMapperer) override;
  void expandCheckImmOperandSimple(raw_ostream &OS, int OpIndex,
                                   StringRef FunctionMapper) override;
  void expandCheckRegOperand(raw_ostream &OS, int OpIndex, const Record *Reg,
                             StringRef FunctionMapper) override;
  void expandCheckRegOperandSimple(raw_ostream &OS, int OpIndex,
                                   StringRef FunctionMapper) override;
  void expandCheckSameRegOperand(raw_ostream &OS, int First,
                                 int Second) override;
  void expandCheckNumOperands(raw_ostream &OS, int NumOps) override;
  void expandCheckOpcode(raw_ostream &OS, const Record *Inst) override;

  void expandCheckPseudo(raw_ostream &OS, const RecVec &Opcodes) override;
  void expandCheckOpcode(raw_ostream &OS, const RecVec &Opcodes) override;
  void expandPredicateSequence(raw_ostream &OS, const RecVec &Sequence,
                               bool IsCheckAll) override;
  void expandTIIFunctionCall(raw_ostream &OS, StringRef MethodName) override;
  void expandCheckIsRegOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckIsImmOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckInvalidRegOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckFunctionPredicate(raw_ostream &OS, StringRef MCInstFn,
                                    StringRef MachineInstrFn) override;
  void expandCheckFunctionPredicateWithTII(raw_ostream &OS, StringRef MCInstFn,
                                           StringRef MachineInstrFn,
                                           StringRef TIIPtr) override;
  void expandCheckNonPortable(raw_ostream &OS, StringRef CodeBlock) override;
  void expandPredicate(raw_ostream &OS, const Record *Rec) override;
  void expandReturnStatement(raw_ostream &OS, const Record *Rec) override;
  void expandOpcodeSwitchCase(raw_ostream &OS, const Record *Rec) override;
  void expandOpcodeSwitchStatement(raw_ostream &OS, const RecVec &Cases,
                                   const Record *Default) override;
  void expandStatement(raw_ostream &OS, const Record *Rec) override;

  // STI only
  void expandSTIPredicate(raw_ostream &OS,
                          const STIPredicateFunction &Fn) override;
};

class PredicateExpanderCapstone : public PredicateExpander {
  using PredicateExpander::PredicateExpander;

  void expandHeader(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandPrologue(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandOpcodeGroup(raw_ostream &OS, const OpcodeGroup &Group,
                         bool ShouldUpdateOpcodeMask) override;
  void expandBody(raw_ostream &OS, const STIPredicateFunction &Fn) override;
  void expandEpilogue(raw_ostream &OS, const STIPredicateFunction &Fn) override;

  void expandTrue(raw_ostream &OS) override;
  void expandFalse(raw_ostream &OS) override;
  void expandCheckImmOperand(raw_ostream &OS, int OpIndex, int ImmVal,
                             StringRef FunctionMapper) override;
  void expandCheckImmOperand(raw_ostream &OS, int OpIndex, StringRef ImmVal,
                             StringRef FunctionMapperer) override;
  void expandCheckImmOperandSimple(raw_ostream &OS, int OpIndex,
                                   StringRef FunctionMapper) override;
  void expandCheckRegOperand(raw_ostream &OS, int OpIndex, const Record *Reg,
                             StringRef FunctionMapper) override;
  void expandCheckRegOperandSimple(raw_ostream &OS, int OpIndex,
                                   StringRef FunctionMapper) override;
  void expandCheckSameRegOperand(raw_ostream &OS, int First,
                                 int Second) override;
  void expandCheckNumOperands(raw_ostream &OS, int NumOps) override;
  void expandCheckOpcode(raw_ostream &OS, const Record *Inst) override;

  void expandCheckPseudo(raw_ostream &OS, const RecVec &Opcodes) override;
  void expandCheckOpcode(raw_ostream &OS, const RecVec &Opcodes) override;
  void expandPredicateSequence(raw_ostream &OS, const RecVec &Sequence,
                               bool IsCheckAll) override;
  void expandTIIFunctionCall(raw_ostream &OS, StringRef MethodName) override;
  void expandCheckIsRegOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckIsImmOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckInvalidRegOperand(raw_ostream &OS, int OpIndex) override;
  void expandCheckFunctionPredicate(raw_ostream &OS, StringRef MCInstFn,
                                    StringRef MachineInstrFn) override;
  void expandCheckFunctionPredicateWithTII(raw_ostream &OS, StringRef MCInstFn,
                                           StringRef MachineInstrFn,
                                           StringRef TIIPtr) override;
  void expandCheckNonPortable(raw_ostream &OS, StringRef CodeBlock) override;
  void expandPredicate(raw_ostream &OS, const Record *Rec) override;
  void expandReturnStatement(raw_ostream &OS, const Record *Rec) override;
  void expandOpcodeSwitchCase(raw_ostream &OS, const Record *Rec) override;
  void expandOpcodeSwitchStatement(raw_ostream &OS, const RecVec &Cases,
                                   const Record *Default) override;
  void expandStatement(raw_ostream &OS, const Record *Rec) override;

  // STI only
  void expandSTIPredicate(raw_ostream &OS,
                          const STIPredicateFunction &Fn) override;
};

} // namespace llvm

#endif
