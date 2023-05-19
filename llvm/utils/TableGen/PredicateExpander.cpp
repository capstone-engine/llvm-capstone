//===--------------------- PredicateExpander.cpp --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// Functionalities used by the Tablegen backends to expand machine predicates.
//
//===----------------------------------------------------------------------===//

#include "PredicateExpander.h"
#include "CodeGenSchedule.h" // Definition of STIPredicateFunction.

namespace llvm {

void PredicateExpanderLLVM::expandTrue(raw_ostream &OS) { OS << "true"; }
void PredicateExpanderLLVM::expandFalse(raw_ostream &OS) { OS << "false"; }

void PredicateExpanderLLVM::expandCheckImmOperand(raw_ostream &OS, int OpIndex,
                                                  int ImmVal,
                                                  StringRef FunctionMapper) {
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ") << ImmVal;
}

void PredicateExpanderLLVM::expandCheckImmOperand(raw_ostream &OS, int OpIndex,
                                                  StringRef ImmVal,
                                                  StringRef FunctionMapper) {
  if (ImmVal.empty())
    expandCheckImmOperandSimple(OS, OpIndex, FunctionMapper);

  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ") << ImmVal;
}

void PredicateExpanderLLVM::expandCheckImmOperandSimple(
    raw_ostream &OS, int OpIndex, StringRef FunctionMapper) {
  if (shouldNegate())
    OS << "!";
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
}

void PredicateExpanderLLVM::expandCheckRegOperand(raw_ostream &OS, int OpIndex,
                                                  const Record *Reg,
                                                  StringRef FunctionMapper) {
  assert(Reg->isSubClassOf("Register") && "Expected a register Record!");

  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ");
  const StringRef Str = Reg->getValueAsString("Namespace");
  if (!Str.empty())
    OS << Str << "::";
  OS << Reg->getName();
}

void PredicateExpanderLLVM::expandCheckRegOperandSimple(
    raw_ostream &OS, int OpIndex, StringRef FunctionMapper) {
  if (shouldNegate())
    OS << "!";
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg()";
  if (!FunctionMapper.empty())
    OS << ")";
}

void PredicateExpanderLLVM::expandCheckInvalidRegOperand(raw_ostream &OS,
                                                         int OpIndex) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg() " << (shouldNegate() ? "!= " : "== ") << "0";
}

void PredicateExpanderLLVM::expandCheckSameRegOperand(raw_ostream &OS,
                                                      int First, int Second) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << First
     << ").getReg() " << (shouldNegate() ? "!=" : "==") << " MI"
     << (isByRef() ? "." : "->") << "getOperand(" << Second << ").getReg()";
}

void PredicateExpanderLLVM::expandCheckNumOperands(raw_ostream &OS,
                                                   int NumOps) {
  OS << "MI" << (isByRef() ? "." : "->") << "getNumOperands() "
     << (shouldNegate() ? "!= " : "== ") << NumOps;
}

void PredicateExpanderLLVM::expandCheckOpcode(raw_ostream &OS,
                                              const Record *Inst) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOpcode() "
     << (shouldNegate() ? "!= " : "== ") << Inst->getValueAsString("Namespace")
     << "::" << Inst->getName();
}

void PredicateExpanderLLVM::expandCheckOpcode(raw_ostream &OS,
                                              const RecVec &Opcodes) {
  assert(!Opcodes.empty() && "Expected at least one opcode to check!");
  bool First = true;

  if (Opcodes.size() == 1) {
    OS << "( ";
    expandCheckOpcode(OS, Opcodes[0]);
    OS << " )";
    return;
  }

  OS << '(';
  increaseIndentLevel();
  for (const Record *Rec : Opcodes) {
    OS << '\n';
    OS.indent(getIndentLevel() * 2);
    if (!First)
      OS << (shouldNegate() ? "&& " : "|| ");

    expandCheckOpcode(OS, Rec);
    First = false;
  }

  OS << '\n';
  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  OS << ')';
}

void PredicateExpanderLLVM::expandCheckPseudo(raw_ostream &OS,
                                              const RecVec &Opcodes) {
  if (shouldExpandForMC())
    expandFalse(OS);
  else
    expandCheckOpcode(OS, Opcodes);
}

void PredicateExpanderLLVM::expandPredicateSequence(raw_ostream &OS,
                                                    const RecVec &Sequence,
                                                    bool IsCheckAll) {
  assert(!Sequence.empty() && "Found an invalid empty predicate set!");
  if (Sequence.size() == 1)
    return expandPredicate(OS, Sequence[0]);

  // Okay, there is more than one predicate in the set.
  bool First = true;
  OS << (shouldNegate() ? "!(" : "(");
  increaseIndentLevel();

  bool OldValue = shouldNegate();
  setNegatePredicate(false);
  for (const Record *Rec : Sequence) {
    OS << '\n';
    OS.indent(getIndentLevel() * 2);
    if (!First)
      OS << (IsCheckAll ? "&& " : "|| ");
    expandPredicate(OS, Rec);
    First = false;
  }
  OS << '\n';
  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  OS << ')';
  setNegatePredicate(OldValue);
}

void PredicateExpanderLLVM::expandTIIFunctionCall(raw_ostream &OS,
                                                  StringRef MethodName) {
  OS << (shouldNegate() ? "!" : "");
  OS << TargetName << (shouldExpandForMC() ? "_MC::" : "InstrInfo::");
  OS << MethodName << (isByRef() ? "(MI)" : "(*MI)");
}

void PredicateExpanderLLVM::expandCheckIsRegOperand(raw_ostream &OS,
                                                    int OpIndex) {
  OS << (shouldNegate() ? "!" : "") << "MI" << (isByRef() ? "." : "->")
     << "getOperand(" << OpIndex << ").isReg() ";
}

void PredicateExpanderLLVM::expandCheckIsImmOperand(raw_ostream &OS,
                                                    int OpIndex) {
  OS << (shouldNegate() ? "!" : "") << "MI" << (isByRef() ? "." : "->")
     << "getOperand(" << OpIndex << ").isImm() ";
}

void PredicateExpanderLLVM::expandCheckFunctionPredicateWithTII(
    raw_ostream &OS, StringRef MCInstFn, StringRef MachineInstrFn,
    StringRef TIIPtr) {
  if (!shouldExpandForMC()) {
    OS << (TIIPtr.empty() ? "TII" : TIIPtr) << "->" << MachineInstrFn;
    OS << (isByRef() ? "(MI)" : "(*MI)");
    return;
  }

  OS << MCInstFn << (isByRef() ? "(MI" : "(*MI") << ", MCII)";
}

void PredicateExpanderLLVM::expandCheckFunctionPredicate(
    raw_ostream &OS, StringRef MCInstFn, StringRef MachineInstrFn) {
  OS << (shouldExpandForMC() ? MCInstFn : MachineInstrFn)
     << (isByRef() ? "(MI)" : "(*MI)");
}

void PredicateExpanderLLVM::expandCheckNonPortable(raw_ostream &OS,
                                                   StringRef Code) {
  if (shouldExpandForMC())
    return expandFalse(OS);

  OS << '(' << Code << ')';
}

void PredicateExpanderLLVM::expandReturnStatement(raw_ostream &OS,
                                                  const Record *Rec) {
  std::string Buffer;
  raw_string_ostream SS(Buffer);

  SS << "return ";
  expandPredicate(SS, Rec);
  SS << ";";
  OS << Buffer;
}

void PredicateExpanderLLVM::expandOpcodeSwitchCase(raw_ostream &OS,
                                                   const Record *Rec) {
  const RecVec &Opcodes = Rec->getValueAsListOfDefs("Opcodes");
  for (const Record *Opcode : Opcodes) {
    OS.indent(getIndentLevel() * 2);
    OS << "case " << Opcode->getValueAsString("Namespace")
       << "::" << Opcode->getName() << ":\n";
  }

  increaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  expandStatement(OS, Rec->getValueAsDef("CaseStmt"));
  decreaseIndentLevel();
}

void PredicateExpanderLLVM::expandOpcodeSwitchStatement(raw_ostream &OS,
                                                        const RecVec &Cases,
                                                        const Record *Default) {
  std::string Buffer;
  raw_string_ostream SS(Buffer);

  SS << "switch(MI" << (isByRef() ? "." : "->") << "getOpcode()) {\n";
  for (const Record *Rec : Cases) {
    expandOpcodeSwitchCase(SS, Rec);
    SS << '\n';
  }

  // Expand the default case.
  SS.indent(getIndentLevel() * 2);
  SS << "default:\n";

  increaseIndentLevel();
  SS.indent(getIndentLevel() * 2);
  expandStatement(SS, Default);
  decreaseIndentLevel();
  SS << '\n';

  SS.indent(getIndentLevel() * 2);
  SS << "} // end of switch-stmt";
  OS << Buffer;
}

void PredicateExpanderLLVM::expandStatement(raw_ostream &OS,
                                            const Record *Rec) {
  // Assume that padding has been added by the caller.
  if (Rec->isSubClassOf("MCOpcodeSwitchStatement")) {
    expandOpcodeSwitchStatement(OS, Rec->getValueAsListOfDefs("Cases"),
                                Rec->getValueAsDef("DefaultCase"));
    return;
  }

  if (Rec->isSubClassOf("MCReturnStatement")) {
    expandReturnStatement(OS, Rec->getValueAsDef("Pred"));
    return;
  }

  llvm_unreachable("No known rules to expand this MCStatement");
}

void PredicateExpanderLLVM::expandPredicate(raw_ostream &OS,
                                            const Record *Rec) {
  // Assume that padding has been added by the caller.
  if (Rec->isSubClassOf("MCTrue")) {
    if (shouldNegate())
      return expandFalse(OS);
    return expandTrue(OS);
  }

  if (Rec->isSubClassOf("MCFalse")) {
    if (shouldNegate())
      return expandTrue(OS);
    return expandFalse(OS);
  }

  if (Rec->isSubClassOf("CheckNot")) {
    flipNegatePredicate();
    expandPredicate(OS, Rec->getValueAsDef("Pred"));
    flipNegatePredicate();
    return;
  }

  if (Rec->isSubClassOf("CheckIsRegOperand"))
    return expandCheckIsRegOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckIsImmOperand"))
    return expandCheckIsImmOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckRegOperand"))
    return expandCheckRegOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsDef("Reg"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckRegOperandSimple"))
    return expandCheckRegOperandSimple(OS, Rec->getValueAsInt("OpIndex"),
                                       Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckInvalidRegOperand"))
    return expandCheckInvalidRegOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckImmOperand"))
    return expandCheckImmOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsInt("ImmVal"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckImmOperand_s"))
    return expandCheckImmOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsString("ImmVal"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckImmOperandSimple"))
    return expandCheckImmOperandSimple(OS, Rec->getValueAsInt("OpIndex"),
                                       Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckSameRegOperand"))
    return expandCheckSameRegOperand(OS, Rec->getValueAsInt("FirstIndex"),
                                     Rec->getValueAsInt("SecondIndex"));

  if (Rec->isSubClassOf("CheckNumOperands"))
    return expandCheckNumOperands(OS, Rec->getValueAsInt("NumOps"));

  if (Rec->isSubClassOf("CheckPseudo"))
    return expandCheckPseudo(OS, Rec->getValueAsListOfDefs("ValidOpcodes"));

  if (Rec->isSubClassOf("CheckOpcode"))
    return expandCheckOpcode(OS, Rec->getValueAsListOfDefs("ValidOpcodes"));

  if (Rec->isSubClassOf("CheckAll"))
    return expandPredicateSequence(OS, Rec->getValueAsListOfDefs("Predicates"),
                                   /* AllOf */ true);

  if (Rec->isSubClassOf("CheckAny"))
    return expandPredicateSequence(OS, Rec->getValueAsListOfDefs("Predicates"),
                                   /* AllOf */ false);

  if (Rec->isSubClassOf("CheckFunctionPredicate")) {
    return expandCheckFunctionPredicate(
        OS, Rec->getValueAsString("MCInstFnName"),
        Rec->getValueAsString("MachineInstrFnName"));
  }

  if (Rec->isSubClassOf("CheckFunctionPredicateWithTII")) {
    return expandCheckFunctionPredicateWithTII(
        OS, Rec->getValueAsString("MCInstFnName"),
        Rec->getValueAsString("MachineInstrFnName"),
        Rec->getValueAsString("TIIPtrName"));
  }

  if (Rec->isSubClassOf("CheckNonPortable"))
    return expandCheckNonPortable(OS, Rec->getValueAsString("CodeBlock"));

  if (Rec->isSubClassOf("TIIPredicate"))
    return expandTIIFunctionCall(OS, Rec->getValueAsString("FunctionName"));

  llvm_unreachable("No known rules to expand this MCInstPredicate");
}

void PredicateExpanderLLVM::expandHeader(raw_ostream &OS,
                                         const STIPredicateFunction &Fn) {
  const Record *Rec = Fn.getDeclaration();
  StringRef FunctionName = Rec->getValueAsString("Name");

  OS.indent(getIndentLevel() * 2);
  OS << "bool ";
  if (shouldExpandDefinition())
    OS << getClassPrefix() << "::";
  OS << FunctionName << "(";
  if (shouldExpandForMC())
    OS << "const MCInst " << (isByRef() ? "&" : "*") << "MI";
  else
    OS << "const MachineInstr " << (isByRef() ? "&" : "*") << "MI";
  if (Rec->getValueAsBit("UpdatesOpcodeMask"))
    OS << ", APInt &Mask";
  OS << (shouldExpandForMC() ? ", unsigned ProcessorID) const " : ") const ");
  if (shouldExpandDefinition()) {
    OS << "{\n";
    return;
  }

  if (Rec->getValueAsBit("OverridesBaseClassMember"))
    OS << "override";
  OS << ";\n";
}

void PredicateExpanderLLVM::expandPrologue(raw_ostream &OS,
                                           const STIPredicateFunction &Fn) {
  RecVec Delegates = Fn.getDeclaration()->getValueAsListOfDefs("Delegates");
  bool UpdatesOpcodeMask =
      Fn.getDeclaration()->getValueAsBit("UpdatesOpcodeMask");

  increaseIndentLevel();
  unsigned IndentLevel = getIndentLevel();
  for (const Record *Delegate : Delegates) {
    OS.indent(IndentLevel * 2);
    OS << "if (" << Delegate->getValueAsString("Name") << "(MI";
    if (UpdatesOpcodeMask)
      OS << ", Mask";
    if (shouldExpandForMC())
      OS << ", ProcessorID";
    OS << "))\n";
    OS.indent((1 + IndentLevel) * 2);
    OS << "return true;\n\n";
  }

  if (shouldExpandForMC())
    return;

  OS.indent(IndentLevel * 2);
  OS << "unsigned ProcessorID = getSchedModel().getProcessorID();\n";
}

void PredicateExpanderLLVM::expandOpcodeGroup(raw_ostream &OS,
                                              const OpcodeGroup &Group,
                                              bool ShouldUpdateOpcodeMask) {
  const OpcodeInfo &OI = Group.getOpcodeInfo();
  for (const PredicateInfo &PI : OI.getPredicates()) {
    const APInt &ProcModelMask = PI.ProcModelMask;
    bool FirstProcID = true;
    for (unsigned I = 0, E = ProcModelMask.getActiveBits(); I < E; ++I) {
      if (!ProcModelMask[I])
        continue;

      if (FirstProcID) {
        OS.indent(getIndentLevel() * 2);
        OS << "if (ProcessorID == " << I;
      } else {
        OS << " || ProcessorID == " << I;
      }
      FirstProcID = false;
    }

    OS << ") {\n";

    increaseIndentLevel();
    OS.indent(getIndentLevel() * 2);
    if (ShouldUpdateOpcodeMask) {
      if (PI.OperandMask.isZero())
        OS << "Mask.clearAllBits();\n";
      else
        OS << "Mask = " << PI.OperandMask << ";\n";
      OS.indent(getIndentLevel() * 2);
    }
    OS << "return ";
    expandPredicate(OS, PI.Predicate);
    OS << ";\n";
    decreaseIndentLevel();
    OS.indent(getIndentLevel() * 2);
    OS << "}\n";
  }
}

void PredicateExpanderLLVM::expandBody(raw_ostream &OS,
                                       const STIPredicateFunction &Fn) {
  bool UpdatesOpcodeMask =
      Fn.getDeclaration()->getValueAsBit("UpdatesOpcodeMask");

  unsigned IndentLevel = getIndentLevel();
  OS.indent(IndentLevel * 2);
  OS << "switch(MI" << (isByRef() ? "." : "->") << "getOpcode()) {\n";
  OS.indent(IndentLevel * 2);
  OS << "default:\n";
  OS.indent(IndentLevel * 2);
  OS << "  break;";

  for (const OpcodeGroup &Group : Fn.getGroups()) {
    for (const Record *Opcode : Group.getOpcodes()) {
      OS << '\n';
      OS.indent(IndentLevel * 2);
      OS << "case " << getTargetName() << "::" << Opcode->getName() << ":";
    }

    OS << '\n';
    increaseIndentLevel();
    expandOpcodeGroup(OS, Group, UpdatesOpcodeMask);

    OS.indent(getIndentLevel() * 2);
    OS << "break;\n";
    decreaseIndentLevel();
  }

  OS.indent(IndentLevel * 2);
  OS << "}\n";
}

void PredicateExpanderLLVM::expandEpilogue(raw_ostream &OS,
                                           const STIPredicateFunction &Fn) {
  OS << '\n';
  OS.indent(getIndentLevel() * 2);
  OS << "return ";
  expandPredicate(OS, Fn.getDefaultReturnPredicate());
  OS << ";\n";

  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  StringRef FunctionName = Fn.getDeclaration()->getValueAsString("Name");
  OS << "} // " << ClassPrefix << "::" << FunctionName << "\n\n";
}

void PredicateExpanderLLVM::expandSTIPredicate(raw_ostream &OS,
                                               const STIPredicateFunction &Fn) {
  const Record *Rec = Fn.getDeclaration();
  if (shouldExpandForMC() && !Rec->getValueAsBit("ExpandForMC"))
    return;

  expandHeader(OS, Fn);
  if (shouldExpandDefinition()) {
    expandPrologue(OS, Fn);
    expandBody(OS, Fn);
    expandEpilogue(OS, Fn);
  }
}

//---------------
// Capstone
//---------------

void PredicateExpanderCapstone::expandTrue(raw_ostream &OS) { OS << "true"; }
void PredicateExpanderCapstone::expandFalse(raw_ostream &OS) { OS << "false"; }

void PredicateExpanderCapstone::expandCheckImmOperand(raw_ostream &OS, int OpIndex,
                                                  int ImmVal,
                                                  StringRef FunctionMapper) {
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ") << ImmVal;
}

void PredicateExpanderCapstone::expandCheckImmOperand(raw_ostream &OS, int OpIndex,
                                                  StringRef ImmVal,
                                                  StringRef FunctionMapper) {
  if (ImmVal.empty())
    expandCheckImmOperandSimple(OS, OpIndex, FunctionMapper);

  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ") << ImmVal;
}

void PredicateExpanderCapstone::expandCheckImmOperandSimple(
    raw_ostream &OS, int OpIndex, StringRef FunctionMapper) {
  if (shouldNegate())
    OS << "!";
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getImm()";
  if (!FunctionMapper.empty())
    OS << ")";
}

void PredicateExpanderCapstone::expandCheckRegOperand(raw_ostream &OS, int OpIndex,
                                                  const Record *Reg,
                                                  StringRef FunctionMapper) {
  assert(Reg->isSubClassOf("Register") && "Expected a register Record!");

  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg()";
  if (!FunctionMapper.empty())
    OS << ")";
  OS << (shouldNegate() ? " != " : " == ");
  const StringRef Str = Reg->getValueAsString("Namespace");
  if (!Str.empty())
    OS << Str << "::";
  OS << Reg->getName();
}

void PredicateExpanderCapstone::expandCheckRegOperandSimple(
    raw_ostream &OS, int OpIndex, StringRef FunctionMapper) {
  if (shouldNegate())
    OS << "!";
  if (!FunctionMapper.empty())
    OS << FunctionMapper << "(";
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg()";
  if (!FunctionMapper.empty())
    OS << ")";
}

void PredicateExpanderCapstone::expandCheckInvalidRegOperand(raw_ostream &OS,
                                                         int OpIndex) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << OpIndex
     << ").getReg() " << (shouldNegate() ? "!= " : "== ") << "0";
}

void PredicateExpanderCapstone::expandCheckSameRegOperand(raw_ostream &OS,
                                                      int First, int Second) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOperand(" << First
     << ").getReg() " << (shouldNegate() ? "!=" : "==") << " MI"
     << (isByRef() ? "." : "->") << "getOperand(" << Second << ").getReg()";
}

void PredicateExpanderCapstone::expandCheckNumOperands(raw_ostream &OS,
                                                   int NumOps) {
  OS << "MI" << (isByRef() ? "." : "->") << "getNumOperands() "
     << (shouldNegate() ? "!= " : "== ") << NumOps;
}

void PredicateExpanderCapstone::expandCheckOpcode(raw_ostream &OS,
                                              const Record *Inst) {
  OS << "MI" << (isByRef() ? "." : "->") << "getOpcode() "
     << (shouldNegate() ? "!= " : "== ") << Inst->getValueAsString("Namespace")
     << "::" << Inst->getName();
}

void PredicateExpanderCapstone::expandCheckOpcode(raw_ostream &OS,
                                              const RecVec &Opcodes) {
  assert(!Opcodes.empty() && "Expected at least one opcode to check!");
  bool First = true;

  if (Opcodes.size() == 1) {
    OS << "( ";
    expandCheckOpcode(OS, Opcodes[0]);
    OS << " )";
    return;
  }

  OS << '(';
  increaseIndentLevel();
  for (const Record *Rec : Opcodes) {
    OS << '\n';
    OS.indent(getIndentLevel() * 2);
    if (!First)
      OS << (shouldNegate() ? "&& " : "|| ");

    expandCheckOpcode(OS, Rec);
    First = false;
  }

  OS << '\n';
  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  OS << ')';
}

void PredicateExpanderCapstone::expandCheckPseudo(raw_ostream &OS,
                                              const RecVec &Opcodes) {
  if (shouldExpandForMC())
    expandFalse(OS);
  else
    expandCheckOpcode(OS, Opcodes);
}

void PredicateExpanderCapstone::expandPredicateSequence(raw_ostream &OS,
                                                    const RecVec &Sequence,
                                                    bool IsCheckAll) {
  assert(!Sequence.empty() && "Found an invalid empty predicate set!");
  if (Sequence.size() == 1)
    return expandPredicate(OS, Sequence[0]);

  // Okay, there is more than one predicate in the set.
  bool First = true;
  OS << (shouldNegate() ? "!(" : "(");
  increaseIndentLevel();

  bool OldValue = shouldNegate();
  setNegatePredicate(false);
  for (const Record *Rec : Sequence) {
    OS << '\n';
    OS.indent(getIndentLevel() * 2);
    if (!First)
      OS << (IsCheckAll ? "&& " : "|| ");
    expandPredicate(OS, Rec);
    First = false;
  }
  OS << '\n';
  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  OS << ')';
  setNegatePredicate(OldValue);
}

void PredicateExpanderCapstone::expandTIIFunctionCall(raw_ostream &OS,
                                                  StringRef MethodName) {
  OS << (shouldNegate() ? "!" : "");
  OS << TargetName << (shouldExpandForMC() ? "_MC::" : "InstrInfo::");
  OS << MethodName << (isByRef() ? "(MI)" : "(*MI)");
}

void PredicateExpanderCapstone::expandCheckIsRegOperand(raw_ostream &OS,
                                                    int OpIndex) {
  OS << (shouldNegate() ? "!" : "") << "MI" << (isByRef() ? "." : "->")
     << "getOperand(" << OpIndex << ").isReg() ";
}

void PredicateExpanderCapstone::expandCheckIsImmOperand(raw_ostream &OS,
                                                    int OpIndex) {
  OS << (shouldNegate() ? "!" : "") << "MI" << (isByRef() ? "." : "->")
     << "getOperand(" << OpIndex << ").isImm() ";
}

void PredicateExpanderCapstone::expandCheckFunctionPredicateWithTII(
    raw_ostream &OS, StringRef MCInstFn, StringRef MachineInstrFn,
    StringRef TIIPtr) {
  if (!shouldExpandForMC()) {
    OS << (TIIPtr.empty() ? "TII" : TIIPtr) << "->" << MachineInstrFn;
    OS << (isByRef() ? "(MI)" : "(*MI)");
    return;
  }

  OS << MCInstFn << (isByRef() ? "(MI" : "(*MI") << ", MCII)";
}

void PredicateExpanderCapstone::expandCheckFunctionPredicate(
    raw_ostream &OS, StringRef MCInstFn, StringRef MachineInstrFn) {
  OS << (shouldExpandForMC() ? MCInstFn : MachineInstrFn)
     << (isByRef() ? "(MI)" : "(*MI)");
}

void PredicateExpanderCapstone::expandCheckNonPortable(raw_ostream &OS,
                                                   StringRef Code) {
  if (shouldExpandForMC())
    return expandFalse(OS);

  OS << '(' << Code << ')';
}

void PredicateExpanderCapstone::expandReturnStatement(raw_ostream &OS,
                                                  const Record *Rec) {
  std::string Buffer;
  raw_string_ostream SS(Buffer);

  SS << "return ";
  expandPredicate(SS, Rec);
  SS << ";";
  OS << Buffer;
}

void PredicateExpanderCapstone::expandOpcodeSwitchCase(raw_ostream &OS,
                                                   const Record *Rec) {
  const RecVec &Opcodes = Rec->getValueAsListOfDefs("Opcodes");
  for (const Record *Opcode : Opcodes) {
    OS.indent(getIndentLevel() * 2);
    OS << "case " << Opcode->getValueAsString("Namespace")
       << "_" << Opcode->getName() << ":\n";
  }

  increaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  expandStatement(OS, Rec->getValueAsDef("CaseStmt"));
  decreaseIndentLevel();
}

void PredicateExpanderCapstone::expandOpcodeSwitchStatement(raw_ostream &OS,
                                                        const RecVec &Cases,
                                                        const Record *Default) {
  std::string Buffer;
  raw_string_ostream SS(Buffer);

  SS << "switch(MI" << (isByRef() ? "." : "->") << "getOpcode()) {\n";
  for (const Record *Rec : Cases) {
    expandOpcodeSwitchCase(SS, Rec);
    SS << '\n';
  }

  // Expand the default case.
  SS.indent(getIndentLevel() * 2);
  SS << "default:\n";

  increaseIndentLevel();
  SS.indent(getIndentLevel() * 2);
  expandStatement(SS, Default);
  decreaseIndentLevel();
  SS << '\n';

  SS.indent(getIndentLevel() * 2);
  SS << "} // end of switch-stmt";
  OS << Buffer;
}

void PredicateExpanderCapstone::expandStatement(raw_ostream &OS,
                                            const Record *Rec) {
  // Assume that padding has been added by the caller.
  if (Rec->isSubClassOf("MCOpcodeSwitchStatement")) {
    expandOpcodeSwitchStatement(OS, Rec->getValueAsListOfDefs("Cases"),
                                Rec->getValueAsDef("DefaultCase"));
    return;
  }

  if (Rec->isSubClassOf("MCReturnStatement")) {
    expandReturnStatement(OS, Rec->getValueAsDef("Pred"));
    return;
  }

  llvm_unreachable("No known rules to expand this MCStatement");
}

void PredicateExpanderCapstone::expandPredicate(raw_ostream &OS,
                                            const Record *Rec) {
  // Assume that padding has been added by the caller.
  if (Rec->isSubClassOf("MCTrue")) {
    if (shouldNegate())
      return expandFalse(OS);
    return expandTrue(OS);
  }

  if (Rec->isSubClassOf("MCFalse")) {
    if (shouldNegate())
      return expandTrue(OS);
    return expandFalse(OS);
  }

  if (Rec->isSubClassOf("CheckNot")) {
    flipNegatePredicate();
    expandPredicate(OS, Rec->getValueAsDef("Pred"));
    flipNegatePredicate();
    return;
  }

  if (Rec->isSubClassOf("CheckIsRegOperand"))
    return expandCheckIsRegOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckIsImmOperand"))
    return expandCheckIsImmOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckRegOperand"))
    return expandCheckRegOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsDef("Reg"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckRegOperandSimple"))
    return expandCheckRegOperandSimple(OS, Rec->getValueAsInt("OpIndex"),
                                       Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckInvalidRegOperand"))
    return expandCheckInvalidRegOperand(OS, Rec->getValueAsInt("OpIndex"));

  if (Rec->isSubClassOf("CheckImmOperand"))
    return expandCheckImmOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsInt("ImmVal"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckImmOperand_s"))
    return expandCheckImmOperand(OS, Rec->getValueAsInt("OpIndex"),
                                 Rec->getValueAsString("ImmVal"),
                                 Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckImmOperandSimple"))
    return expandCheckImmOperandSimple(OS, Rec->getValueAsInt("OpIndex"),
                                       Rec->getValueAsString("FunctionMapper"));

  if (Rec->isSubClassOf("CheckSameRegOperand"))
    return expandCheckSameRegOperand(OS, Rec->getValueAsInt("FirstIndex"),
                                     Rec->getValueAsInt("SecondIndex"));

  if (Rec->isSubClassOf("CheckNumOperands"))
    return expandCheckNumOperands(OS, Rec->getValueAsInt("NumOps"));

  if (Rec->isSubClassOf("CheckPseudo"))
    return expandCheckPseudo(OS, Rec->getValueAsListOfDefs("ValidOpcodes"));

  if (Rec->isSubClassOf("CheckOpcode"))
    return expandCheckOpcode(OS, Rec->getValueAsListOfDefs("ValidOpcodes"));

  if (Rec->isSubClassOf("CheckAll"))
    return expandPredicateSequence(OS, Rec->getValueAsListOfDefs("Predicates"),
                                   /* AllOf */ true);

  if (Rec->isSubClassOf("CheckAny"))
    return expandPredicateSequence(OS, Rec->getValueAsListOfDefs("Predicates"),
                                   /* AllOf */ false);

  if (Rec->isSubClassOf("CheckFunctionPredicate")) {
    return expandCheckFunctionPredicate(
        OS, Rec->getValueAsString("MCInstFnName"),
        Rec->getValueAsString("MachineInstrFnName"));
  }

  if (Rec->isSubClassOf("CheckFunctionPredicateWithTII")) {
    return expandCheckFunctionPredicateWithTII(
        OS, Rec->getValueAsString("MCInstFnName"),
        Rec->getValueAsString("MachineInstrFnName"),
        Rec->getValueAsString("TIIPtrName"));
  }

  if (Rec->isSubClassOf("CheckNonPortable"))
    return expandCheckNonPortable(OS, Rec->getValueAsString("CodeBlock"));

  if (Rec->isSubClassOf("TIIPredicate"))
    return expandTIIFunctionCall(OS, Rec->getValueAsString("FunctionName"));

  llvm_unreachable("No known rules to expand this MCInstPredicate");
}

void PredicateExpanderCapstone::expandHeader(raw_ostream &OS,
                                         const STIPredicateFunction &Fn) {
  const Record *Rec = Fn.getDeclaration();
  StringRef FunctionName = Rec->getValueAsString("Name");

  OS.indent(getIndentLevel() * 2);
  OS << "bool ";
  if (shouldExpandDefinition())
    OS << getClassPrefix() << "::";
  OS << FunctionName << "(";
  if (shouldExpandForMC())
    OS << "const MCInst " << (isByRef() ? "&" : "*") << "MI";
  else
    OS << "const MachineInstr " << (isByRef() ? "&" : "*") << "MI";
  if (Rec->getValueAsBit("UpdatesOpcodeMask"))
    OS << ", APInt &Mask";
  OS << (shouldExpandForMC() ? ", unsigned ProcessorID) const " : ") const ");
  if (shouldExpandDefinition()) {
    OS << "{\n";
    return;
  }

  if (Rec->getValueAsBit("OverridesBaseClassMember"))
    OS << "override";
  OS << ";\n";
}

void PredicateExpanderCapstone::expandPrologue(raw_ostream &OS,
                                           const STIPredicateFunction &Fn) {
  RecVec Delegates = Fn.getDeclaration()->getValueAsListOfDefs("Delegates");
  bool UpdatesOpcodeMask =
      Fn.getDeclaration()->getValueAsBit("UpdatesOpcodeMask");

  increaseIndentLevel();
  unsigned IndentLevel = getIndentLevel();
  for (const Record *Delegate : Delegates) {
    OS.indent(IndentLevel * 2);
    OS << "if (" << Delegate->getValueAsString("Name") << "(MI";
    if (UpdatesOpcodeMask)
      OS << ", Mask";
    if (shouldExpandForMC())
      OS << ", ProcessorID";
    OS << "))\n";
    OS.indent((1 + IndentLevel) * 2);
    OS << "return true;\n\n";
  }

  if (shouldExpandForMC())
    return;

  OS.indent(IndentLevel * 2);
  OS << "unsigned ProcessorID = getSchedModel().getProcessorID();\n";
}

void PredicateExpanderCapstone::expandOpcodeGroup(raw_ostream &OS,
                                              const OpcodeGroup &Group,
                                              bool ShouldUpdateOpcodeMask) {
  const OpcodeInfo &OI = Group.getOpcodeInfo();
  for (const PredicateInfo &PI : OI.getPredicates()) {
    const APInt &ProcModelMask = PI.ProcModelMask;
    bool FirstProcID = true;
    for (unsigned I = 0, E = ProcModelMask.getActiveBits(); I < E; ++I) {
      if (!ProcModelMask[I])
        continue;

      if (FirstProcID) {
        OS.indent(getIndentLevel() * 2);
        OS << "if (ProcessorID == " << I;
      } else {
        OS << " || ProcessorID == " << I;
      }
      FirstProcID = false;
    }

    OS << ") {\n";

    increaseIndentLevel();
    OS.indent(getIndentLevel() * 2);
    if (ShouldUpdateOpcodeMask) {
      if (PI.OperandMask.isZero())
        OS << "Mask.clearAllBits();\n";
      else
        OS << "Mask = " << PI.OperandMask << ";\n";
      OS.indent(getIndentLevel() * 2);
    }
    OS << "return ";
    expandPredicate(OS, PI.Predicate);
    OS << ";\n";
    decreaseIndentLevel();
    OS.indent(getIndentLevel() * 2);
    OS << "}\n";
  }
}

void PredicateExpanderCapstone::expandBody(raw_ostream &OS,
                                       const STIPredicateFunction &Fn) {
  bool UpdatesOpcodeMask =
      Fn.getDeclaration()->getValueAsBit("UpdatesOpcodeMask");

  unsigned IndentLevel = getIndentLevel();
  OS.indent(IndentLevel * 2);
  OS << "switch(MI" << (isByRef() ? "." : "->") << "getOpcode()) {\n";
  OS.indent(IndentLevel * 2);
  OS << "default:\n";
  OS.indent(IndentLevel * 2);
  OS << "  break;";

  for (const OpcodeGroup &Group : Fn.getGroups()) {
    for (const Record *Opcode : Group.getOpcodes()) {
      OS << '\n';
      OS.indent(IndentLevel * 2);
      OS << "case " << getTargetName() << "::" << Opcode->getName() << ":";
    }

    OS << '\n';
    increaseIndentLevel();
    expandOpcodeGroup(OS, Group, UpdatesOpcodeMask);

    OS.indent(getIndentLevel() * 2);
    OS << "break;\n";
    decreaseIndentLevel();
  }

  OS.indent(IndentLevel * 2);
  OS << "}\n";
}

void PredicateExpanderCapstone::expandEpilogue(raw_ostream &OS,
                                           const STIPredicateFunction &Fn) {
  OS << '\n';
  OS.indent(getIndentLevel() * 2);
  OS << "return ";
  expandPredicate(OS, Fn.getDefaultReturnPredicate());
  OS << ";\n";

  decreaseIndentLevel();
  OS.indent(getIndentLevel() * 2);
  StringRef FunctionName = Fn.getDeclaration()->getValueAsString("Name");
  OS << "} // " << ClassPrefix << "::" << FunctionName << "\n\n";
}

void PredicateExpanderCapstone::expandSTIPredicate(raw_ostream &OS,
                                               const STIPredicateFunction &Fn) {
  const Record *Rec = Fn.getDeclaration();
  if (shouldExpandForMC() && !Rec->getValueAsBit("ExpandForMC"))
    return;

  expandHeader(OS, Fn);
  if (shouldExpandDefinition()) {
    expandPrologue(OS, Fn);
    expandBody(OS, Fn);
    expandEpilogue(OS, Fn);
  }
}

} // namespace llvm
