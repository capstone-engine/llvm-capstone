//===- AsmWriterEmitter.cpp - Generate an assembly writer -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits an assembly printer for the current target.
// Note that this is currently fairly skeletal, but will grow over time.
//
//===----------------------------------------------------------------------===//

#include "AsmWriterInst.h"
#include "CodeGenInstruction.h"
#include "CodeGenRegisters.h"
#include "CodeGenTarget.h"
#include "Printer.h"
#include "SequenceToOffsetTable.h"
#include "Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "asm-writer-emitter"

namespace {

class AsmWriterEmitter {
  RecordKeeper &Records;
  CodeGenTarget Target;
  PrinterLLVM &PI;
  ArrayRef<const CodeGenInstruction *> NumberedInstructions;
  std::vector<AsmWriterInst> Instructions;

public:
  AsmWriterEmitter(RecordKeeper &R, PrinterLLVM &PI);

  void run();
private:
  void EmitGetMnemonic(
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits);
  void EmitPrintInstruction(
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits);
  void EmitInstructions(std::vector<AsmWriterInst> &Insts,
                        bool PassSubtarget);
  void EmitGetRegisterName();
  void EmitRegisterNameString(StringRef AltName,
                              const std::deque<CodeGenRegister> &Registers);
  void EmitPrintAliasInstruction();

  void FindUniqueOperandCommands(std::vector<std::string> &UOC,
                                 std::vector<std::vector<unsigned>> &InstIdxs,
                                 std::vector<unsigned> &InstOpsUsed,
                                 bool PassSubtarget) const;
};

} // end anonymous namespace

/// EmitInstructions - Emit the last instruction in the vector and any other
/// instructions that are suitably similar to it.
void AsmWriterEmitter::EmitInstructions(std::vector<AsmWriterInst> &Insts,
                             bool PassSubtarget) {
  AsmWriterInst FirstInst = Insts.back();
  Insts.pop_back();

  std::vector<AsmWriterInst> SimilarInsts;
  unsigned DifferingOperand = ~0;
  for (unsigned i = Insts.size(); i != 0; --i) {
    unsigned DiffOp = Insts[i-1].MatchesAllButOneOp(FirstInst);
    if (DiffOp != ~1U) {
      if (DifferingOperand == ~0U)  // First match!
        DifferingOperand = DiffOp;

      // If this differs in the same operand as the rest of the instructions in
      // this class, move it to the SimilarInsts list.
      if (DifferingOperand == DiffOp || DiffOp == ~0U) {
        SimilarInsts.push_back(Insts[i-1]);
        Insts.erase(Insts.begin()+i-1);
      }
    }
  }
  PI.asmWriterEmitInstruction(FirstInst,
                              SimilarInsts,
                              DifferingOperand,
                              PassSubtarget);

}

void AsmWriterEmitter::
FindUniqueOperandCommands(std::vector<std::string> &UniqueOperandCommands,
                          std::vector<std::vector<unsigned>> &InstIdxs,
                          std::vector<unsigned> &InstOpsUsed,
                          bool PassSubtarget) const {
  // This vector parallels UniqueOperandCommands, keeping track of which
  // instructions each case are used for.  It is a comma separated string of
  // enums.
  std::vector<std::string> InstrsForCase;
  InstrsForCase.resize(UniqueOperandCommands.size());
  InstOpsUsed.assign(UniqueOperandCommands.size(), 0);

  for (size_t i = 0, e = Instructions.size(); i != e; ++i) {
    const AsmWriterInst &Inst = Instructions[i];
    if (Inst.Operands.empty())
      continue;   // Instruction already done.

    std::string Command = "    "+Inst.Operands[0].getCode(PassSubtarget)+"\n";

    // Check to see if we already have 'Command' in UniqueOperandCommands.
    // If not, add it.
    auto I = llvm::find(UniqueOperandCommands, Command);
    if (I != UniqueOperandCommands.end()) {
      size_t idx = I - UniqueOperandCommands.begin();
      InstrsForCase[idx] += ", ";
      InstrsForCase[idx] += Inst.CGI->TheDef->getName();
      InstIdxs[idx].push_back(i);
    } else {
      UniqueOperandCommands.push_back(std::move(Command));
      InstrsForCase.push_back(std::string(Inst.CGI->TheDef->getName()));
      InstIdxs.emplace_back();
      InstIdxs.back().push_back(i);

      // This command matches one operand so far.
      InstOpsUsed.push_back(1);
    }
  }

  // For each entry of UniqueOperandCommands, there is a set of instructions
  // that uses it.  If the next command of all instructions in the set are
  // identical, fold it into the command.
  for (size_t CommandIdx = 0, e = UniqueOperandCommands.size();
       CommandIdx != e; ++CommandIdx) {

    const auto &Idxs = InstIdxs[CommandIdx];

    for (unsigned Op = 1; ; ++Op) {
      // Find the first instruction in the set.
      const AsmWriterInst &FirstInst = Instructions[Idxs.front()];
      // If this instruction has no more operands, we isn't anything to merge
      // into this command.
      if (FirstInst.Operands.size() == Op)
        break;

      // Otherwise, scan to see if all of the other instructions in this command
      // set share the operand.
      if (any_of(drop_begin(Idxs), [&](unsigned Idx) {
            const AsmWriterInst &OtherInst = Instructions[Idx];
            return OtherInst.Operands.size() == Op ||
                   OtherInst.Operands[Op] != FirstInst.Operands[Op];
          }))
        break;

      // Okay, everything in this command set has the same next operand.  Add it
      // to UniqueOperandCommands and remember that it was consumed.
      std::string Command = "    " +
        FirstInst.Operands[Op].getCode(PassSubtarget) + "\n";

      UniqueOperandCommands[CommandIdx] += Command;
      InstOpsUsed[CommandIdx]++;
    }
  }

  // Prepend some of the instructions each case is used for onto the case val.
  for (unsigned i = 0, e = InstrsForCase.size(); i != e; ++i) {
    std::string Instrs = InstrsForCase[i];
    if (Instrs.size() > 70) {
      Instrs.erase(Instrs.begin()+70, Instrs.end());
      Instrs += "...";
    }

    if (!Instrs.empty())
      UniqueOperandCommands[i] = "    // " + Instrs + "\n" +
        UniqueOperandCommands[i];
  }
}

static void UnescapeString(std::string &Str) {
  for (unsigned i = 0; i != Str.size(); ++i) {
    if (Str[i] == '\\' && i != Str.size()-1) {
      switch (Str[i+1]) {
      default: continue;  // Don't execute the code after the switch.
      case 'a': Str[i] = '\a'; break;
      case 'b': Str[i] = '\b'; break;
      case 'e': Str[i] = 27; break;
      case 'f': Str[i] = '\f'; break;
      case 'n': Str[i] = '\n'; break;
      case 'r': Str[i] = '\r'; break;
      case 't': Str[i] = '\t'; break;
      case 'v': Str[i] = '\v'; break;
      case '"': Str[i] = '\"'; break;
      case '\'': Str[i] = '\''; break;
      case '\\': Str[i] = '\\'; break;
      }
      // Nuke the second character.
      Str.erase(Str.begin()+i+1);
    }
  }
}

/// UnescapeAliasString - Supports literal braces in InstAlias asm string which
/// are escaped with '\\' to avoid being interpreted as variants. Braces must
/// be unescaped before c++ code is generated as (e.g.):
///
///   AsmString = "foo \{$\x01\}";
///
/// causes non-standard escape character warnings.
static void UnescapeAliasString(std::string &Str) {
  for (unsigned i = 0; i != Str.size(); ++i) {
    if (Str[i] == '\\' && i != Str.size()-1) {
      switch (Str[i+1]) {
      default: continue;  // Don't execute the code after the switch.
      case '{': Str[i] = '{'; break;
      case '}': Str[i] = '}'; break;
      }
      // Nuke the second character.
      Str.erase(Str.begin()+i+1);
    }
  }
}

void AsmWriterEmitter::EmitGetMnemonic(
    std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
    unsigned &BitsLeft, unsigned &AsmStrBits) {
  Record *AsmWriter = Target.getAsmWriter();
  StringRef ClassName = AsmWriter->getValueAsString("AsmWriterClassName");
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  PI.asmWriterEmitGetMnemonic(Target.getName().str(), ClassName);

  // Build an aggregate string, and build a table of offsets into it.
  SequenceToOffsetTable<std::string> StringTable(PrinterLLVM::getLanguage(), true);

  /// OpcodeInfo - This encodes the index of the string to use for the first
  /// chunk of the output as well as indices used for operand printing.
  std::vector<uint64_t> OpcodeInfo(NumberedInstructions.size());
  const unsigned OpcodeInfoBits = 64;

  // Add all strings to the string table upfront so it can generate an optimized
  // representation.
  for (AsmWriterInst &AWI : Instructions) {
    if (AWI.Operands[0].OperandType ==
                 AsmWriterOperand::isLiteralTextOperand &&
        !AWI.Operands[0].Str.empty()) {
      std::string Str = AWI.Operands[0].Str;
      UnescapeString(Str);
      StringTable.add(Str);
    }
  }

  StringTable.layout();

  unsigned MaxStringIdx = 0;
  for (AsmWriterInst &AWI : Instructions) {
    unsigned Idx;
    if (AWI.Operands[0].OperandType != AsmWriterOperand::isLiteralTextOperand ||
        AWI.Operands[0].Str.empty()) {
      // Something handled by the asmwriter printer, but with no leading string.
      Idx = StringTable.get("");
    } else {
      std::string Str = AWI.Operands[0].Str;
      UnescapeString(Str);
      Idx = StringTable.get(Str);
      MaxStringIdx = std::max(MaxStringIdx, Idx);

      // Nuke the string from the operand list.  It is now handled!
      AWI.Operands.erase(AWI.Operands.begin());
    }

    // Bias offset by one since we want 0 as a sentinel.
    OpcodeInfo[AWI.CGIIndex] = Idx+1;
  }

  // Figure out how many bits we used for the string index.
  AsmStrBits = Log2_32_Ceil(MaxStringIdx + 2);

  // To reduce code size, we compactify common instructions into a few bits
  // in the opcode-indexed table.
  BitsLeft = OpcodeInfoBits - AsmStrBits;

  while (true) {
    std::vector<std::string> UniqueOperandCommands;
    std::vector<std::vector<unsigned>> InstIdxs;
    std::vector<unsigned> NumInstOpsHandled;
    FindUniqueOperandCommands(UniqueOperandCommands, InstIdxs,
                              NumInstOpsHandled, PassSubtarget);

    // If we ran out of operands to print, we're done.
    if (UniqueOperandCommands.empty()) break;

    // Compute the number of bits we need to represent these cases, this is
    // ceil(log2(numentries)).
    unsigned NumBits = Log2_32_Ceil(UniqueOperandCommands.size());

    // If we don't have enough bits for this operand, don't include it.
    if (NumBits > BitsLeft) {
      LLVM_DEBUG(errs() << "Not enough bits to densely encode " << NumBits
                        << " more bits\n");
      break;
    }

    // Otherwise, we can include this in the initial lookup table.  Add it in.
    for (size_t i = 0, e = InstIdxs.size(); i != e; ++i) {
      unsigned NumOps = NumInstOpsHandled[i];
      for (unsigned Idx : InstIdxs[i]) {
        OpcodeInfo[Instructions[Idx].CGIIndex] |=
          (uint64_t)i << (OpcodeInfoBits-BitsLeft);
        // Remove the info about this operand from the instruction.
        AsmWriterInst &Inst = Instructions[Idx];
        if (!Inst.Operands.empty()) {
          assert(NumOps <= Inst.Operands.size() &&
                 "Can't remove this many ops!");
          Inst.Operands.erase(Inst.Operands.begin(),
                              Inst.Operands.begin()+NumOps);
        }
      }
    }
    BitsLeft -= NumBits;

    // Remember the handlers for this set of operands.
    TableDrivenOperandPrinters.push_back(std::move(UniqueOperandCommands));
  }

  // Emit the string table itself.
  PI.asmWriterEmitAsmStrs(StringTable);
  PI.asmWriterEmitMnemonicDecodeTable(OpcodeInfoBits,
                                      BitsLeft,
                                      AsmStrBits,
                                      NumberedInstructions,
                                      OpcodeInfo);
}

/// EmitPrintInstruction - Generate the code for the "printInstruction" method
/// implementation. Destroys all instances of AsmWriterInst information, by
/// clearing the Instructions vector.
void AsmWriterEmitter::EmitPrintInstruction(
    std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
    unsigned &BitsLeft, unsigned &AsmStrBits) {
  Record *AsmWriter = Target.getAsmWriter();
  StringRef ClassName = AsmWriter->getValueAsString("AsmWriterClassName");
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  PI.asmWriterEmitPrintInstruction(Target.getName().str(),
                                   TableDrivenOperandPrinters,
                                   BitsLeft, AsmStrBits,
                                   ClassName, PassSubtarget);

  // Okay, delete instructions with no operand info left.
  llvm::erase_if(Instructions,
                 [](AsmWriterInst &Inst) { return Inst.Operands.empty(); });

  // Because this is a vector, we want to emit from the end.  Reverse all of the
  // elements in the vector.
  std::reverse(Instructions.begin(), Instructions.end());


  // Now that we've emitted all of the operand info that fit into 64 bits, emit
  // information for those instructions that are left.  This is a less dense
  // encoding, but we expect the main 64-bit table to handle the majority of
  // instructions.
  if (!Instructions.empty()) {
    // Find the opcode # of inline asm.
    PI.asmWriterEmitInstrSwitch();
    while (!Instructions.empty())
      EmitInstructions(Instructions, PassSubtarget);

    PI.asmWriterEmitCompoundClosure(2, true, false);
  }

  PI.asmWriterEmitCompoundClosure(0, true, false);
}

void AsmWriterEmitter::EmitRegisterNameString(
    StringRef AltName,
    const std::deque<CodeGenRegister> &Registers) {
  SequenceToOffsetTable<std::string> StringTable(PrinterLLVM::getLanguage());
  SmallVector<std::string, 4> AsmNames(Registers.size());
  unsigned i = 0;
  for (const auto &Reg : Registers) {
    std::string &AsmName = AsmNames[i++];

    // "NoRegAltName" is special. We don't need to do a lookup for that,
    // as it's just a reference to the default register name.
    if (AltName == "" || AltName == "NoRegAltName") {
      AsmName = std::string(Reg.TheDef->getValueAsString("AsmName"));
      if (AsmName.empty())
        AsmName = std::string(Reg.getName());
    } else {
      // Make sure the register has an alternate name for this index.
      std::vector<Record*> AltNameList =
        Reg.TheDef->getValueAsListOfDefs("RegAltNameIndices");
      unsigned Idx = 0, e;
      for (e = AltNameList.size();
           Idx < e && (AltNameList[Idx]->getName() != AltName);
           ++Idx)
        ;
      // If the register has an alternate name for this index, use it.
      // Otherwise, leave it empty as an error flag.
      if (Idx < e) {
        std::vector<StringRef> AltNames =
          Reg.TheDef->getValueAsListOfStrings("AltNames");
        if (AltNames.size() <= Idx)
          PrintFatalError(Reg.TheDef->getLoc(),
                          "Register definition missing alt name for '" +
                          AltName + "'.");
        AsmName = std::string(AltNames[Idx]);
      }
    }
    StringTable.add(AsmName);
  }

  StringTable.layout();
  PI.asmWriterEmitStringLiteralDef(StringTable, AltName);
  PI.asmWriterEmitRegAsmOffsets(Registers.size(), AsmNames, StringTable, AltName);
}

void AsmWriterEmitter::EmitGetRegisterName() {
  Record *AsmWriter = Target.getAsmWriter();
  StringRef ClassName = AsmWriter->getValueAsString("AsmWriterClassName");
  const auto &Registers = Target.getRegBank().getRegisters();
  const std::vector<Record*> &AltNameIndices = Target.getRegAltNameIndices();
  bool hasAltNames = AltNameIndices.size() > 1;
  StringRef Namespace = Registers.front().TheDef->getValueAsString("Namespace");

  PI.asmWriterEmitGetRegNameAssert(Target.getName().str(),
                                   ClassName,
                                   hasAltNames,
                                   Registers.size());

  if (hasAltNames) {
    for (const Record *R : AltNameIndices)
      EmitRegisterNameString(R->getName(), Registers);
  } else
    EmitRegisterNameString("", Registers);

  PI.asmWriterEmitAltIdxSwitch(hasAltNames, AltNameIndices, Namespace);
}

namespace {

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

  void addOperand(StringRef Op, int OpIdx, int PrintMethodIdx = -1) {
    assert(OpIdx >= 0 && OpIdx < 0xFE && "Idx out of range");
    assert(PrintMethodIdx >= -1 && PrintMethodIdx < 0xFF &&
           "Idx out of range");
    OpMap[Op] = std::make_pair(OpIdx, PrintMethodIdx);
  }

  unsigned getNumMIOps() { return NumMIOps; }

  StringRef getResult() { return Result; }

  bool isOpMapped(StringRef Op) { return OpMap.find(Op) != OpMap.end(); }
  int getOpIndex(StringRef Op) { return OpMap[Op].first; }
  std::pair<int, int> &getOpData(StringRef Op) { return OpMap[Op]; }

  std::pair<StringRef, StringRef::iterator> parseName(StringRef::iterator Start,
                                                      StringRef::iterator End) {
    StringRef::iterator I = Start;
    StringRef::iterator Next;
    if (*I == '{') {
      // ${some_name}
      Start = ++I;
      while (I != End && *I != '}')
        ++I;
      Next = I;
      // eat the final '}'
      if (Next != End)
        ++Next;
    } else {
      // $name, just eat the usual suspects.
      while (I != End && (isAlnum(*I) || *I == '_'))
        ++I;
      Next = I;
    }

    return std::make_pair(StringRef(Start, I - Start), Next);
  }

  std::string formatAliasString(uint32_t &UnescapedSize) {
    // Directly mangle mapped operands into the string. Each operand is
    // identified by a '$' sign followed by a byte identifying the number of the
    // operand. We add one to the index to avoid zero bytes.
    StringRef ASM(AsmString);
    std::string OutString;
    raw_string_ostream OS(OutString);
    for (StringRef::iterator I = ASM.begin(), E = ASM.end(); I != E;) {
      OS << *I;
      ++UnescapedSize;
      if (*I == '$') {
        StringRef Name;
        std::tie(Name, I) = parseName(++I, E);
        assert(isOpMapped(Name) && "Unmapped operand!");

        int OpIndex, PrintIndex;
        std::tie(OpIndex, PrintIndex) = getOpData(Name);
        if (PrintIndex == -1) {
          // Can use the default printOperand route.
          OS << format("\\x%02X", (unsigned char)OpIndex + 1);
          ++UnescapedSize;
        } else {
          // 3 bytes if a PrintMethod is needed: 0xFF, the MCInst operand
          // number, and which of our pre-detected Methods to call.
          OS << format("\\xFF\\x%02X\\x%02X", OpIndex + 1, PrintIndex + 1);
          UnescapedSize += 3;
        }
      } else {
        ++I;
      }
    }
    return OutString;
  }

  bool operator==(const IAPrinter &RHS) const {
    if (NumMIOps != RHS.NumMIOps)
      return false;
    if (Conds.size() != RHS.Conds.size())
      return false;

    unsigned Idx = 0;
    for (const auto &str : Conds)
      if (str != RHS.Conds[Idx++])
        return false;

    return true;
  }
};

} // end anonymous namespace

static unsigned CountNumOperands(StringRef AsmString, unsigned Variant) {
  return AsmString.count(' ') + AsmString.count('\t');
}

namespace {

struct AliasPriorityComparator {
  typedef std::pair<CodeGenInstAlias, int> ValueType;
  bool operator()(const ValueType &LHS, const ValueType &RHS) const {
    if (LHS.second ==  RHS.second) {
      // We don't actually care about the order, but for consistency it
      // shouldn't depend on pointer comparisons.
      return LessRecordByID()(LHS.first.TheDef, RHS.first.TheDef);
    }

    // Aliases with larger priorities should be considered first.
    return LHS.second > RHS.second;
  }
};

} // end anonymous namespace

void AsmWriterEmitter::EmitPrintAliasInstruction() {
  Record *AsmWriter = Target.getAsmWriter();

  PI.emitIncludeToggle("PRINT_ALIAS_INSTR", true);

  //////////////////////////////
  // Gather information about aliases we need to print
  //////////////////////////////

  // Emit the method that prints the alias instruction.
  StringRef ClassName = AsmWriter->getValueAsString("AsmWriterClassName");
  unsigned Variant = AsmWriter->getValueAsInt("Variant");
  bool PassSubtarget = AsmWriter->getValueAsInt("PassSubtarget");

  std::vector<Record*> AllInstAliases =
    Records.getAllDerivedDefinitions("InstAlias");

  // Create a map from the qualified name to a list of potential matches.
  typedef std::set<std::pair<CodeGenInstAlias, int>, AliasPriorityComparator>
      AliasWithPriority;
  std::map<std::string, AliasWithPriority> AliasMap;
  for (Record *R : AllInstAliases) {
    int Priority = R->getValueAsInt("EmitPriority");
    if (Priority < 1)
      continue; // Aliases with priority 0 are never emitted.

    const DagInit *DI = R->getValueAsDag("ResultInst");
    AliasMap[getQualifiedName(DI->getOperatorAsDef(R->getLoc()))].insert(
        std::make_pair(CodeGenInstAlias(R, Target), Priority));
  }

  // A map of which conditions need to be met for each instruction operand
  // before it can be matched to the mnemonic.
  std::map<std::string, std::vector<IAPrinter>> IAPrinterMap;

  std::vector<std::pair<std::string, bool>> PrintMethods;

  // A list of MCOperandPredicates for all operands in use, and the reverse map
  std::vector<const Record*> MCOpPredicates;
  DenseMap<const Record*, unsigned> MCOpPredicateMap;

  for (auto &Aliases : AliasMap) {
    // Collection of instruction alias rules. May contain ambiguous rules.
    std::vector<IAPrinter> IAPs;

    for (auto &Alias : Aliases.second) {
      const CodeGenInstAlias &CGA = Alias.first;
      unsigned LastOpNo = CGA.ResultInstOperandIndex.size();
      std::string FlatInstAsmString =
         CodeGenInstruction::FlattenAsmStringVariants(CGA.ResultInst->AsmString,
                                                      Variant);
      unsigned NumResultOps = CountNumOperands(FlatInstAsmString, Variant);

      std::string FlatAliasAsmString =
          CodeGenInstruction::FlattenAsmStringVariants(CGA.AsmString, Variant);
      UnescapeAliasString(FlatAliasAsmString);

      // Don't emit the alias if it has more operands than what it's aliasing.
      if (NumResultOps < CountNumOperands(FlatAliasAsmString, Variant))
        continue;

      StringRef Namespace = Target.getName();
      unsigned NumMIOps = 0;
      for (auto &ResultInstOpnd : CGA.ResultInst->Operands)
        NumMIOps += ResultInstOpnd.MINumOperands;

      IAPrinter IAP(CGA.Result->getAsString(), FlatAliasAsmString, NumMIOps);

      unsigned MIOpNum = 0;
      for (unsigned i = 0, e = LastOpNo; i != e; ++i) {
        // Skip over tied operands as they're not part of an alias declaration.
        auto &Operands = CGA.ResultInst->Operands;
        while (true) {
          unsigned OpNum = Operands.getSubOperandNumber(MIOpNum).first;
          if (Operands[OpNum].MINumOperands == 1 &&
              Operands[OpNum].getTiedRegister() != -1) {
            // Tied operands of different RegisterClass should be explicit within
            // an instruction's syntax and so cannot be skipped.
            int TiedOpNum = Operands[OpNum].getTiedRegister();
            if (Operands[OpNum].Rec->getName() ==
                Operands[TiedOpNum].Rec->getName()) {
              ++MIOpNum;
              continue;
            }
          }
          break;
        }

        // Ignore unchecked result operands.
        while (IAP.getCondCount() < MIOpNum)
          IAP.addCond(PI.asmWriterGetPatCondKIgnore());

        const CodeGenInstAlias::ResultOperand &RO = CGA.ResultOperands[i];

        switch (RO.Kind) {
        case CodeGenInstAlias::ResultOperand::K_Record: {
          const Record *Rec = RO.getRecord();
          StringRef ROName = RO.getName();
          int PrintMethodIdx = -1;

          // These two may have a PrintMethod, which we want to record (if it's
          // the first time we've seen it) and provide an index for the aliasing
          // code to use.
          if (Rec->isSubClassOf("RegisterOperand") ||
              Rec->isSubClassOf("Operand")) {
            StringRef PrintMethod = Rec->getValueAsString("PrintMethod");
            bool IsPCRel =
                Rec->getValueAsString("OperandType") == "OPERAND_PCREL";
            if (PrintMethod != "" && PrintMethod != "printOperand") {
              PrintMethodIdx = llvm::find_if(PrintMethods,
                                             [&](auto &X) {
                                               return X.first == PrintMethod;
                                             }) -
                               PrintMethods.begin();
              if (static_cast<unsigned>(PrintMethodIdx) == PrintMethods.size())
                PrintMethods.emplace_back(std::string(PrintMethod), IsPCRel);
            }
          }

          if (Rec->isSubClassOf("RegisterOperand"))
            Rec = Rec->getValueAsDef("RegClass");
          if (Rec->isSubClassOf("RegisterClass")) {
            if (!IAP.isOpMapped(ROName)) {
              IAP.addOperand(ROName, MIOpNum, PrintMethodIdx);
              Record *R = CGA.ResultOperands[i].getRecord();
              if (R->isSubClassOf("RegisterOperand"))
                R = R->getValueAsDef("RegClass");
              IAP.addCond(std::string(
                  formatv(PI.asmWriterGetPatCondKRegClass(),
                          Namespace, R->getName())));
            } else {
              IAP.addCond(std::string(formatv(
                  PI.asmWriterGetPatCondKTiedReg(), IAP.getOpIndex(ROName))));
            }
          } else {
            // Assume all printable operands are desired for now. This can be
            // overridden in the InstAlias instantiation if necessary.
            IAP.addOperand(ROName, MIOpNum, PrintMethodIdx);

            // There might be an additional predicate on the MCOperand
            unsigned Entry = MCOpPredicateMap[Rec];
            if (!Entry) {
              if (!Rec->isValueUnset("MCOperandPredicate")) {
                MCOpPredicates.push_back(Rec);
                Entry = MCOpPredicates.size();
                MCOpPredicateMap[Rec] = Entry;
              } else
                break; // No conditions on this operand at all
            }
            IAP.addCond(
                std::string(formatv(PI.asmWriterGetPatCondKCustom(), Entry)));
          }
          break;
        }
        case CodeGenInstAlias::ResultOperand::K_Imm: {
          // Just because the alias has an immediate result, doesn't mean the
          // MCInst will. An MCExpr could be present, for example.
          auto Imm = CGA.ResultOperands[i].getImm();
          int32_t Imm32 = int32_t(Imm);
          if (Imm != Imm32)
            PrintFatalError("Matching an alias with an immediate out of the "
                            "range of int32_t is not supported");
          IAP.addCond(std::string(
              formatv(PI.asmWriterGetPatCondKImm(), Imm32)));
          break;
        }
        case CodeGenInstAlias::ResultOperand::K_Reg:
          if (!CGA.ResultOperands[i].getRegister()) {
            IAP.addCond(std::string(formatv(
                PI.asmWriterGetPatCondKNoReg(), Namespace)));
            break;
          }

          StringRef Reg = CGA.ResultOperands[i].getRegister()->getName();
          IAP.addCond(std::string(
              formatv(PI.asmWriterGetPatCondKReg(), Namespace, Reg)));
          break;
        }

        MIOpNum += RO.getMINumOperands();
      }

      std::vector<Record *> ReqFeatures;
      if (PassSubtarget) {
        // We only consider ReqFeatures predicates if PassSubtarget
        std::vector<Record *> RF =
            CGA.TheDef->getValueAsListOfDefs("Predicates");
        copy_if(RF, std::back_inserter(ReqFeatures), [](Record *R) {
          return R->getValueAsBit("AssemblerMatcherPredicate");
        });
      }

      for (Record *const R : ReqFeatures) {
        const DagInit *D = R->getValueAsDag("AssemblerCondDag");
        std::string CombineType = D->getOperator()->getAsString();
        if (CombineType != "any_of" && CombineType != "all_of")
          PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
        if (D->getNumArgs() == 0)
          PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
        bool IsOr = CombineType == "any_of";
        // Change (any_of FeatureAll, (any_of ...)) to (any_of FeatureAll, ...).
        if (IsOr && D->getNumArgs() == 2 && isa<DagInit>(D->getArg(1))) {
          DagInit *RHS = dyn_cast<DagInit>(D->getArg(1));
          SmallVector<Init *> Args{D->getArg(0)};
          SmallVector<StringInit *> ArgNames{D->getArgName(0)};
          for (unsigned i = 0, e = RHS->getNumArgs(); i != e; ++i) {
            Args.push_back(RHS->getArg(i));
            ArgNames.push_back(RHS->getArgName(i));
          }
          D = DagInit::get(D->getOperator(), nullptr, Args, ArgNames);
        }

        for (auto *Arg : D->getArgs()) {
          bool IsNeg = false;
          if (auto *NotArg = dyn_cast<DagInit>(Arg)) {
            if (NotArg->getOperator()->getAsString() != "not" ||
                NotArg->getNumArgs() != 1)
              PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");
            Arg = NotArg->getArg(0);
            IsNeg = true;
          }
          if (!isa<DefInit>(Arg) ||
              !cast<DefInit>(Arg)->getDef()->isSubClassOf("SubtargetFeature"))
            PrintFatalError(R->getLoc(), "Invalid AssemblerCondDag!");

          IAP.addCond(std::string(formatv(
              PI.asmWriterGetPatCondKFeature(), IsOr ? "Or" : "",
              IsNeg ? "Neg" : "", Namespace, Arg->getAsString())));
        }
        // If an AssemblerPredicate with ors is used, note end of list should
        // these be combined.
        if (IsOr)
          IAP.addCond(PI.asmWriterGetPatCondKEndOrFeature());
      }

      IAPrinterMap[Aliases.first].push_back(std::move(IAP));
    }
  }

  //////////////////////////////
  // Write out the printAliasInstr function
  //////////////////////////////

  std::string PatternsForOpcode;
  raw_string_ostream OpcodeO(PatternsForOpcode);

  unsigned PatternCount = 0;
  std::string Patterns;
  raw_string_ostream PatternO(Patterns);

  unsigned CondCount = 0;
  std::string Conds;
  raw_string_ostream CondO(Conds);

  // All flattened alias strings.
  std::map<std::string, uint32_t> AsmStringOffsets;
  std::vector<std::pair<uint32_t, std::string>> AsmStrings;
  size_t AsmStringsSize = 0;

  // Iterate over the opcodes in enum order so they are sorted by opcode for
  // binary search.
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    auto It = IAPrinterMap.find(getQualifiedName(Inst->TheDef));
    if (It == IAPrinterMap.end())
      continue;
    std::vector<IAPrinter> &IAPs = It->second;
    std::vector<IAPrinter*> UniqueIAPs;

    // Remove any ambiguous alias rules.
    for (auto &LHS : IAPs) {
      bool IsDup = false;
      for (const auto &RHS : IAPs) {
        if (&LHS != &RHS && LHS == RHS) {
          IsDup = true;
          break;
        }
      }

      if (!IsDup)
        UniqueIAPs.push_back(&LHS);
    }

    if (UniqueIAPs.empty()) continue;

    unsigned PatternStart = PatternCount;

    // Insert the pattern start and opcode in the pattern list for debugging.
    PatternO << formatv(PI.asmWriterGetPatOpcStart(), It->first, PatternStart);

    for (IAPrinter *IAP : UniqueIAPs) {
      // Start each condition list with a comment of the resulting pattern that
      // we're trying to match.
      unsigned CondStart = CondCount;
      CondO << formatv(PI.asmWriterGetCondPatStart(), IAP->getResult(), CondStart);
      for (const auto &Cond : IAP->getConds())
        CondO << PI.asmWriterGetCond(Cond);
      CondCount += IAP->getCondCount();

      // After operands have been examined, re-encode the alias string with
      // escapes indicating how operands should be printed.
      uint32_t UnescapedSize = 0;
      std::string EncodedAsmString = IAP->formatAliasString(UnescapedSize);
      auto Insertion =
          AsmStringOffsets.insert({EncodedAsmString, AsmStringsSize});
      if (Insertion.second) {
        // If the string is new, add it to the vector.
        AsmStrings.push_back({AsmStringsSize, EncodedAsmString});
        AsmStringsSize += UnescapedSize + 1;
      }
      unsigned AsmStrOffset = Insertion.first->second;

      PatternO << formatv(PI.asmWriterGetPatternFormat(), AsmStrOffset,
                          CondStart, IAP->getNumMIOps(), IAP->getCondCount());
      ++PatternCount;
    }

    OpcodeO << formatv(PI.asmWriterGetOpcodeFormat(), It->first, PatternStart,
                       PatternCount - PatternStart);
  }

  if (OpcodeO.str().empty()) {
    PI.asmWriterEmitPrintAliasInstrHeader(Target.getName().str(), ClassName, PassSubtarget);
    PI.asmWriterEmitPrintAliasInstrBodyRetFalse();
    PI.emitIncludeToggle("PRINT_ALIAS_INSTR", false, false);
    return;
  }

  // Forward declare the validation method if needed.
  if (!MCOpPredicates.empty())
    PI.asmWriterEmitDeclValid(Target.getName().str(), ClassName);

  PI.asmWriterEmitPrintAliasInstrHeader(Target.getName().str(),
                                        ClassName,
                                        PassSubtarget);
  PI.asmWriterEmitPrintAliasInstrBody(OpcodeO,
                                      PatternO,
                                      CondO,
                                      AsmStrings,
                                      MCOpPredicates,
                                      Target.getName().str(),
                                      ClassName,
                                      PassSubtarget);

  //////////////////////////////
  // Write out the printCustomAliasOperand function
  //////////////////////////////

  PI.asmWriterEmitPrintAliasOp(Target.getName().str(),
                               ClassName,
                               PrintMethods,
                               PassSubtarget);

  PI.asmWriterEmitPrintMC(Target.getName().str(), ClassName, MCOpPredicates);
  PI.emitIncludeToggle("PRINT_ALIAS_INSTR", false, false);
}

AsmWriterEmitter::AsmWriterEmitter(RecordKeeper &R, PrinterLLVM &PI) : Records(R), Target(R), PI(PI) {
  Record *AsmWriter = Target.getAsmWriter();
  unsigned Variant = AsmWriter->getValueAsInt("Variant");

  // Get the instruction numbering.
  NumberedInstructions = Target.getInstructionsByEnumValue();

  for (unsigned i = 0, e = NumberedInstructions.size(); i != e; ++i) {
    const CodeGenInstruction *I = NumberedInstructions[i];
    if (!I->AsmString.empty() && I->TheDef->getName() != "PHI")
      Instructions.emplace_back(*I, i, Variant);
  }
}

void AsmWriterEmitter::run() {
  std::vector<std::vector<std::string>> TableDrivenOperandPrinters;
  unsigned BitsLeft = 0;
  unsigned AsmStrBits = 0;

  PI.asmWriterEmitSourceFileHeader();
  EmitGetMnemonic(TableDrivenOperandPrinters, BitsLeft, AsmStrBits);
  EmitPrintInstruction(TableDrivenOperandPrinters, BitsLeft, AsmStrBits);
  EmitGetRegisterName();
  EmitPrintAliasInstruction();
}

namespace llvm {

void EmitAsmWriter(RecordKeeper &RK, raw_ostream &OS) {
  PrinterLanguage const PL = PrinterLLVM::getLanguage();
  PrinterLLVM *PI;

  formatted_raw_ostream FOS(OS);
  if (PL == PRINTER_LANG_CPP) {
    PI = new PrinterLLVM(FOS);
  } else if (PL == PRINTER_LANG_CAPSTONE_C) {
    PI = new PrinterCapstone(FOS);
  } else {
    llvm_unreachable("AsmWriterEmitter does not support the given output language.");
  }
  AsmWriterEmitter(RK, *PI).run();
  delete PI;
}

} // end namespace llvm
