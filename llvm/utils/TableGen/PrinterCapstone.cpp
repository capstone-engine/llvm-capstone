//===------------- PrinterCapstone.cpp - Printer Capstone -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Emits the generated decoder C code for the Capstone.
//
//===----------------------------------------------------------------------===//

#include "CodeGenTarget.h"
#include "Printer.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <algorithm>
#include <bitset>

static void emitDefaultSourceFileHeader(raw_ostream &OS) {
  OS << "/* Capstone Disassembly Engine, https://www.capstone-engine.org */\n"
     << "/* By Nguyen Anh Quynh <aquynh@gmail.com>, 2013-2022, */\n"
     << "/*    Rot127 <unisono@quyllur.org> 2022-2023 */\n"
     << "/* Automatically generated file by Capstone's LLVM TableGen "
        "Disassembler "
        "Backend. */\n\n"
     << "/* LLVM-commit: <commit> */\n"
     << "/* LLVM-tag: <tag> */\n\n"
     << "/* Do not edit. */\n\n"
     << "/* Capstone's LLVM TableGen Backends: */\n"
     << "/* https://github.com/capstone-engine/llvm-capstone */\n\n";
}

namespace llvm {

/// Prints `namespace <name> {` and `} // end namespace <name>` to the output
/// stream. If Name == "" it emits an anonymous namespace.
void PrinterCapstone::emitNamespace(std::string const &Name, bool Begin,
                                    std::string const &Comment = "") const {
  return;
}

/// Prints
/// ```
/// #ifdef <Name>
/// #undef <Name>
/// ```
/// and
/// `#endif // <Name>`
/// Used to control inclusion of a code block via a macro definition.
void PrinterCapstone::emitIncludeToggle(std::string const &Name, bool Begin,
                                        bool Newline, bool UndefAtEnd) const {
  std::set<std::string> Ignore = {"GET_REGINFO_TARGET_DESC",
                                  "GET_REGINFO_HEADER",
                                  "GET_MNEMONIC_CHECKER",
                                  "GET_MNEMONIC_SPELL_CHECKER",
                                  "GET_MATCHER_IMPLEMENTATION",
                                  "GET_SUBTARGET_FEATURE_NAME",
                                  "GET_REGISTER_MATCHER",
                                  "GET_OPERAND_DIAGNOSTIC_TYPES",
                                  "GET_ASSEMBLER_HEADER",
                                  "GET_INSTRINFO_HEADER",
                                  "GET_INSTRINFO_HELPER_DECLS",
                                  "GET_INSTRINFO_HELPERS",
                                  "GET_INSTRINFO_CTOR_DTOR",
                                  "GET_INSTRINFO_OPERAND_ENUM",
                                  "GET_INSTRINFO_NAMED_OPS",
                                  "GET_INSTRINFO_OPERAND_TYPES_ENUM",
                                  "GET_INSTRINFO_OPERAND_TYPE",
                                  "GET_INSTRINFO_MEM_OPERAND_SIZE",
                                  "GET_INSTRINFO_LOGICAL_OPERAND_SIZE_MAP",
                                  "GET_INSTRINFO_LOGICAL_OPERAND_TYPE_MAP",
                                  "GET_INSTRINFO_MC_HELPER_DECLS",
                                  "GET_INSTRINFO_MC_HELPERS",
                                  "ENABLE_INSTR_PREDICATE_VERIFIER",
                                  "GET_SUBTARGETINFO_MC_DESC",
                                  "GET_SUBTARGETINFO_TARGET_DESC",
                                  "GET_SUBTARGETINFO_HEADER",
                                  "GET_SUBTARGETINFO_CTOR",
                                  "GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS",
                                  "GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS",
                                  "GET_SUBTARGETINFO_MACRO"};
  if (Ignore.find(Name) != Ignore.end())
    return;
  if (Begin) {
    OS << "#ifdef " << Name << "\n";
    if (!UndefAtEnd)
      OS << "#undef " << Name << "\n\n";
  } else {
    if (UndefAtEnd)
      OS << "#undef " << Name << "\n";
    OS << "#endif // " << Name << (Newline ? "\n\n" : "\n");
  }
}

void PrinterCapstone::emitIfNotDef(std::string const &Name, bool Begin) const {
  if (Name == "NDEBUG")
    return;
  if (Begin) {
    OS << "#ifndef " << Name << "\n";
  } else {
    OS << "#endif // " << Name << "\n\n";
  }
}

void PrinterCapstone::regInfoEmitSourceFileHeader(
    std::string const &Desc) const {
  static unsigned Count = 0;
  if (Count > 0) {
    // Only emit it once at the beginning.
    return;
  }
  emitDefaultSourceFileHeader(OS);
  ++Count;
}

void writeFile(std::string Filename, std::string const &Str) {
  std::error_code EC;
  ToolOutputFile InsnMapFile(Filename, EC, sys::fs::OF_Text);
  if (EC)
    PrintFatalNote("Could no write \"" + Filename + "\" Error:\n" +
                   EC.message());
  InsnMapFile.os() << Str;
  InsnMapFile.keep();
}

// runEnums - Print out enum values for all of the registers.
void PrinterCapstone::regInfoEmitEnums(CodeGenTarget const &Target,
                                       CodeGenRegBank const &Bank) const {
  std::string CSRegEnumStr;
  raw_string_ostream CSRegEnum(CSRegEnumStr);
  emitDefaultSourceFileHeader(CSRegEnum);

  const auto &Registers = Bank.getRegisters();

  // Register enums are stored as uint16_t in the tables. Make sure we'll fit.
  assert(Registers.size() <= 0xffff && "Too many regs to fit in tables");

  emitIncludeToggle("GET_REGINFO_ENUM", true);
  StringRef TargetName = Target.getName();

  OS << "enum {\n  " << TargetName << "_NoRegister,\n";
  CSRegEnum << "\t" << TargetName << "_REG_INVALID = 0,\n";

  for (const auto &Reg : Registers) {
    OS << "  " << TargetName << "_" << Reg.getName() << " = " << Reg.EnumValue
       << ",\n";
    CSRegEnum << "\t" << TargetName << "_REG_" << Reg.getName() << " = "
              << Reg.EnumValue << ",\n";
  }
  assert(Registers.size() == Registers.back().EnumValue &&
         "Register enum value mismatch!");
  OS << "  NUM_TARGET_REGS // " << Registers.size() + 1 << "\n";
  OS << "};\n";
  CSRegEnum << "\t" << TargetName << "_REG_ENDING, // " << Registers.size() + 1
            << "\n";

  writeFile(TargetName.str() + "GenCSRegEnum.inc", CSRegEnumStr);

  const auto &RegisterClasses = Bank.getRegClasses();
  if (!RegisterClasses.empty()) {

    // RegisterClass enums are stored as uint16_t in the tables.
    assert(RegisterClasses.size() <= 0xffff &&
           "Too many register classes to fit in tables");

    OS << "\n// Register classes\n\n";
    OS << "enum {\n";
    for (const auto &RC : RegisterClasses)
      OS << "  " << TargetName << "_" << RC.getName() << "RegClassID"
         << " = " << RC.EnumValue << ",\n";
    OS << "\n};\n";
  }

  const std::vector<Record *> &RegAltNameIndices =
      Target.getRegAltNameIndices();
  // If the only definition is the default NoRegAltName, we don't need to
  // emit anything.
  if (RegAltNameIndices.size() > 1) {
    OS << "\n// Register alternate name indices\n\n";
    OS << "enum {\n";
    for (unsigned I = 0, E = RegAltNameIndices.size(); I != E; ++I)
      OS << "  " << TargetName << "_" << RegAltNameIndices[I]->getName()
         << ",\t// " << I << "\n";
    OS << "  NUM_TARGET_REG_ALT_NAMES = " << RegAltNameIndices.size() << "\n";
    OS << "};\n";
  }

  auto &SubRegIndices = Bank.getSubRegIndices();
  if (!SubRegIndices.empty()) {
    OS << "\n// Subregister indices\n\n";
    std::string const Namespace = SubRegIndices.front().getNamespace();
    OS << "enum {\n  " << TargetName << "_NoSubRegister,\n";
    unsigned I = 0;
    for (const auto &Idx : SubRegIndices)
      OS << "  " << TargetName << "_" << Idx.getName() << ",\t// " << ++I
         << "\n";
    OS << "  " << TargetName << "_NUM_TARGET_SUBREGS\n};\n";
  }
  emitIncludeToggle("GET_REGINFO_ENUM", false);
}

void PrinterCapstone::regInfoEmitRegDiffLists(
    std::string const TargetName,
    SequenceToOffsetTable<DiffVec> const &DiffSeqs) const {
  OS << "static const MCPhysReg " << TargetName << "RegDiffLists[] = {\n";
  DiffSeqs.emit(OS, [](raw_ostream &OS, uint16_t Val) { OS << Val; });
  OS << "};\n\n";
}

void PrinterCapstone::regInfoEmitLaneMaskLists(
    std::string const TargetName,
    SequenceToOffsetTable<MaskVec> const &LaneMaskSeqs) const {
  return;
}

void PrinterCapstone::regInfoEmitSubRegIdxLists(
    std::string const TargetName,
    SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> const
        &SubRegIdxSeqs) const {
  OS << "static const uint16_t " << TargetName << "SubRegIdxLists[] = {\n";
  SubRegIdxSeqs.emit(OS, [](raw_ostream &OS, const CodeGenSubRegIndex *Idx) {
    OS << Idx->EnumValue;
  });
  OS << "};\n\n";
}

void PrinterCapstone::regInfoEmitSubRegIdxSizes(
    std::string const TargetName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitSubRegStrTable(
    std::string const TargetName,
    SequenceToOffsetTable<std::string> const &RegStrings) const {
  OS << "static const MCRegisterDesc " << TargetName
     << "RegDesc[] = { // Descriptors\n";
  OS << "  { " << RegStrings.get("") << ", 0, 0, 0, 0, 0 },\n";
}

void PrinterCapstone::regInfoEmitRegDesc(
    SequenceToOffsetTable<MaskVec> const &LaneMaskSeqs,
    std::deque<CodeGenRegister> const &Regs,
    SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> const
        &SubRegIdxSeqs,
    SequenceToOffsetTable<DiffVec> const &DiffSeqs,
    SmallVector<SubRegIdxVec, 4> const &SubRegIdxLists,
    SmallVector<DiffVec, 4> const &SubRegLists,
    SmallVector<DiffVec, 4> const &SuperRegLists,
    SmallVector<DiffVec, 4> const &RegUnitLists,
    SmallVector<unsigned, 4> const &RegUnitInitScale,
    SmallVector<MaskVec, 4> const &RegUnitLaneMasks,
    SequenceToOffsetTable<std::string> const &RegStrings) const {
  unsigned I = 0;
  for (const auto &Reg : Regs) {
    OS << "  { " << RegStrings.get(std::string(Reg.getName())) << ", "
       << DiffSeqs.get(SubRegLists[I]) << ", " << DiffSeqs.get(SuperRegLists[I])
       << ", " << SubRegIdxSeqs.get(SubRegIdxLists[I]) << ", "
       << (DiffSeqs.get(RegUnitLists[I]) * 16 + RegUnitInitScale[I]) << ", "
       << LaneMaskSeqs.get(RegUnitLaneMasks[I]) << " },\n";
    ++I;
  }
  OS << "};\n\n"; // End of register descriptors...
}

void PrinterCapstone::regInfoEmitRegUnitRoots(
    std::string const TargetName, CodeGenRegBank const &RegBank) const {
  return;
}

static std::string getQualifiedNameCCS(const Record *R) {
  std::string Namespace;
  if (R->getValue("Namespace"))
    Namespace = std::string(R->getValueAsString("Namespace"));
  if (Namespace.empty())
    return std::string(R->getName());
  return StringRef(Namespace).str() + "_" + R->getName().str();
}

void PrinterCapstone::regInfoEmitRegClasses(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<std::string> &RegClassStrings,
    CodeGenTarget const &Target) const {
  for (const auto &RC : RegClasses) {
    ArrayRef<Record *> const Order = RC.getOrder();

    // Give the register class a legal C name if it's anonymous.
    const std::string &Name = RC.getName();

    RegClassStrings.add(Name);

    // Emit the register list now (unless it would be a zero-length array).
    if (!Order.empty()) {
      OS << "  // " << Name << " Register Class...\n"
         << "  static const MCPhysReg " << Name << "[] = {\n    ";
      for (Record *Reg : Order) {
        OS << getQualifiedNameCCS(Reg) << ", ";
      }
      OS << "\n  };\n\n";

      OS << "  // " << Name << " Bit set.\n"
         << "  static const uint8_t " << Name << "Bits[] = {\n    ";
      PrinterBitVectorEmitter BVE;
      for (Record *Reg : Order) {
        BVE.add(Target.getRegBank().getReg(Reg)->EnumValue);
      }
      BVE.print(OS);
      OS << "\n  };\n\n";
    }
  }
}

void PrinterCapstone::regInfoEmitStrLiteralRegClasses(
    std::string const TargetName,
    SequenceToOffsetTable<std::string> const &RegClassStrings) const {
  return;
}

void PrinterCapstone::regInfoEmitMCRegClassesTable(
    std::string const TargetName,
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<std::string> &RegClassStrings) const {
  OS << "static const MCRegisterClass " << TargetName
     << "MCRegisterClasses[] = {\n";

  for (const auto &RC : RegClasses) {
    ArrayRef<Record *> const Order = RC.getOrder();
    std::string const RCName = Order.empty() ? "nullptr" : RC.getName();
    std::string const RCBitsName =
        Order.empty() ? "nullptr" : RC.getName() + "Bits";
    std::string const RCBitsSize =
        Order.empty() ? "0" : "sizeof(" + RCBitsName + ")";
    assert(isInt<8>(RC.CopyCost) && "Copy cost too large.");
    // For Capstone we are only interested in:
    // RegClass Name, Size group and bit size
    OS << "  { " << RCName << ", " << RCBitsName << ", " << RCBitsSize
       << " },\n";
  }

  OS << "};\n\n";
}

void PrinterCapstone::regInfoEmitRegEncodingTable(
    std::string const TargetName,
    std::deque<CodeGenRegister> const &Regs) const {
  OS << "static const uint16_t " << TargetName;
  OS << "RegEncodingTable[] = {\n";
  // Add entry for NoRegister
  OS << "  0,\n";
  for (const auto &RE : Regs) {
    Record *Reg = RE.TheDef;
    BitsInit *BI = Reg->getValueAsBitsInit("HWEncoding");
    uint64_t Value = 0;
    for (unsigned I = 0, Ie = BI->getNumBits(); I != Ie; ++I) {
      if (BitInit *B = dyn_cast<BitInit>(BI->getBit(I)))
        Value |= (uint64_t)B->getValue() << I;
    }
    OS << "  " << Value << ",\n";
  }
  OS << "};\n"; // End of HW encoding table
  return;
}

void PrinterCapstone::regInfoEmitMCRegInfoInit(
    std::string const TargetName, CodeGenRegBank const &RegBank,
    std::deque<CodeGenRegister> const &Regs,
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitInfoDwarfRegsRev(
    StringRef const &Namespace, DwarfRegNumsVecTy &DwarfRegNums,
    unsigned MaxLength, bool IsCtor) const {
  return;
}

void PrinterCapstone::regInfoEmitInfoDwarfRegs(StringRef const &Namespace,
                                               DwarfRegNumsVecTy &DwarfRegNums,
                                               unsigned MaxLength,
                                               bool IsCtor) const {
  return;
}

void PrinterCapstone::regInfoEmitInfoRegMapping(StringRef const &Namespace,
                                                unsigned MaxLength,
                                                bool IsCtor) const {
  return;
}

void PrinterCapstone::regInfoEmitHeaderIncludes() const { return; }

void PrinterCapstone::regInfoEmitHeaderExternRegClasses(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  return;
}

void PrinterCapstone::regInfoEmitHeaderDecl(
    std::string const &TargetName, std::string const &ClassName,
    bool SubRegsPresent, bool DeclareGetPhysRegBaseClass) const {
  return;
}

void PrinterCapstone::regInfoEmitExternRegClassesArr(
    std::string const &TargetName) const {
  return;
}

void PrinterCapstone::regInfoEmitVTSeqs(
    SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs)
    const {
  return;
}

void PrinterCapstone::regInfoEmitSubRegIdxTable(
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitRegClassInfoTable(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs,
    CodeGenHwModes const &CGH, unsigned NumModes) const {
  return;
}

void PrinterCapstone::regInfoEmitSubClassMaskTable(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SmallVector<IdxList, 8> &SuperRegIdxLists,
    SequenceToOffsetTable<IdxList, deref<std::less<>>> &SuperRegIdxSeqs,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices,
    BitVector &MaskBV) const {
  return;
}

void PrinterCapstone::regInfoEmitSuperRegIdxSeqsTable(
    SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs)
    const {
  return;
}

void PrinterCapstone::regInfoEmitSuperClassesTable(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  return;
}

void PrinterCapstone::regInfoEmitRegClassMethods(
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::string const &TargetName) const {
  return;
}

void PrinterCapstone::regInfomitRegClassInstances(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs,
    SmallVector<IdxList, 8> const &SuperRegIdxLists,
    std::string const &TargetName) const {
  return;
}

void PrinterCapstone::regInfoEmitRegClassTable(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  return;
}

void PrinterCapstone::regInfoEmitCostPerUseTable(
    std::vector<unsigned> const &AllRegCostPerUse, unsigned NumRegCosts) const {
  return;
}

void PrinterCapstone::regInfoEmitInAllocatableClassTable(
    llvm::BitVector const &InAllocClass) const {
  return;
}

void PrinterCapstone::regInfoEmitRegExtraDesc(std::string const &TargetName,
                                              unsigned NumRegCosts) const {
  return;
}

void PrinterCapstone::regInfoEmitSubClassSubRegGetter(
    std::string const &ClassName, unsigned SubRegIndicesSize,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices,
    std::list<CodeGenRegisterClass> const &RegClasses,
    CodeGenRegBank &RegBank) const {
  return;
}

void PrinterCapstone::regInfoEmitRegClassWeight(
    CodeGenRegBank const &RegBank, std::string const &ClassName) const {
  return;
}

void PrinterCapstone::regInfoEmitRegUnitWeight(
    CodeGenRegBank const &RegBank, std::string const &ClassName,
    bool RegUnitsHaveUnitWeight) const {
  return;
}

void PrinterCapstone::regInfoEmitGetNumRegPressureSets(
    std::string const &ClassName, unsigned NumSets) const {
  return;
}

void PrinterCapstone::regInfoEmitGetRegPressureTables(
    CodeGenRegBank const &RegBank, std::string const &ClassName,
    unsigned NumSets) const {
  return;
}

void PrinterCapstone::regInfoEmitRCSetsTable(
    std::string const &ClassName, unsigned NumRCs,
    SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
    std::vector<std::vector<int>> const &PSets) const {
  return;
}

void PrinterCapstone::regInfoEmitGetRegUnitPressureSets(
    SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
    CodeGenRegBank const &RegBank, std::string const &ClassName,
    std::vector<std::vector<int>> const &PSets) const {
  return;
}

void PrinterCapstone::regInfoEmitExternTableDecl(
    std::string const &TargetName) const {
  return;
}

void PrinterCapstone::regInfoEmitRegClassInit(
    std::string const &TargetName, std::string const &ClassName,
    CodeGenRegBank const &RegBank,
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::deque<CodeGenRegister> const &Regs, unsigned SubRegIndicesSize) const {
  return;
}

void PrinterCapstone::regInfoEmitSaveListTable(
    Record const *CSRSet, SetTheory::RecVec const *Regs) const {
  return;
}

void PrinterCapstone::regInfoEmitRegMaskTable(std::string const &CSRSetName,
                                              BitVector &Covered) const {
  return;
}

void PrinterCapstone::regInfoEmitGetRegMasks(
    std::vector<Record *> const &CSRSets, std::string const &ClassName) const {
  return;
}

void PrinterCapstone::regInfoEmitGPRCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  return;
}
void PrinterCapstone::regInfoEmitFixedRegCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  return;
}

void PrinterCapstone::regInfoEmitArgRegCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  return;
}

void PrinterCapstone::regInfoEmitGetRegMaskNames(
    std::vector<Record *> const &CSRSets, std::string const &ClassName) const {
  return;
}

void PrinterCapstone::regInfoEmitGetFrameLowering(
    std::string const &TargetName) const {
  return;
}

void PrinterCapstone::regInfoEmitComposeSubRegIndicesImplHead(
    std::string const &ClName) const {
  return;
}

void PrinterCapstone::regInfoEmitComposeSubRegIndicesImplBody(
    SmallVector<SmallVector<CodeGenSubRegIndex *, 4>, 4> const &Rows,
    unsigned SubRegIndicesSize, SmallVector<unsigned, 4> const &RowMap) const {
  return;
}

void PrinterCapstone::regInfoEmitLaneMaskComposeSeq(
    SmallVector<SmallVector<MaskRolPair, 1>, 4> const &Sequences,
    SmallVector<unsigned, 4> const &SubReg2SequenceIndexMap,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitComposeSubRegIdxLaneMask(
    std::string const &ClName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitComposeSubRegIdxLaneMaskRev(
    std::string const &ClName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  return;
}

void PrinterCapstone::regInfoEmitIsConstantPhysReg(
    std::deque<CodeGenRegister> const &Regs,
    std::string const &ClassName) const {}

void patchQualifier(std::string &Code) {
  while (Code.find("::") != std::string::npos)
    Code = Regex("::").sub("_", Code);
}

void patchNullptr(std::string &Code) {
  while (Code.find("nullptr") != std::string::npos)
    Code = Regex("nullptr").sub("NULL", Code);
}

void patchIsGetImmReg(std::string &Code) {
  Regex Pattern = Regex("[a-zA-Z0-9_]+\\.(get|is)(Imm|Reg)\\(\\)");
  SmallVector<StringRef> Matches;
  while (Pattern.match(Code, &Matches)) {
    StringRef Match = Matches[0];
    StringRef Op = Match.split(".").first;
    StringRef Func = Match.split(".").second.trim(")");
    Code = Code.replace(Code.find(Match), Match.size(),
                        "MCOperand_" + Func.str() + Op.str() + ")");
  }
}

std::string edgeCaseTemplArg(std::string &Code) {
  size_t const B = Code.find_first_of("<");
  size_t const E = Code.find(">");
  if (B == std::string::npos) {
    // No template
    PrintFatalNote("Edge case for C++ code not handled: " + Code);
  }
  std::string const &DecName = Code.substr(0, B);
  std::string Args = Code.substr(B + 1, E - B - 1);
  std::string Rest = Code.substr(E + 1);
  if (Code.find("printSVERegOp") != std::string::npos)
    // AArch64: Template argument is of type char and
    // no argument is interpreded as 0
    return DecName + "_0" + Rest;

  PrintFatalNote("Edge case for C++ code not handled: " + Code);
}

std::string handleDefaultArg(const std::string &TargetName,
                                    std::string &Code) {
  static SmallVector<std::pair<std::string, std::string>>
      AArch64TemplFuncWithDefaults = {// Default is 1
                                      {"printVectorIndex", "1"},
                                      // Default is false == 0
                                      {"printPrefetchOp", "0"}};
  SmallVector<std::pair<std::string, std::string>> *TemplFuncWithDefaults;
  if (TargetName == "AArch64")
    TemplFuncWithDefaults = &AArch64TemplFuncWithDefaults;
  else
    return Code;
  for (std::pair Func : *TemplFuncWithDefaults) {
    while (Code.find(Func.first) != std::string::npos) {
      unsigned long const B =
          Code.find(Func.first) + std::string(Func.first).size();
      if (Code[B] == '_' || Code[B] == '<')
        // False positive or already fixed.
        break;
      std::string const &DecName = Code.substr(0, B);
      std::string Rest = Code.substr(B);
      Code = DecName + "_" + Func.second + Rest;
    }
  }
  // No template function, false positive or fixed
  return Code;
}

void patchTemplateArgs(const std::string &TargetName,
                              std::string &Code) {
  Code = handleDefaultArg(TargetName, Code);

  size_t B = Code.find_first_of("<");
  size_t E = Code.find(">");
  while (B != std::string::npos && E != std::string::npos) {
    std::string const &DecName = Code.substr(0, B);
    std::string Args = Code.substr(B + 1, E - B - 1);
    std::string Rest = Code.substr(E + 1);
    if (Args.empty()) {
      Code = edgeCaseTemplArg(Code);
      B = Code.find_first_of("<");
      E = Code.find(">");
      continue;
    }
    while ((Args.find("true") != std::string::npos) ||
          (Args.find("false") != std::string::npos) ||
          (Args.find(",") != std::string::npos) ||
          (Args.find("'") != std::string::npos)) {
      Args = Regex("true").sub("1", Args);
      Args = Regex("false").sub("0", Args);
      Args = Regex(" *, *").sub("_", Args);
      Args = Regex("'").sub("", Args);
    }
    Code = DecName + "_" + Args + Rest;
    B = Code.find_first_of("<");
    E = Code.find(">");
  }
}

std::string PrinterCapstone::translateToC(std::string const &TargetName,
                                          std::string const &Code) {
  std::string PatchedCode(Code);
  patchQualifier(PatchedCode);
  patchNullptr(PatchedCode);
  patchIsGetImmReg(PatchedCode);
  patchTemplateArgs(TargetName, PatchedCode);
  return PatchedCode;
}

void PrinterCapstone::decoderEmitterEmitOpDecoder(raw_ostream &DecoderOS,
                                                  const OperandInfo &Op) const {
  unsigned const Indent = 4;
  DecoderOS.indent(Indent) << GuardPrefix;
  if (Op.Decoder.find("<") != std::string::npos) {
    DecoderOS << translateToC(TargetName, Op.Decoder);
  } else {
    DecoderOS << Op.Decoder;
  }

  DecoderOS << "(MI, insn, Address, Decoder)" << GuardPostfix << " { "
            << (Op.HasCompleteDecoder ? "" : "*DecodeComplete = false; ")
            << "return " << ReturnFail << "; } \\\n";
}

void PrinterCapstone::decoderEmitterEmitOpBinaryParser(
    raw_ostream &DecOS, const OperandInfo &OpInfo) const {
  unsigned const Indent = 4;
  const std::string &Decoder = (OpInfo.Decoder.find("<") != std::string::npos)
                                   ? translateToC(TargetName, OpInfo.Decoder)
                                   : OpInfo.Decoder;

  bool const UseInsertBits = OpInfo.numFields() != 1 || OpInfo.InitValue != 0;

  if (UseInsertBits) {
    DecOS.indent(Indent) << "tmp = 0x";
    DecOS.write_hex(OpInfo.InitValue);
    DecOS << "; \\\n";
  }

  for (const EncodingField &EF : OpInfo) {
    DecOS.indent(Indent);
    if (UseInsertBits)
      DecOS << "tmp |= ";
    else
      DecOS << "tmp = ";
    DecOS << "fieldname(insn, " << EF.Base << ", " << EF.Width << ')';
    if (UseInsertBits)
      DecOS << " << " << EF.Offset;
    else if (EF.Offset != 0)
      DecOS << " << " << EF.Offset;
    DecOS << "; \\\n";
  }

  if (Decoder != "") {
    DecOS.indent(Indent) << GuardPrefix << Decoder
                         << "(MI, tmp, Address, Decoder)" << GuardPostfix
                         << " { "
                         << (OpInfo.HasCompleteDecoder
                                 ? ""
                                 : "*DecodeComplete = false; ")
                         << "return " << ReturnFail << "; } \\\n";
  } else {
    DecOS.indent(Indent) << "MCOperand_CreateImm0(MI, tmp); \\\n";
  }
}

bool PrinterCapstone::decoderEmitterEmitPredicateMatchAux(
    const Init &Val, bool ParenIfBinOp, raw_ostream &PredOS) const {
  if (auto *D = dyn_cast<DefInit>(&Val)) {
    if (!D->getDef()->isSubClassOf("SubtargetFeature"))
      return true;

    std::string Subtarget =
        StringRef(PredicateNamespace).str() + "_" + D->getAsString();
    PredOS << PredicateNamespace << "_getFeatureBits(Inst->csh->mode, "
           << Subtarget << ")";
    return false;
  }
  if (auto *D = dyn_cast<DagInit>(&Val)) {
    std::string const Op = D->getOperator()->getAsString();
    if (Op == "not" && D->getNumArgs() == 1) {
      PredOS << '!';
      return decoderEmitterEmitPredicateMatchAux(*D->getArg(0), true, PredOS);
    }
    if ((Op == "any_of" || Op == "all_of") && D->getNumArgs() > 0) {
      bool const Paren =
          D->getNumArgs() > 1 && std::exchange(ParenIfBinOp, true);
      if (Paren)
        PredOS << '(';
      ListSeparator LS(Op == "any_of" ? " || " : " && ");
      for (auto *Arg : D->getArgs()) {
        PredOS << LS;
        if (decoderEmitterEmitPredicateMatchAux(*Arg, ParenIfBinOp, PredOS))
          return true;
      }
      if (Paren)
        PredOS << ')';
      return false;
    }
  }
  return true;
}

bool PrinterCapstone::decoderEmitterEmitPredicateMatch(
    raw_ostream &PredOS, const ListInit *Predicates, unsigned Opc) const {
  bool IsFirstEmission = true;
  for (unsigned I = 0; I < Predicates->size(); ++I) {
    Record *Pred = Predicates->getElementAsRecord(I);
    if (!Pred->getValue("AssemblerMatcherPredicate"))
      continue;

    if (!isa<DagInit>(Pred->getValue("AssemblerCondDag")->getValue()))
      continue;

    if (!IsFirstEmission)
      PredOS << " && ";
    if (decoderEmitterEmitPredicateMatchAux(
            *Pred->getValueAsDag("AssemblerCondDag"), Predicates->size() > 1,
            PredOS))
      PrintFatalError(Pred->getLoc(), "Invalid AssemblerCondDag!");
    IsFirstEmission = false;
  }
  return !Predicates->empty();
}

void PrinterCapstone::decoderEmitterEmitFieldFromInstruction() const {
  OS << "// Helper function for extracting fields from encoded instructions.\n"
     << "#define FieldFromInstruction(fname, InsnType) \\\n"
     << "static InsnType fname(InsnType insn, unsigned startBit, unsigned "
        "numBits) \\\n"
     << "{ \\\n"
     << "  InsnType fieldMask; \\\n"
     << "  if (numBits == sizeof(InsnType) * 8) \\\n"
     << "    fieldMask = (InsnType)(-1LL); \\\n"
     << "  else \\\n"
     << "    fieldMask = (((InsnType)1 << numBits) - 1) << startBit; \\\n"
     << "  return (insn & fieldMask) >> startBit; \\\n"
     << "}\n\n";
}

void PrinterCapstone::decoderEmitterEmitInsertBits() const { return; }

void PrinterCapstone::decoderEmitterEmitDecodeInstruction(
    bool IsVarLenInst) const {
  OS << "#define DecodeInstruction(fname, fieldname, decoder, InsnType) \\\n"
     << "static DecodeStatus fname(const uint8_t DecodeTable[], "
        "MCInst *MI, \\\n"
     << "                                      InsnType insn, uint64_t "
        "Address, const void *Decoder) { \\\n"
     << "  const uint8_t *Ptr = DecodeTable; \\\n"
     << "  uint64_t CurFieldValue = 0; \\\n"
     << "  DecodeStatus S = MCDisassembler_Success; \\\n"
     << "  while (true) { \\\n"
     << "    switch (*Ptr) { \\\n"
     << "    default: \\\n"
     << "      return MCDisassembler_Fail; \\\n"
     << "    case MCD_OPC_ExtractField: { \\\n"
     << "      unsigned Start = *++Ptr; \\\n"
     << "      unsigned Len = *++Ptr; \\\n"
     << "      ++Ptr; \\\n";
  if (IsVarLenInst) {
    OS << "      makeUp(insn, Start + Len); \\\n";
  }
  OS << "      CurFieldValue = fieldname(insn, Start, Len); \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_FilterValue: { \\\n"
     << "      /* Decode the field value. */ \\\n"
     << "      unsigned Len; \\\n"
     << "      uint64_t Val = decodeULEB128(++Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
     << "      unsigned NumToSkip = *Ptr++; \\\n"
     << "      NumToSkip |= (*Ptr++) << 8; \\\n"
     << "      NumToSkip |= (*Ptr++) << 16; \\\n"
     << "      /* Perform the filter operation. */ \\\n"
     << "      if (Val != CurFieldValue) \\\n"
     << "        Ptr += NumToSkip; \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_CheckField: { \\\n"
     << "      unsigned Start = *++Ptr; \\\n"
     << "      unsigned Len = *++Ptr; \\\n";
  if (IsVarLenInst) {
    OS << "      makeUp(insn, Start + Len); \\\n";
  }
  OS << "      uint64_t FieldValue = fieldname(insn, Start, Len); "
        "\\\n"
     << "      /* Decode the field value. */ \\\n"
     << "      unsigned PtrLen = 0; \\\n"
     << "      uint64_t ExpectedValue = decodeULEB128(++Ptr, &PtrLen); \\\n"
     << "      Ptr += PtrLen; \\\n"
     << "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
     << "      unsigned NumToSkip = *Ptr++; \\\n"
     << "      NumToSkip |= (*Ptr++) << 8; \\\n"
     << "      NumToSkip |= (*Ptr++) << 16; \\\n"
     << "      /* If the actual and expected values don't match, skip. */ \\\n"
     << "      if (ExpectedValue != FieldValue) \\\n"
     << "        Ptr += NumToSkip; \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_CheckPredicate: { \\\n"
     << "      unsigned Len; \\\n"
     << "      /* Decode the Predicate Index value. */ \\\n"
     << "      unsigned PIdx = decodeULEB128(++Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
     << "      unsigned NumToSkip = *Ptr++; \\\n"
     << "      NumToSkip |= (*Ptr++) << 8; \\\n"
     << "      NumToSkip |= (*Ptr++) << 16; \\\n"
     << "      /* Check the predicate. */ \\\n"
     << "      bool Pred = checkDecoderPredicate(MI, PIdx); \\\n"
     << "      if (!Pred) \\\n"
     << "        Ptr += NumToSkip; \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_Decode: { \\\n"
     << "      unsigned Len; \\\n"
     << "      /* Decode the Opcode value. */ \\\n"
     << "      unsigned Opc = decodeULEB128(++Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      unsigned DecodeIdx = decodeULEB128(Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      MCInst_clear(MI); \\\n"
     << "      MCInst_setOpcode(MI, Opc); \\\n"
     << "      bool DecodeComplete = false; \\\n";
  if (IsVarLenInst) {
    OS << "      Len = InstrLenTable[Opc]; \\\n"
       << "      makeUp(insn, Len); \\\n";
  }
  OS << "      S = decoder(S, DecodeIdx, insn, MI, Address, "
        "Decoder, &DecodeComplete); \\\n"
     << "      return S; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_TryDecode: { \\\n"
     << "      unsigned Len; \\\n"
     << "      /* Decode the Opcode value. */ \\\n"
     << "      unsigned Opc = decodeULEB128(++Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      unsigned DecodeIdx = decodeULEB128(Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      /* NumToSkip is a plain 24-bit integer. */ \\\n"
     << "      unsigned NumToSkip = *Ptr++; \\\n"
     << "      NumToSkip |= (*Ptr++) << 8; \\\n"
     << "      NumToSkip |= (*Ptr++) << 16; \\\n"
     << "      /* Perform the decode operation. */ \\\n"
     << "      MCInst TmpMI = { 0 }; \\\n"
     << "      TmpMI.MRI = MI->MRI; \\\n"
     << "      TmpMI.csh = MI->csh; \\\n"
     << "      MCInst_setOpcode(&TmpMI, Opc); \\\n"
     << "      bool DecodeComplete = false; \\\n"
     << "      S = decoder(S, DecodeIdx, insn, &TmpMI, Address, "
     << "Decoder, &DecodeComplete); \\\n"
     << "      if (DecodeComplete) { \\\n"
     << "        /* Decoding complete. */ \\\n"
     << "        MCInst_updateWithTmpMI(MI, &TmpMI); \\\n"
     << "        return S; \\\n"
     << "      } else { \\\n"
     << "        /* If the decoding was incomplete, skip. */ \\\n"
     << "        Ptr += NumToSkip; \\\n"
     << "        /* Reset decode status. This also drops a SoftFail status "
        "that could be */ \\\n"
     << "        /* set before the decode attempt. */ \\\n"
     << "        S = MCDisassembler_Success; \\\n"
     << "      } \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_SoftFail: { \\\n"
     << "      /* Decode the mask values. */ \\\n"
     << "      unsigned Len; \\\n"
     << "      uint64_t PositiveMask = decodeULEB128(++Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      uint64_t NegativeMask = decodeULEB128(Ptr, &Len); \\\n"
     << "      Ptr += Len; \\\n"
     << "      bool Fail = (insn & PositiveMask) != 0 || (~insn & "
        "NegativeMask) != 0; \\\n"
     << "      if (Fail) \\\n"
     << "        S = MCDisassembler_SoftFail; \\\n"
     << "      break; \\\n"
     << "    } \\\n"
     << "    case MCD_OPC_Fail: { \\\n"
     << "      return MCDisassembler_Fail; \\\n"
     << "    } \\\n"
     << "    } \\\n"
     << "  } \\\n"
     << "  /* Bogisity detected in disassembler state machine! */ \\\n"
     << "}\n\n";

  std::set<std::string> HasTwoByteInsns = {"ARM"};
  std::set<std::string> HasFourByteInsns = {"ARM", "PPC", "AArch64", "Alpha"};

  if (HasTwoByteInsns.find(TargetName) != HasTwoByteInsns.end())
    OS << "FieldFromInstruction(fieldFromInstruction_2, uint16_t)\n"
       << "DecodeToMCInst(decodeToMCInst_2, fieldFromInstruction_2, uint16_t)\n"
       << "DecodeInstruction(decodeInstruction_2, fieldFromInstruction_2, "
          "decodeToMCInst_2, uint16_t)\n\n";
  if (HasFourByteInsns.find(TargetName) != HasFourByteInsns.end())
    OS << "FieldFromInstruction(fieldFromInstruction_4, uint32_t)\n"
       << "DecodeToMCInst(decodeToMCInst_4, fieldFromInstruction_4, uint32_t)\n"
       << "DecodeInstruction(decodeInstruction_4, fieldFromInstruction_4, "
          "decodeToMCInst_4, uint32_t)\n";
  // Special case: The LLVM disassembler uses uint64_t values for decoding.
  // Although PPC instructions are 4 bytes wide.
  if (TargetName == "PPC")
    OS << "FieldFromInstruction(fieldFromInstruction_4, uint64_t)\n"
       << "DecodeToMCInst(decodeToMCInst_4, fieldFromInstruction_4, uint64_t)\n"
       << "DecodeInstruction(decodeInstruction_4, fieldFromInstruction_4, "
          "decodeToMCInst_4, uint64_t)\n";
}

void PrinterCapstone::decoderEmitterEmitTable(
    DecoderTable &Table, unsigned BitWidth, StringRef Namespace,
    std::vector<EncodingAndInst> &NumberedEncodings) const {
  unsigned Indent = 0;
  OS.indent(Indent) << "static const uint8_t DecoderTable" << Namespace
                    << BitWidth << "[] = {\n";

  Indent += 2;

  // FIXME: We may be able to use the NumToSkip values to recover
  // appropriate indentation levels.
  DecoderTable::const_iterator I = Table.begin();
  DecoderTable::const_iterator const E = Table.end();
  while (I != E) {
    assert(I < E && "incomplete decode table entry!");

    uint64_t const Pos = I - Table.begin();
    OS << "/* " << Pos << " */";
    OS.PadToColumn(12);

    switch (*I) {
    default:
      PrintFatalError("invalid decode table opcode");
    case MCD::OPC_ExtractField: {
      ++I;
      unsigned const Start = *I++;
      unsigned const Len = *I++;
      OS.indent(Indent) << "MCD_OPC_ExtractField, " << Start << ", " << Len
                        << ",  // Inst{";
      if (Len > 1)
        OS << (Start + Len - 1) << "-";
      OS << Start << "} ...\n";
      break;
    }
    case MCD::OPC_FilterValue: {
      ++I;
      OS.indent(Indent) << "MCD_OPC_FilterValue, ";
      // The filter value is ULEB128 encoded.
      while (*I >= 128)
        OS << (unsigned)*I++ << ", ";
      OS << (unsigned)*I++ << ", ";

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_CheckField: {
      ++I;
      unsigned const Start = *I++;
      unsigned const Len = *I++;
      OS.indent(Indent) << "MCD_OPC_CheckField, " << Start << ", " << Len
                        << ", "; // << Val << ", " << NumToSkip << ",\n";
      // ULEB128 encoded field value.
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";
      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_CheckPredicate: {
      ++I;
      OS.indent(Indent) << "MCD_OPC_CheckPredicate, ";
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;
      OS << "// Skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_Decode:
    case MCD::OPC_TryDecode: {
      bool const IsTry = *I == MCD::OPC_TryDecode;
      ++I;
      // Extract the ULEB128 encoded Opcode to a buffer.
      uint8_t Buffer[16], *P = Buffer;
      while ((*P++ = *I++) >= 128)
        assert((P - Buffer) <= (ptrdiff_t)sizeof(Buffer) &&
               "ULEB128 value too large!");
      // Decode the Opcode value.
      unsigned const Opc = decodeULEB128(Buffer);
      OS.indent(Indent) << "MCD_OPC_" << (IsTry ? "Try" : "") << "Decode, ";
      for (P = Buffer; *P >= 128; ++P)
        OS << (unsigned)*P << ", ";
      OS << (unsigned)*P << ", ";

      // Decoder index.
      for (; *I >= 128; ++I)
        OS << (unsigned)*I << ", ";
      OS << (unsigned)*I++ << ", ";

      if (!IsTry) {
        OS << "// Opcode: " << NumberedEncodings[Opc] << "\n";
        break;
      }

      // Fallthrough for OPC_TryDecode.

      // 24-bit numtoskip value.
      uint8_t Byte = *I++;
      uint32_t NumToSkip = Byte;
      OS << (unsigned)Byte << ", ";
      Byte = *I++;
      OS << (unsigned)Byte << ", ";
      NumToSkip |= Byte << 8;
      Byte = *I++;
      OS << utostr(Byte) << ", ";
      NumToSkip |= Byte << 16;

      OS << "// Opcode: " << NumberedEncodings[Opc]
         << ", skip to: " << ((I - Table.begin()) + NumToSkip) << "\n";
      break;
    }
    case MCD::OPC_SoftFail: {
      ++I;
      OS.indent(Indent) << "MCD_OPC_SoftFail";
      // Positive mask
      uint64_t Value = 0;
      unsigned Shift = 0;
      do {
        OS << ", " << (unsigned)*I;
        Value += (*I & 0x7f) << Shift;
        Shift += 7;
      } while (*I++ >= 128);
      if (Value > 127) {
        OS << " /* 0x";
        OS.write_hex(Value);
        OS << " */";
      }
      // Negative mask
      Value = 0;
      Shift = 0;
      do {
        OS << ", " << (unsigned)*I;
        Value += (*I & 0x7f) << Shift;
        Shift += 7;
      } while (*I++ >= 128);
      if (Value > 127) {
        OS << " /* 0x";
        OS.write_hex(Value);
        OS << " */";
      }
      OS << ",\n";
      break;
    }
    case MCD::OPC_Fail: {
      ++I;
      OS.indent(Indent) << "MCD_OPC_Fail,\n";
      break;
    }
    }
  }
  OS.indent(Indent) << "0\n";

  Indent -= 2;

  OS.indent(Indent) << "};\n\n";
}

void PrinterCapstone::decoderEmitterEmitInstrLenTable(
    std::vector<unsigned> &InstrLen) const {
  OS << "static const uint8_t InstrLenTable[] = {\n";
  for (unsigned const &Len : InstrLen) {
    OS << Len << ",\n";
  }
  OS << "};\n\n";
}

void PrinterCapstone::decoderEmitterEmitPredicateFunction(
    PredicateSet &Predicates, unsigned Indentation) const {
  // The predicate function is just a big switch statement based on the
  // input predicate index.
  OS.indent(Indentation)
      << "static bool checkDecoderPredicate(MCInst *Inst, unsigned Idx"
      << ") {\n";
  Indentation += 2;
  if (!Predicates.empty()) {
    OS.indent(Indentation) << "switch (Idx) {\n";
    OS.indent(Indentation)
        << "default: /* llvm_unreachable(\"Invalid index!\"); */\n";
    unsigned Index = 0;
    for (const auto &Predicate : Predicates) {
      OS.indent(Indentation) << "case " << Index++ << ":\n";
      OS.indent(Indentation + 2) << "return (" << Predicate << ");\n";
    }
    OS.indent(Indentation) << "}\n";
  } else {
    // No case statement to emit
    OS.indent(Indentation) << "/* llvm_unreachable(\"Invalid index!\"); */\n";
  }
  Indentation -= 2;
  OS.indent(Indentation) << "}\n\n";
}

void PrinterCapstone::decoderEmitterEmitDecoderFunction(
    DecoderSet &Decoders, unsigned Indentation) const {
  // The decoder function is just a big switch statement based on the
  // input decoder index.
  OS.indent(Indentation)
      << "#define DecodeToMCInst(fname, fieldname, InsnType) \\\n"
      << "static DecodeStatus fname(DecodeStatus S, unsigned Idx, InsnType "
         "insn, MCInst *MI, \\\n"
      << "		uint64_t Address, const void *Decoder, bool "
         "*DecodeComplete) \\\n"
      << "{ \\\n";
  Indentation += 2;
  OS.indent(Indentation) << "*DecodeComplete = true; \\\n";
  OS.indent(Indentation) << "InsnType tmp; \\\n";
  OS.indent(Indentation) << "switch (Idx) { \\\n";
  OS.indent(Indentation)
      << "default: /* llvm_unreachable(\"Invalid index!\"); */ \\\n";
  unsigned Index = 0;
  for (const auto &Decoder : Decoders) {
    OS.indent(Indentation) << "case " << Index++ << ": \\\n";
    OS << Decoder;
    OS.indent(Indentation + 2) << "return S; \\\n";
  }
  OS.indent(Indentation) << "} \\\n";
  Indentation -= 2;
  OS.indent(Indentation) << "}\n\n";
}

void PrinterCapstone::decoderEmitterEmitIncludes() const {
  OS << "#include \"../../MCInst.h\"\n"
     << "#include \"../../LEB128.h\"\n\n";
}

void PrinterCapstone::decoderEmitterEmitSourceFileHeader() const {
  emitDefaultSourceFileHeader(OS);
}

//-------------------------
// Backend: AsmWriter
//-------------------------

void PrinterCapstone::asmWriterEmitSourceFileHeader() const {
  emitDefaultSourceFileHeader(OS);
  OS << "#include <capstone/platform.h>\n"
     << "#include <assert.h>\n\n";
}

void PrinterCapstone::asmWriterEmitGetMnemonic(
    std::string const &TargetName, StringRef const &ClassName) const {
  OS << "/// getMnemonic - This method is automatically generated by "
        "tablegen\n"
        "/// from the instruction set description.\n"
        "static MnemonicBitsInfo getMnemonic(MCInst *MI, SStream *O) {\n";
}

void PrinterCapstone::asmWriterEmitAsmStrs(
    SequenceToOffsetTable<std::string> const &StrTable) const {
  StrTable.emitStringLiteralDef(OS, "  static const char AsmStrs[]");
}

void PrinterCapstone::asmWriterEmitMnemonicDecodeTable(
    unsigned const OpcodeInfoBits, unsigned BitsLeft,
    unsigned const &AsmStrBits,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
    std::vector<uint64_t> const &OpcodeInfo) const {
  // Emit the lookup tables in pieces to minimize wasted bytes.
  unsigned BytesNeeded = ((OpcodeInfoBits - BitsLeft) + 7) / 8;
  unsigned Table = 0, Shift = 0;
  SmallString<128> BitsString;
  raw_svector_ostream BitsOS(BitsString);
  // If the total bits is more than 32-bits we need to use a 64-bit type.
  BitsOS << "  uint" << ((BitsLeft < (OpcodeInfoBits - 32)) ? 64 : 32)
         << "_t Bits = 0;\n";
  while (BytesNeeded != 0) {
    // Figure out how big this table section needs to be, but no bigger than 4.
    unsigned TableSize = std::min(1 << Log2_32(BytesNeeded), 4);
    BytesNeeded -= TableSize;
    TableSize *= 8; // Convert to bits;
    uint64_t const Mask = (1ULL << TableSize) - 1;
    OS << "  static const uint" << TableSize << "_t OpInfo" << Table
       << "[] = {\n";
    for (unsigned I = 0, E = NumberedInstructions.size(); I != E; ++I) {
      OS << "    " << ((OpcodeInfo[I] >> Shift) & Mask) << "U,\t// "
         << NumberedInstructions[I]->TheDef->getName() << "\n";
    }
    OS << "  };\n\n";
    // Emit string to combine the individual table lookups.
    BitsOS << "  Bits |= ";
    // If the total bits is more than 32-bits we need to use a 64-bit type.
    if (BitsLeft < (OpcodeInfoBits - 32))
      BitsOS << "(uint64_t)";
    BitsOS << "OpInfo" << Table << "[MCInst_getOpcode(MI)] << " << Shift
           << ";\n";
    // Prepare the shift for the next iteration and increment the table count.
    Shift += TableSize;
    ++Table;
  }

  OS << "  // Emit the opcode for the instruction.\n";
  OS << BitsString;

  // Return mnemonic string and bits.
  OS << "  MnemonicBitsInfo MBI = {\n"
     << "#ifndef CAPSTONE_DIET\n"
     << "    AsmStrs+(Bits & " << (1 << AsmStrBits) - 1 << ")-1,\n"
     << "#else\n"
     << "    NULL,\n"
     << "#endif // CAPSTONE_DIET\n"
     << "    Bits\n"
     << "  };\n";
  OS << "  return MBI;\n";
  OS << "}\n\n";
}

void PrinterCapstone::asmWriterEmitPrintInstruction(
    std::string const &TargetName,
    std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
    unsigned &BitsLeft, unsigned &AsmStrBits, StringRef const &ClassName,
    bool PassSubtarget) const {
  const unsigned OpcodeInfoBits = 64;
  // This function has some huge switch statements that causing excessive
  // compile time in LLVM profile instrumenation build. This print function
  // usually is not frequently called in compilation. Here we disable the
  // profile instrumenation for this function.
  OS << "/// printInstruction - This method is automatically generated by "
        "tablegen\n"
        "/// from the instruction set description.\n"
        "static void "
     << "printInstruction(MCInst *MI, uint64_t Address, "
     << "SStream *O) {\n";

  // Emit the initial tab character.
  OS << "  SStream_concat0(O, \"\");\n";

  // Emit the starting string.
  OS << "  MnemonicBitsInfo MnemonicInfo = getMnemonic(MI, O);\n\n";
  OS << "  SStream_concat0(O, MnemonicInfo.first);\n\n";

  OS << "  uint" << ((BitsLeft < (OpcodeInfoBits - 32)) ? 64 : 32)
     << "_t Bits = MnemonicInfo.second;\n"
     << "  assert(Bits != 0 && \"Cannot print this instruction.\");\n";

  // Output the table driven operand information.
  BitsLeft = OpcodeInfoBits - AsmStrBits;
  for (unsigned I = 0, E = TableDrivenOperandPrinters.size(); I != E; ++I) {
    std::vector<std::string> &Commands = TableDrivenOperandPrinters[I];

    // Compute the number of bits we need to represent these cases, this is
    // ceil(log2(numentries)).
    unsigned const NumBits = Log2_32_Ceil(Commands.size());
    assert(NumBits <= BitsLeft && "consistency error");

    // Emit code to extract this field from Bits.
    OS << "\n  // Fragment " << I << " encoded into " << NumBits << " bits for "
       << Commands.size() << " unique commands.\n";

    if (Commands.size() == 2) {
      // Emit two possibilitys with if/else.
      OS << "  if ((Bits >> " << (OpcodeInfoBits - BitsLeft) << ") & "
         << ((1 << NumBits) - 1) << ") {\n"
         << translateToC(TargetName, Commands[1]) << "  } else {\n"
         << translateToC(TargetName, Commands[0]) << "  }\n\n";
    } else if (Commands.size() == 1) {
      // Emit a single possibility.
      OS << translateToC(TargetName, Commands[0]) << "\n\n";
    } else {
      OS << "  switch ((Bits >> " << (OpcodeInfoBits - BitsLeft) << ") & "
         << ((1 << NumBits) - 1) << ") {\n"
         << "  default: assert(0 && \"Invalid command number.\");\n";

      // Print out all the cases.
      for (unsigned J = 0, F = Commands.size(); J != F; ++J) {
        OS << "  case " << J << ":\n";
        OS << translateToC(TargetName, Commands[J]);
        OS << "    break;\n";
      }
      OS << "  }\n\n";
    }
    BitsLeft -= NumBits;
  }
}

void PrinterCapstone::asmWriterEmitOpCases(
    std::vector<std::pair<std::string, AsmWriterOperand>> &OpsToPrint,
    bool PassSubtarget) const {
  OS << "    case " << OpsToPrint.back().first << ":";
  AsmWriterOperand const TheOp = OpsToPrint.back().second;
  OpsToPrint.pop_back();

  // Check to see if any other operands are identical in this list, and if so,
  // emit a case label for them.
  for (unsigned I = OpsToPrint.size(); I != 0; --I)
    if (OpsToPrint[I - 1].second == TheOp) {
      OS << "\n    case " << OpsToPrint[I - 1].first << ":";
      OpsToPrint.erase(OpsToPrint.begin() + I - 1);
    }

  // Finally, emit the code.
  OS << "\n      " << translateToC(TargetName, TheOp.getCode(PassSubtarget));
  OS << "\n      break;\n";
}

void PrinterCapstone::asmWriterEmitInstrSwitch() const {
  OS << "  switch (MCInst_getOpcode(MI)) {\n";
  OS << "  default: assert(0 && \"Unexpected opcode.\");\n";
}

void PrinterCapstone::asmWriterEmitCompoundClosure(unsigned Indent,
                                                   bool Newline,
                                                   bool Semicolon) const {
  for (; Indent > 0; --Indent) {
    OS << " ";
  }
  OS << "}";
  if (Semicolon)
    OS << ";";
  if (Newline)
    OS << "\n";
}

void PrinterCapstone::asmWriterEmitInstruction(
    AsmWriterInst const &FirstInst,
    std::vector<AsmWriterInst> const &SimilarInsts, unsigned DifferingOperand,
    bool PassSubtarget) const {
  OS << "  case " << FirstInst.CGI->Namespace << "_"
     << FirstInst.CGI->TheDef->getName() << ":\n";
  for (const AsmWriterInst &AWI : SimilarInsts)
    OS << "  case " << AWI.CGI->Namespace << "_" << AWI.CGI->TheDef->getName()
       << ":\n";
  for (unsigned I = 0, E = FirstInst.Operands.size(); I != E; ++I) {
    if (I != DifferingOperand) {
      // If the operand is the same for all instructions, just print it.
      OS << "    "
         << translateToC(TargetName,
                         FirstInst.Operands[I].getCode(PassSubtarget));
    } else {
      // If this is the operand that varies between all of the instructions,
      // emit a switch for just this operand now.
      OS << "    switch (MCInst_getOpcode(MI)) {\n";
      OS << "    default: assert(0 && \"Unexpected opcode.\");\n";
      std::vector<std::pair<std::string, AsmWriterOperand>> OpsToPrint;
      OpsToPrint.push_back(
          std::make_pair(FirstInst.CGI->Namespace.str() + "_" +
                             FirstInst.CGI->TheDef->getName().str(),
                         FirstInst.Operands[I]));

      for (const AsmWriterInst &AWI : SimilarInsts) {
        OpsToPrint.push_back(std::make_pair(
            AWI.CGI->Namespace.str() + "_" + AWI.CGI->TheDef->getName().str(),
            AWI.Operands[I]));
      }
      std::reverse(OpsToPrint.begin(), OpsToPrint.end());
      while (!OpsToPrint.empty())
        asmWriterEmitOpCases(OpsToPrint, PassSubtarget);
      OS << "    }";
    }
    OS << "\n";
  }
  OS << "    break;\n";
}

void PrinterCapstone::asmWriterEmitGetRegNameAssert(
    std::string const &TargetName, StringRef const &ClassName, bool HasAltNames,
    unsigned RegSize) const {

  OS << "\n\n/// getRegisterName - This method is automatically generated by "
        "tblgen\n"
        "/// from the register set description.  This returns the assembler "
        "name\n"
        "/// for the specified register.\n"
        "static const char *";
  if (HasAltNames)
    OS << "\ngetRegisterName(unsigned RegNo, unsigned AltIdx) {\n";
  else
    OS << "getRegisterName(unsigned RegNo) {\n";
  OS << "#ifndef CAPSTONE_DIET\n";
  OS << "  assert(RegNo && RegNo < " << (RegSize + 1)
     << " && \"Invalid register number!\");\n"
     << "\n";
}

void PrinterCapstone::asmWriterEmitStringLiteralDef(
    SequenceToOffsetTable<std::string> const &StringTable,
    StringRef const &AltName) const {
  StringTable.emitStringLiteralDef(OS, Twine("  static const char AsmStrs") +
                                           AltName + "[]");
}

void PrinterCapstone::asmWriterEmitRegAsmOffsets(
    unsigned RegSizes, SmallVector<std::string, 4> const &AsmNames,
    SequenceToOffsetTable<std::string> const &StringTable,
    StringRef const &AltName) const {
  OS << "  static const " << getMinimalTypeForRange(StringTable.size() - 1, 32)
     << " RegAsmOffset" << AltName << "[] = {";
  for (unsigned I = 0, E = RegSizes; I != E; ++I) {
    if ((I % 14) == 0)
      OS << "\n    ";
    OS << StringTable.get(AsmNames[I]) << ", ";
  }
  OS << "\n  };\n"
     << "\n";
}

void PrinterCapstone::asmWriterEmitAltIdxSwitch(
    bool HasAltNames, std::vector<Record *> const &AltNameIndices,
    StringRef const &Namespace) const {
  if (HasAltNames) {
    OS << "  switch(AltIdx) {\n"
       << "  default: assert(0 && \"Invalid register alt name "
          "index!\");\n";
    for (const Record *R : AltNameIndices) {
      StringRef const AltName = R->getName();
      OS << "  case ";
      if (!Namespace.empty())
        OS << Namespace << "_";
      OS << AltName << ":\n";
      if (R->isValueUnset("FallbackRegAltNameIndex"))
        OS << "    assert(*(AsmStrs" << AltName << "+RegAsmOffset" << AltName
           << "[RegNo-1]) &&\n"
           << "           \"Invalid alt name index for register!\");\n";
      else {
        OS << "    if (!*(AsmStrs" << AltName << "+RegAsmOffset" << AltName
           << "[RegNo-1]))\n"
           << "      return getRegisterName(RegNo, ";
        if (!Namespace.empty())
          OS << Namespace << "_";
        OS << R->getValueAsDef("FallbackRegAltNameIndex")->getName() << ");\n";
      }
      OS << "    return AsmStrs" << AltName << "+RegAsmOffset" << AltName
         << "[RegNo-1];\n";
    }
    OS << "  }\n";
  } else {
    OS << "  assert (*(AsmStrs+RegAsmOffset[RegNo-1]) &&\n"
       << "          \"Invalid alt name index for register!\");\n"
       << "  return AsmStrs+RegAsmOffset[RegNo-1];\n";
  }
  OS << "#else\n"
     << "  return NULL;\n"
     << "#endif // CAPSTONE_DIET\n";
  OS << "}\n";
}

char const *PrinterCapstone::asmWriterGetPatCondKIgnore() const {
  return "AliasPatternCond_K_Ignore, 0";
}

char const *PrinterCapstone::asmWriterGetPatCondKRegClass() const {
  return "AliasPatternCond_K_RegClass, {0}_{1}RegClassID";
}

char const *PrinterCapstone::asmWriterGetPatCondKTiedReg() const {
  return "AliasPatternCond_K_TiedReg, {0}";
}

char const *PrinterCapstone::asmWriterGetPatCondKCustom() const {
  return "AliasPatternCond_K_Custom, {0}";
}

char const *PrinterCapstone::asmWriterGetPatCondKImm() const {
  return "AliasPatternCond_K_Imm, (uint32_t){0}";
}

char const *PrinterCapstone::asmWriterGetPatCondKNoReg() const {
  return "AliasPatternCond_K_Reg, {0}_NoRegister";
}

char const *PrinterCapstone::asmWriterGetPatCondKReg() const {
  return "AliasPatternCond_K_Reg, {0}_{1}";
}

char const *PrinterCapstone::asmWriterGetPatCondKFeature() const {
  return "AliasPatternCond_K_{0}{1}Feature, {2}_{3}";
}

char const *PrinterCapstone::asmWriterGetPatCondKEndOrFeature() const {
  return "AliasPatternCond_K_EndOrFeatures, 0";
}

char const *PrinterCapstone::asmWriterGetPatOpcStart() const {
  return "    // {0} - {1}\n";
}

char const *PrinterCapstone::asmWriterGetCondPatStart() const {
  return "    // {0} - {1}\n";
}

std::string PrinterCapstone::asmWriterGetCond(std::string const &Cond) const {
  return formatv("    {{{0}},\n", Cond);
}

char const *PrinterCapstone::asmWriterGetPatternFormat() const {
  return "    {{{0}, {1}, {2}, {3} },\n";
}

char const *PrinterCapstone::asmWriterGetOpcodeFormat() const {
  return "    {{{0}, {1}, {2} },\n";
}

void PrinterCapstone::asmWriterEmitPrintAliasInstrHeader(
    std::string const &TargetName, StringRef const &ClassName,
    bool PassSubtarget) const {
  OS << "static bool printAliasInstr(MCInst"
     << " *MI, uint64_t Address, "
     << "SStream *OS) {\n"
     << "#ifndef CAPSTONE_DIET\n";
}

void PrinterCapstone::asmWriterEmitPrintAliasInstrBodyRetFalse() const {
  OS << "  return false;\n";
  OS << "#endif // CAPSTONE_DIET\n";
  OS << "}\n\n";
}

void PrinterCapstone::asmWriterEmitDeclValid(std::string const &TargetName,
                                             StringRef const &ClassName) const {
  OS << "static bool " << TargetName << ClassName
     << "ValidateMCOperand(const MCOperand *MCOp,\n"
     << "                  unsigned PredicateIndex);\n";
}

void PrinterCapstone::asmWriterEmitPrintAliasInstrBody(
    raw_string_ostream &OpcodeO, raw_string_ostream &PatternO,
    raw_string_ostream &CondO,
    std::vector<std::pair<uint32_t, std::string>> const &AsmStrings,
    std::vector<const Record *> const &MCOpPredicates,
    std::string const &TargetName, StringRef const &ClassName,
    bool PassSubtarget) const {
  OS.indent(2) << "static const PatternsForOpcode OpToPatterns[] = {\n";
  OS << OpcodeO.str();
  OS.indent(2) << "{0},"; // Null terminated to ease binary search.
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const AliasPattern Patterns[] = {\n";
  OS << PatternO.str();
  OS.indent(2) << "{0},";
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const AliasPatternCond Conds[] = {\n";
  OS << CondO.str();
  OS.indent(2) << "{0},";
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const char AsmStrings[] =\n";
  for (const auto &P : AsmStrings) {
    OS.indent(4) << "/* " << P.first << " */ \"" << P.second << "\\0\"\n";
  }

  OS.indent(2) << ";\n\n";

  // Assert that the opcode table is sorted. Use a static local constructor to
  // ensure that the check only happens once on first run.
  OS << "#ifndef NDEBUG\n";
  OS.indent(2) << "//static struct SortCheck {\n";
  OS.indent(2) << "//  SortCheck(ArrayRef<PatternsForOpcode> OpToPatterns) {\n";
  OS.indent(2) << "//    assert(std::is_sorted(\n";
  OS.indent(2)
      << "//               OpToPatterns.begin(), OpToPatterns.end(),\n";
  OS.indent(2) << "//               [](const PatternsForOpcode &L, const "
                  "//PatternsForOpcode &R) {\n";
  OS.indent(2) << "//                 return L.Opcode < R.Opcode;\n";
  OS.indent(2) << "//               }) &&\n";
  OS.indent(2)
      << "//           \"tablegen failed to sort opcode patterns\");\n";
  OS.indent(2) << "//  }\n";
  OS.indent(2) << "//} sortCheckVar(OpToPatterns);\n";
  OS << "#endif\n\n";

  OS.indent(2) << "AliasMatchingData M = {\n";
  OS.indent(2) << "  OpToPatterns,\n";
  OS.indent(2) << "  Patterns,\n";
  OS.indent(2) << "  Conds,\n";
  OS.indent(2) << "  AsmStrings,\n";
  if (!MCOpPredicates.empty())
    OS.indent(2) << "  " << TargetName << ClassName << "ValidateMCOperand,\n";
  else
    OS.indent(2) << "  NULL,\n";
  OS.indent(2) << "};\n";

  OS.indent(2) << "const char *AsmString = matchAliasPatterns(MI, &M);\n";
  OS.indent(2) << "if (!AsmString) return false;\n\n";

  // Code that prints the alias, replacing the operands with the ones from the
  // MCInst.
  OS << "  unsigned I = 0;\n";
  OS << "  while (AsmString[I] != ' ' && AsmString[I] != '\\t' &&\n";
  OS << "         AsmString[I] != '$' && AsmString[I] != '\\0')\n";
  OS << "    ++I;\n"
     << "  char *substr = malloc(I+1);\n"
     << "  memcpy(substr, AsmString, I);\n"
     << "  substr[I] = '\\0';\n"
     << "  SStream_concat0(OS, substr);\n"
     << "  free(substr);\n";

  OS << "  if (AsmString[I] != '\\0') {\n";
  OS << "    if (AsmString[I] == ' ' || AsmString[I] == '\\t') {\n";
  OS << "      SStream_concat1(OS, ' ');\n";
  OS << "      ++I;\n";
  OS << "    }\n";
  OS << "    do {\n";
  OS << "      if (AsmString[I] == '$') {\n";
  OS << "        ++I;\n";
  OS << "        if (AsmString[I] == (char)0xff) {\n";
  OS << "          ++I;\n";
  OS << "          int OpIdx = AsmString[I++] - 1;\n";
  OS << "          int PrintMethodIdx = AsmString[I++] - 1;\n";
  OS << "          printCustomAliasOperand(MI, Address, OpIdx, "
        "PrintMethodIdx, ";
  OS << "OS);\n";
  OS << "        } else\n";
  OS << "          printOperand(MI, ((unsigned)AsmString[I++]) - 1, ";
  OS << "OS);\n";
  OS << "      } else {\n";
  OS << "        SStream_concat1(OS, AsmString[I++]);\n";
  OS << "      }\n";
  OS << "    } while (AsmString[I] != '\\0');\n";
  OS << "  }\n\n";

  OS << "  return true;\n";
  OS << "#else\n"
     << "  return false;\n";
  OS << "#endif // CAPSTONE_DIET\n";
  OS << "}\n\n";
}

void PrinterCapstone::asmWriterEmitPrintAliasOp(
    std::string const &TargetName, StringRef const &ClassName,
    std::vector<std::pair<std::string, bool>> const &PrintMethods,
    bool PassSubtarget) const {
  OS << "static void printCustomAliasOperand(\n"
     << "         MCInst *MI, uint64_t Address, unsigned OpIdx,\n"
     << "         unsigned PrintMethodIdx,\n"
     << "         SStream *OS) {\n"
     << "#ifndef CAPSTONE_DIET\n";
  if (PrintMethods.empty())
    OS << "  llvm_unreachable(\"Unknown PrintMethod kind\");\n";
  else {
    OS << "  switch (PrintMethodIdx) {\n"
       << "  default:\n"
       << "    assert(0 && \"Unknown PrintMethod kind\");\n"
       << "    break;\n";

    for (unsigned I = 0; I < PrintMethods.size(); ++I) {
      OS << "  case " << I << ":\n";
      std::string PrintMethod =
          PrinterCapstone::translateToC(TargetName, PrintMethods[I].first);
      OS << "    " << PrintMethod << "(MI, "
         << (PrintMethods[I].second ? "Address, " : "") << "OpIdx, "
         << "OS);\n"
         << "    break;\n";
    }
    OS << "  }\n";
  }
  OS << "#endif // CAPSTONE_DIET\n";
  OS << "}\n\n";
}

void PrinterCapstone::asmWriterEmitPrintMC(
    std::string const &TargetName, StringRef const &ClassName,
    std::vector<const Record *> const &MCOpPredicates) const {
  if (!MCOpPredicates.empty()) {
    OS << "static bool " << TargetName << ClassName
       << "ValidateMCOperand(const MCOperand *MCOp,\n"
       << "                  unsigned PredicateIndex) {\n"
       << "  switch (PredicateIndex) {\n"
       << "  default:\n"
       << "    assert(0 && \"Unknown MCOperandPredicate kind\");\n"
       << "    break;\n";

    for (unsigned I = 0; I < MCOpPredicates.size(); ++I) {
      StringRef const MCOpPred =
          MCOpPredicates[I]->getValueAsString("MCOperandPredicate");
      OS << "  case " << I + 1 << ": {\n";
      std::string PrintMethod =
          PrinterCapstone::translateToC(TargetName, MCOpPred.data());
      OS << PrintMethod << "\n"
         << "    }\n";
    }
    OS << "  }\n"
       << "}\n\n";
  }
}

//-------------------------
// Backend: Subtarget
//-------------------------

void PrinterCapstone::subtargetEmitSourceFileHeader() const {
  emitDefaultSourceFileHeader(OS);
}

void PrinterCapstone::subtargetEmitFeatureEnum(
    DenseMap<Record *, unsigned> &FeatureMap,
    std::vector<Record *> const &DefList, unsigned N) const {
  StringRef TN = StringRef(TargetName);
  // Open enumeration.
  OS << "enum {\n";

  // For each record
  for (unsigned I = 0; I < N; ++I) {
    // Next record
    Record *Def = DefList[I];

    // Get and emit name
    OS << "  " << TN << "_" << Def->getName() << " = " << I << ",\n";

    // Save the index for this feature.
    FeatureMap[Def] = I;
  }

  OS << "  " << TN << "_"
     << "NumSubtargetFeatures = " << N << "\n";

  // Close enumeration and namespace
  OS << "};\n";
}

void PrinterCapstone::subtargetEmitGetSTIMacro(
    StringRef const &Value, StringRef const &Attribute) const {}

void PrinterCapstone::subtargetEmitHwModes(CodeGenHwModes const &CGH,
                                           std::string const &ClassName) const {
}

void PrinterCapstone::subtargetEmitFeatureKVHeader(
    std::string const &Target) const {
  // Begin feature table
}

void PrinterCapstone::subtargetEmitFeatureKVPartI(
    std::string const &Target, StringRef const &CommandLineName,
    StringRef const &Name, StringRef const &Desc) const {}

void PrinterCapstone::subtargetEmitFeatureKVPartII() const {}

void PrinterCapstone::subtargetEmitPrintFeatureMask(
    std::array<uint64_t, MAX_SUBTARGET_WORDS> const &Mask) const {}

void PrinterCapstone::subtargetEmitFeatureKVEnd() const {}

void PrinterCapstone::subtargetEmitCPUKVHeader(
    std::string const &Target) const {}

void PrinterCapstone::subtargetEmitCPUKVEnd() const {}

void PrinterCapstone::subtargetEmitCPUKVPartI(StringRef const &Name) const {}

void PrinterCapstone::subtargetEmitCPUKVPartII() const {}

void PrinterCapstone::subtargetEmitCPUKVPartIII(
    std::string const &ProcModelName) const {}

void PrinterCapstone::subtargetEmitDBGMacrosBegin() const {}

void PrinterCapstone::subtargetEmitDBGMacrosEnd() const {}

void PrinterCapstone::subtargetEmitFunctionalItinaryUnits(
    CodeGenSchedModels const &SchedModels) const {}

std::string const PrinterCapstone::subtargetGetBeginStageTable(
    std::string const &TargetName) const {
  return "";
}

std::string const PrinterCapstone::subtargetGetBeginOperandCycleTable(
    std::string const &TargetName) const {
  return "";
}

std::string const PrinterCapstone::subtargetGetBeginBypassTable(
    std::string const &TargetName) const {
  return "";
}

std::string const PrinterCapstone::subtargetGetEndStageTable() const {
  return "";
}

std::string const PrinterCapstone::subtargetGetEndOperandCycleTable() const {
  return "";
}

std::string const PrinterCapstone::subtargetGetEndBypassTable() const {
  return "";
}

// subtargetFormItineraryStageString - Compose a string containing the stage
// data initialization for the specified itinerary.  N is the number
// of stages.
void PrinterCapstone::subtargetFormItineraryStageString(
    std::string const &Name, Record *ItinData, std::string &ItinString,
    unsigned &NStages) const {}

// FormItineraryOperandCycleString - Compose a string containing the
// operand cycle initialization for the specified itinerary.  N is the
// number of operands that has cycles specified.
void PrinterCapstone::subtargetFormItineraryOperandCycleString(
    Record *ItinData, std::string &ItinString, unsigned &NOperandCycles) const {
}

void PrinterCapstone::subtargetFormItineraryBypassString(
    const std::string &Name, Record *ItinData, std::string &ItinString,
    unsigned NOperandCycles) const {}

std::string
PrinterCapstone::subtargetGetStageEntryPartI(std::string const &ItinStageString,
                                             unsigned StageCount) const {
  return "";
}
std::string
PrinterCapstone::subtargetGetStageEntryPartII(unsigned StageCount,
                                              unsigned NStages) const {
  return "";
}
std::string PrinterCapstone::subtargetGetStageEntryPartIII() const {
  return "";
}

std::string PrinterCapstone::subtargetGetOperandCycleEntryPartI(
    std::string const &ItinOperandCycleString) const {
  return "";
}

std::string PrinterCapstone::subtargetGetOperandCycleEntryPartII(
    unsigned OperandCycleCount, unsigned NOperandCycles) const {
  return "";
}

std::string PrinterCapstone::subtargetGetOperandCycleEntryPartIII(
    std::string const &OperandIdxComment) const {
  return "";
}

std::string PrinterCapstone::subtargetGetOperandCycleEntryPartIV(
    std::string const &ItinBypassString,
    std::string const &OperandIdxComment) const {
  return "";
}

void PrinterCapstone::subtargetEmitProcessorItineraryTable(
    std::string const &ItinsDefName, std::vector<InstrItinerary> &ItinList,
    CodeGenSchedModels const &SchedModels) const {}

void PrinterCapstone::subtargetEmitPreOperandTableComment() const {}

// Emit SchedClass tables for all processors and associated global tables.
void PrinterCapstone::subtargetEmitSchedClassTables(
    SchedClassTablesT &SchedTables, std::string const &TargetName,
    CodeGenSchedModels const &SchedModels) const {}

unsigned PrinterCapstone::subtargetEmitRegisterFileTables(
    CodeGenProcModel const &ProcModel) const {
  return 0;
}

void PrinterCapstone::subtargetEmitMCExtraProcInfoTableHeader(
    std::string const &ProcModelName) const {}

void PrinterCapstone::subtargetEmitMCExtraProcInfoTableEnd() const {}

void PrinterCapstone::subtargetEmitReorderBufferSize(
    int64_t ReorderBufferSize) const {}

void PrinterCapstone::subtargetEmitMaxRetirePerCycle(
    int64_t MaxRetirePerCycle) const {}

void PrinterCapstone::subtargetEmitRegisterFileInfo(
    CodeGenProcModel const &ProcModel, unsigned NumRegisterFiles,
    unsigned NumCostEntries) const {}

void PrinterCapstone::subtargetEmitResourceDescriptorLoadQueue(
    unsigned QueueID) const {}

void PrinterCapstone::subtargetEmitResourceDescriptorStoreQueue(
    unsigned QueueID) const {}

void PrinterCapstone::subtargetEmitProcessorResourceSubUnits(
    const CodeGenProcModel &ProcModel,
    CodeGenSchedModels const &SchedModels) const {}

void PrinterCapstone::subtargetEmitMCProcResourceDescHeader(
    std::string const &ProcModelName) const {}

void PrinterCapstone::subtargetEmitMCProcResourceDescEnd() const {}

void PrinterCapstone::subtargetEmitMCProcResourceDesc(
    Record const *PRDef, Record const *SuperDef,
    std::string const &ProcModelName, unsigned SubUnitsOffset,
    unsigned SuperIdx, unsigned NumUnits, int BufferSize, unsigned I,
    unsigned const SubUnitsBeginOffset) const {}

// Emit either the value defined in the TableGen Record, or the default
// value defined in the C++ header. The Record is null if the processor does not
// define a model.
void PrinterCapstone::subtargetEmitProcessorProp(Record const *R,
                                                 StringRef const Name,
                                                 char Separator) const {}

void PrinterCapstone::subtargetEmitProcModelHeader(
    std::string const &ModelName) const {}

void PrinterCapstone::subtargetEmitProcModel(
    CodeGenProcModel const &PM, CodeGenSchedModels const &SchedModels) const {}

void PrinterCapstone::subtargetEmitResolveVariantSchedClassImplHdr() const {}

void PrinterCapstone::subtargetEmitResolveVariantSchedClassImplEnd() const {}

void PrinterCapstone::subtargetEmitSchedClassSwitch() const {}

void PrinterCapstone::subtargetEmitSchedClassCase(
    unsigned VC, std::string const &SCName) const {}

void PrinterCapstone::subtargetEmitSchedClassProcGuard(
    unsigned Pi, bool OnlyExpandMCInstPredicates,
    std::string const &ModelName) const {}

// Indent <= -1 (default = -1) means previous PE indent level.
void PrinterCapstone::subtargetEmitPredicates(
    CodeGenSchedTransition const &T, CodeGenSchedClass const &SC,
    bool (*IsTruePredicate)(Record const *Rec), int Indent) const {}

void PrinterCapstone::subtargetEmitProcTransitionEnd() const {}

void PrinterCapstone::subtargetEmitSchedClassCaseEnd(
    CodeGenSchedClass const &SC) const {}

void PrinterCapstone::subtargetEmitSchedClassSwitchEnd() const {}

// Used by method `SubtargetEmitter::emitSchedModelHelpersImpl()` to generate
// epilogue code for the auto-generated helper.
void PrinterCapstone::subtargetEmitSchedModelHelperEpilogue(
    bool ShouldReturnZero) const {}

void PrinterCapstone::subtargetEmitGenMCSubtargetInfoClass(
    std::string const &TargetName, bool OverrideGetHwMode) const {}

void PrinterCapstone::subtargetEmitMCSubtargetInfoImpl(
    std::string const &TargetName, unsigned NumFeatures, unsigned NumProcs,
    bool SchedModelHasItin) const {}

void PrinterCapstone::subtargetEmitIncludeSTIDesc() const {}

void PrinterCapstone::subtargetEmitDFAPacketizerClass(
    std::string const &TargetName, std::string const &ClassName,
    bool OverrideGetHwMode) const {}

void PrinterCapstone::subtargetEmitDFASubtargetInfoImpl(
    std::string const &TargetName, std::string const &ClassName,
    unsigned NumFeatures, unsigned NumProcs, bool SchedModelHasItin) const {}

void PrinterCapstone::subtargetEmitDFAPacketizerClassEnd() const {}

void PrinterCapstone::subtargetEmitSTICtor() const {}

void PrinterCapstone::subtargetEmitExternKVArrays(
    std::string const &TargetName, bool SchedModelsHasItin) const {}

void PrinterCapstone::subtargetEmitClassDefs(std::string const &TargetName,
                                             std::string const &ClassName,
                                             unsigned NumFeatures,
                                             unsigned NumProcs,
                                             bool SchedModelsHasItin) const {}

void PrinterCapstone::subtargetEmitResolveSchedClassHdr(
    std::string const &ClassName) const {}

void PrinterCapstone::subtargetEmitResolveSchedClassEnd(
    std::string const &ClassName) const {}

void PrinterCapstone::subtargetEmitResolveVariantSchedClass(
    std::string const &TargetName, std::string const &ClassName) const {}

void PrinterCapstone::subtargetEmitPredicateProlog(
    const RecordKeeper &Records) const {}

void PrinterCapstone::subtargetEmitParseFeaturesFunction(
    std::string const &TargetName,
    std::vector<Record *> const &Features) const {}

void PrinterCapstone::subtargetEmitExpandedSTIPreds(
    StringRef const &TargetName, std::string const &ClassName,
    CodeGenSchedModels const &SchedModels) {}

void PrinterCapstone::subtargetPrepareSchedClassPreds(
    StringRef const &TargetName, bool OnlyExpandMCInstPredicates) {}

void PrinterCapstone::subtargetEmitExpandedSTIPredsMCAnaDecl(
    StringRef const &TargetName, CodeGenSchedModels const &SchedModels) {}

void PrinterCapstone::subtargetEmitExpandedSTIPredsMCAnaDefs(
    StringRef const &TargetName, std::string const &ClassPrefix,
    CodeGenSchedModels const &SchedModels) const {}

void PrinterCapstone::subtargetEmitExpandedSTIPredsHeader(
    StringRef const &TargetName, CodeGenSchedModels const &SchedModels) {}

void PrinterCapstone::subtargetEmitStageAndSycleTables(
    std::string const &StageTable, std::string const &OperandCycleTable,
    std::string const &BypassTable) const {}

//---------------------------
// Backend: InstrInfoEmitter
//---------------------------

void PrinterCapstone::instrInfoEmitSourceFileHeader() const {
  emitDefaultSourceFileHeader(OS);
}

void PrinterCapstone::instrInfoSetOperandInfoStr(
    std::string &Res, Record const *OpR, CGIOperandList::OperandInfo const &Op,
    CGIOperandList::ConstraintInfo const &Constraint) const {
  if (OpR->isSubClassOf("RegisterOperand"))
    OpR = OpR->getValueAsDef("RegClass");
  if (OpR->isSubClassOf("RegisterClass"))
    Res += OpR->getValueAsString("Namespace").str() + "_" +
           OpR->getName().str() + "RegClassID, ";
  else if (OpR->isSubClassOf("PointerLikeRegClass"))
    Res += utostr(OpR->getValueAsInt("RegClassKind")) + ", ";
  else
    // -1 means the operand does not have a fixed register class.
    Res += "-1, ";

  // Fill in applicable flags.
  Res += "0";

  // Ptr value whose register class is resolved via callback.
  if (OpR->isSubClassOf("PointerLikeRegClass"))
    Res += "|(1<<MCOI_LookupPtrRegClass)";

  // Predicate operands.  Check to see if the original unexpanded operand
  // was of type PredicateOp.
  if (Op.Rec->isSubClassOf("PredicateOp"))
    Res += "|(1<<MCOI_Predicate)";

  // Optional def operands.  Check to see if the original unexpanded operand
  // was of type OptionalDefOperand.
  if (Op.Rec->isSubClassOf("OptionalDefOperand"))
    Res += "|(1<<MCOI_OptionalDef)";

  // Branch target operands.  Check to see if the original unexpanded
  // operand was of type BranchTargetOperand.
  if (Op.Rec->isSubClassOf("BranchTargetOperand"))
    Res += "|(1<<MCOI_BranchTarget)";

  // Fill in operand type.
  Res += ", ";
  assert(!Op.OperandType.empty() && "Invalid operand type.");
  std::string OpTypeCpy = Op.OperandType;
  if (OpTypeCpy.find("VPRED") != std::string::npos ||
      OpTypeCpy.find("IMPLICIT_IMM") != std::string::npos)
    OpTypeCpy = Regex("OPERAND").sub("OP", OpTypeCpy);
  Res += OpTypeCpy.replace(OpTypeCpy.find("::"), 2, "_");

  // Fill in constraint info.
  Res += ", ";

  if (Constraint.isNone())
    Res += "0";
  else if (Constraint.isEarlyClobber())
    Res += "CONSTRAINT_MCOI_EARLY_CLOBBER";
  else {
    assert(Constraint.isTied());
    Res +=
        "CONSTRAINT_MCOI_TIED_TO(" + utostr(Constraint.getTiedOperand()) + ")";
  }
}

void PrinterCapstone::instrInfoPrintDefList(
    const std::vector<Record *> &Uses, unsigned Num,
    std::string (*GetQualifiedName)(Record const *R)) const {}

void PrinterCapstone::instrInfoEmitOperandInfoTable(
    std::vector<std::string> const &OperandInfo, unsigned N) const {
  OS << "static const MCOperandInfo OperandInfo" << N << "[] = { ";
  for (const std::string &Info : OperandInfo)
    OS << "{ " << Info << " }, ";
  OS << "};\n";
}

void PrinterCapstone::instrInfoEmitMCInstrDescHdr(
    std::string TargetName) const {
  OS << "\nstatic const MCInstrDesc " << TargetName << "Insts[] = {\n";
}

void PrinterCapstone::instrInfoEmitMCInstrDescEnd() const { OS << "};\n\n"; }

void PrinterCapstone::instrInfoEmitRecord(CodeGenSchedModels const &SchedModels,
                                          CodeGenInstruction const &Inst,
                                          unsigned Num, int MinOperands) const {
  OS << "  { " << MinOperands << ", ";
}

void PrinterCapstone::instrInfoEmitTargetIndepFlags(
    CodeGenInstruction const &Inst, bool GetAllowRegisterRenaming) const {}

void PrinterCapstone::instrInfoEmitTSFFlags(uint64_t Value) const {}

void PrinterCapstone::instrInfoEmitUseDefsLists(
    std::map<std::vector<Record *>, unsigned> &EmittedLists,
    std::vector<Record *> const &ImplicitOps) const {}

void PrinterCapstone::instrInfoEmitOperandInfo(
    std::vector<std::string> const &OperandInfo,
    OperandInfoMapTy const &OpInfo) const {
  if (OperandInfo.empty())
    OS << "0";
  else
    OS << "OperandInfo" << OpInfo.find(OperandInfo)->second;
}

void PrinterCapstone::instrInfoEmitRecordEnd(
    unsigned InstNum, std::string const &InstName) const {
  OS << " },  // Inst #" << InstNum << " = " << InstName << "\n";
}

void PrinterCapstone::instrInfoEmitStringLiteralDef(
    std::string const &TargetName,
    SequenceToOffsetTable<std::string> InstrNames) const {}

void PrinterCapstone::instrInfoEmitInstrNameIndices(
    std::string const &TargetName,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
    SequenceToOffsetTable<std::string> const &InstrNames) const {}

void PrinterCapstone::instrInfoEmitInstrDeprFeatures(
    std::string const &TargetName, std::string const &TargetNamespace,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
    SequenceToOffsetTable<std::string> const &InstrNames) const {}

void PrinterCapstone::instrInfoEmitInstrComplexDeprInfos(
    std::string const &TargetName,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {}

void PrinterCapstone::instrInfoEmitMCInstrInfoInitRoutine(
    std::string const &TargetName, unsigned NumberedInstrSize,
    bool HasDeprecationFeatures, bool HasComplexDeprecationInfos) const {}

void PrinterCapstone::instrInfoEmitClassStruct(
    std::string const &ClassName) const {}

void PrinterCapstone::instrInfoEmitTIIHelperMethod(
    StringRef const &TargetName, Record const *Rec,
    bool ExpandDefinition) const {}

void PrinterCapstone::instrInfoEmitExternArrays(
    std::string const &TargetName, bool HasDeprecationFeatures,
    bool HasComplexDeprecationInfos) const {}

void PrinterCapstone::instrInfoEmitMCInstrInfoInit(
    std::string const &TargetName, std::string const &ClassName,
    unsigned NumberedInstrSize, bool HasDeprecationFeatures,
    bool HasComplexDeprecationInfos) const {}

void PrinterCapstone::instrInfoEmitOperandEnum(
    std::map<std::string, unsigned> const &Operands) const {}

void PrinterCapstone::instrInfoEmitGetNamedOperandIdx(
    std::map<std::string, unsigned> const &Operands,
    OpNameMapTy const &OperandMap) const {}

void PrinterCapstone::instrInfoEmitOpTypeEnumPartI() const {}

void PrinterCapstone::instrInfoEmitOpTypeEnumPartII(StringRef const &OpName,
                                                    unsigned EnumVal) const {}

void PrinterCapstone::instrInfoEmitOpTypeEnumPartIII() const {}

void PrinterCapstone::instrInfoEmitOpTypeOffsetTable(
    std::vector<int> OperandOffsets, unsigned OpRecSize,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {}

void PrinterCapstone::instrInfoEmitOpcodeOpTypesTable(
    unsigned EnumVal, std::vector<Record *> const &OperandRecords,
    std::vector<int> OperandOffsets,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {}

void PrinterCapstone::instrInfoEmitGetOpTypeHdr() const {}

void PrinterCapstone::instrInfoEmitGetOpTypeReturn() const {}

void PrinterCapstone::instrInfoEmitGetOpTypeUnreachable() const {}

void PrinterCapstone::instrInfoEmitGetOpTypeEnd() const {}

void PrinterCapstone::instrInfoEmitGetMemOpSizeHdr() const {}

void PrinterCapstone::instrInfoEmitGetOpMemSizeTbl(
    std::map<int, std::vector<StringRef>> const &SizeToOperandName) const {}

std::string
PrinterCapstone::instrInfoGetInstMapEntry(StringRef const &Namespace,
                                          StringRef const &InstrName) const {
  return Namespace.str() + "_" + InstrName.str();
}

void PrinterCapstone::instrInfoEmitGetLogicalOpSizeHdr() const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpSizeTable(
    size_t LogicalOpListSize,
    std::vector<const std::vector<unsigned> *> const &LogicalOpSizeList) const {
}

void PrinterCapstone::instrInfoEmitGetLogicalOpSizeSwitch(
    std::map<unsigned, std::vector<std::string>> InstMap) const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpSizeReturn() const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpSizeEnd() const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpIdx() const {}

std::string
PrinterCapstone::instrInfoGetOpTypeListEntry(StringRef const &Namespace,
                                             StringRef const &OpName) const {
  return Namespace.str() + "_OpTypes_" + OpName.str();
}

void PrinterCapstone::instrInfoEmitGetLogicalOpTypeHdr() const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpTypeTable(
    size_t OpTypeListSize,
    std::vector<const std::vector<std::string> *> const &LogicalOpTypeList)
    const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpTypeSwitch(
    std::map<unsigned, std::vector<std::string>> InstMap) const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpTypeReturn() const {}

void PrinterCapstone::instrInfoEmitGetLogicalOpTypeEnd() const {}

void PrinterCapstone::instrInfoEmitDeclareMCInstFeatureClasses() const {}

void PrinterCapstone::instrInfoEmitPredFcnDecl(
    RecVec const &TIIPredicates) const {}

void PrinterCapstone::instrInfoEmitPredFcnImpl(StringRef const &TargetName,
                                               RecVec const &TIIPredicates) {}

void PrinterCapstone::instrInfoEmitInstrPredVerifierIncludes() const {}

void PrinterCapstone::instrInfoEmitSubtargetFeatureBitEnumeration(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {}

void PrinterCapstone::instrInfoEmitEmitSTFNameTable(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {}

void PrinterCapstone::instrInfoEmitFeatureBitsEnum(
    std::vector<std::vector<Record *>> const &FeatureBitsets) const {}

void PrinterCapstone::instrInfoEmitFeatureBitsArray(
    std::vector<std::vector<Record *>> const &FeatureBitsets,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        &SubtargetFeatures) const {}

void PrinterCapstone::instrInfoEmitPredVerifier(
    std::vector<std::vector<Record *>> const &FeatureBitsets,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        &SubtargetFeatures,
    CodeGenTarget const &Target) const {}

void PrinterCapstone::instrInfoEmitEnums(
    CodeGenTarget const &Target, StringRef const &Namespace,
    CodeGenSchedModels const &SchedModels) const {
  emitIncludeToggle("GET_INSTRINFO_ENUM", true);

  unsigned Num = 0;
  OS << "  enum {\n";
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue())
    OS << "    " << Namespace << "_" << Inst->TheDef->getName()
       << "\t= " << Num++ << ",\n";
  OS << "    INSTRUCTION_LIST_END = " << Num << "\n";
  OS << "  };\n\n";
  emitIncludeToggle("GET_INSTRINFO_ENUM", false);
}

void PrinterCapstone::instrInfoEmitTIIPredicates(StringRef const &TargetName,
                                                 RecVec const &TIIPredicates,
                                                 bool ExpandDefinition) {}

void PrinterCapstone::instrInfoEmitComputeAssemblerAvailableFeatures(
    StringRef const &TargetName,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {}

//--------------------------
// Backend: AsmMatcher
//--------------------------

namespace {

std::string getImplicitUses(StringRef const &TargetName,
                            CodeGenInstruction const *Inst) {
  std::string Flags = "{ ";
  for (Record const *U : Inst->ImplicitUses) {
    assert(U->isSubClassOf("Register"));
    Flags += TargetName.str() + "_REG_" + U->getName().str() + ", ";
  }
  Flags += "0 }";
  return Flags;
}
std::string getImplicitDefs(StringRef const &TargetName,
                            CodeGenInstruction const *Inst) {
  std::string Flags = "{ ";
  for (Record const *U : Inst->ImplicitDefs) {
    assert(U->isSubClassOf("Register"));
    Flags += TargetName.str() + "_REG_" + U->getName().str() + ", ";
  }
  Flags += "0 }";
  return Flags;
}

static inline std::string normalizedMnemonic(StringRef const &Mn,
                                             const bool Upper = true) {
  auto Mnemonic = Upper ? Mn.upper() : Mn.str();
  std::replace(Mnemonic.begin(), Mnemonic.end(), '.', '_');
  std::replace(Mnemonic.begin(), Mnemonic.end(), '+', 'p');
  std::replace(Mnemonic.begin(), Mnemonic.end(), '-', 'm');
  std::replace(Mnemonic.begin(), Mnemonic.end(), '/', 's');

  Mnemonic = StringRef(Regex("[{}]").sub("", Mnemonic));
  return Mnemonic;
}

static inline std::string
getNormalMnemonic(std::unique_ptr<MatchableInfo> const &MI,
                  const bool Upper = true) {
  return normalizedMnemonic(MI->Mnemonic);
}

std::string getReqFeatures(StringRef const &TargetName, AsmMatcherInfo &AMI,
                           std::unique_ptr<MatchableInfo> const &MI, bool UseMI,
                           CodeGenInstruction const *CGI) {
  std::string Flags = "{ ";
  std::string Mn = getNormalMnemonic(MI);
  // The debug if
  if ((CGI->isBranch || CGI->isReturn) && !CGI->isCall) {
    Flags += TargetName.str() + "_GRP_JUMP, ";
  }
  if (CGI->isReturn) {
    Flags += TargetName.str() + "_GRP_RET, ";
  }
  if (CGI->isCall) {
    Flags += TargetName.str() + "_GRP_CALL, ";
  }
  for (const auto &OpInfo : CGI->Operands.OperandList) {
    if (OpInfo.OperandType == "MCOI::OPERAND_PCREL" &&
        (CGI->isBranch || CGI->isReturn || CGI->isIndirectBranch || CGI->isCall)) {
      Flags += TargetName.str() + "_GRP_BRANCH_RELATIVE, ";
    }
  }
  // The group flags <ARCH>_GRP_PRIVILEGE and <ARCH>_GRP_INT (interrupt) are not
  // handled here. LLVM does not provide this info.
  for (Record *Predicate : CGI->TheDef->getValueAsListOfDefs("Predicates")) {
    if (const SubtargetFeatureInfo *Feature =
            AMI.getSubtargetFeature(Predicate))
      Flags += TargetName.str() + "_FEATURE_" +
               Feature->TheDef->getName().str() + ", ";
  }
  Flags += "0 }";
  return Flags;
}

std::string getLLVMInstEnumName(StringRef const &TargetName,
                                CodeGenInstruction const *CGI) {
  std::string UniqueName = CGI->TheDef->getName().str();
  std::string Enum = TargetName.str() + "_" + UniqueName;
  std::replace(Enum.begin(), Enum.end(), '/', 's');
  return Enum;
}

std::string getArchSupplInfoPPC(StringRef const &TargetName,
                                CodeGenInstruction const *CGI,
                                raw_string_ostream &PPCFormatEnum) {
  static std::set<std::string> Formats;
  // Get instruction format
  ArrayRef<std::pair<Record *, SMRange>> SCs = CGI->TheDef->getSuperClasses();
  if (SCs.empty()) {
    llvm_unreachable("A CGI without superclass should not exist.");
  }

  // Get base instruction format class "I"
  const Record *PrevSC = nullptr;
  // Superclasses are in post-order. So we go through them backwards.
  // The class before the "I" class is the format class.
  for (int I = SCs.size() - 1; I >= 0; --I) {
    const Record *SC = SCs[I].first;
    if (SC->getName() == "I") {
      if (!PrevSC)
        llvm_unreachable("I class has no predecessor.");
      std::string Format = "PPC_INSN_FORM_" + PrevSC->getName().upper();
      if (Formats.find(Format) == Formats.end()) {
        PPCFormatEnum << Format + ",\n";
      }
      Formats.emplace(Format);
      return "{{ " + Format + " }}";
    }
    PrevSC = SC;
  }
  // Pseudo instructions
  return "{{ 0 }}";
}

std::string getArchSupplInfo(StringRef const &TargetName,
                             CodeGenInstruction const *CGI,
                             raw_string_ostream &PPCFormatEnum) {
  if (TargetName == "PPC")
    return getArchSupplInfoPPC(TargetName, CGI, PPCFormatEnum);
  return "{{ 0 }}";
}

Record *argInitOpToRecord(Init *ArgInit) {
  DagInit *SubArgDag = dyn_cast<DagInit>(ArgInit);
  if (SubArgDag)
    ArgInit = SubArgDag->getOperator();
  DefInit *Arg = dyn_cast<DefInit>(ArgInit);
  Record *Rec = Arg->getDef();
  return Rec;
}

std::string getPrimaryCSOperandType(Record const *OpRec) {
  std::string OperandType;
  if (OpRec->isSubClassOf("PredicateOperand"))
    return "CS_OP_PRED";

  if (OpRec->isSubClassOf("RegisterClass") ||
      OpRec->isSubClassOf("PointerLikeRegClass"))
    OperandType = "OPERAND_REGISTER";
  else if (OpRec->isSubClassOf("Operand") ||
           OpRec->isSubClassOf("RegisterOperand"))
    OperandType = std::string(OpRec->getValueAsString("OperandType"));
  else
    return "CS_OP_INVALID";

  if (OperandType == "OPERAND_UNKNOWN") {
    if (OpRec->getValueAsDef("Type")->getValueAsInt("Size") == 0)
      // Pseudo type
      return "CS_OP_INVALID";
    OperandType = "OPERAND_IMMEDIATE";
  }
  if (OperandType == "OPERAND_PCREL" || OperandType == "OPERAND_IMMEDIATE")
    OperandType = "CS_OP_IMM";
  else if (OperandType == "OPERAND_MEMORY")
    OperandType = "CS_OP_MEM";
  else if (OperandType == "OPERAND_REGISTER")
    OperandType = "CS_OP_REG";
  // Arch dependent special Op types
  else if (OperandType == "OPERAND_VPRED_N" || OperandType == "OPERAND_VPRED_R")
    return "CS_OP_INVALID";
  else if (OperandType == "OPERAND_IMPLICIT_IMM_0")
    return "CS_OP_IMM";
  else
    PrintFatalNote("Unhandled OperandType: " + OperandType);
  return OperandType;
}

/// Compares both lists of super classes for any matches.
/// It ignores very common (Architecture independent)
/// super classes (DAGOperand, RegisterOperand, PatFrags
/// etc.). Because those will certainly lead to false positives.
bool compareTypeSuperClasses(ArrayRef<std::pair<Record *, SMRange>> OpTypeSC,
                             ArrayRef<std::pair<Record *, SMRange>> PatTypeSC) {
  std::vector<std::string> IgnoredSC = {
      "DAGOperand",    "Operand",         "PatFrag",           "PatFrags",
      "RegisterClass", "RegisterOperand", "SDPatternOperator", "ValueType"};
  std::vector<Record *> OpSCToTest;
  std::vector<Record *> PatSCToTest;

  // Go backwards over super clases (backwards over the inheritance tree) until
  // we find a SC to ignore.
  for (auto SCPair : reverse(OpTypeSC)) {
    if (find(IgnoredSC, SCPair.first->getName()) != IgnoredSC.end())
      break;
    OpSCToTest.emplace_back(SCPair.first);
  }
  for (auto SCPair : reverse(PatTypeSC)) {
    if (find(IgnoredSC, SCPair.first->getName()) != IgnoredSC.end())
      break;
    PatSCToTest.emplace_back(SCPair.first);
  }
  return any_of(OpSCToTest, [&](Record *OpSCRec) {
    return find(PatSCToTest, OpSCRec) != PatSCToTest.end();
  });
}

/// @brief Checks if the given operand is part of a pattern of type iPTR.
/// @param OpRec The operand Record.
/// @param OpName The operand name (Rn, imm, offset etc.)
/// @param PatternDag The pattern DAG to search in.
/// @param PartOfPTRPattern True, if the given pattern is of type iPTR. False
/// otherwise.
/// @param MatchByTypeName If true, the same type names are treated as a valid
/// match.
/// @param MatchByTypeSuperClasses If true, a valid match is also if any type
/// super classes are the same.
/// @return True, if the pattern contains a node with the same name (and
/// optionally the same type name or same super class type) as the given  operand.
/// False otherwise.
bool opIsPartOfiPTRPattern(Record const *OpRec, StringRef const &OpName,
                           DagInit *PatternDag, bool PartOfPTRPattern,
                           bool MatchByTypeName = false,
                           bool MatchByTypeSuperClasses = false) {
  for (unsigned I = 0; I < PatternDag->getNumArgs(); ++I) {
    DagInit *DagArg = dyn_cast<DagInit>(PatternDag->getArg(I));
    if (DagArg) { // Another pattern. Search in it.
      Record *DagRec = dyn_cast<DefInit>(DagArg->getOperator())->getDef();

      // Check if DAG operator is of type iPTR.
      if (DagRec->getValue("Value") &&
          getValueType(DagRec) == MVT::SimpleValueType::iPTR)
        PartOfPTRPattern = true;
      // Complex patterns define their type in "Ty"
      if (DagRec->getValue("Ty") && getValueType(DagRec->getValueAsDef("Ty")) ==
                                        MVT::SimpleValueType::iPTR)
        PartOfPTRPattern = true;
      if (opIsPartOfiPTRPattern(OpRec, OpName, DagArg, PartOfPTRPattern,
                                MatchByTypeName, MatchByTypeSuperClasses))
        return true;
      continue;
    }

    DefInit *LeaveDef = dyn_cast<DefInit>(PatternDag->getArg(I));
    if (!LeaveDef)
      return false;
    bool Matches;
    StringRef const &PatOpName = PatternDag->getArgNameStr(I);
    Matches = OpName.equals(PatOpName);
    if (MatchByTypeName) {
      std::string OpInitType = OpRec->getNameInitAsString();
      std::string PatOpType = PatternDag->getArg(I)->getAsString();
      Matches |= OpInitType == PatOpType;
    }
    if (MatchByTypeSuperClasses) {
      std::string OpInitType = OpRec->getNameInitAsString();
      std::string PatOpType = PatternDag->getArg(I)->getAsString();
      RecordKeeper &RK = OpRec->getRecords();
      ArrayRef<std::pair<Record *, SMRange>> OpTypeSC =
          RK.getDef(OpInitType)->getSuperClasses();
      ArrayRef<std::pair<Record *, SMRange>> PatTypeSC =
          RK.getDef(PatOpType)->getSuperClasses();
      Matches |= compareTypeSuperClasses(OpTypeSC, PatTypeSC);
    }
    if (Matches) {
      if (PartOfPTRPattern)
        return true;
      return false;
    }
  }
  return false;
}

/// Try to match a patterns resulting instr. ops to the operands of a CGI by
/// type. If it matches it returns the index of the CGI operand from which on
/// the pattern ops match (counted for OutOps + InOps). If it doens't match it
/// returns -1
int comparePatternResultToCGIOps(CodeGenInstruction const *CGI,
                                 DagInit *PatternResDag) {
  if (PatternResDag->getNumArgs() == 0)
    return -1;
  DagInit *InDI = CGI->TheDef->getValueAsDag("InOperandList");
  DagInit *OutDI = CGI->TheDef->getValueAsDag("OutOperandList");
  unsigned NumOuts = OutDI->getNumArgs();
  unsigned NumOps = OutDI->getNumArgs() + InDI->getNumArgs();
  int32_t PatMatchStart = -1;
  for (unsigned I = 0, J = 0; I < NumOps; ++I) {
    Init *OpInit;
    bool IsOutOp = I < NumOuts;
    if (IsOutOp) {
      OpInit = OutDI->getArg(I);
    } else {
      OpInit = InDI->getArg(I - NumOuts);
    }
    std::string PatOpType = PatternResDag->getArg(J)->getAsString();
    std::string OpType = OpInit->getAsString();

    if (PatOpType == OpType) {
      // Select next pattern op
      if (PatMatchStart == -1)
        PatMatchStart = I;
      J++;
      if (J >= PatternResDag->getNumArgs())
        // Done
        return PatMatchStart;
    } else if (PatMatchStart != -1) {
      return -1;
    }
  }
  // Nothing matched
  return -1;
}

std::string getCSOperandType(StringRef const &TargetName,
    CodeGenInstruction const *CGI, Record const *OpRec, StringRef const &OpName,
    std::map<std::string, std::vector<Record *>> const InsnPatternMap) {
  std::string OperandType = getPrimaryCSOperandType(OpRec);

  if (TargetName.equals("AArch64") && OperandType != "CS_OP_MEM") {
    // The definitions of AArch64 are so broken, when it comes to memory operands,
    // that we just search for the op name enclosed in [].
    if (Regex("\\[.*\\$" + OpName.str() + ".*]").match(CGI->AsmString))
      return OperandType += " | CS_OP_MEM";
  }

  DagInit *PatternDag = nullptr;
  if (OperandType == "CS_OP_MEM")
    // It is only marked as mem, we treat it as immediate.
    OperandType += " | CS_OP_IMM";
  else if (OpRec->getValue("Type") && getValueType(OpRec->getValueAsDef("Type")) ==
                                        MVT::SimpleValueType::iPTR)
    OperandType += " | CS_OP_MEM";
  else if (!CGI->TheDef->isValueUnset("Pattern") && !CGI->TheDef->getValueAsListInit("Pattern")->empty()) {
    // Check if operand is part of a pattern with a memory type (iPTR)
    ListInit *PatternList = CGI->TheDef->getValueAsListInit("Pattern");
    PatternDag = dyn_cast<DagInit>(PatternList->getValues()[0]);
  } else if (!InsnPatternMap.empty()) {
    // Pattern field is not set in the CGI.
    // But there might be (multiple) patterns in the record keeper
    // for this CGI
    std::string CGIName = CGI->TheDef->getName().str();
    if (InsnPatternMap.find(CGIName) == InsnPatternMap.end())
      return OperandType;

    bool OpTypeIsPartOfAnyPattern =
        any_of(InsnPatternMap.at(CGIName), [&](Record *PatternDag) {
          return opIsPartOfiPTRPattern(
              OpRec, OpName, PatternDag->getValueAsDag("PatternToMatch"), false,
              true);
        });
    if (OpTypeIsPartOfAnyPattern)
      OperandType += " | CS_OP_MEM";
    return OperandType;
  }
  if (PatternDag && opIsPartOfiPTRPattern(OpRec, OpName, PatternDag, false))
    OperandType += " | CS_OP_MEM";
  return OperandType;
}

std::string getCSOperandEncoding(CodeGenInstruction const *CGI,
                                 Record const *OpRec, StringRef const &OpName) {
  BitsInit const *const InstrBits =
      !CGI->TheDef->getValueAsBit("isPseudo")
          ? CGI->TheDef->getValueAsBitsInit("Inst")
          : nullptr;
  if (!InstrBits)
    return "{ 0 }";

  std::string ResultStr;
  raw_string_ostream Result(ResultStr);
  int64_t const Size = CGI->TheDef->getValueAsInt("Size") * 8;
  // For some reason even on 2 byte THUMB instructions the Inst field has 32
  // bits with the first 16 left as 0, so we skip them.
  // I assume the same may happen for other architectures as well.
  unsigned const BitCount = InstrBits->getNumBits();
  unsigned const StartIdx = BitCount - Size;

  struct {
    unsigned OperandPiecesCount;
    std::array<unsigned, 8> Indexes;
    std::array<unsigned, 8> Sizes;
  } EncodingData{};

  // scan all bits one by one to try and find any references of the operand
  for (unsigned InstrBitIdx = StartIdx; InstrBitIdx != BitCount;
       ++InstrBitIdx) {
    VarBitInit const *VarBit;
    if ((VarBit = dyn_cast<VarBitInit>(
             InstrBits->getBit(BitCount - InstrBitIdx - 1))) &&
        VarBit->getBitVar()->getAsString() == OpName) {
      unsigned const BitNum = (InstrBitIdx + VarBit->getBitNum()) >= BitCount
                                  ? BitCount - InstrBitIdx - 1
                                  : VarBit->getBitNum();
      if (EncodingData.OperandPiecesCount == 8)
        llvm_unreachable("Too many operand pieces in the instruction!");

      // place current index
      EncodingData.Indexes[EncodingData.OperandPiecesCount] =
          InstrBitIdx - StartIdx;

      unsigned VarBitIdx;
      // this is meant for getting the size of the operand and to also see
      // whether there are more pieces of the operand further
      for (VarBitIdx = 1; VarBitIdx <= BitNum; ++VarBitIdx) {
        VarBit = dyn_cast<VarBitInit>(
            InstrBits->getBit(BitCount - (InstrBitIdx + VarBitIdx) - 1));

        if (VarBit && VarBit->getBitVar()->getAsString() == OpName)
          continue;
        break;
      }
      // place current size
      EncodingData.Sizes[EncodingData.OperandPiecesCount] = VarBitIdx;
      ++EncodingData.OperandPiecesCount;

      // if we broke out of the loop before it finishes, it means we aren't
      // done here. more pieces of the operand are to be found
      if (VarBitIdx <= BitNum) {
        InstrBitIdx += VarBitIdx - 1;
        continue;
      }
      break;
    }
  }

  // if no references were found we exit, otherwise we add the encoding to the
  // string
  if (!EncodingData.OperandPiecesCount)
    return "{ 0 }";
  Result << "{ " << EncodingData.OperandPiecesCount << ", { ";

  for (unsigned i = 0; i != EncodingData.OperandPiecesCount; ++i) {
    if (i)
      Result << ", ";
    Result << EncodingData.Indexes[i];
  }
  Result << " }, { ";
  for (unsigned i = 0; i != EncodingData.OperandPiecesCount; ++i) {
    if (i)
      Result << ", ";
    Result << EncodingData.Sizes[i];
  }
  Result << " } }";
  return ResultStr;
}

std::string getCSOpcodeEncoding(CodeGenInstruction const *CGI) {
  BitsInit const *const InstrBits =
      !CGI->TheDef->getValueAsBit("isPseudo")
          ? CGI->TheDef->getValueAsBitsInit("Inst")
          : nullptr;
  if (!InstrBits)
    return "{ 0 }";

  std::string ResultStr;
  raw_string_ostream Result(ResultStr);
  int64_t const Size = CGI->TheDef->getValueAsInt("Size") * 8;
  unsigned const BitCount = InstrBits->getNumBits();
  unsigned const StartIdx = BitCount - Size;

  struct {
    std::bitset<64> Bits;
    std::array<unsigned, 64> Indexes;
    unsigned BitCount;
  } OpcodeData{};

  for (unsigned InstrBitIdx = StartIdx; InstrBitIdx != BitCount;
       ++InstrBitIdx) {
    if (auto const *const Bit =
            dyn_cast<BitInit>(InstrBits->getBit(BitCount - InstrBitIdx - 1))) {
      if (OpcodeData.BitCount == 64)
        llvm_unreachable("Instruction's opcode size is greater than 8 bytes!");
      OpcodeData.Bits.set(OpcodeData.BitCount, Bit->getValue());
      OpcodeData.Indexes[OpcodeData.BitCount] = InstrBitIdx - StartIdx;
      ++OpcodeData.BitCount;
    }
  }

  // Most likely unreachable since there is no instruction to my knowledge that
  // doesn't have any opcode bits
  if (!OpcodeData.BitCount)
    llvm_unreachable("Instruction without opcode bits!");

  Result << "{ " << OpcodeData.Bits.to_ullong() << ", { ";
  for (auto Current = OpcodeData.Indexes.begin(),
            end = Current + OpcodeData.BitCount;
       Current != end; ++Current) {
    if (Current != OpcodeData.Indexes.begin())
      Result << ", ";
    Result << *Current;
  }
  Result << " }, " << OpcodeData.BitCount << " }";
  return ResultStr;
}

void printInsnMapEntry(StringRef const &TargetName, AsmMatcherInfo &AMI,
                       std::unique_ptr<MatchableInfo> const &MI, bool UseMI,
                       CodeGenInstruction const *CGI,
                       raw_string_ostream &InsnMap, unsigned InsnNum,
                       raw_string_ostream &PPCFormatEnum) {
  InsnMap << "{\n";
  InsnMap.indent(2) << "/* "
                    << (CGI->AsmString != "" ? CGI->AsmString
                                             : "<No AsmString>")
                    << " */\n";
  // adds id
  InsnMap.indent(2) << getLLVMInstEnumName(TargetName, CGI) << " /* " << InsnNum
                    << " */";
  InsnMap << ", " << TargetName << "_INS_"
          << (UseMI ? getNormalMnemonic(MI) : "INVALID") << ",\n";
  // no diet only
  InsnMap.indent(2) << "#ifndef CAPSTONE_DIET\n";
  if (UseMI) {
    InsnMap.indent(4) << getImplicitUses(TargetName, CGI) << ", ";
    InsnMap << getImplicitDefs(TargetName, CGI) << ", ";
    InsnMap << getReqFeatures(TargetName, AMI, MI, UseMI, CGI) << ", ";
    InsnMap << ((CGI->isBranch || CGI->isReturn) ? "1" : "0") << ", ";
    InsnMap << (CGI->isIndirectBranch ? "1" : "0") << ", ";
    InsnMap << getArchSupplInfo(TargetName, CGI, PPCFormatEnum) << ",\n";
    InsnMap.indent(4) << getCSOpcodeEncoding(CGI);
  } else {
    InsnMap.indent(4) << "{ 0 }, { 0 }, { 0 }, 0, 0, {{ 0 }}, { 0 }";
  }
  InsnMap << '\n';
  InsnMap.indent(2) << "#endif\n";
  InsnMap << "},\n";
}

static std::string getCSAccess(short Access) {
  if (Access == 1)
    return "CS_AC_READ";
  else if (Access == 2)
    return "CS_AC_WRITE";
  else if (Access == 3)
    return "CS_AC_READ | CS_AC_WRITE";
  else if (Access == 0)
    return "CS_AC_INVALID";
  else
    PrintFatalNote("Invalid access flags set.");
}

/// @brief Returns the operand data type. If it is a float it updates
/// OperandType as well.
/// @param Op The operand.
/// @param OperandType The operand type.
/// @return The strig of data types.
std::string getOperandDataTypes(Record const *Op, std::string &OperandType) {
  MVT::SimpleValueType VT;
  std::vector<Record *> OpDataTypes;

  if (!Op->getValue("RegTypes") && Op->getValue("RegClass") &&
      OperandType.find("CS_OP_REG") != std::string::npos)
    Op = Op->getValueAsDef("RegClass");

  if (!(Op->getValue("Type") || Op->getValue("RegTypes")))
    return "{ CS_DATA_TYPE_LAST }";

  if (OperandType.find("CS_OP_REG") != std::string::npos &&
      Op->getValue("RegTypes")) {
    OpDataTypes = Op->getValueAsListOfDefs("RegTypes");
  } else {
    Record *OpType = Op->getValueAsDef("Type");
    VT = getValueType(OpType);
    bool IsFloat = false;
    for (uint8_t V = MVT::SimpleValueType::FIRST_FP_VALUETYPE;
         V <= MVT::SimpleValueType::LAST_FP_VALUETYPE; V++)
      IsFloat |= (VT == V);

    if (IsFloat)
      OperandType = (OperandType.find("MEM") != std::string::npos)
                        ? "CS_OP_MEM | CS_OP_FP"
                        : "CS_OP_FP";
    StringRef EnumVT = getEnumName(VT);
    return "{ CS_DATA_TYPE_" + EnumVT.substr(5).str() + ", CS_DATA_TYPE_LAST }";
  }

  std::string DataTypes = "{ ";
  for (Record *Type : OpDataTypes) {
    StringRef EnumVT = getEnumName(getValueType(Type));
    DataTypes += "CS_DATA_TYPE_" + EnumVT.substr(5).str() + ", ";
  }
  DataTypes += "CS_DATA_TYPE_LAST }";
  return DataTypes;
}

typedef struct OpData {
  Record *Rec;
  std::string OpAsm;
  std::string OpType;
  std::string DataTypes;
  unsigned Access; ///< 0b00 = unkown, 0b01 = In, 0b10 = Out, 0b11 = In and Out
  std::string OpEncoding;
  std::string str() const {
    return "Asm: " + OpAsm + " Type: " + OpType +
           " Access: " + std::to_string(Access);
  }
} OpData;

uint8_t getOpAccess(CodeGenInstruction const *CGI, std::string OperandType,
                    bool IsOutOp) {
  if (OperandType.find("CS_OP_MEM") != std::string::npos) {
    if (CGI->mayLoad_Unset && CGI->mayStore_Unset) {
      return 0;
    } else if (CGI->mayLoad && CGI->mayStore)
      return 3;
    else if (CGI->mayLoad)
      return 1;
    else if (CGI->mayStore)
      return 2;
  }
  return IsOutOp ? 2 : 1;
}

void addComplexOperand(
    StringRef const &TargetName, CodeGenInstruction const *CGI, Record const *ComplexOp,
    StringRef const &ArgName, bool IsOutOp, std::vector<OpData> &InsOps,
    std::string const &Encoding,
    std::map<std::string, std::vector<Record *>> const InsnPatternMap) {
  DagInit *SubOps = ComplexOp->getValueAsDag("MIOperandInfo");

  std::string const &ComplOperandType = getPrimaryCSOperandType(ComplexOp);

  unsigned E = SubOps->getNumArgs();
  for (unsigned I = 0; I != E; ++I) {
    Init *ArgInit = SubOps->getArg(I);
    Record *SubOp = argInitOpToRecord(ArgInit);
    // Determine Operand type
    std::string OperandType;
    std::string SubOperandType =
        getCSOperandType(TargetName, CGI, SubOp, SubOp->getName().str(), InsnPatternMap);
    std::string ComplOperandType =
        getCSOperandType(TargetName, CGI, ComplexOp, ArgName, InsnPatternMap);
    if (ComplOperandType == "CS_OP_MEM")
      OperandType = ComplOperandType + " | " + SubOperandType;
    else if (!CGI->TheDef->isValueUnset("Pattern") && !CGI->TheDef->getValueAsListInit("Pattern")->empty()) {
      OperandType = SubOperandType;
      ListInit *PatternList = CGI->TheDef->getValueAsListInit("Pattern");
      DagInit *PatternDag = dyn_cast<DagInit>(PatternList->getValues()[0]);
      if (PatternDag && opIsPartOfiPTRPattern(SubOp, SubOps->getArgNameStr(I),
                                              PatternDag, false))
        OperandType += " | CS_OP_MEM";
    } else
      OperandType = SubOperandType;

    unsigned AccessFlag = getOpAccess(CGI, OperandType, IsOutOp);
    std::string OpDataTypes = getOperandDataTypes(SubOp, SubOperandType);

    // Check if Operand was already seen before (as In or Out operand).
    // If so update its access flags.
    std::string OpName = ArgName.str() + " - " + SubOp->getName().str();
    InsOps.push_back(OpData{SubOp, std::move(OpName), std::move(OperandType),
                            std::move(OpDataTypes), AccessFlag, Encoding});
  }
}

void printInsnOpMapEntry(
    CodeGenTarget const &Target, std::unique_ptr<MatchableInfo> const &MI,
    bool UseMI, CodeGenInstruction const *CGI, raw_string_ostream &InsnOpMap,
    unsigned InsnNum,
    std::map<std::string, std::vector<Record *>> const InsnPatternMap) {
  StringRef TargetName = Target.getName();

  // Instruction without mnemonic.
  if (!UseMI) {
    std::string LLVMEnum = getLLVMInstEnumName(TargetName, CGI);
    // Write the C struct of the Instruction operands.
    // The many braces are necessary because of this bug from
    // medieval times:
    // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=53119
    InsnOpMap << "{{{ /* " + LLVMEnum + " (" << InsnNum
              << ") - " + TargetName + "_INS_" +
                     (UseMI ? getNormalMnemonic(MI) : "INVALID") + " - " +
                     CGI->AsmString + " */\n";
    InsnOpMap << " 0\n";
    InsnOpMap << "}}},\n";
    return;
  }

  DagInit *InDI = CGI->TheDef->getValueAsDag("InOperandList");
  DagInit *OutDI = CGI->TheDef->getValueAsDag("OutOperandList");
  unsigned NumDefs = OutDI->getNumArgs();

  unsigned E = OutDI->getNumArgs() + InDI->getNumArgs();
  bool IsOutOp;
  std::vector<OpData> InsOps;
  // Interate over every In and Out operand and get its Def.
  for (unsigned I = 0; I != E; ++I) {
    Init *ArgInit;
    StringRef ArgName;
    IsOutOp = I < NumDefs;
    if (IsOutOp) {
      ArgInit = OutDI->getArg(I);
      ArgName = OutDI->getArgNameStr(I);
    } else {
      ArgInit = InDI->getArg(I - NumDefs);
      ArgName = InDI->getArgNameStr(I - NumDefs);
    }
    Record *Rec = argInitOpToRecord(ArgInit);

    std::string const &Encoding = getCSOperandEncoding(CGI, Rec, ArgName);

    // Add complex operands.
    // Operands which effectively consists of two or more operands.
    if (Rec->getValue("MIOperandInfo")) {
      if (Rec->getValueAsDag("MIOperandInfo")->getNumArgs() > 0) {
        addComplexOperand(TargetName, CGI, Rec, ArgName, IsOutOp, InsOps,
              Encoding, InsnPatternMap);
        continue;
      }
    }

    // Determine Operand type
    std::string OperandType =
        getCSOperandType(TargetName, CGI, Rec, ArgName, InsnPatternMap);
    if (OperandType == "")
      continue;

    std::string OpDataTypes = getOperandDataTypes(Rec, OperandType);

    // Check if Operand was already seen before (as In or Out operand).
    // If so update its access flags.
    unsigned AccessFlag = getOpAccess(CGI, OperandType, IsOutOp);
    InsOps.push_back(OpData{Rec, ArgName.str(), std::move(OperandType),
                            std::move(OpDataTypes), AccessFlag,
                            std::move(Encoding)});
  }

  if (InsOps.size() > 15) {
    for (OpData const &OD : InsOps) {
      PrintNote(OD.str());
      OD.Rec->dump();
    }
    PrintFatalNote("Inst has more then 15 operands: " + CGI->AsmString);
  }

  std::string LLVMEnum = getLLVMInstEnumName(TargetName, CGI);
  // Write the C struct of the Instruction operands.
  InsnOpMap << "{ /* " + LLVMEnum + " (" << InsnNum
            << ") - " + TargetName + "_INS_" +
                   (UseMI ? getNormalMnemonic(MI) : "INVALID") + " - " +
                   CGI->AsmString + " */\n";
  InsnOpMap << "{\n";
  for (OpData const &OD : InsOps) {
    InsnOpMap.indent(2) << "{ " << OD.OpType << ", " << getCSAccess(OD.Access)
                        << ", " << OD.DataTypes << ", " << OD.OpEncoding
                        << " }, /* " << OD.OpAsm << " */\n";
  }
  InsnOpMap.indent(2) << "{ 0 }\n";
  InsnOpMap << "}},\n";
}

void printInsnNameMapEnumEntry(StringRef const &TargetName,
                               std::unique_ptr<MatchableInfo> const &MI,
                               raw_string_ostream &InsnNameMap,
                               raw_string_ostream &InsnEnum) {
  static std::set<std::string> MnemonicsSeen;
  static std::set<std::string> EnumsSeen;

  std::string Mnemonic = normalizedMnemonic(MI->Mnemonic.str(), false);
  if (MnemonicsSeen.find(Mnemonic) != MnemonicsSeen.end())
    return;

  std::string EnumName =
      TargetName.str() + "_INS_" + normalizedMnemonic(StringRef(Mnemonic));
  InsnNameMap.indent(2) << "\"" + Mnemonic + "\", // " + EnumName + "\n";
  if (EnumsSeen.find(EnumName) == EnumsSeen.end())
    InsnEnum.indent(2) << EnumName + ",\n";

  MnemonicsSeen.emplace(Mnemonic);
  EnumsSeen.emplace(EnumName);
}

void printFeatureEnumEntry(StringRef const &TargetName, AsmMatcherInfo &AMI,
                           CodeGenInstruction const *CGI,
                           raw_string_ostream &FeatureEnum,
                           raw_string_ostream &FeatureNameArray) {
  static std::set<std::string> Features;
  std::string EnumName;

  for (std::pair<Record *, SubtargetFeatureInfo> ST : AMI.SubtargetFeatures) {
    const SubtargetFeatureInfo &STF = ST.second;
    std::string Feature = STF.TheDef->getName().str();
    if (Features.find(Feature) != Features.end())
      continue;
    Features.emplace(Feature);

    // Enum
    EnumName = TargetName.str() + "_FEATURE_" + STF.TheDef->getName().str();
    FeatureEnum << EnumName;
    if (Features.size() == 1)
      FeatureEnum << " = 128";
    FeatureEnum << ",\n";

    // Enum name map
    FeatureNameArray << "{ " + EnumName + ", \"" + STF.TheDef->getName().str() +
                            "\" },\n";
  }
}

/// Emits enum entries for each operand group.
/// The operand group name is equal printer method of the operand.
/// printSORegRegOperand -> SORegRegOperand
void printOpPrintGroupEnum(StringRef const &TargetName,
                           CodeGenInstruction const *CGI,
                           raw_string_ostream &OpGroupEnum) {
  static std::set<std::string> OpGroups;
  // Some operand groups, which exists, are never passed here.
  // So we add them manually.
  static const std::set<std::string> ARMExceptions = {
      "RegImmShift",
      "LdStmModeOperand",
      "MandatoryInvertedPredicateOperand",
  };
  static const std::set<std::string> AArch64Exceptions = {
      "VectorIndex_8",
      "PrefetchOp_1",
      "LogicalImm_int8_t",
      "LogicalImm_int16_t",
      "InverseCondCode",
      "AMNoIndex",
      "PSBHintOp",
      "BTIHintOp",
      "ImplicitlyTypedVectorList",
      "SVERegOp_0",
      "SVELogicalImm_int16_t",
      "SVELogicalImm_int32_t",
      "SVELogicalImm_int64_t",
      "ZPRasFPR_128"};
  static const std::set<std::string> PPCExceptions = {
      "S12ImmOperand", // PS S12 immediates. Used as memory disponent.
  };

  bool NoExceptions = false;
  const std::set<std::string> *Exc;
  if (TargetName == "ARM")
    Exc = &ARMExceptions;
  else if (TargetName == "AArch64")
    Exc = &AArch64Exceptions;
  else if (TargetName == "PPC")
    Exc = &PPCExceptions;
  else
    NoExceptions = true;

  if (OpGroups.empty() && !NoExceptions) {
    for (const std::string &OpGroup : *Exc) {
      OpGroupEnum.indent(2) << TargetName + "_OP_GROUP_" + OpGroup + " = "
                            << OpGroups.size() << ",\n";
      OpGroups.emplace(OpGroup);
    }
  }

  for (const CGIOperandList::OperandInfo &Op : CGI->Operands) {
    std::string OpGroup =
        PrinterCapstone::translateToC(TargetName.str(), Op.PrinterMethodName)
            .substr(5);
    if (OpGroups.find(OpGroup) != OpGroups.end())
      continue;
    OpGroupEnum.indent(2) << TargetName + "_OP_GROUP_" + OpGroup + " = "
                          << OpGroups.size() << ",\n";
    OpGroups.emplace(OpGroup);
  }
}

void printInsnAliasEnum(CodeGenTarget const &Target,
                        raw_string_ostream &AliasEnum,
                        raw_string_ostream &AliasMnemMap) {
  RecordKeeper &Records = Target.getTargetRecord()->getRecords();
  std::vector<Record *> AllInstAliases =
      Records.getAllDerivedDefinitions("InstAlias");
  std::set<std::string> AliasMnemonicsSeen;

  for (Record *AliasRec : AllInstAliases) {
    int Priority = AliasRec->getValueAsInt("EmitPriority");
    if (Priority < 1)
      continue; // Aliases with priority 0 are never emitted.
    const DagInit *AliasDag = AliasRec->getValueAsDag("ResultInst");
    DefInit *DI = dyn_cast<DefInit>(AliasDag->getOperator());
    CodeGenInstruction *RealInst = &Target.getInstruction(DI->getDef());

    StringRef AliasAsm = AliasRec->getValueAsString("AsmString");
    SmallVector<StringRef, 1> Matches;
    // Some Alias only differ by operands. Get only the mnemonic part.
    Regex("^[a-zA-Z0-9+-.]+").match(AliasAsm, &Matches);
    StringRef &AliasMnemonic = Matches[0];
    std::string NormAliasMnem = Target.getName().str() + "_INS_ALIAS_" +
                                normalizedMnemonic(AliasMnemonic);
    if (AliasMnemonicsSeen.find(NormAliasMnem) != AliasMnemonicsSeen.end())
      continue;

    AliasMnemonicsSeen.emplace(NormAliasMnem);

    AliasEnum << "\t" + NormAliasMnem + ", // Real instr.: " +
                     getLLVMInstEnumName(Target.getName(), RealInst) + "\n";

    AliasMnemMap << "\t{ " + NormAliasMnem + ", \"" + normalizedMnemonic(AliasMnemonic, false) + "\" },\n";
  }
}

void addInsnsToPatternMap(Record *Pattern,
                          std::map<std::string, std::vector<Record *>> &Map,
                          ArrayRef<Init *> ResInsns) {
  for (Init *RI : ResInsns) {
    Record *RIRec =
        dyn_cast<DefInit>(dyn_cast<DagInit>(RI)->getOperator())->getDef();
    if (!RIRec->getValue("isPseudo"))
      continue; // No instruction
    if (RIRec->getValueAsBit("isPseudo")) {
      if (!RIRec->getValue("ResultInst"))
        continue;
      // Add the resulting instruction of this pseudo instruction as well.
      addInsnsToPatternMap(
          Pattern, Map, ArrayRef<Init *>(RIRec->getValueAsDag("ResultInst")));
    }
    std::string RIName = dyn_cast<DagInit>(RI)->getOperator()->getAsString();
    if (Map.find(RIName) == Map.end()) {
      std::vector<Record *> PatVec;
      PatVec.emplace_back(Pattern);
      Map.emplace(RIName, std::move(PatVec));
    } else {
      std::vector<Record *> &PatternVec = Map.at(RIName);
      PatternVec.emplace_back(Pattern);
    }
  }
}

/// Returns a map with instruction name and its pattern.
/// Only needed by archs which, very annoyingly,
/// do not set the Pattern field in CGIs (like AArch64).
void getInsnPatternMap(CodeGenTarget const &Target,
                       std::map<std::string, std::vector<Record *>> &Map) {
  for (Record *P :
       Target.getTargetRecord()->getRecords().getAllDerivedDefinitions("Pat")) {
    ListInit *ResInsns = P->getValueAsListInit("ResultInstrs");
    addInsnsToPatternMap(P, Map, ResInsns->getValues());
  }
}

} // namespace

/// This function emits all the mapping files and
/// Instruction enum for the current architecture.
void PrinterCapstone::asmMatcherEmitMatchTable(CodeGenTarget const &Target,
                                               AsmMatcherInfo &Info,
                                               StringToOffsetTable &StringTable,
                                               unsigned VariantCount) const {
  std::string InsnMapStr;
  std::string InsnOpMapStr;
  std::string InsnNameMapStr;
  std::string InsnEnumStr;
  std::string FeatureEnumStr;
  std::string FeatureNameArrayStr;
  std::string OpGroupStr;
  std::string PPCFormatEnumStr;
  std::string AliasEnumStr;
  std::string AliasMnemMapStr;
  raw_string_ostream InsnMap(InsnMapStr);
  raw_string_ostream InsnOpMap(InsnOpMapStr);
  raw_string_ostream InsnNameMap(InsnNameMapStr);
  raw_string_ostream InsnEnum(InsnEnumStr);
  raw_string_ostream FeatureEnum(FeatureEnumStr);
  raw_string_ostream FeatureNameArray(FeatureNameArrayStr);
  raw_string_ostream OpGroups(OpGroupStr);
  raw_string_ostream PPCFormatEnum(PPCFormatEnumStr);
  raw_string_ostream AliasEnum(AliasEnumStr);
  raw_string_ostream AliasMnemMap(AliasMnemMapStr);
  emitDefaultSourceFileHeader(InsnMap);
  emitDefaultSourceFileHeader(InsnOpMap);
  emitDefaultSourceFileHeader(InsnNameMap);
  emitDefaultSourceFileHeader(InsnEnum);
  emitDefaultSourceFileHeader(FeatureEnum);
  emitDefaultSourceFileHeader(FeatureNameArray);
  emitDefaultSourceFileHeader(OpGroups);
  emitDefaultSourceFileHeader(PPCFormatEnum);
  emitDefaultSourceFileHeader(AliasEnum);
  emitDefaultSourceFileHeader(AliasMnemMap);

  // Currently we ignore any other Asm variant then the primary.
  Record *AsmVariant = Target.getAsmParserVariant(0);

  AsmVariantInfo Variant;
  Variant.RegisterPrefix = AsmVariant->getValueAsString("RegisterPrefix");
  Variant.TokenizingCharacters =
      AsmVariant->getValueAsString("TokenizingCharacters");
  Variant.SeparatorCharacters =
      AsmVariant->getValueAsString("SeparatorCharacters");
  Variant.BreakCharacters = AsmVariant->getValueAsString("BreakCharacters");
  Variant.Name = AsmVariant->getValueAsString("Name");
  Variant.AsmVariantNo = AsmVariant->getValueAsInt("Variant");
  SmallPtrSet<Record *, 16> SingletonRegisters;

  // Map instructino name to DefInit of pattern.
  // This is noly necessary for AArch64 currently.
  // Because CGI->pattern is not set in the td files.
  // So we search all patterns save them under the name
  // of the instruciton they belong to.
  std::map<std::string, std::vector<Record *>> InsnPatternMap;

  if (Target.getName().equals(StringRef("AArch64")))
    getInsnPatternMap(Target, InsnPatternMap);

  // The CS mapping tables, for instructions and their operands,
  // need an entry for every CodeGenInstruction.
  unsigned InsnNum = 0;
  for (const CodeGenInstruction *CGI : Target.getInstructionsByEnumValue()) {
    auto MI = std::make_unique<MatchableInfo>(*CGI);
    bool UseMI = true;
    MI->tokenizeAsmString(Info, Variant);

    // Ignore "codegen only" instructions.
    if (CGI->TheDef->getValueAsBit("isCodeGenOnly") ||
        MI->AsmOperands.empty()) {
      UseMI = false;
      MI->Mnemonic = "invalid";
    } else
      MI->Mnemonic = MI->AsmOperands[0].Token;
    printInsnNameMapEnumEntry(Target.getName(), MI, InsnNameMap, InsnEnum);
    printFeatureEnumEntry(Target.getName(), Info, CGI, FeatureEnum,
                          FeatureNameArray);
    printOpPrintGroupEnum(Target.getName(), CGI, OpGroups);

    printInsnOpMapEntry(Target, MI, UseMI, CGI, InsnOpMap, InsnNum,
                        InsnPatternMap);
    printInsnMapEntry(Target.getName(), Info, MI, UseMI, CGI, InsnMap, InsnNum,
                      PPCFormatEnum);

    ++InsnNum;
  }
  printInsnAliasEnum(Target, AliasEnum, AliasMnemMap);

  std::string TName = Target.getName().str();
  std::string InsnMapFilename = TName + "GenCSMappingInsn.inc";
  writeFile(InsnMapFilename, InsnMapStr);
  InsnMapFilename = TName + "GenCSMappingInsnOp.inc";
  writeFile(InsnMapFilename, InsnOpMapStr);
  InsnMapFilename = TName + "GenCSMappingInsnName.inc";
  writeFile(InsnMapFilename, InsnNameMapStr);
  InsnMapFilename = TName + "GenCSInsnEnum.inc";
  writeFile(InsnMapFilename, InsnEnumStr);
  InsnMapFilename = TName + "GenCSFeatureEnum.inc";
  writeFile(InsnMapFilename, FeatureEnumStr);
  InsnMapFilename = TName + "GenCSFeatureName.inc";
  writeFile(InsnMapFilename, FeatureNameArrayStr);
  InsnMapFilename = TName + "GenCSOpGroup.inc";
  writeFile(InsnMapFilename, OpGroupStr);
  InsnMapFilename = TName + "GenCSAliasEnum.inc";
  writeFile(InsnMapFilename, AliasEnumStr);
  InsnMapFilename = TName + "GenCSAliasMnemMap.inc";
  writeFile(InsnMapFilename, AliasMnemMapStr);
  if (TName == "PPC") {
    InsnMapFilename = "PPCGenCSInsnFormatsEnum.inc";
    writeFile(InsnMapFilename, PPCFormatEnumStr);
  }
}

void PrinterCapstone::asmMatcherEmitSourceFileHeader(
    std::string const &Desc) const {}
void PrinterCapstone::asmMatcherEmitDeclarations(bool HasOptionalOperands,
                                                 bool ReportMultipleNearMisses,
                                                 bool HasOperandInfos) const {}
void PrinterCapstone::asmMatcherEmitOperandDiagTypes(
    std::set<StringRef> const Types) const {}

void PrinterCapstone::asmMatcherEmitGetSubtargetFeatureName(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        SubtargetFeatures) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionI(
    StringRef const &TargetName, StringRef const &ClassName,
    std::string const &TargetOperandClass, bool HasOptionalOperands,
    size_t MaxNumOperands) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionII(
    std::string const &EnumName, StringRef const &AsmMatchConverter) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionIII(
    std::string const &EnumName, std::string const TargetOperandClass,
    bool HasOptionalOperands, MatchableInfo::AsmOperand const &Op,
    MatchableInfo::ResOperand const &OpInfo) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionIV(
    std::string const &EnumName, int64_t Val) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionV(
    std::string const &EnumName, std::string const &Reg) const {}
void PrinterCapstone::asmMatcherEmitConversionFunctionVI() const {}
void PrinterCapstone::asmMatcherWriteCvtOSToOS() const {}
void PrinterCapstone::asmMatcherEmitOperandFunctionI(
    StringRef const &TargetName, StringRef const &ClassName) const {}
void PrinterCapstone::asmMatcherEmitOperandFunctionII(
    std::string const &EnumName, MatchableInfo::AsmOperand const &Op,
    MatchableInfo::ResOperand const &OpInfo) const {}
void PrinterCapstone::asmMatcherEmitOperandFunctionIII(
    std::string const &EnumName) const {}
void PrinterCapstone::asmMatcherEmitOperandFunctionIV(
    std::string const &EnumName) const {}
void PrinterCapstone::asmMatcherEmitOperandFunctionV() const {}
void PrinterCapstone::asmMatcherEmitTiedOperandEnum(
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {}
void PrinterCapstone::asmMatcherWriteOpOSToOS() const {}
void PrinterCapstone::asmMatcherEmitTiedOpTable(
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {}
void PrinterCapstone::asmMatcherEmitTiedOpEmptyTable() const {}
void PrinterCapstone::asmMatcherEmitOperandConvKindEnum(
    SmallSetVector<CachedHashString, 16> OperandConversionKinds) const {}
void PrinterCapstone::asmMatcherEmitInstrConvKindEnum(
    SmallSetVector<CachedHashString, 16> InstructionConversionKinds) const {}
void PrinterCapstone::asmMatcherEmitConversionTable(
    size_t MaxRowLength,
    std::vector<std::vector<uint8_t>> const ConversionTable,
    SmallSetVector<CachedHashString, 16> InstructionConversionKinds,
    SmallSetVector<CachedHashString, 16> OperandConversionKinds,
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {}
void PrinterCapstone::asmMatcherEmitMatchClassKindEnum(
    std::forward_list<ClassInfo> const &Infos) const {}
void PrinterCapstone::asmMatcherEmitMatchClassDiagStrings(
    AsmMatcherInfo const &Info) const {}
void PrinterCapstone::asmMatcherEmitRegisterMatchErrorFunc(
    AsmMatcherInfo &Info) const {}
void PrinterCapstone::asmMatcherEmitIsSubclassI() const {}
bool PrinterCapstone::asmMatcherEmitIsSubclassII(
    bool EmittedSwitch, std::string const &Name) const {
  return true;
}
void PrinterCapstone::asmMatcherEmitIsSubclassIII(StringRef const &Name) const {
}
void PrinterCapstone::asmMatcherEmitIsSubclassIV(
    std::vector<StringRef> const &SuperClasses) const {}
void PrinterCapstone::asmMatcherEmitIsSubclassV(bool EmittedSwitch) const {}
void PrinterCapstone::asmMatcherEmitValidateOperandClass(
    AsmMatcherInfo &Info) const {}
void PrinterCapstone::asmMatcherEmitMatchClassKindNames(
    std::forward_list<ClassInfo> &Infos) const {}
void PrinterCapstone::asmMatcherEmitAsmTiedOperandConstraints(
    CodeGenTarget &Target, AsmMatcherInfo &Info) const {}
std::string PrinterCapstone::getNameForFeatureBitset(
    const std::vector<Record *> &FeatureBitset) const {
  return "";
}
void PrinterCapstone::asmMatcherEmitFeatureBitsetEnum(
    std::vector<std::vector<Record *>> const FeatureBitsets) const {}
void PrinterCapstone::asmMatcherEmitFeatureBitsets(
    std::vector<std::vector<Record *>> const FeatureBitsets,
    AsmMatcherInfo const &Info) const {}
void PrinterCapstone::asmMatcherEmitMatchEntryStruct(
    unsigned MaxMnemonicIndex, unsigned NumConverters, size_t MaxNumOperands,
    std::vector<std::vector<Record *>> const FeatureBitsets,
    AsmMatcherInfo const &Info) const {}
void PrinterCapstone::asmMatcherEmitMatchFunction(
    CodeGenTarget const &Target, Record const *AsmParser,
    StringRef const &ClassName, bool HasMnemonicFirst, bool HasOptionalOperands,
    bool ReportMultipleNearMisses, bool HasMnemonicAliases,
    size_t MaxNumOperands, bool HasDeprecation,
    unsigned int VariantCount) const {}
void PrinterCapstone::asmMatcherEmitMnemonicSpellChecker(
    CodeGenTarget const &Target, unsigned VariantCount) const {}
void PrinterCapstone::asmMatcherEmitMnemonicChecker(
    CodeGenTarget const &Target, unsigned VariantCount, bool HasMnemonicFirst,
    bool HasMnemonicAliases) const {}
void PrinterCapstone::asmMatcherEmitCustomOperandParsing(
    unsigned MaxMask, CodeGenTarget &Target, AsmMatcherInfo const &Info,
    StringRef ClassName, StringToOffsetTable &StringTable,
    unsigned MaxMnemonicIndex, unsigned MaxFeaturesIndex, bool HasMnemonicFirst,
    Record const &AsmParser) const {}
void PrinterCapstone::asmMatcherEmitIncludes() const {}
void PrinterCapstone::asmMatcherEmitMnemonicTable(
    StringToOffsetTable &StringTable) const {}
void PrinterCapstone::asmMatcherEmitMatchRegisterName(
    Record const *AsmParser,
    std::vector<StringMatcher::StringPair> const Matches) const {}
void PrinterCapstone::asmMatcherEmitMatchTokenString(
    std::vector<StringMatcher::StringPair> const Matches) const {}
void PrinterCapstone::asmMatcherEmitMatchRegisterAltName(
    Record const *AsmParser,
    std::vector<StringMatcher::StringPair> const Matches) const {}
void PrinterCapstone::asmMatcherEmitMnemonicAliasVariant(
    std::vector<StringMatcher::StringPair> const &Cases,
    unsigned Indent) const {}
void PrinterCapstone::asmMatcherAppendMnemonicAlias(
    Record const *R, std::string const &FeatureMask,
    std::string &MatchCode) const {}
void PrinterCapstone::asmMatcherAppendMnemonic(Record const *R,
                                               std::string &MatchCode) const {}
void PrinterCapstone::asmMatcherAppendMnemonicAliasEnd(
    std::string &MatchCode) const {}
void PrinterCapstone::asmMatcherEmitApplyMnemonicAliasesI() const {}
void PrinterCapstone::asmMatcherEmitApplyMnemonicAliasesII(
    int AsmParserVariantNo) const {}
void PrinterCapstone::asmMatcherEmitApplyMnemonicAliasesIII() const {}
void PrinterCapstone::asmMatcherEmitApplyMnemonicAliasesIV() const {}
void PrinterCapstone::asmMatcherEmitApplyMnemonicAliasesV() const {}
void PrinterCapstone::asmMatcherEmitSTFBitEnum(AsmMatcherInfo &Info) const {}
void PrinterCapstone::asmMatcherEmitComputeAssemblerAvailableFeatures(
    AsmMatcherInfo &Info, StringRef const &ClassName) const {}

void PrinterCapstone::searchableTablesWriteFiles() const {
  std::string Filename = TargetName + "GenSystemRegister.inc";
  std::string HeaderStr;
  raw_string_ostream Header(HeaderStr);
  emitDefaultSourceFileHeader(Header);
  raw_string_ostream &Decl = searchableTablesGetOS(ST_DECL_OS);
  raw_string_ostream &Impl = searchableTablesGetOS(ST_IMPL_OS);
  writeFile(Filename, Header.str() + Decl.str() + Impl.str());

  raw_string_ostream &SysOpsEnum = searchableTablesGetOS(ST_ENUM_SYSOPS_OS);
  if (!SysOpsEnum.str().empty()) {
    Filename = TargetName + "GenCSSystemOperandsEnum.inc";
    writeFile(Filename, Header.str() + SysOpsEnum.str());
  }
}

raw_string_ostream &PrinterCapstone::searchableTablesGetOS(StreamType G) const {
  // Very bad design here. But we only use it for our dirty generation
  // for Capstone so it is not meant to be reliable.
  static bool Init = false;
  static std::string SysRegDecl;
  static raw_string_ostream *SysRegDeclOS;
  static std::string SysRegEnum;
  static raw_string_ostream *SysOpsEnumOS;
  static std::string SysRegImpl;
  static raw_string_ostream *SysRegImplOS;
  if (!Init) {
    SysRegDeclOS = new raw_string_ostream(SysRegDecl);
    SysRegImplOS = new raw_string_ostream(SysRegImpl);
    SysOpsEnumOS = new raw_string_ostream(SysRegEnum);
    Init = true;
  }

  switch (G) {
  default:
    assert(0 && "No stream specified.");
  case ST_DECL_OS:
    return *SysRegDeclOS;
  case ST_IMPL_OS:
    return *SysRegImplOS;
  case ST_ENUM_SYSOPS_OS:
    return *SysOpsEnumOS;
  }
}

void PrinterCapstone::searchableTablesEmitGenericEnum(
    const GenericEnum &Enum) const {
  // We do not emit enums here, but generate them when we print the tables
  // Because the table has the type information for its fields,
  // we have a chance to distinguish between Sys regs, imms and other alias.
}

void PrinterCapstone::searchableTablesEmitGenericTable(
    const GenericTable &Enum) const {}

void PrinterCapstone::searchableTablesEmitIfdef(const std::string Guard,
                                                StreamType ST) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST);
  OutS << "#ifdef " << Guard << "\n";
}

void PrinterCapstone::searchableTablesEmitEndif(StreamType ST) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST);
  OutS << "#endif\n\n";
}

void PrinterCapstone::searchableTablesEmitUndef(
    std::string const &Guard) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "#undef " << Guard << "\n";
}

std::string PrinterCapstone::searchableTablesSearchableFieldType(
    const GenericTable &Table, const SearchIndex &Index,
    const GenericField &Field, TypeContext Ctx) const {
  if (isa<StringRecTy>(Field.RecType)) {
    if (Ctx == TypeInStaticStruct)
      return "const char *";
    if (Ctx == TypeInTempStruct)
      return "const char *";
    return "const char *";
  }
  if (BitsRecTy *BI = dyn_cast<BitsRecTy>(Field.RecType)) {
    unsigned NumBits = BI->getNumBits();
    if (NumBits <= 8)
      return "uint8_t";
    if (NumBits <= 16)
      return "uint16_t";
    if (NumBits <= 32)
      return "uint32_t";
    if (NumBits <= 64)
      return "uint64_t";
    PrintFatalError(Index.Loc, Twine("In table '") + Table.Name +
                                   "' lookup method '" + Index.Name +
                                   "', key field '" + Field.Name +
                                   "' of type bits is too large");
  } else if (Field.Enum || Field.IsIntrinsic || Field.IsInstruction)
    return "unsigned";
  PrintFatalError(Index.Loc,
                  Twine("In table '") + Table.Name + "' lookup method '" +
                      Index.Name + "', key field '" + Field.Name +
                      "' has invalid type: " + Field.RecType->getAsString());
}

std::string PrinterCapstone::searchableTablesPrimaryRepresentation(
    SMLoc Loc, const GenericField &Field, Init *I,
    StringRef const &InstrinsicEnumName) const {
  if (StringInit *SI = dyn_cast<StringInit>(I)) {
    if (Field.IsCode || SI->hasCodeFormat()) {
      std::string Code = Regex("::").sub("_", std::string(SI->getValue()));
      while (Code.find("::") != std::string::npos)
        Code = Regex("::").sub("_", Code);
      return Code;
    } else
      return Regex("::").sub("_", SI->getAsString());
  } else if (BitsInit *BI = dyn_cast<BitsInit>(I))
    return "0x" + utohexstr(getAsInt(BI));
  else if (BitInit *BI = dyn_cast<BitInit>(I))
    return BI->getValue() ? "true" : "false";
  else if (Field.IsIntrinsic)
    return "Intrinsic_" + InstrinsicEnumName.str();
  else if (Field.IsInstruction)
    return Regex("::").sub("_", I->getAsString());
  else if (Field.Enum) {
    auto *Entry = Field.Enum->EntryMap[cast<DefInit>(I)->getDef()];
    if (!Entry)
      PrintFatalError(Loc,
                      Twine("Entry for field '") + Field.Name + "' is null");
    return Regex("::").sub("_", std::string(Entry->first));
  }
  PrintFatalError(Loc, Twine("invalid field type for field '") + Field.Name +
                           "'; expected: bit, bits, string, or code");
}

std::string getTableNamespacePrefix(const GenericTable &Table,
                                    std::string TargetName) {
  // Sometimes table type are wrapped into namespaces.
  // In Capstone we need to prepend the name to those types in this case.
  std::set<std::pair<std::string, std::string>> AArch64NSTypePairs = {
      {"AArch64SysReg", "SysReg"},
      {"AArch64PState", "PStateImm0_15"},
      {"AArch64PState", "PStateImm0_1"},
      {"AArch64SVCR", "SVCR"},
      {"AArch64AT", "AT"},
      {"AArch64DB", "DB"},
      {"AArch64DBnXS", "DBnXS"},
      {"AArch64DC", "DC"},
      {"AArch64IC", "IC"},
      {"AArch64ISB", "ISB"},
      {"AArch64TSB", "TSB"},
      {"AArch64PRFM", "PRFM"},
      {"AArch64SVEPRFM", "SVEPRFM"},
      {"AArch64RPRFM", "RPRFM"},
      {"AArch64SVCR", "SVCR"},
      {"AArch64SVEPredPattern", "SVEPREDPAT"},
      {"AArch64SVEVecLenSpecifier", "SVEVECLENSPECIFIER"},
      {"AArch64ExactFPImm", "ExactFPImm"},
      {"AArch64BTIHint", "BTI"},
      {"AArch64TLBI", "TLBI"},
      {"AArch64PRCTX", "PRCTX"},
      {"AArch64BTIHint", "BTI"},
      {"AArch64PSBHint", "PSB"},
  };

  std::set<std::pair<std::string, std::string>> ARMNSTypePairs = {
      {"ARMSysReg", "MClassSysReg"},
      {"ARMBankedReg", "BankedReg"},
  };

  std::set<std::pair<std::string, std::string>> *NSTable;

  if (TargetName != "AArch64" && TargetName != "ARM")
    return Table.CppTypeName + "_";

  if (TargetName == "AArch64")
    NSTable = &AArch64NSTypePairs;
  else if (TargetName == "ARM")
    NSTable = &ARMNSTypePairs;
  else
    PrintFatalNote("No Namespace Type table defined for target.");

  for (auto NSTPair : *NSTable) {
    if (NSTPair.second == Table.CppTypeName)
      return NSTPair.first + "_";
  }
  PrintNote("No namespace defined for type: " + Table.CppTypeName);
  return "";
}

void PrinterCapstone::searchableTablesEmitLookupDeclaration(
    const GenericTable &Table, const SearchIndex &Index, StreamType ST) {
  raw_string_ostream &OutS = (ST == ST_DECL_OS)
                                 ? searchableTablesGetOS(ST_DECL_OS)
                                 : searchableTablesGetOS(ST_IMPL_OS);
  std::string NamespacePre = getTableNamespacePrefix(Table, TargetName);
  OutS << "const " << NamespacePre << Table.CppTypeName << " *" << NamespacePre
       << Index.Name << "(";

  ListSeparator LS;
  for (const auto &Field : Index.Fields)
    OutS << LS
         << searchableTablesSearchableFieldType(Table, Index, Field,
                                                TypeInArgument)
         << " " << Field.Name;
  OutS << ")";
  if (ST == ST_DECL_OS) {
    OutS << ";\n";
  } else if (ST == ST_IMPL_OS)
    OutS << " {\n";
}

void PrinterCapstone::searchableTablesEmitIndexTypeStruct(
    const GenericTable &Table, const SearchIndex &Index) {
  for (const auto &Field : Index.Fields) {
    if (isa<StringRecTy>(Field.RecType)) {
      EmittingNameLookup = isa<StringRecTy>(Field.RecType);
    }
  }
}

void PrinterCapstone::searchableTablesEmitIndexArrayI() const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  if (EmittingNameLookup)
    OutS << "  static const struct IndexTypeStr Index[] = {\n";
  else
    OutS << "  static const struct IndexType Index[] = {\n";
}

void PrinterCapstone::searchableTablesEmitIndexArrayII() const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "    { ";
}

void PrinterCapstone::searchableTablesEmitIndexArrayIII(
    ListSeparator &LS, std::string Repr) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << LS << Repr;
}

void PrinterCapstone::searchableTablesEmitIndexArrayIV(
    std::pair<Record *, unsigned> const &Entry) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << ", " << Entry.second << " },\n";
}

void PrinterCapstone::searchableTablesEmitIndexArrayV() const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "  };\n\n";
}

void PrinterCapstone::searchableTablesEmitIsContiguousCase(
    StringRef const &IndexName, const GenericTable &Table,
    const SearchIndex &Index, bool IsPrimary) {
  searchableTablesEmitReturns(Table, Index, IsPrimary);
}

void PrinterCapstone::searchableTablesEmitIfFieldCase(
    const GenericField &Field, std::string const &FirstRepr,
    std::string const &LastRepr) const {}

void PrinterCapstone::searchableTablesEmitKeyTypeStruct(
    const GenericTable &Table, const SearchIndex &Index) const {}

void PrinterCapstone::searchableTablesEmitKeyArray(const GenericTable &Table,
                                                   const SearchIndex &Index,
                                                   bool IsPrimary) const {}

void PrinterCapstone::searchableTablesEmitIndexLamda(
    const SearchIndex &Index, StringRef const &IndexName,
    StringRef const &IndexTypeName) const {}

void PrinterCapstone::searchableTablesEmitReturns(const GenericTable &Table,
                                                  const SearchIndex &Index,
                                                  bool IsPrimary) {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  if (EmittingNameLookup) {
    OutS << "   unsigned i = binsearch_IndexTypeStrEncoding(Index, "
            "ARR_SIZE(Index), ";
    EmittingNameLookup = false;
  } else
    OutS << "   unsigned i = binsearch_IndexTypeEncoding(Index, "
            "ARR_SIZE(Index), ";
  for (const auto &Field : Index.Fields)
    OutS << Field.Name;
  OutS << ");\n"
       << "   if (i == -1)\n"
       << "      return NULL;\n"
       << "   else\n"
       << "      return &" << Table.Name << "[Index[i].index];\n";
  OutS << "}\n\n";
}

void PrinterCapstone::searchableTablesEmitMapI(
    const GenericTable &Table) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "static const " << getTableNamespacePrefix(Table, TargetName)
       << Table.CppTypeName << " " << Table.Name << "[] = {\n";

  raw_string_ostream &EnumOS = searchableTablesGetOS(ST_ENUM_SYSOPS_OS);
  EnumOS << "#ifdef GET_ENUM_VALUES_" << Table.CppTypeName << "\n";
  EnumOS << "#undef GET_ENUM_VALUES_" << Table.CppTypeName << "\n";
}

void PrinterCapstone::searchableTablesEmitMapII() const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "  { ";
}

uint64_t BitsInitToUInt(const BitsInit *BI) {
  uint64_t Value = 0;
  for (unsigned I = 0, Ie = BI->getNumBits(); I != Ie; ++I) {
    if (BitInit *B = dyn_cast<BitInit>(BI->getBit(I)))
      Value |= (uint64_t)B->getValue() << I;
  }
  return Value;
}

unsigned getEnumValue(Record *Entry) {
  if (!Entry->getValue("EnumValueField") ||
      Entry->isValueUnset("EnumValueField")) {
    // Guess field which has the encoding.
    if (Entry->getValue("Encoding")) {
      BitsInit *BI = Entry->getValueAsBitsInit("Encoding");
      return BitsInitToUInt(BI);
    }
    Entry->dump();
    PrintFatalNote("Which of those fields above are the encoding/enum value?");
  }
  StringRef EnumValField = Entry->getValueAsString("EnumValueField");
  return BitsInitToUInt(Entry->getValueAsBitsInit(EnumValField));
}

void PrinterCapstone::searchableTablesEmitMapIII(const GenericTable &Table,
                                                 ListSeparator &LS,
                                                 GenericField const &Field,
                                                 StringRef &IntrinsicEnum,
                                                 Record *Entry) const {
  static std::set<std::string> EnumNamesSeen;
  unsigned EnumVal = getEnumValue(Entry);

  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << LS;
  std::string EnumName;
  std::string Repr = searchableTablesPrimaryRepresentation(
      Table.Locs[0], Field, Entry->getValueInit(Field.Name), IntrinsicEnum);

  // Emit table field
  if (Field.Name == "Name" || Field.Name == "AltName") {
    // Prepend the enum id to the name field
    std::string OpName = Repr;
    while (OpName.find("\"") != std::string::npos)
      OpName = Regex("\"").sub("", OpName);
    EnumName = TargetName + "_" + StringRef(Table.CppTypeName).upper() + "_" +
               StringRef(OpName).upper();
    Repr = "\"" + OpName + "\", { ." + StringRef(Table.CppTypeName).lower() +
           " = " + EnumName + " }";
    OutS << Repr;

    // Emit enum name
    if (EnumNamesSeen.find(EnumName) != EnumNamesSeen.end())
      return;
    EnumNamesSeen.emplace(EnumName);

    raw_string_ostream &EnumOS = searchableTablesGetOS(ST_ENUM_SYSOPS_OS);
    EnumOS << "\t" + EnumName + " = " << format("0x%x", EnumVal) << ",\n";
  } else {
    OutS << Regex("{ *}").sub("{0}", Repr);
  }
}

void PrinterCapstone::searchableTablesEmitMapIV(unsigned i) const {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << " }, // " << i << "\n";
}

void PrinterCapstone::searchableTablesEmitMapV() {
  raw_string_ostream &OutS = searchableTablesGetOS(ST_IMPL_OS);
  OutS << "  };\n\n";

  raw_string_ostream &EnumOS = searchableTablesGetOS(ST_ENUM_SYSOPS_OS);
  EnumOS << "#endif\n\n";
}

} // end namespace llvm
