//===------------ PrinterLLVM.cpp - LLVM C++ code printer -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implementation of the LLVM C++ printer.
//
//===----------------------------------------------------------------------===//

#include "AsmWriterInst.h"
#include "CodeGenSchedule.h"
#include "Printer.h"
#include "PrinterTypes.h"
#include "SubtargetEmitterTypes.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/TableGen/TableGenBackend.h"

namespace llvm {

cl::OptionCategory PrinterLang("Select output language of backends");

static cl::opt<std::string>
    PrinterLangOpt("printerLang", cl::init("C++"),
                   cl::desc("Output language options: \"C++\" (default), "
                            "\"CCS\" (C Capstone style)"),
                   cl::cat(PrinterLang));

// Print a BitVector as a sequence of hex numbers using a little-endian mapping.
// Width is the number of bits per hex number.
void printBitVectorAsHex(raw_ostream &OS, const BitVector &Bits,
                         unsigned Width) {
  assert(Width <= 32 && "Width too large");
  unsigned const Digits = (Width + 3) / 4;
  for (unsigned I = 0, E = Bits.size(); I < E; I += Width) {
    unsigned Value = 0;
    for (unsigned J = 0; J != Width && I + J != E; ++J)
      Value |= Bits.test(I + J) << J;
    OS << format("0x%0*x, ", Digits, Value);
  }
}

void PrinterBitVectorEmitter::add(unsigned V) {
  if (V >= Values.size())
    Values.resize(((V / 8) + 1) * 8); // Round up to the next byte.
  Values[V] = true;
}

void PrinterBitVectorEmitter::print(raw_ostream &OS) {
  printBitVectorAsHex(OS, Values, 8);
}

//
// General PrinterLLVM methods
//

PrinterLLVM::PrinterLLVM(formatted_raw_ostream &OS) : OS(OS) {}
PrinterLLVM::PrinterLLVM(formatted_raw_ostream &OS, std::string TargetName)
    : OS(OS), TargetName(TargetName) {}
PrinterLLVM::PrinterLLVM(formatted_raw_ostream &OS,
                         std::string PredicateNamespace, std::string GPrefix,
                         std::string GPostfix, std::string ROK,
                         std::string RFail, std::string L, std::string Target)
    : OS(OS), TargetName(std::move(Target)),
      PredicateNamespace(std::move(PredicateNamespace)),
      GuardPrefix(std::move(GPrefix)), GuardPostfix(std::move(GPostfix)),
      ReturnOK(std::move(ROK)), ReturnFail(std::move(RFail)),
      Locals(std::move(L)) {}
PrinterLLVM::~PrinterLLVM() {}

PrinterLanguage PrinterLLVM::getLanguage() {
  if (PrinterLangOpt == "C++")
    return PRINTER_LANG_CPP;
  if (PrinterLangOpt == "CCS")
    return PRINTER_LANG_CAPSTONE_C;

  PrintFatalNote("Unkown output language for printer selected.");
}

/// Prints `namespace <name> {` and `} // end namespace <name>` to the output
/// stream. If Name == "" it emits an anonymous namespace.
void PrinterLLVM::emitNamespace(std::string const &Name, bool Begin,
                                std::string const &Comment) const {
  if (Begin) {
    OS << "namespace " << Name;
    std::string const Bracket = (Name == "" ? "{" : " {");
    if (Comment != "")
      OS << Bracket << " // " << Comment << "\n";
    else
      OS << Bracket << "\n\n";
    return;
  }

  if (Name == "") {
    OS << "} // end anonymous namespace\n\n";
  } else {
    OS << "} // end namespace " << Name << "\n\n";
  }
}

/// Prints
/// ```
/// #ifdef <Name>
/// #undef <Name>
/// ```
/// and
/// `#endif // <Name>`
/// Used to control inclusion of a code block via a macro definition.
void PrinterLLVM::emitIncludeToggle(std::string const &Name, bool Begin,
                                    bool Newline, bool UndefAtEnd) const {
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

void PrinterLLVM::regInfoEmitSourceFileHeader(std::string const &Desc) const {
  emitSourceFileHeader(Desc, OS);
}

// runEnums - Print out enum values for all of the registers.
void PrinterLLVM::regInfoEmitEnums(CodeGenTarget const &Target,
                                   CodeGenRegBank const &Bank) const {
  const auto &Registers = Bank.getRegisters();

  // Register enums are stored as uint16_t in the tables. Make sure we'll fit.
  assert(Registers.size() <= 0xffff && "Too many regs to fit in tables");

  StringRef const Namespace =
      Registers.front().TheDef->getValueAsString("Namespace");

  OS << "\n#ifdef GET_REGINFO_ENUM\n";
  OS << "#undef GET_REGINFO_ENUM\n\n";

  OS << "namespace llvm {\n\n";

  OS << "class MCRegisterClass;\n"
     << "extern const MCRegisterClass " << Target.getName()
     << "MCRegisterClasses[];\n\n";

  if (!Namespace.empty())
    OS << "namespace " << Namespace << " {\n";
  OS << "enum {\n  NoRegister,\n";

  for (const auto &Reg : Registers)
    OS << "  " << Reg.getName() << " = " << Reg.EnumValue << ",\n";
  assert(Registers.size() == Registers.back().EnumValue &&
         "Register enum value mismatch!");
  OS << "  NUM_TARGET_REGS // " << Registers.size() + 1 << "\n";
  OS << "};\n";
  if (!Namespace.empty())
    OS << "} // end namespace " << Namespace << "\n";

  const auto &RegisterClasses = Bank.getRegClasses();
  if (!RegisterClasses.empty()) {

    // RegisterClass enums are stored as uint16_t in the tables.
    assert(RegisterClasses.size() <= 0xffff &&
           "Too many register classes to fit in tables");

    OS << "\n// Register classes\n\n";
    if (!Namespace.empty())
      OS << "namespace " << Namespace << " {\n";
    OS << "enum {\n";
    for (const auto &RC : RegisterClasses)
      OS << "  " << RC.getName() << "RegClassID"
         << " = " << RC.EnumValue << ",\n";
    OS << "\n};\n";
    if (!Namespace.empty())
      OS << "} // end namespace " << Namespace << "\n\n";
  }

  const std::vector<Record *> &RegAltNameIndices =
      Target.getRegAltNameIndices();
  // If the only definition is the default NoRegAltName, we don't need to
  // emit anything.
  if (RegAltNameIndices.size() > 1) {
    OS << "\n// Register alternate name indices\n\n";
    if (!Namespace.empty())
      OS << "namespace " << Namespace << " {\n";
    OS << "enum {\n";
    for (unsigned I = 0, E = RegAltNameIndices.size(); I != E; ++I)
      OS << "  " << RegAltNameIndices[I]->getName() << ",\t// " << I << "\n";
    OS << "  NUM_TARGET_REG_ALT_NAMES = " << RegAltNameIndices.size() << "\n";
    OS << "};\n";
    if (!Namespace.empty())
      OS << "} // end namespace " << Namespace << "\n\n";
  }

  auto &SubRegIndices = Bank.getSubRegIndices();
  if (!SubRegIndices.empty()) {
    OS << "\n// Subregister indices\n\n";
    std::string const Namespace = SubRegIndices.front().getNamespace();
    if (!Namespace.empty())
      OS << "namespace " << Namespace << " {\n";
    OS << "enum : uint16_t {\n  NoSubRegister,\n";
    unsigned I = 0;
    for (const auto &Idx : SubRegIndices)
      OS << "  " << Idx.getName() << ",\t// " << ++I << "\n";
    OS << "  NUM_TARGET_SUBREGS\n};\n";
    if (!Namespace.empty())
      OS << "} // end namespace " << Namespace << "\n\n";
  }

  OS << "// Register pressure sets enum.\n";
  if (!Namespace.empty())
    OS << "namespace " << Namespace << " {\n";
  OS << "enum RegisterPressureSets {\n";
  unsigned const NumSets = Bank.getNumRegPressureSets();
  for (unsigned I = 0; I < NumSets; ++I) {
    const RegUnitSet &RegUnits = Bank.getRegSetAt(I);
    OS << "  " << RegUnits.Name << " = " << I << ",\n";
  }
  OS << "};\n";
  if (!Namespace.empty())
    OS << "} // end namespace " << Namespace << '\n';
  OS << '\n';

  OS << "} // end namespace llvm\n\n";
  OS << "#endif // GET_REGINFO_ENUM\n\n";
}

void PrinterLLVM::regInfoEmitRegDiffLists(
    std::string const TargetName,
    SequenceToOffsetTable<DiffVec> const &DiffSeqs) const {
  OS << "extern const MCPhysReg " << TargetName << "RegDiffLists[] = {\n";
  DiffSeqs.emit(OS, [](raw_ostream &OS, uint16_t Val) { OS << Val; });
  OS << "};\n\n";
}

static void regInfoPrintMask(raw_ostream &OS, LaneBitmask Val) {
  OS << "LaneBitmask(0x" << PrintLaneMask(Val) << ')';
}

void PrinterLLVM::regInfoEmitLaneMaskLists(
    std::string const TargetName,
    SequenceToOffsetTable<MaskVec> const &LaneMaskSeqs) const {
  OS << "extern const LaneBitmask " << TargetName << "LaneMaskLists[] = {\n";
  LaneMaskSeqs.emit(OS, regInfoPrintMask, "LaneBitmask::getAll()");
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitSubRegIdxLists(
    std::string const TargetName,
    SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> const
        &SubRegIdxSeqs) const {
  OS << "extern const uint16_t " << TargetName << "SubRegIdxLists[] = {\n";
  SubRegIdxSeqs.emit(OS, [](raw_ostream &OS, const CodeGenSubRegIndex *Idx) {
    OS << Idx->EnumValue;
  });
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitSubRegIdxSizes(
    std::string const TargetName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "extern const MCRegisterInfo::SubRegCoveredBits " << TargetName
     << "SubRegIdxRanges[] = {\n";
  OS << "  { " << (uint16_t)-1 << ", " << (uint16_t)-1 << " },\n";
  for (const auto &Idx : SubRegIndices) {
    OS << "  { " << Idx.Offset << ", " << Idx.Size << " },\t// "
       << Idx.getName() << "\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitSubRegStrTable(
    std::string const TargetName,
    SequenceToOffsetTable<std::string> const &RegStrings) const {
  RegStrings.emitStringLiteralDef(OS, Twine("extern const char ") + TargetName +
                                          "RegStrings[]");

  OS << "extern const MCRegisterDesc " << TargetName
     << "RegDesc[] = { // Descriptors\n";
  OS << "  { " << RegStrings.get("") << ", 0, 0, 0, 0, 0 },\n";
}

void PrinterLLVM::regInfoEmitRegDesc(
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

void PrinterLLVM::regInfoEmitRegUnitRoots(std::string const TargetName,
                                          CodeGenRegBank const &RegBank) const {
  OS << "extern const MCPhysReg " << TargetName << "RegUnitRoots[][2] = {\n";
  for (unsigned I = 0, E = RegBank.getNumNativeRegUnits(); I != E; ++I) {
    ArrayRef<const CodeGenRegister *> const Roots =
        RegBank.getRegUnit(I).getRoots();
    assert(!Roots.empty() && "All regunits must have a root register.");
    assert(Roots.size() <= 2 && "More than two roots not supported yet.");
    OS << "  { ";
    ListSeparator LS;
    for (const CodeGenRegister *R : Roots)
      OS << LS << getQualifiedName(R->TheDef);
    OS << " },\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitRegClasses(
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
         << "  const MCPhysReg " << Name << "[] = {\n    ";
      for (Record *Reg : Order) {
        OS << getQualifiedName(Reg) << ", ";
      }
      OS << "\n  };\n\n";

      OS << "  // " << Name << " Bit set.\n"
         << "  const uint8_t " << Name << "Bits[] = {\n    ";
      PrinterBitVectorEmitter BVE;
      for (Record *Reg : Order) {
        BVE.add(Target.getRegBank().getReg(Reg)->EnumValue);
      }
      BVE.print(OS);
      OS << "\n  };\n\n";
    }
  }
}

void PrinterLLVM::regInfoEmitStrLiteralRegClasses(
    std::string const TargetName,
    SequenceToOffsetTable<std::string> const &RegClassStrings) const {
  RegClassStrings.emitStringLiteralDef(
      OS, Twine("extern const char ") + TargetName + "RegClassStrings[]");
}

void PrinterLLVM::regInfoEmitMCRegClassesTable(
    std::string const TargetName,
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<std::string> &RegClassStrings) const {
  OS << "extern const MCRegisterClass " << TargetName
     << "MCRegisterClasses[] = {\n";

  for (const auto &RC : RegClasses) {
    ArrayRef<Record *> const Order = RC.getOrder();
    std::string const RCName = Order.empty() ? "nullptr" : RC.getName();
    std::string const RCBitsName =
        Order.empty() ? "nullptr" : RC.getName() + "Bits";
    std::string const RCBitsSize =
        Order.empty() ? "0" : "sizeof(" + RCBitsName + ")";
    assert(isInt<8>(RC.CopyCost) && "Copy cost too large.");
    uint32_t RegSize = 0;
    if (RC.RSI.isSimple())
      RegSize = RC.RSI.getSimple().RegSize;
    OS << "  { " << RCName << ", " << RCBitsName << ", "
       << RegClassStrings.get(RC.getName()) << ", " << RC.getOrder().size()
       << ", " << RCBitsSize << ", " << RC.getQualifiedName() + "RegClassID"
       << ", " << RegSize << ", " << RC.CopyCost << ", "
       << (RC.Allocatable ? "true" : "false") << " },\n";
  }

  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitRegEncodingTable(
    std::string const TargetName,
    std::deque<CodeGenRegister> const &Regs) const {
  OS << "extern const uint16_t " << TargetName;
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
}

void PrinterLLVM::regInfoEmitMCRegInfoInit(
    std::string const TargetName, CodeGenRegBank const &RegBank,
    std::deque<CodeGenRegister> const &Regs,
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "static inline void Init" << TargetName
     << "MCRegisterInfo(MCRegisterInfo *RI, unsigned RA, "
     << "unsigned DwarfFlavour = 0, unsigned EHFlavour = 0, unsigned PC = 0) "
        "{\n"
     << "  RI->InitMCRegisterInfo(" << TargetName << "RegDesc, "
     << Regs.size() + 1 << ", RA, PC, " << TargetName << "MCRegisterClasses, "
     << RegClasses.size() << ", " << TargetName << "RegUnitRoots, "
     << RegBank.getNumNativeRegUnits() << ", " << TargetName << "RegDiffLists, "
     << TargetName << "LaneMaskLists, " << TargetName << "RegStrings, "
     << TargetName << "RegClassStrings, " << TargetName << "SubRegIdxLists, "
     << (std::distance(SubRegIndices.begin(), SubRegIndices.end()) + 1) << ",\n"
     << TargetName << "SubRegIdxRanges, " << TargetName
     << "RegEncodingTable);\n\n";
}

void PrinterLLVM::regInfoEmitInfoDwarfRegsRev(StringRef const &Namespace,
                                              DwarfRegNumsVecTy &DwarfRegNums,
                                              unsigned MaxLength,
                                              bool IsCtor) const {
  OS << "// " << Namespace << " Dwarf<->LLVM register mappings.\n";

  // Emit reverse information about the dwarf register numbers.
  for (unsigned j = 0; j < 2; ++j) {
    for (unsigned I = 0, E = MaxLength; I != E; ++I) {
      OS << "extern const MCRegisterInfo::DwarfLLVMRegPair " << Namespace;
      OS << (j == 0 ? "DwarfFlavour" : "EHFlavour");
      OS << I << "Dwarf2L[]";

      if (!IsCtor) {
        OS << " = {\n";

        // Store the mapping sorted by the LLVM reg num so lookup can be done
        // with a binary search.
        std::map<uint64_t, Record *> Dwarf2LMap;
        for (auto &DwarfRegNum : DwarfRegNums) {
          int DwarfRegNo = DwarfRegNum.second[I];
          if (DwarfRegNo < 0)
            continue;
          Dwarf2LMap[DwarfRegNo] = DwarfRegNum.first;
        }

        for (auto &I : Dwarf2LMap)
          OS << "  { " << I.first << "U, " << getQualifiedName(I.second)
             << " },\n";

        OS << "};\n";
      } else {
        OS << ";\n";
      }

      // We have to store the size in a const global, it's used in multiple
      // places.
      OS << "extern const unsigned " << Namespace
         << (j == 0 ? "DwarfFlavour" : "EHFlavour") << I << "Dwarf2LSize";
      if (!IsCtor)
        OS << " = std::size(" << Namespace
           << (j == 0 ? "DwarfFlavour" : "EHFlavour") << I << "Dwarf2L);\n\n";
      else
        OS << ";\n\n";
    }
  }
}

void PrinterLLVM::regInfoEmitInfoDwarfRegs(StringRef const &Namespace,
                                           DwarfRegNumsVecTy &DwarfRegNums,
                                           unsigned MaxLength,
                                           bool IsCtor) const {
  for (unsigned J = 0; J < 2; ++J) {
    for (unsigned I = 0, E = MaxLength; I != E; ++I) {
      OS << "extern const MCRegisterInfo::DwarfLLVMRegPair " << Namespace;
      OS << (J == 0 ? "DwarfFlavour" : "EHFlavour");
      OS << I << "L2Dwarf[]";
      if (!IsCtor) {
        OS << " = {\n";
        // Store the mapping sorted by the Dwarf reg num so lookup can be done
        // with a binary search.
        for (auto &DwarfRegNum : DwarfRegNums) {
          int RegNo = DwarfRegNum.second[I];
          if (RegNo == -1) // -1 is the default value, don't emit a mapping.
            continue;

          OS << "  { " << getQualifiedName(DwarfRegNum.first) << ", " << RegNo
             << "U },\n";
        }
        OS << "};\n";
      } else {
        OS << ";\n";
      }

      // We have to store the size in a const global, it's used in multiple
      // places.
      OS << "extern const unsigned " << Namespace
         << (J == 0 ? "DwarfFlavour" : "EHFlavour") << I << "L2DwarfSize";
      if (!IsCtor)
        OS << " = std::size(" << Namespace
           << (J == 0 ? "DwarfFlavour" : "EHFlavour") << I << "L2Dwarf);\n\n";
      else
        OS << ";\n\n";
    }
  }
}

void PrinterLLVM::regInfoEmitInfoRegMapping(StringRef const &Namespace,
                                            unsigned MaxLength,
                                            bool IsCtor) const {
  if (MaxLength == 0) {
    OS << "}\n\n";
    return;
  }

  // Emit reverse information about the dwarf register numbers.
  for (unsigned J = 0; J < 2; ++J) {
    OS << "  switch (";
    if (J == 0)
      OS << "DwarfFlavour";
    else
      OS << "EHFlavour";
    OS << ") {\n"
       << "  default:\n"
       << "    llvm_unreachable(\"Unknown DWARF flavour\");\n";

    for (unsigned I = 0, E = MaxLength; I != E; ++I) {
      OS << "  case " << I << ":\n";
      OS << "    ";
      if (!IsCtor)
        OS << "RI->";
      std::string Tmp;
      raw_string_ostream(Tmp)
          << Namespace << (J == 0 ? "DwarfFlavour" : "EHFlavour") << I
          << "Dwarf2L";
      OS << "mapDwarfRegsToLLVMRegs(" << Tmp << ", " << Tmp << "Size, ";
      if (J == 0)
        OS << "false";
      else
        OS << "true";
      OS << ");\n";
      OS << "    break;\n";
    }
    OS << "  }\n";
  }

  // Emit information about the dwarf register numbers.
  for (unsigned J = 0; J < 2; ++J) {
    OS << "  switch (";
    if (J == 0)
      OS << "DwarfFlavour";
    else
      OS << "EHFlavour";
    OS << ") {\n"
       << "  default:\n"
       << "    llvm_unreachable(\"Unknown DWARF flavour\");\n";

    for (unsigned I = 0, E = MaxLength; I != E; ++I) {
      OS << "  case " << I << ":\n";
      OS << "    ";
      if (!IsCtor)
        OS << "RI->";
      std::string Tmp;
      raw_string_ostream(Tmp)
          << Namespace << (J == 0 ? "DwarfFlavour" : "EHFlavour") << I
          << "L2Dwarf";
      OS << "mapLLVMRegsToDwarfRegs(" << Tmp << ", " << Tmp << "Size, ";
      if (J == 0)
        OS << "false";
      else
        OS << "true";
      OS << ");\n";
      OS << "    break;\n";
    }
    OS << "  }\n";
  }

  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitHeaderIncludes() const {
  OS << "#include \"llvm/CodeGen/TargetRegisterInfo.h\"\n\n";
}

void PrinterLLVM::regInfoEmitHeaderExternRegClasses(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  for (const auto &RC : RegClasses) {
    const std::string &Name = RC.getName();

    // Output the extern for the instance.
    OS << "  extern const TargetRegisterClass " << Name << "RegClass;\n";
  }
}

void PrinterLLVM::regInfoEmitHeaderDecl(std::string const &TargetName,
                                        std::string const &ClassName,
                                        bool SubRegsPresent,
                                        bool DeclareGetPhysRegBaseClass) const {
  OS << "class " << TargetName << "FrameLowering;\n\n";

  OS << "struct " << ClassName << " : public TargetRegisterInfo {\n"
     << "  explicit " << ClassName
     << "(unsigned RA, unsigned D = 0, unsigned E = 0,\n"
     << "      unsigned PC = 0, unsigned HwMode = 0);\n";
  if (SubRegsPresent) {
    OS << "  unsigned composeSubRegIndicesImpl"
       << "(unsigned, unsigned) const override;\n"
       << "  LaneBitmask composeSubRegIndexLaneMaskImpl"
       << "(unsigned, LaneBitmask) const override;\n"
       << "  LaneBitmask reverseComposeSubRegIndexLaneMaskImpl"
       << "(unsigned, LaneBitmask) const override;\n"
       << "  const TargetRegisterClass *getSubClassWithSubReg"
       << "(const TargetRegisterClass *, unsigned) const override;\n"
       << "  const TargetRegisterClass *getSubRegisterClass"
       << "(const TargetRegisterClass *, unsigned) const override;\n";
  }
  OS << "  const RegClassWeight &getRegClassWeight("
     << "const TargetRegisterClass *RC) const override;\n"
     << "  unsigned getRegUnitWeight(unsigned RegUnit) const override;\n"
     << "  unsigned getNumRegPressureSets() const override;\n"
     << "  const char *getRegPressureSetName(unsigned Idx) const override;\n"
     << "  unsigned getRegPressureSetLimit(const MachineFunction &MF, unsigned "
        "Idx) const override;\n"
     << "  const int *getRegClassPressureSets("
     << "const TargetRegisterClass *RC) const override;\n"
     << "  const int *getRegUnitPressureSets("
     << "unsigned RegUnit) const override;\n"
     << "  ArrayRef<const char *> getRegMaskNames() const override;\n"
     << "  ArrayRef<const uint32_t *> getRegMasks() const override;\n"
     << "  bool isGeneralPurposeRegister(const MachineFunction &, "
     << "MCRegister) const override;\n"
     << "  bool isFixedRegister(const MachineFunction &, "
     << "MCRegister) const override;\n"
     << "  bool isArgumentRegister(const MachineFunction &, "
     << "MCRegister) const override;\n"
     << "  bool isConstantPhysReg(MCRegister PhysReg) const override final;\n"
     << "  /// Devirtualized TargetFrameLowering.\n"
     << "  static const " << TargetName << "FrameLowering *getFrameLowering(\n"
     << "      const MachineFunction &MF);\n";
  if (DeclareGetPhysRegBaseClass) {
    OS << "  const TargetRegisterClass *getPhysRegBaseClass(MCRegister Reg) "
          "const override;\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitExternRegClassesArr(
    std::string const &TargetName) const {
  OS << "extern const MCRegisterClass " << TargetName
     << "MCRegisterClasses[];\n";
}

static void printSimpleValueType(raw_ostream &OS, MVT::SimpleValueType VT) {
  OS << getEnumName(VT);
}

void PrinterLLVM::regInfoEmitVTSeqs(
    SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs)
    const {
  OS << "\nstatic const MVT::SimpleValueType VTLists[] = {\n";
  VTSeqs.emit(OS, printSimpleValueType, "MVT::Other");
  OS << "};\n";
}

static void printMask(raw_ostream &OS, LaneBitmask Val) {
  OS << "LaneBitmask(0x" << PrintLaneMask(Val) << ')';
}

void PrinterLLVM::regInfoEmitSubRegIdxTable(
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "\nstatic const char *SubRegIndexNameTable[] = { \"";

  for (const auto &Idx : SubRegIndices) {
    OS << Idx.getName();
    OS << "\", \"";
  }
  OS << "\" };\n\n";

  // Emit SubRegIndex lane masks, including 0.
  OS << "\nstatic const LaneBitmask SubRegIndexLaneMaskTable[] = {\n  "
        "LaneBitmask::getAll(),\n";
  for (const auto &Idx : SubRegIndices) {
    printMask(OS << "  ", Idx.LaneMask);
    OS << ", // " << Idx.getName() << '\n';
  }
  OS << " };\n\n";

  OS << "\n";
}

void PrinterLLVM::regInfoEmitRegClassInfoTable(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs,
    CodeGenHwModes const &CGH, unsigned NumModes) const {
  OS << "\nstatic const TargetRegisterInfo::RegClassInfo RegClassInfos[]"
     << " = {\n";
  for (unsigned M = 0; M < NumModes; ++M) {
    unsigned EV = 0;
    OS << "  // Mode = " << M << " (";
    if (M == 0)
      OS << "Default";
    else
      OS << CGH.getMode(M).Name;
    OS << ")\n";
    for (const auto &RC : RegClasses) {
      assert(RC.EnumValue == EV && "Unexpected order of register classes");
      ++EV;
      (void)EV;
      const RegSizeInfo &RI = RC.RSI.get(M);
      OS << "  { " << RI.RegSize << ", " << RI.SpillSize << ", "
         << RI.SpillAlignment;
      std::vector<MVT::SimpleValueType> VTs;
      for (const ValueTypeByHwMode &VVT : RC.VTs)
        VTs.push_back(VVT.get(M).SimpleTy);
      OS << ", VTLists+" << VTSeqs.get(VTs) << " },    // " << RC.getName()
         << '\n';
    }
  }
  OS << "};\n";

  OS << "\nstatic const TargetRegisterClass *const "
     << "NullRegClasses[] = { nullptr };\n\n";
}

void PrinterLLVM::regInfoEmitSubClassMaskTable(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SmallVector<IdxList, 8> &SuperRegIdxLists,
    SequenceToOffsetTable<IdxList, deref<std::less<>>> &SuperRegIdxSeqs,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices,
    BitVector &MaskBV) const {
  for (const auto &RC : RegClasses) {
    OS << "static const uint32_t " << RC.getName() << "SubClassMask[] = {\n  ";
    printBitVectorAsHex(OS, RC.getSubClasses(), 32);

    // Emit super-reg class masks for any relevant SubRegIndices that can
    // project into RC.
    IdxList &SRIList = SuperRegIdxLists[RC.EnumValue];
    for (auto &Idx : SubRegIndices) {
      MaskBV.reset();
      RC.getSuperRegClasses(&Idx, MaskBV);
      if (MaskBV.none())
        continue;
      SRIList.push_back(&Idx);
      OS << "\n  ";
      printBitVectorAsHex(OS, MaskBV, 32);
      OS << "// " << Idx.getName();
    }
    SuperRegIdxSeqs.add(SRIList);
    OS << "\n};\n\n";
  }
}

static void printSubRegIndex(raw_ostream &OS, const CodeGenSubRegIndex *Idx) {
  OS << Idx->EnumValue;
}

void PrinterLLVM::regInfoEmitSuperRegIdxSeqsTable(
    SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs)
    const {
  OS << "static const uint16_t SuperRegIdxSeqs[] = {\n";
  SuperRegIdxSeqs.emit(OS, printSubRegIndex);
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitSuperClassesTable(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  for (const auto &RC : RegClasses) {
    ArrayRef<CodeGenRegisterClass *> const Supers = RC.getSuperClasses();

    // Skip classes without supers.  We can reuse NullRegClasses.
    if (Supers.empty())
      continue;

    OS << "static const TargetRegisterClass *const " << RC.getName()
       << "Superclasses[] = {\n";
    for (const auto *Super : Supers)
      OS << "  &" << Super->getQualifiedName() << "RegClass,\n";
    OS << "  nullptr\n};\n\n";
  }
}

void PrinterLLVM::regInfoEmitRegClassMethods(
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::string const &TargetName) const {
    for (const auto &RC : RegClasses) {
      if (!RC.AltOrderSelect.empty()) {
        OS << "\nstatic inline unsigned " << RC.getName()
           << "AltOrderSelect(const MachineFunction &MF) {"
           << RC.AltOrderSelect << "}\n\n"
           << "static ArrayRef<MCPhysReg> " << RC.getName()
           << "GetRawAllocationOrder(const MachineFunction &MF) {\n";
        for (unsigned oi = 1 , oe = RC.getNumOrders(); oi != oe; ++oi) {
          ArrayRef<Record*> Elems = RC.getOrder(oi);
          if (!Elems.empty()) {
            OS << "  static const MCPhysReg AltOrder" << oi << "[] = {";
            for (unsigned elem = 0; elem != Elems.size(); ++elem)
              OS << (elem ? ", " : " ") << getQualifiedName(Elems[elem]);
            OS << " };\n";
          }
        }
        OS << "  const MCRegisterClass &MCR = " << TargetName
           << "MCRegisterClasses[" << RC.getQualifiedName() + "RegClassID];\n"
           << "  const ArrayRef<MCPhysReg> Order[] = {\n"
           << "    ArrayRef(MCR.begin(), MCR.getNumRegs()";
        for (unsigned oi = 1, oe = RC.getNumOrders(); oi != oe; ++oi)
          if (RC.getOrder(oi).empty())
            OS << "),\n    ArrayRef<MCPhysReg>(";
          else
            OS << "),\n    ArrayRef(AltOrder" << oi;
        OS << ")\n  };\n  const unsigned Select = " << RC.getName()
           << "AltOrderSelect(MF);\n  assert(Select < " << RC.getNumOrders()
           << ");\n  return Order[Select];\n}\n";
      }
    }
}

void PrinterLLVM::regInfomitRegClassInstances(
    std::list<CodeGenRegisterClass> const &RegClasses,
    SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs,
    SmallVector<IdxList, 8> const &SuperRegIdxLists,
    std::string const &TargetName) const {
  for (const auto &RC : RegClasses) {
    OS << "  extern const TargetRegisterClass " << RC.getName()
       << "RegClass = {\n    " << '&' << TargetName << "MCRegisterClasses["
       << RC.getName() << "RegClassID],\n    " << RC.getName()
       << "SubClassMask,\n    SuperRegIdxSeqs + "
       << SuperRegIdxSeqs.get(SuperRegIdxLists[RC.EnumValue]) << ",\n    ";
    printMask(OS, RC.LaneMask);
    OS << ",\n    " << (unsigned)RC.AllocationPriority << ",\n    "
       << (RC.GlobalPriority ? "true" : "false") << ",\n    "
       << format("0x%02x", RC.TSFlags) << ", /* TSFlags */\n    "
       << (RC.HasDisjunctSubRegs ? "true" : "false")
       << ", /* HasDisjunctSubRegs */\n    "
       << (RC.CoveredBySubRegs ? "true" : "false")
       << ", /* CoveredBySubRegs */\n    ";
    if (RC.getSuperClasses().empty())
      OS << "NullRegClasses,\n    ";
    else
      OS << RC.getName() << "Superclasses,\n    ";
    if (RC.AltOrderSelect.empty())
      OS << "nullptr\n";
    else
      OS << RC.getName() << "GetRawAllocationOrder\n";
    OS << "  };\n\n";
  }
}

void PrinterLLVM::regInfoEmitRegClassTable(
    std::list<CodeGenRegisterClass> const &RegClasses) const {
  OS << "  const TargetRegisterClass *const RegisterClasses[] = {\n";
  for (const auto &RC : RegClasses)
    OS << "    &" << RC.getQualifiedName() << "RegClass,\n";
  OS << "  };\n";
}

void PrinterLLVM::regInfoEmitCostPerUseTable(
    std::vector<unsigned> const &AllRegCostPerUse, unsigned NumRegCosts) const {
  OS << "\nstatic const uint8_t "
     << "CostPerUseTable[] = { \n";
  for (unsigned int I = 0; I < NumRegCosts; ++I) {
    for (unsigned J = I, E = AllRegCostPerUse.size(); J < E; J += NumRegCosts)
      OS << AllRegCostPerUse[J] << ", ";
  }
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitInAllocatableClassTable(
    llvm::BitVector const &InAllocClass) const {
  OS << "\nstatic const bool "
     << "InAllocatableClassTable[] = { \n";
  for (unsigned I = 0, E = InAllocClass.size(); I < E; ++I) {
    OS << (InAllocClass[I] ? "true" : "false") << ", ";
  }
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitRegExtraDesc(std::string const &TargetName,
                                          unsigned NumRegCosts) const {
  OS << "\nstatic const TargetRegisterInfoDesc " << TargetName
     << "RegInfoDesc = { // Extra Descriptors\n";
  OS << "CostPerUseTable, " << NumRegCosts << ", "
     << "InAllocatableClassTable";
  OS << "};\n\n";
}

void PrinterLLVM::regInfoEmitSubClassSubRegGetter(
    std::string const &ClassName, unsigned SubRegIndicesSize,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices,
    std::list<CodeGenRegisterClass> const &RegClasses,
    CodeGenRegBank &RegBank) const {
  // Emit getSubClassWithSubReg.
  OS << "const TargetRegisterClass *" << ClassName
     << "::getSubClassWithSubReg(const TargetRegisterClass *RC, unsigned Idx)"
     << " const {\n";
  // Use the smallest type that can hold a regclass ID with room for a
  // sentinel.
  if (RegClasses.size() <= UINT8_MAX)
    OS << "  static const uint8_t Table[";
  else if (RegClasses.size() <= UINT16_MAX)
    OS << "  static const uint16_t Table[";
  else
    PrintFatalError("Too many register classes.");
  OS << RegClasses.size() << "][" << SubRegIndicesSize << "] = {\n";
  for (const auto &RC : RegClasses) {
    OS << "    {\t// " << RC.getName() << "\n";
    for (auto &Idx : SubRegIndices) {
      if (CodeGenRegisterClass *SRC = RC.getSubClassWithSubReg(&Idx))
        OS << "      " << SRC->EnumValue + 1 << ",\t// " << Idx.getName()
           << " -> " << SRC->getName() << "\n";
      else
        OS << "      0,\t// " << Idx.getName() << "\n";
    }
    OS << "    },\n";
  }
  OS << "  };\n  assert(RC && \"Missing regclass\");\n"
     << "  if (!Idx) return RC;\n  --Idx;\n"
     << "  assert(Idx < " << SubRegIndicesSize << " && \"Bad subreg\");\n"
     << "  unsigned TV = Table[RC->getID()][Idx];\n"
     << "  return TV ? getRegClass(TV - 1) : nullptr;\n}\n\n";

  // Emit getSubRegisterClass
  OS << "const TargetRegisterClass *" << ClassName
     << "::getSubRegisterClass(const TargetRegisterClass *RC, unsigned Idx)"
     << " const {\n";

  // Use the smallest type that can hold a regclass ID with room for a
  // sentinel.
  if (RegClasses.size() <= UINT8_MAX)
    OS << "  static const uint8_t Table[";
  else if (RegClasses.size() <= UINT16_MAX)
    OS << "  static const uint16_t Table[";
  else
    PrintFatalError("Too many register classes.");

  OS << RegClasses.size() << "][" << SubRegIndicesSize << "] = {\n";

  for (const auto &RC : RegClasses) {
    OS << "    {\t// " << RC.getName() << '\n';
    for (auto &Idx : SubRegIndices) {
      std::optional<std::pair<CodeGenRegisterClass *, CodeGenRegisterClass *>>
          MatchingSubClass = RC.getMatchingSubClassWithSubRegs(RegBank, &Idx);

      unsigned EnumValue = 0;
      if (MatchingSubClass) {
        CodeGenRegisterClass *SubRegClass = MatchingSubClass->second;
        EnumValue = SubRegClass->EnumValue + 1;
      }

      OS << "      " << EnumValue << ",\t// " << RC.getName() << ':'
         << Idx.getName();

      if (MatchingSubClass) {
        CodeGenRegisterClass *SubRegClass = MatchingSubClass->second;
        OS << " -> " << SubRegClass->getName();
      }

      OS << '\n';
    }

    OS << "    },\n";
  }
  OS << "  };\n  assert(RC && \"Missing regclass\");\n"
     << "  if (!Idx) return RC;\n  --Idx;\n"
     << "  assert(Idx < " << SubRegIndicesSize << " && \"Bad subreg\");\n"
     << "  unsigned TV = Table[RC->getID()][Idx];\n"
     << "  return TV ? getRegClass(TV - 1) : nullptr;\n}\n\n";
}

void PrinterLLVM::regInfoEmitRegClassWeight(
    CodeGenRegBank const &RegBank, std::string const &ClassName) const {
  OS << "/// Get the weight in units of pressure for this register class.\n"
     << "const RegClassWeight &" << ClassName << "::\n"
     << "getRegClassWeight(const TargetRegisterClass *RC) const {\n"
     << "  static const RegClassWeight RCWeightTable[] = {\n";
  for (const auto &RC : RegBank.getRegClasses()) {
    const CodeGenRegister::Vec &Regs = RC.getMembers();
    OS << "    {" << RC.getWeight(RegBank) << ", ";
    if (Regs.empty() || RC.Artificial)
      OS << '0';
    else {
      std::vector<unsigned> RegUnits;
      RC.buildRegUnitSet(RegBank, RegUnits);
      OS << RegBank.getRegUnitSetWeight(RegUnits);
    }
    OS << "},  \t// " << RC.getName() << "\n";
  }
  OS << "  };\n"
     << "  return RCWeightTable[RC->getID()];\n"
     << "}\n\n";
}

void PrinterLLVM::regInfoEmitRegUnitWeight(CodeGenRegBank const &RegBank,
                                           std::string const &ClassName,
                                           bool RegUnitsHaveUnitWeight) const {
  OS << "/// Get the weight in units of pressure for this register unit.\n"
     << "unsigned " << ClassName << "::\n"
     << "getRegUnitWeight(unsigned RegUnit) const {\n"
     << "  assert(RegUnit < " << RegBank.getNumNativeRegUnits()
     << " && \"invalid register unit\");\n";
  if (!RegUnitsHaveUnitWeight) {
    OS << "  static const uint8_t RUWeightTable[] = {\n    ";
    for (unsigned UnitIdx = 0, UnitEnd = RegBank.getNumNativeRegUnits();
         UnitIdx < UnitEnd; ++UnitIdx) {
      const RegUnit &RU = RegBank.getRegUnit(UnitIdx);
      assert(RU.Weight < 256 && "RegUnit too heavy");
      OS << RU.Weight << ", ";
    }
    OS << "};\n"
       << "  return RUWeightTable[RegUnit];\n";
  } else {
    OS << "  // All register units have unit weight.\n"
       << "  return 1;\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitGetNumRegPressureSets(std::string const &ClassName,
                                                   unsigned NumSets) const {
  OS << "\n"
     << "// Get the number of dimensions of register pressure.\n"
     << "unsigned " << ClassName << "::getNumRegPressureSets() const {\n"
     << "  return " << NumSets << ";\n}\n\n";
}

void PrinterLLVM::regInfoEmitGetRegPressureTables(CodeGenRegBank const &RegBank,
                                                  std::string const &ClassName,
                                                  unsigned NumSets) const {
  OS << "// Get the name of this register unit pressure set.\n"
     << "const char *" << ClassName << "::\n"
     << "getRegPressureSetName(unsigned Idx) const {\n"
     << "  static const char *PressureNameTable[] = {\n";
  unsigned MaxRegUnitWeight = 0;
  for (unsigned I = 0; I < NumSets; ++I) {
    const RegUnitSet &RegUnits = RegBank.getRegSetAt(I);
    MaxRegUnitWeight = std::max(MaxRegUnitWeight, RegUnits.Weight);
    OS << "    \"" << RegUnits.Name << "\",\n";
  }
  OS << "  };\n"
     << "  return PressureNameTable[Idx];\n"
     << "}\n\n";

  OS << "// Get the register unit pressure limit for this dimension.\n"
     << "// This limit must be adjusted dynamically for reserved registers.\n"
     << "unsigned " << ClassName << "::\n"
     << "getRegPressureSetLimit(const MachineFunction &MF, unsigned Idx) const "
        "{\n"
     << "  static const " << getMinimalTypeForRange(MaxRegUnitWeight, 32)
     << " PressureLimitTable[] = {\n";
  for (unsigned I = 0; I < NumSets; ++I) {
    const RegUnitSet &RegUnits = RegBank.getRegSetAt(I);
    OS << "    " << RegUnits.Weight << ",  \t// " << I << ": " << RegUnits.Name
       << "\n";
  }
  OS << "  };\n"
     << "  return PressureLimitTable[Idx];\n"
     << "}\n\n";
}

static void printInt(raw_ostream &OS, int Val) { OS << Val; }

void PrinterLLVM::regInfoEmitRCSetsTable(
    std::string const &ClassName, unsigned NumRCs,
    SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
    std::vector<std::vector<int>> const &PSets) const {
  OS << "/// Table of pressure sets per register class or unit.\n"
     << "static const int RCSetsTable[] = {\n";
  PSetsSeqs.emit(OS, printInt, "-1");
  OS << "};\n\n";

  OS << "/// Get the dimensions of register pressure impacted by this "
     << "register class.\n"
     << "/// Returns a -1 terminated array of pressure set IDs\n"
     << "const int *" << ClassName << "::\n"
     << "getRegClassPressureSets(const TargetRegisterClass *RC) const {\n";
  OS << "  static const " << getMinimalTypeForRange(PSetsSeqs.size() - 1, 32)
     << " RCSetStartTable[] = {\n    ";
  for (unsigned I = 0, E = NumRCs; I != E; ++I) {
    OS << PSetsSeqs.get(PSets[I]) << ",";
  }
  OS << "};\n"
     << "  return &RCSetsTable[RCSetStartTable[RC->getID()]];\n"
     << "}\n\n";
}

void PrinterLLVM::regInfoEmitGetRegUnitPressureSets(
    SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
    CodeGenRegBank const &RegBank, std::string const &ClassName,
    std::vector<std::vector<int>> const &PSets) const {
  OS << "/// Get the dimensions of register pressure impacted by this "
     << "register unit.\n"
     << "/// Returns a -1 terminated array of pressure set IDs\n"
     << "const int *" << ClassName << "::\n"
     << "getRegUnitPressureSets(unsigned RegUnit) const {\n"
     << "  assert(RegUnit < " << RegBank.getNumNativeRegUnits()
     << " && \"invalid register unit\");\n";
  OS << "  static const " << getMinimalTypeForRange(PSetsSeqs.size() - 1, 32)
     << " RUSetStartTable[] = {\n    ";
  for (unsigned UnitIdx = 0, UnitEnd = RegBank.getNumNativeRegUnits();
       UnitIdx < UnitEnd; ++UnitIdx) {
    OS << PSetsSeqs.get(PSets[RegBank.getRegUnit(UnitIdx).RegClassUnitSetsIdx])
       << ",";
  }
  OS << "};\n"
     << "  return &RCSetsTable[RUSetStartTable[RegUnit]];\n"
     << "}\n\n";
}

void PrinterLLVM::regInfoEmitExternTableDecl(
    std::string const &TargetName) const {
  OS << "extern const MCRegisterDesc " << TargetName << "RegDesc[];\n";
  OS << "extern const MCPhysReg " << TargetName << "RegDiffLists[];\n";
  OS << "extern const LaneBitmask " << TargetName << "LaneMaskLists[];\n";
  OS << "extern const char " << TargetName << "RegStrings[];\n";
  OS << "extern const char " << TargetName << "RegClassStrings[];\n";
  OS << "extern const MCPhysReg " << TargetName << "RegUnitRoots[][2];\n";
  OS << "extern const uint16_t " << TargetName << "SubRegIdxLists[];\n";
  OS << "extern const MCRegisterInfo::SubRegCoveredBits " << TargetName
     << "SubRegIdxRanges[];\n";
  OS << "extern const uint16_t " << TargetName << "RegEncodingTable[];\n";
}

void PrinterLLVM::regInfoEmitRegClassInit(
    std::string const &TargetName, std::string const &ClassName,
    CodeGenRegBank const &RegBank,
    std::list<CodeGenRegisterClass> const &RegClasses,
    std::deque<CodeGenRegister> const &Regs, unsigned SubRegIndicesSize) const {
  OS << ClassName << "::\n"
     << ClassName
     << "(unsigned RA, unsigned DwarfFlavour, unsigned EHFlavour,\n"
        "      unsigned PC, unsigned HwMode)\n"
     << "  : TargetRegisterInfo(&" << TargetName << "RegInfoDesc"
     << ", RegisterClasses, RegisterClasses+" << RegClasses.size() << ",\n"
     << "             SubRegIndexNameTable, SubRegIndexLaneMaskTable,\n"
     << "             ";
  printMask(OS, RegBank.CoveringLanes);
  OS << ", RegClassInfos, HwMode) {\n"
     << "  InitMCRegisterInfo(" << TargetName << "RegDesc, " << Regs.size() + 1
     << ", RA, PC,\n                     " << TargetName
     << "MCRegisterClasses, " << RegClasses.size() << ",\n"
     << "                     " << TargetName << "RegUnitRoots,\n"
     << "                     " << RegBank.getNumNativeRegUnits() << ",\n"
     << "                     " << TargetName << "RegDiffLists,\n"
     << "                     " << TargetName << "LaneMaskLists,\n"
     << "                     " << TargetName << "RegStrings,\n"
     << "                     " << TargetName << "RegClassStrings,\n"
     << "                     " << TargetName << "SubRegIdxLists,\n"
     << "                     " << SubRegIndicesSize + 1 << ",\n"
     << "                     " << TargetName << "SubRegIdxRanges,\n"
     << "                     " << TargetName << "RegEncodingTable);\n\n";
}

void PrinterLLVM::regInfoEmitSaveListTable(
    Record const *CSRSet, SetTheory::RecVec const *Regs) const {
  OS << "static const MCPhysReg " << CSRSet->getName() << "_SaveList[] = { ";
  for (unsigned R = 0, Re = Regs->size(); R != Re; ++R)
    OS << getQualifiedName((*Regs)[R]) << ", ";
  OS << "0 };\n";
}

void PrinterLLVM::regInfoEmitRegMaskTable(std::string const &CSRSetName,
                                          BitVector &Covered) const {
  OS << "static const uint32_t " << CSRSetName << "_RegMask[] = { ";
  printBitVectorAsHex(OS, Covered, 32);
  OS << "};\n";
}

void PrinterLLVM::regInfoEmitGetRegMasks(std::vector<Record *> const &CSRSets,
                                         std::string const &ClassName) const {
  OS << "ArrayRef<const uint32_t *> " << ClassName
     << "::getRegMasks() const {\n";
  if (!CSRSets.empty()) {
    OS << "  static const uint32_t *const Masks[] = {\n";
    for (Record *CSRSet : CSRSets)
      OS << "    " << CSRSet->getName() << "_RegMask,\n";
    OS << "  };\n";
    OS << "  return ArrayRef(Masks);\n";
  } else {
    OS << "  return std::nullopt;\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitGPRCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  OS << "bool " << ClassName << "::\n"
     << "isGeneralPurposeRegister(const MachineFunction &MF, "
     << "MCRegister PhysReg) const {\n"
     << "  return\n";
  for (const CodeGenRegisterCategory &Category : RegCategories)
    if (Category.getName() == "GeneralPurposeRegisters") {
      for (const CodeGenRegisterClass *RC : Category.getClasses())
        OS << "      " << RC->getQualifiedName()
           << "RegClass.contains(PhysReg) ||\n";
      break;
    }
  OS << "      false;\n";
  OS << "}\n\n";
}
void PrinterLLVM::regInfoEmitFixedRegCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  OS << "bool " << ClassName << "::\n"
     << "isFixedRegister(const MachineFunction &MF, "
     << "MCRegister PhysReg) const {\n"
     << "  return\n";
  for (const CodeGenRegisterCategory &Category : RegCategories)
    if (Category.getName() == "FixedRegisters") {
      for (const CodeGenRegisterClass *RC : Category.getClasses())
        OS << "      " << RC->getQualifiedName()
           << "RegClass.contains(PhysReg) ||\n";
      break;
    }
  OS << "      false;\n";
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitArgRegCheck(
    std::string const &ClassName,
    std::list<CodeGenRegisterCategory> const &RegCategories) const {
  OS << "bool " << ClassName << "::\n"
     << "isArgumentRegister(const MachineFunction &MF, "
     << "MCRegister PhysReg) const {\n"
     << "  return\n";
  for (const CodeGenRegisterCategory &Category : RegCategories)
    if (Category.getName() == "ArgumentRegisters") {
      for (const CodeGenRegisterClass *RC : Category.getClasses())
        OS << "      " << RC->getQualifiedName()
           << "RegClass.contains(PhysReg) ||\n";
      break;
    }
  OS << "      false;\n";
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitIsConstantPhysReg(
    std::deque<CodeGenRegister> const &Regs,
    std::string const &ClassName) const {
  OS << "bool " << ClassName << "::\n"
     << "isConstantPhysReg(MCRegister PhysReg) const {\n"
     << "  return\n";
  for (const auto &Reg : Regs)
    if (Reg.Constant)
      OS << "      PhysReg == " << getQualifiedName(Reg.TheDef) << " ||\n";
  OS << "      false;\n";
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitGetRegMaskNames(
    std::vector<Record *> const &CSRSets, std::string const &ClassName) const {
  OS << "ArrayRef<const char *> " << ClassName
     << "::getRegMaskNames() const {\n";
  if (!CSRSets.empty()) {
    OS << "  static const char *Names[] = {\n";
    for (Record *CSRSet : CSRSets)
      OS << "    " << '"' << CSRSet->getName() << '"' << ",\n";
    OS << "  };\n";
    OS << "  return ArrayRef(Names);\n";
  } else {
    OS << "  return std::nullopt;\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitGetFrameLowering(
    std::string const &TargetName) const {
  OS << "const " << TargetName << "FrameLowering *\n"
     << TargetName
     << "GenRegisterInfo::getFrameLowering(const MachineFunction &MF) {\n"
     << "  return static_cast<const " << TargetName << "FrameLowering *>(\n"
     << "      MF.getSubtarget().getFrameLowering());\n"
     << "}\n\n";
}

void PrinterLLVM::regInfoEmitComposeSubRegIndicesImplHead(
    std::string const &ClName) const {
  OS << "unsigned " << ClName
     << "::composeSubRegIndicesImpl(unsigned IdxA, unsigned IdxB) const {\n";
}

void PrinterLLVM::regInfoEmitComposeSubRegIndicesImplBody(
    SmallVector<SmallVector<CodeGenSubRegIndex *, 4>, 4> const &Rows,
    unsigned SubRegIndicesSize, SmallVector<unsigned, 4> const &RowMap) const {
  if (Rows.size() > 1) {
    OS << "  static const " << getMinimalTypeForRange(Rows.size(), 32)
       << " RowMap[" << SubRegIndicesSize << "] = {\n    ";
    for (unsigned I = 0, E = SubRegIndicesSize; I != E; ++I)
      OS << RowMap[I] << ", ";
    OS << "\n  };\n";
  }

  // Output the rows.
  OS << "  static const " << getMinimalTypeForRange(SubRegIndicesSize + 1, 32)
     << " Rows[" << Rows.size() << "][" << SubRegIndicesSize << "] = {\n";
  for (unsigned R = 0, Re = Rows.size(); R != Re; ++R) {
    OS << "    { ";
    for (unsigned I = 0, E = SubRegIndicesSize; I != E; ++I)
      if (Rows[R][I])
        OS << Rows[R][I]->getQualifiedName() << ", ";
      else
        OS << "0, ";
    OS << "},\n";
  }
  OS << "  };\n\n";

  OS << "  --IdxA; assert(IdxA < " << SubRegIndicesSize << "); (void) IdxA;\n"
     << "  --IdxB; assert(IdxB < " << SubRegIndicesSize << ");\n";
  if (Rows.size() > 1)
    OS << "  return Rows[RowMap[IdxA]][IdxB];\n";
  else
    OS << "  return Rows[0][IdxB];\n";
  OS << "}\n\n";
}

void PrinterLLVM::regInfoEmitLaneMaskComposeSeq(
    SmallVector<SmallVector<MaskRolPair, 1>, 4> const &Sequences,
    SmallVector<unsigned, 4> const &SubReg2SequenceIndexMap,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "  struct MaskRolOp {\n"
        "    LaneBitmask Mask;\n"
        "    uint8_t  RotateLeft;\n"
        "  };\n"
        "  static const MaskRolOp LaneMaskComposeSequences[] = {\n";
  unsigned Idx = 0;
  for (size_t S = 0, Se = Sequences.size(); S != Se; ++S) {
    OS << "    ";
    const SmallVectorImpl<MaskRolPair> &Sequence = Sequences[S];
    for (size_t P = 0, Pe = Sequence.size(); P != Pe; ++P) {
      const MaskRolPair &MRP = Sequence[P];
      printMask(OS << "{ ", MRP.Mask);
      OS << format(", %2u }, ", MRP.RotateLeft);
    }
    OS << "{ LaneBitmask::getNone(), 0 }";
    if (S + 1 != Se)
      OS << ", ";
    OS << "  // Sequence " << Idx << "\n";
    Idx += Sequence.size() + 1;
  }
  auto *IntType = getMinimalTypeForRange(*std::max_element(
      SubReg2SequenceIndexMap.begin(), SubReg2SequenceIndexMap.end()));
  OS << "  };\n"
        "  static const "
     << IntType << " CompositeSequences[] = {\n";
  for (size_t I = 0, E = SubRegIndices.size(); I != E; ++I) {
    OS << "    ";
    OS << SubReg2SequenceIndexMap[I];
    if (I + 1 != E)
      OS << ",";
    OS << " // to " << SubRegIndices[I].getName() << "\n";
  }
  OS << "  };\n\n";
}

void PrinterLLVM::regInfoEmitComposeSubRegIdxLaneMask(
    std::string const &ClName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "LaneBitmask " << ClName
     << "::composeSubRegIndexLaneMaskImpl(unsigned IdxA, "
        "LaneBitmask LaneMask) const {\n"
        "  --IdxA; assert(IdxA < "
     << SubRegIndices.size()
     << " && \"Subregister index out of bounds\");\n"
        "  LaneBitmask Result;\n"
        "  for (const MaskRolOp *Ops =\n"
        "       &LaneMaskComposeSequences[CompositeSequences[IdxA]];\n"
        "       Ops->Mask.any(); ++Ops) {\n"
        "    LaneBitmask::Type M = LaneMask.getAsInteger() & "
        "Ops->Mask.getAsInteger();\n"
        "    if (unsigned S = Ops->RotateLeft)\n"
        "      Result |= LaneBitmask((M << S) | (M >> (LaneBitmask::BitWidth - "
        "S)));\n"
        "    else\n"
        "      Result |= LaneBitmask(M);\n"
        "  }\n"
        "  return Result;\n"
        "}\n\n";
}

void PrinterLLVM::regInfoEmitComposeSubRegIdxLaneMaskRev(
    std::string const &ClName,
    std::deque<CodeGenSubRegIndex> const &SubRegIndices) const {
  OS << "LaneBitmask " << ClName
     << "::reverseComposeSubRegIndexLaneMaskImpl(unsigned IdxA, "
        " LaneBitmask LaneMask) const {\n"
        "  LaneMask &= getSubRegIndexLaneMask(IdxA);\n"
        "  --IdxA; assert(IdxA < "
     << SubRegIndices.size()
     << " && \"Subregister index out of bounds\");\n"
        "  LaneBitmask Result;\n"
        "  for (const MaskRolOp *Ops =\n"
        "       &LaneMaskComposeSequences[CompositeSequences[IdxA]];\n"
        "       Ops->Mask.any(); ++Ops) {\n"
        "    LaneBitmask::Type M = LaneMask.getAsInteger();\n"
        "    if (unsigned S = Ops->RotateLeft)\n"
        "      Result |= LaneBitmask((M >> S) | (M << (LaneBitmask::BitWidth - "
        "S)));\n"
        "    else\n"
        "      Result |= LaneBitmask(M);\n"
        "  }\n"
        "  return Result;\n"
        "}\n\n";
}

void PrinterLLVM::regInfoEmitRegBaseClassMapping(
    std::string const &ClassName,
    SmallVector<const CodeGenRegisterClass *> const BaseClasses,
    std::vector<uint8_t> const Mapping) const {
  OS << "\n// Register to base register class mapping\n\n";
  OS << "\n";
  OS << "const TargetRegisterClass *" << ClassName
     << "::getPhysRegBaseClass(MCRegister Reg)"
     << " const {\n";
  OS << "  static const TargetRegisterClass *BaseClasses["
     << (BaseClasses.size() + 1) << "] = {\n";
  OS << "    nullptr,\n";
  for (const auto *const RC : BaseClasses)
    OS << "    &" << RC->getQualifiedName() << "RegClass,\n";
  OS << "  };\n";
  OS << "  static const uint8_t Mapping[" << Mapping.size() << "] = {\n    ";
  for (const uint8_t Value : Mapping)
    OS << (unsigned)Value << ",";
  OS << "  };\n\n";
  OS << "  assert(Reg < sizeof(Mapping));\n";
  OS << "  return BaseClasses[Mapping[Reg]];\n";
  OS << "}\n";
}

//-------------------------
// Backend: DecoderEmitter
//-------------------------

void PrinterLLVM::decoderEmitterEmitOpDecoder(raw_ostream &DecoderOS,
                                              const OperandInfo &Op) const {
  unsigned const Indent = 4;
  DecoderOS.indent(Indent) << GuardPrefix << Op.Decoder
                           << "(MI, insn, Address, Decoder)" << GuardPostfix
                           << " { "
                           << (Op.HasCompleteDecoder
                                   ? ""
                                   : "DecodeComplete = false; ")
                           << "return MCDisassembler::Fail; }\n";
}

void PrinterLLVM::decoderEmitterEmitOpBinaryParser(
    raw_ostream &DecOS, const OperandInfo &OpInfo) const {
  unsigned const Indent = 4;
  const std::string &Decoder = OpInfo.Decoder;

  bool const UseInsertBits = OpInfo.numFields() != 1 || OpInfo.InitValue != 0;

  if (UseInsertBits) {
    DecOS.indent(Indent) << "tmp = 0x";
    DecOS.write_hex(OpInfo.InitValue);
    DecOS << ";\n";
  }

  for (const EncodingField &EF : OpInfo) {
    DecOS.indent(Indent);
    if (UseInsertBits)
      DecOS << "insertBits(tmp, ";
    else
      DecOS << "tmp = ";
    DecOS << "fieldFromInstruction(insn, " << EF.Base << ", " << EF.Width
          << ')';
    if (UseInsertBits)
      DecOS << ", " << EF.Offset << ", " << EF.Width << ')';
    else if (EF.Offset != 0)
      DecOS << " << " << EF.Offset;
    DecOS << ";\n";
  }

  if (Decoder != "") {
    DecOS.indent(Indent) << GuardPrefix << Decoder
                         << "(MI, tmp, Address, Decoder)" << GuardPostfix
                         << " { "
                         << (OpInfo.HasCompleteDecoder
                                 ? ""
                                 : "DecodeComplete = false; ")
                         << "return MCDisassembler::Fail; }\n";
  } else {
    DecOS.indent(Indent) << "MI.addOperand(MCOperand::createImm(tmp));\n";
  }
}

// If ParenIfBinOp is true, print a surrounding () if Val uses && or ||.
bool PrinterLLVM::decoderEmitterEmitPredicateMatchAux(
    const Init &Val, bool ParenIfBinOp, raw_ostream &PredOS) const {
  if (auto *D = dyn_cast<DefInit>(&Val)) {
    if (!D->getDef()->isSubClassOf("SubtargetFeature"))
      return true;
    PredOS << "Bits[" << PredicateNamespace << "::" << D->getAsString() << "]";
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

bool PrinterLLVM::decoderEmitterEmitPredicateMatch(raw_ostream &PredOS,
                                                   const ListInit *Predicates,
                                                   unsigned Opc) const {
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

// emitFieldFromInstruction - Emit the templated helper function
// fieldFromInstruction().
// On Windows we make sure that this function is not inlined when
// using the VS compiler. It has a bug which causes the function
// to be optimized out in some circustances. See llvm.org/pr38292
void PrinterLLVM::decoderEmitterEmitFieldFromInstruction() const {
  OS << "// Helper functions for extracting fields from encoded instructions.\n"
     << "// InsnType must either be integral or an APInt-like object that "
        "must:\n"
     << "// * be default-constructible and copy-constructible\n"
     << "// * be constructible from an APInt (this can be private)\n"
     << "// * Support insertBits(bits, startBit, numBits)\n"
     << "// * Support extractBitsAsZExtValue(numBits, startBit)\n"
     << "// * Support the ~, &, ==, and != operators with other objects of "
        "the same type\n"
     << "// * Support the != and bitwise & with uint64_t\n"
     << "// * Support put (<<) to raw_ostream&\n"
     << "template <typename InsnType>\n"
     << "#if defined(_MSC_VER) && !defined(__clang__)\n"
     << "__declspec(noinline)\n"
     << "#endif\n"
     << "static std::enable_if_t<std::is_integral<InsnType>::value, InsnType>\n"
     << "fieldFromInstruction(const InsnType &insn, unsigned startBit,\n"
     << "                     unsigned numBits) {\n"
     << "  assert(startBit + numBits <= 64 && \"Cannot support >64-bit "
        "extractions!\");\n"
     << "  assert(startBit + numBits <= (sizeof(InsnType) * 8) &&\n"
     << "         \"Instruction field out of bounds!\");\n"
     << "  InsnType fieldMask;\n"
     << "  if (numBits == sizeof(InsnType) * 8)\n"
     << "    fieldMask = (InsnType)(-1LL);\n"
     << "  else\n"
     << "    fieldMask = (((InsnType)1 << numBits) - 1) << startBit;\n"
     << "  return (insn & fieldMask) >> startBit;\n"
     << "}\n"
     << "\n"
     << "template <typename InsnType>\n"
     << "static std::enable_if_t<!std::is_integral<InsnType>::value, "
        "uint64_t>\n"
     << "fieldFromInstruction(const InsnType &insn, unsigned startBit,\n"
     << "                     unsigned numBits) {\n"
     << "  return insn.extractBitsAsZExtValue(numBits, startBit);\n"
     << "}\n\n";
}

// emitInsertBits - Emit the templated helper function insertBits().
void PrinterLLVM::decoderEmitterEmitInsertBits() const {
  OS << "// Helper function for inserting bits extracted from an encoded "
        "instruction into\n"
     << "// a field.\n"
     << "template <typename InsnType>\n"
     << "static std::enable_if_t<std::is_integral<InsnType>::value>\n"
     << "insertBits(InsnType &field, InsnType bits, unsigned startBit, "
        "unsigned numBits) {\n"
     << "  assert(startBit + numBits <= sizeof field * 8);\n"
     << "  field |= (InsnType)bits << startBit;\n"
     << "}\n"
     << "\n"
     << "template <typename InsnType>\n"
     << "static std::enable_if_t<!std::is_integral<InsnType>::value>\n"
     << "insertBits(InsnType &field, uint64_t bits, unsigned startBit, "
        "unsigned numBits) {\n"
     << "  field.insertBits(bits, startBit, numBits);\n"
     << "}\n\n";
}

// emitDecodeInstruction - Emit the templated helper function
// decodeInstruction().
void PrinterLLVM::decoderEmitterEmitDecodeInstruction(bool IsVarLenInst) const {
  OS << "template <typename InsnType>\n"
     << "static DecodeStatus decodeInstruction(const uint8_t DecodeTable[], "
        "MCInst &MI,\n"
     << "                                      InsnType insn, uint64_t "
        "Address,\n"
     << "                                      const MCDisassembler *DisAsm,\n"
     << "                                      const MCSubtargetInfo &STI";
  if (IsVarLenInst) {
    OS << ",\n"
       << "                                      llvm::function_ref<void(APInt "
          "&,"
       << " uint64_t)> makeUp";
  }
  OS << ") {\n"
     << "  const FeatureBitset &Bits = STI.getFeatureBits();\n"
     << "\n"
     << "  const uint8_t *Ptr = DecodeTable;\n"
     << "  uint64_t CurFieldValue = 0;\n"
     << "  DecodeStatus S = MCDisassembler::Success;\n"
     << "  while (true) {\n"
     << "    ptrdiff_t Loc = Ptr - DecodeTable;\n"
     << "    switch (*Ptr) {\n"
     << "    default:\n"
     << "      errs() << Loc << \": Unexpected decode table opcode!\\n\";\n"
     << "      return MCDisassembler::Fail;\n"
     << "    case MCD::OPC_ExtractField: {\n"
     << "      unsigned Start = *++Ptr;\n"
     << "      unsigned Len = *++Ptr;\n"
     << "      ++Ptr;\n";
  if (IsVarLenInst) {
    OS << "      makeUp(insn, Start + Len);\n";
  }
  OS << "      CurFieldValue = fieldFromInstruction(insn, Start, Len);\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_ExtractField(\" << Start << "
        "\", \"\n"
     << "                   << Len << \"): \" << CurFieldValue << \"\\n\");\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_FilterValue: {\n"
     << "      // Decode the field value.\n"
     << "      unsigned Len;\n"
     << "      uint64_t Val = decodeULEB128(++Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      // NumToSkip is a plain 24-bit integer.\n"
     << "      unsigned NumToSkip = *Ptr++;\n"
     << "      NumToSkip |= (*Ptr++) << 8;\n"
     << "      NumToSkip |= (*Ptr++) << 16;\n"
     << "\n"
     << "      // Perform the filter operation.\n"
     << "      if (Val != CurFieldValue)\n"
     << "        Ptr += NumToSkip;\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_FilterValue(\" << Val << "
        "\", \" << NumToSkip\n"
     << "                   << \"): \" << ((Val != CurFieldValue) ? \"FAIL:\" "
        ": \"PASS:\")\n"
     << "                   << \" continuing at \" << (Ptr - DecodeTable) << "
        "\"\\n\");\n"
     << "\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_CheckField: {\n"
     << "      unsigned Start = *++Ptr;\n"
     << "      unsigned Len = *++Ptr;\n";
  if (IsVarLenInst) {
    OS << "      makeUp(insn, Start + Len);\n";
  }
  OS << "      uint64_t FieldValue = fieldFromInstruction(insn, Start, Len);\n"
     << "      // Decode the field value.\n"
     << "      unsigned PtrLen = 0;\n"
     << "      uint64_t ExpectedValue = decodeULEB128(++Ptr, &PtrLen);\n"
     << "      Ptr += PtrLen;\n"
     << "      // NumToSkip is a plain 24-bit integer.\n"
     << "      unsigned NumToSkip = *Ptr++;\n"
     << "      NumToSkip |= (*Ptr++) << 8;\n"
     << "      NumToSkip |= (*Ptr++) << 16;\n"
     << "\n"
     << "      // If the actual and expected values don't match, skip.\n"
     << "      if (ExpectedValue != FieldValue)\n"
     << "        Ptr += NumToSkip;\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_CheckField(\" << Start << "
        "\", \"\n"
     << "                   << Len << \", \" << ExpectedValue << \", \" << "
        "NumToSkip\n"
     << "                   << \"): FieldValue = \" << FieldValue << \", "
        "ExpectedValue = \"\n"
     << "                   << ExpectedValue << \": \"\n"
     << "                   << ((ExpectedValue == FieldValue) ? \"PASS\\n\" : "
        "\"FAIL\\n\"));\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_CheckPredicate: {\n"
     << "      unsigned Len;\n"
     << "      // Decode the Predicate Index value.\n"
     << "      unsigned PIdx = decodeULEB128(++Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      // NumToSkip is a plain 24-bit integer.\n"
     << "      unsigned NumToSkip = *Ptr++;\n"
     << "      NumToSkip |= (*Ptr++) << 8;\n"
     << "      NumToSkip |= (*Ptr++) << 16;\n"
     << "      // Check the predicate.\n"
     << "      bool Pred;\n"
     << "      if (!(Pred = checkDecoderPredicate(PIdx, Bits)))\n"
     << "        Ptr += NumToSkip;\n"
     << "      (void)Pred;\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_CheckPredicate(\" << PIdx "
        "<< \"): \"\n"
     << "            << (Pred ? \"PASS\\n\" : \"FAIL\\n\"));\n"
     << "\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_Decode: {\n"
     << "      unsigned Len;\n"
     << "      // Decode the Opcode value.\n"
     << "      unsigned Opc = decodeULEB128(++Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      unsigned DecodeIdx = decodeULEB128(Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "\n"
     << "      MI.clear();\n"
     << "      MI.setOpcode(Opc);\n"
     << "      bool DecodeComplete;\n";
  if (IsVarLenInst) {
    OS << "      Len = InstrLenTable[Opc];\n"
       << "      makeUp(insn, Len);\n";
  }
  OS << "      S = decodeToMCInst(S, DecodeIdx, insn, MI, Address, DisAsm, "
        "DecodeComplete);\n"
     << "      assert(DecodeComplete);\n"
     << "\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_Decode: opcode \" << Opc\n"
     << "                   << \", using decoder \" << DecodeIdx << \": \"\n"
     << "                   << (S != MCDisassembler::Fail ? \"PASS\" : "
        "\"FAIL\") << \"\\n\");\n"
     << "      return S;\n"
     << "    }\n"
     << "    case MCD::OPC_TryDecode: {\n"
     << "      unsigned Len;\n"
     << "      // Decode the Opcode value.\n"
     << "      unsigned Opc = decodeULEB128(++Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      unsigned DecodeIdx = decodeULEB128(Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      // NumToSkip is a plain 24-bit integer.\n"
     << "      unsigned NumToSkip = *Ptr++;\n"
     << "      NumToSkip |= (*Ptr++) << 8;\n"
     << "      NumToSkip |= (*Ptr++) << 16;\n"
     << "\n"
     << "      // Perform the decode operation.\n"
     << "      MCInst TmpMI;\n"
     << "      TmpMI.setOpcode(Opc);\n"
     << "      bool DecodeComplete;\n"
     << "      S = decodeToMCInst(S, DecodeIdx, insn, TmpMI, Address, DisAsm, "
        "DecodeComplete);\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_TryDecode: opcode \" << "
        "Opc\n"
     << "                   << \", using decoder \" << DecodeIdx << \": \");\n"
     << "\n"
     << "      if (DecodeComplete) {\n"
     << "        // Decoding complete.\n"
     << "        LLVM_DEBUG(dbgs() << (S != MCDisassembler::Fail ? \"PASS\" : "
        "\"FAIL\") << \"\\n\");\n"
     << "        MI = TmpMI;\n"
     << "        return S;\n"
     << "      } else {\n"
     << "        assert(S == MCDisassembler::Fail);\n"
     << "        // If the decoding was incomplete, skip.\n"
     << "        Ptr += NumToSkip;\n"
     << "        LLVM_DEBUG(dbgs() << \"FAIL: continuing at \" << (Ptr - "
        "DecodeTable) << \"\\n\");\n"
     << "        // Reset decode status. This also drops a SoftFail status "
        "that could be\n"
     << "        // set before the decode attempt.\n"
     << "        S = MCDisassembler::Success;\n"
     << "      }\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_SoftFail: {\n"
     << "      // Decode the mask values.\n"
     << "      unsigned Len;\n"
     << "      uint64_t PositiveMask = decodeULEB128(++Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      uint64_t NegativeMask = decodeULEB128(Ptr, &Len);\n"
     << "      Ptr += Len;\n"
     << "      bool Fail = (insn & PositiveMask) != 0 || (~insn & "
        "NegativeMask) != 0;\n"
     << "      if (Fail)\n"
     << "        S = MCDisassembler::SoftFail;\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_SoftFail: \" << (Fail ? "
        "\"FAIL\\n\" : \"PASS\\n\"));\n"
     << "      break;\n"
     << "    }\n"
     << "    case MCD::OPC_Fail: {\n"
     << "      LLVM_DEBUG(dbgs() << Loc << \": OPC_Fail\\n\");\n"
     << "      return MCDisassembler::Fail;\n"
     << "    }\n"
     << "    }\n"
     << "  }\n"
     << "  llvm_unreachable(\"bogosity detected in disassembler state "
        "machine!\");\n"
     << "}\n\n";
}

// Emit the decoder state machine table.
void PrinterLLVM::decoderEmitterEmitTable(
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
      OS.indent(Indent) << "MCD::OPC_ExtractField, " << Start << ", " << Len
                        << ",  // Inst{";
      if (Len > 1)
        OS << (Start + Len - 1) << "-";
      OS << Start << "} ...\n";
      break;
    }
    case MCD::OPC_FilterValue: {
      ++I;
      OS.indent(Indent) << "MCD::OPC_FilterValue, ";
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
      OS.indent(Indent) << "MCD::OPC_CheckField, " << Start << ", " << Len
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
      OS.indent(Indent) << "MCD::OPC_CheckPredicate, ";
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
      OS.indent(Indent) << "MCD::OPC_" << (IsTry ? "Try" : "") << "Decode, ";
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
      OS.indent(Indent) << "MCD::OPC_SoftFail";
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
      OS.indent(Indent) << "MCD::OPC_Fail,\n";
      break;
    }
    }
  }
  OS.indent(Indent) << "0\n";

  Indent -= 2;

  OS.indent(Indent) << "};\n\n";
}

void PrinterLLVM::decoderEmitterEmitInstrLenTable(
    std::vector<unsigned> &InstrLen) const {
  OS << "static const uint8_t InstrLenTable[] = {\n";
  for (unsigned const &Len : InstrLen) {
    OS << Len << ",\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::decoderEmitterEmitPredicateFunction(
    PredicateSet &Predicates, unsigned Indentation) const {
  // The predicate function is just a big switch statement based on the
  // input predicate index.
  OS.indent(Indentation) << "static bool checkDecoderPredicate(unsigned Idx, "
                         << "const FeatureBitset &Bits) {\n";
  Indentation += 2;
  if (!Predicates.empty()) {
    OS.indent(Indentation) << "switch (Idx) {\n";
    OS.indent(Indentation)
        << "default: llvm_unreachable(\"Invalid index!\");\n";
    unsigned Index = 0;
    for (const auto &Predicate : Predicates) {
      OS.indent(Indentation) << "case " << Index++ << ":\n";
      OS.indent(Indentation + 2) << "return (" << Predicate << ");\n";
    }
    OS.indent(Indentation) << "}\n";
  } else {
    // No case statement to emit
    OS.indent(Indentation) << "llvm_unreachable(\"Invalid index!\");\n";
  }
  Indentation -= 2;
  OS.indent(Indentation) << "}\n\n";
}

void PrinterLLVM::decoderEmitterEmitDecoderFunction(
    DecoderSet &Decoders, unsigned Indentation) const {
  // The decoder function is just a big switch statement based on the
  // input decoder index.
  OS.indent(Indentation) << "template <typename InsnType>\n";
  OS.indent(Indentation) << "static DecodeStatus decodeToMCInst(DecodeStatus S,"
                         << " unsigned Idx, InsnType insn, MCInst &MI,\n";
  OS.indent(Indentation)
      << "                                   uint64_t "
      << "Address, const MCDisassembler *Decoder, bool &DecodeComplete) {\n";
  Indentation += 2;
  OS.indent(Indentation) << "DecodeComplete = true;\n";
  // TODO: When InsnType is large, using uint64_t limits all fields to 64 bits
  // It would be better for emitBinaryParser to use a 64-bit tmp whenever
  // possible but fall back to an InsnType-sized tmp for truly large fields.
  OS.indent(Indentation) << "using TmpType = "
                            "std::conditional_t<std::is_integral<InsnType>::"
                            "value, InsnType, uint64_t>;\n";
  OS.indent(Indentation) << "TmpType tmp;\n";
  OS.indent(Indentation) << "switch (Idx) {\n";
  OS.indent(Indentation) << "default: llvm_unreachable(\"Invalid index!\");\n";
  unsigned Index = 0;
  for (const auto &Decoder : Decoders) {
    OS.indent(Indentation) << "case " << Index++ << ":\n";
    OS << Decoder;
    OS.indent(Indentation + 2) << "return S;\n";
  }
  OS.indent(Indentation) << "}\n";
  Indentation -= 2;
  OS.indent(Indentation) << "}\n\n";
}

void PrinterLLVM::decoderEmitterEmitIncludes() const {
  OS << "#include \"llvm/MC/MCInst.h\"\n";
  OS << "#include \"llvm/MC/MCSubtargetInfo.h\"\n";
  OS << "#include \"llvm/MC/SubtargetFeature.h\"\n";
  OS << "#include \"llvm/Support/DataTypes.h\"\n";
  OS << "#include \"llvm/Support/Debug.h\"\n";
  OS << "#include \"llvm/Support/LEB128.h\"\n";
  OS << "#include \"llvm/Support/raw_ostream.h\"\n";
  OS << "#include <assert.h>\n";
  OS << '\n';
}

void PrinterLLVM::decoderEmitterEmitSourceFileHeader() const {
  llvm::emitSourceFileHeader(" * " + TargetName + " Disassembler", OS);
}

//-------------------------
// Backend: AsmWriter
//-------------------------

void PrinterLLVM::asmWriterEmitSourceFileHeader() const {
  emitSourceFileHeader("Assembly Writer Source Fragment", OS);
}

void PrinterLLVM::asmWriterEmitGetMnemonic(std::string const &TargetName,
                                           StringRef const &ClassName) const {
  OS << "/// getMnemonic - This method is automatically generated by "
        "tablegen\n"
        "/// from the instruction set description.\n"
        "std::pair<const char *, uint64_t> "
     << TargetName << ClassName << "::getMnemonic(const MCInst *MI) {\n";
}

void PrinterLLVM::asmWriterEmitAsmStrs(
    SequenceToOffsetTable<std::string> const &StrTable) const {
  StrTable.emitStringLiteralDef(OS, "  static const char AsmStrs[]");
}

void PrinterLLVM::asmWriterEmitMnemonicDecodeTable(
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
    unsigned TableSize = std::min(llvm::bit_floor(BytesNeeded), 4u);
    BytesNeeded -= TableSize;
    TableSize *= 8; // Convert to bits;
    uint64_t Mask = (1ULL << TableSize) - 1;
    OS << "  static const uint" << TableSize << "_t OpInfo" << Table
      << "[] = {\n";
    for (unsigned i = 0, e = NumberedInstructions.size(); i != e; ++i) {
      OS << "    " << ((OpcodeInfo[i] >> Shift) & Mask) << "U,\t// "
        << NumberedInstructions[i]->TheDef->getName() << "\n";
    }
    OS << "  };\n\n";
    // Emit string to combine the individual table lookups.
    BitsOS << "  Bits |= ";
    // If the total bits is more than 32-bits we need to use a 64-bit type.
    if (BitsLeft < (OpcodeInfoBits - 32))
      BitsOS << "(uint64_t)";
    BitsOS << "OpInfo" << Table << "[MI->getOpcode()] << " << Shift << ";\n";
    // Prepare the shift for the next iteration and increment the table count.
    Shift += TableSize;
    ++Table;
  }

  OS << "  // Emit the opcode for the instruction.\n";
  OS << BitsString;

  // Return mnemonic string and bits.
  OS << "  return {AsmStrs+(Bits & " << (1 << AsmStrBits) - 1
    << ")-1, Bits};\n\n";

  OS << "}\n";
}

void PrinterLLVM::asmWriterEmitPrintInstruction(
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
        "LLVM_NO_PROFILE_INSTRUMENT_FUNCTION\n"
        "void "
     << TargetName << ClassName
     << "::printInstruction(const MCInst *MI, uint64_t Address, "
     << (PassSubtarget ? "const MCSubtargetInfo &STI, " : "")
     << "raw_ostream &O) {\n";

  // Emit the initial tab character.
  OS << "  O << \"\\t\";\n\n";

  // Emit the starting string.
  OS << "  auto MnemonicInfo = getMnemonic(MI);\n\n";
  OS << "  O << MnemonicInfo.first;\n\n";

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
         << Commands[1] << "  } else {\n"
         << Commands[0] << "  }\n\n";
    } else if (Commands.size() == 1) {
      // Emit a single possibility.
      OS << Commands[0] << "\n\n";
    } else {
      OS << "  switch ((Bits >> " << (OpcodeInfoBits - BitsLeft) << ") & "
         << ((1 << NumBits) - 1) << ") {\n"
         << "  default: llvm_unreachable(\"Invalid command number.\");\n";

      // Print out all the cases.
      for (unsigned J = 0, F = Commands.size(); J != F; ++J) {
        OS << "  case " << J << ":\n";
        OS << Commands[J];
        OS << "    break;\n";
      }
      OS << "  }\n\n";
    }
    BitsLeft -= NumBits;
  }
}

void PrinterLLVM::asmWriterEmitOpCases(
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
  OS << "\n      " << TheOp.getCode(PassSubtarget);
  OS << "\n      break;\n";
}

void PrinterLLVM::asmWriterEmitInstrSwitch() const {
  OS << "  switch (MI->getOpcode()) {\n";
  OS << "  default: llvm_unreachable(\"Unexpected opcode.\");\n";
}

void PrinterLLVM::asmWriterEmitCompoundClosure(unsigned Indent, bool Newline,
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

void PrinterLLVM::asmWriterEmitInstruction(
    AsmWriterInst const &FirstInst,
    std::vector<AsmWriterInst> const &SimilarInsts, unsigned DifferingOperand,
    bool PassSubtarget) const {
  OS << "  case " << FirstInst.CGI->Namespace
     << "::" << FirstInst.CGI->TheDef->getName() << ":\n";
  for (const AsmWriterInst &AWI : SimilarInsts)
    OS << "  case " << AWI.CGI->Namespace << "::" << AWI.CGI->TheDef->getName()
       << ":\n";
  for (unsigned I = 0, E = FirstInst.Operands.size(); I != E; ++I) {
    if (I != DifferingOperand) {
      // If the operand is the same for all instructions, just print it.
      OS << "    " << FirstInst.Operands[I].getCode(PassSubtarget);
    } else {
      // If this is the operand that varies between all of the instructions,
      // emit a switch for just this operand now.
      OS << "    switch (MI->getOpcode()) {\n";
      OS << "    default: llvm_unreachable(\"Unexpected opcode.\");\n";
      std::vector<std::pair<std::string, AsmWriterOperand>> OpsToPrint;
      OpsToPrint.push_back(
          std::make_pair(FirstInst.CGI->Namespace.str() +
                             "::" + FirstInst.CGI->TheDef->getName().str(),
                         FirstInst.Operands[I]));

      for (const AsmWriterInst &AWI : SimilarInsts) {
        OpsToPrint.push_back(std::make_pair(
            AWI.CGI->Namespace.str() + "::" + AWI.CGI->TheDef->getName().str(),
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

void PrinterLLVM::asmWriterEmitGetRegNameAssert(std::string const &TargetName,
                                                StringRef const &ClassName,
                                                bool hasAltNames,
                                                unsigned RegSize) const {

  OS <<
  "\n\n/// getRegisterName - This method is automatically generated by tblgen\n"
  "/// from the register set description.  This returns the assembler name\n"
  "/// for the specified register.\n"
  "const char *" << TargetName << ClassName << "::";
  if (hasAltNames)
    OS << "\ngetRegisterName(MCRegister Reg, unsigned AltIdx) {\n";
  else
    OS << "getRegisterName(MCRegister Reg) {\n";
  OS << "  unsigned RegNo = Reg.id();\n"
    << "  assert(RegNo && RegNo < " << (RegSize + 1)
    << " && \"Invalid register number!\");\n"
    << "\n";
}

void PrinterLLVM::asmWriterEmitStringLiteralDef(
    SequenceToOffsetTable<std::string> const &StringTable,
    StringRef const &AltName) const {
  StringTable.emitStringLiteralDef(OS, Twine("  static const char AsmStrs") +
                                           AltName + "[]");
}

void PrinterLLVM::asmWriterEmitRegAsmOffsets(
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

void PrinterLLVM::asmWriterEmitAltIdxSwitch(
    bool HasAltNames, std::vector<Record *> const &AltNameIndices,
    StringRef const &Namespace) const {
  if (HasAltNames) {
    OS << "  switch(AltIdx) {\n"
       << "  default: llvm_unreachable(\"Invalid register alt name "
          "index!\");\n";
    for (const Record *R : AltNameIndices) {
      StringRef const AltName = R->getName();
      OS << "  case ";
      if (!Namespace.empty())
        OS << Namespace << "::";
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
          OS << Namespace << "::";
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
  OS << "}\n";
}

char const *PrinterLLVM::asmWriterGetPatCondKIgnore() const {
  return "AliasPatternCond::K_Ignore, 0";
}

char const *PrinterLLVM::asmWriterGetPatCondKRegClass() const {
  return "AliasPatternCond::K_RegClass, {0}::{1}RegClassID";
}

char const *PrinterLLVM::asmWriterGetPatCondKTiedReg() const {
  return "AliasPatternCond::K_TiedReg, {0}";
}

char const *PrinterLLVM::asmWriterGetPatCondKCustom() const {
  return "AliasPatternCond::K_Custom, {0}";
}

char const *PrinterLLVM::asmWriterGetPatCondKImm() const {
  return "AliasPatternCond::K_Imm, uint32_t({0})";
}

char const *PrinterLLVM::asmWriterGetPatCondKNoReg() const {
  return "AliasPatternCond::K_Reg, {0}::NoRegister";
}

char const *PrinterLLVM::asmWriterGetPatCondKReg() const {
  return "AliasPatternCond::K_Reg, {0}::{1}";
}

char const *PrinterLLVM::asmWriterGetPatCondKFeature() const {
  return "AliasPatternCond::K_{0}{1}Feature, {2}::{3}";
}

char const *PrinterLLVM::asmWriterGetPatCondKEndOrFeature() const {
  return "AliasPatternCond::K_EndOrFeatures, 0";
}

char const *PrinterLLVM::asmWriterGetPatOpcStart() const {
  return "    // {0} - {1}\n";
}

char const *PrinterLLVM::asmWriterGetCondPatStart() const {
  return "    // {0} - {1}\n";
}

std::string PrinterLLVM::asmWriterGetCond(std::string const &Cond) const {
  return formatv("    {{{0}},\n", Cond);
}

char const *PrinterLLVM::asmWriterGetPatternFormat() const {
  return "    {{{0}, {1}, {2}, {3} },\n";
}

char const *PrinterLLVM::asmWriterGetOpcodeFormat() const {
  return "    {{{0}, {1}, {2} },\n";
}

void PrinterLLVM::asmWriterEmitPrintAliasInstrHeader(
    std::string const &TargetName, StringRef const &ClassName,
    bool PassSubtarget) const {
  OS << "bool " << TargetName << ClassName << "::printAliasInstr(const MCInst"
     << " *MI, uint64_t Address, "
     << (PassSubtarget ? "const MCSubtargetInfo &STI, " : "")
     << "raw_ostream &OS) {\n";
}

void PrinterLLVM::asmWriterEmitPrintAliasInstrBodyRetFalse() const {
  OS << "  return false;\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmWriterEmitDeclValid(std::string const &TargetName,
                                         StringRef const &ClassName) const {
  OS << "static bool " << TargetName << ClassName
     << "ValidateMCOperand(const MCOperand &MCOp,\n"
     << "                  const MCSubtargetInfo &STI,\n"
     << "                  unsigned PredicateIndex);\n";
}

void PrinterLLVM::asmWriterEmitPrintAliasInstrBody(
    raw_string_ostream &OpcodeO, raw_string_ostream &PatternO,
    raw_string_ostream &CondO,
    std::vector<std::pair<uint32_t, std::string>> const &AsmStrings,
    std::vector<const Record *> const &MCOpPredicates,
    std::string const &TargetName, StringRef const &ClassName,
    bool PassSubtarget) const {
  OS.indent(2) << "static const PatternsForOpcode OpToPatterns[] = {\n";
  OS << OpcodeO.str();
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const AliasPattern Patterns[] = {\n";
  OS << PatternO.str();
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const AliasPatternCond Conds[] = {\n";
  OS << CondO.str();
  OS.indent(2) << "};\n\n";
  OS.indent(2) << "static const char AsmStrings[] =\n";
  for (const auto &P : AsmStrings) {
    OS.indent(4) << "/* " << P.first << " */ \"" << P.second << "\\0\"\n";
  }

  OS.indent(2) << ";\n\n";

  // Assert that the opcode table is sorted. Use a static local constructor to
  // ensure that the check only happens once on first run.
  OS << "#ifndef NDEBUG\n";
  OS.indent(2) << "static struct SortCheck {\n";
  OS.indent(2) << "  SortCheck(ArrayRef<PatternsForOpcode> OpToPatterns) {\n";
  OS.indent(2) << "    assert(std::is_sorted(\n";
  OS.indent(2) << "               OpToPatterns.begin(), OpToPatterns.end(),\n";
  OS.indent(2) << "               [](const PatternsForOpcode &L, const "
                 "PatternsForOpcode &R) {\n";
  OS.indent(2) << "                 return L.Opcode < R.Opcode;\n";
  OS.indent(2) << "               }) &&\n";
  OS.indent(2) << "           \"tablegen failed to sort opcode patterns\");\n";
  OS.indent(2) << "  }\n";
  OS.indent(2) << "} sortCheckVar(OpToPatterns);\n";
  OS << "#endif\n\n";

  OS.indent(2) << "AliasMatchingData M {\n";
  OS.indent(2) << "  ArrayRef(OpToPatterns),\n";
  OS.indent(2) << "  ArrayRef(Patterns),\n";
  OS.indent(2) << "  ArrayRef(Conds),\n";
  OS.indent(2) << "  StringRef(AsmStrings, std::size(AsmStrings)),\n";
  if (MCOpPredicates.empty())
    OS.indent(2) << "  nullptr,\n";
  else
    OS.indent(2) << "  &" << TargetName << ClassName << "ValidateMCOperand,\n";
  OS.indent(2) << "};\n";

  OS.indent(2) << "const char *AsmString = matchAliasPatterns(MI, "
              << (PassSubtarget ? "&STI" : "nullptr") << ", M);\n";
  OS.indent(2) << "if (!AsmString) return false;\n\n";

  // Code that prints the alias, replacing the operands with the ones from the
  // MCInst.
  OS << "  unsigned I = 0;\n";
  OS << "  while (AsmString[I] != ' ' && AsmString[I] != '\\t' &&\n";
  OS << "         AsmString[I] != '$' && AsmString[I] != '\\0')\n";
  OS << "    ++I;\n";
  OS << "  OS << '\\t' << StringRef(AsmString, I);\n";

  OS << "  if (AsmString[I] != '\\0') {\n";
  OS << "    if (AsmString[I] == ' ' || AsmString[I] == '\\t') {\n";
  OS << "      OS << '\\t';\n";
  OS << "      ++I;\n";
  OS << "    }\n";
  OS << "    do {\n";
  OS << "      if (AsmString[I] == '$') {\n";
  OS << "        ++I;\n";
  OS << "        if (AsmString[I] == (char)0xff) {\n";
  OS << "          ++I;\n";
  OS << "          int OpIdx = AsmString[I++] - 1;\n";
  OS << "          int PrintMethodIdx = AsmString[I++] - 1;\n";
  OS << "          printCustomAliasOperand(MI, Address, OpIdx, PrintMethodIdx, ";
  OS << (PassSubtarget ? "STI, " : "");
  OS << "OS);\n";
  OS << "        } else\n";
  OS << "          printOperand(MI, unsigned(AsmString[I++]) - 1, ";
  OS << (PassSubtarget ? "STI, " : "");
  OS << "OS);\n";
  OS << "      } else {\n";
  OS << "        OS << AsmString[I++];\n";
  OS << "      }\n";
  OS << "    } while (AsmString[I] != '\\0');\n";
  OS << "  }\n\n";

  OS << "  return true;\n";
  OS << "}\n\n";

}

void PrinterLLVM::asmWriterEmitPrintAliasOp(
    std::string const &TargetName, StringRef const &ClassName,
    std::vector<std::pair<std::string, bool>> const &PrintMethods,
    bool PassSubtarget) const {
  OS << "void " << TargetName << ClassName << "::"
     << "printCustomAliasOperand(\n"
     << "         const MCInst *MI, uint64_t Address, unsigned OpIdx,\n"
     << "         unsigned PrintMethodIdx,\n"
     << (PassSubtarget ? "         const MCSubtargetInfo &STI,\n" : "")
     << "         raw_ostream &OS) {\n";
  if (PrintMethods.empty())
    OS << "  llvm_unreachable(\"Unknown PrintMethod kind\");\n";
  else {
    OS << "  switch (PrintMethodIdx) {\n"
       << "  default:\n"
       << "    llvm_unreachable(\"Unknown PrintMethod kind\");\n"
       << "    break;\n";

    for (unsigned I = 0; I < PrintMethods.size(); ++I) {
      OS << "  case " << I << ":\n"
         << "    " << PrintMethods[I].first << "(MI, "
         << (PrintMethods[I].second ? "Address, " : "") << "OpIdx, "
         << (PassSubtarget ? "STI, " : "") << "OS);\n"
         << "    break;\n";
    }
    OS << "  }\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::asmWriterEmitPrintMC(
    std::string const &TargetName, StringRef const &ClassName,
    std::vector<const Record *> const &MCOpPredicates) const {
  if (!MCOpPredicates.empty()) {
    OS << "static bool " << TargetName << ClassName
       << "ValidateMCOperand(const MCOperand &MCOp,\n"
       << "                  const MCSubtargetInfo &STI,\n"
       << "                  unsigned PredicateIndex) {\n"
       << "  switch (PredicateIndex) {\n"
       << "  default:\n"
       << "    llvm_unreachable(\"Unknown MCOperandPredicate kind\");\n"
       << "    break;\n";

    for (unsigned I = 0; I < MCOpPredicates.size(); ++I) {
      StringRef const MCOpPred =
          MCOpPredicates[I]->getValueAsString("MCOperandPredicate");
      OS << "  case " << I + 1 << ": {\n"
         << MCOpPred.data() << "\n"
         << "    }\n";
    }
    OS << "  }\n"
       << "}\n\n";
  }
}

//-------------------------
// Backend: Subtarget
//-------------------------

void PrinterLLVM::subtargetEmitSourceFileHeader() const {
  emitSourceFileHeader("Subtarget Enumeration Source Fragment", OS);
}

void PrinterLLVM::subtargetEmitFeatureEnum(
    DenseMap<Record *, unsigned> &FeatureMap,
    std::vector<Record *> const &DefList, unsigned N) const {
  // Open enumeration.
  OS << "enum {\n";

  // For each record
  for (unsigned I = 0; I < N; ++I) {
    // Next record
    Record *Def = DefList[I];

    // Get and emit name
    OS << "  " << Def->getName() << " = " << I << ",\n";

    // Save the index for this feature.
    FeatureMap[Def] = I;
  }

  OS << "  "
     << "NumSubtargetFeatures = " << N << "\n";

  // Close enumeration and namespace
  OS << "};\n";
}

void PrinterLLVM::subtargetEmitGetSTIMacro(StringRef const &Value,
                                           StringRef const &Attribute) const {
  // Some features default to true, with values set to false if enabled.
  const char *Default = Value == "false" ? "true" : "false";

  // Define the getter with lowercased first char: xxxYyy() { return XxxYyy; }
  const std::string Getter =
      Attribute.substr(0, 1).lower() + Attribute.substr(1).str();

  OS << "GET_SUBTARGETINFO_MACRO(" << Attribute << ", " << Default << ", "
     << Getter << ")\n";
}

void PrinterLLVM::subtargetEmitHwModes(CodeGenHwModes const &CGH,
                                       std::string const &ClassName) const {
  OS << "unsigned " << ClassName << "::getHwMode() const {\n";
  for (unsigned M = 1, NumModes = CGH.getNumModeIds(); M != NumModes; ++M) {
    const HwMode &HM = CGH.getMode(M);
    OS << "  if (checkFeatures(\"" << HM.Features << "\")) return " << M
       << ";\n";
  }
  OS << "  return 0;\n}\n";
}

void PrinterLLVM::subtargetEmitFeatureKVHeader(
    std::string const &Target) const {
  // Begin feature table
  OS << "// Sorted (by key) array of values for CPU features.\n"
     << "extern const llvm::SubtargetFeatureKV " << Target
     << "FeatureKV[] = {\n";
}

void PrinterLLVM::subtargetEmitFeatureKVPartI(std::string const &Target,
                                              StringRef const &CommandLineName,
                                              StringRef const &Name,
                                              StringRef const &Desc) const {
  // Emit as { "feature", "description", { featureEnum }, { i1 , i2 , ... , in }
  OS << "  { "
     << "\"" << CommandLineName << "\", "
     << "\"" << Desc << "\", " << Target << "::" << Name << ", ";
}

void PrinterLLVM::subtargetEmitFeatureKVPartII() const { OS << " },\n"; }

void PrinterLLVM::subtargetEmitPrintFeatureMask(
    std::array<uint64_t, MAX_SUBTARGET_WORDS> const &Mask) const {
  OS << "{ { { ";
  for (unsigned I = 0; I != Mask.size(); ++I) {
    OS << "0x";
    OS.write_hex(Mask[I]);
    OS << "ULL, ";
  }
  OS << "} } }";
}

void PrinterLLVM::subtargetEmitFeatureKVEnd() const { OS << "};\n"; }

void PrinterLLVM::subtargetEmitCPUKVHeader(std::string const &Target) const {
  OS << "// Sorted (by key) array of values for CPU subtype.\n"
     << "extern const llvm::SubtargetSubTypeKV " << Target
     << "SubTypeKV[] = {\n";
}

void PrinterLLVM::subtargetEmitCPUKVEnd() const { OS << "};\n"; }

void PrinterLLVM::subtargetEmitCPUKVPartI(StringRef const &Name) const {
  // Emit as { "cpu", "description", 0, { f1 , f2 , ... fn } },
  OS << " { "
     << "\"" << Name << "\", ";
}

void PrinterLLVM::subtargetEmitCPUKVPartII() const { OS << ", "; }

void PrinterLLVM::subtargetEmitCPUKVPartIII(
    std::string const &ProcModelName) const {
  OS << ", &" << ProcModelName << " },\n";
}

void PrinterLLVM::subtargetEmitDBGMacrosBegin() const {
  OS << "#ifdef DBGFIELD\n"
     << "#error \"<target>GenSubtargetInfo.inc requires a DBGFIELD macro\"\n"
     << "#endif\n"
     << "#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)\n"
     << "#define DBGFIELD(x) x,\n"
     << "#else\n"
     << "#define DBGFIELD(x)\n"
     << "#endif\n";
}

void PrinterLLVM::subtargetEmitDBGMacrosEnd() const {
  OS << "\n#undef DBGFIELD\n";
}

void PrinterLLVM::subtargetEmitFunctionalItinaryUnits(
    CodeGenSchedModels const &SchedModels) const {

  // Multiple processor models may share an itinerary record. Emit it once.
  SmallPtrSet<Record *, 8> ItinsDefSet;

  // Emit functional units for all the itineraries.
  for (const CodeGenProcModel &ProcModel : SchedModels.procModels()) {

    if (!ItinsDefSet.insert(ProcModel.ItinsDef).second)
      continue;

    RecVec FUs = ProcModel.ItinsDef->getValueAsListOfDefs("FU");
    if (FUs.empty())
      continue;

    StringRef const Name = ProcModel.ItinsDef->getName();
    OS << "\n// Functional units for \"" << Name << "\"\n"
       << "namespace " << Name << "FU {\n";

    for (unsigned J = 0, FUN = FUs.size(); J < FUN; ++J)
      OS << "  const InstrStage::FuncUnits " << FUs[J]->getName()
         << " = 1ULL << " << J << ";\n";

    OS << "} // end namespace " << Name << "FU\n";

    RecVec BPs = ProcModel.ItinsDef->getValueAsListOfDefs("BP");
    if (!BPs.empty()) {
      OS << "\n// Pipeline forwarding paths for itineraries \"" << Name
         << "\"\n"
         << "namespace " << Name << "Bypass {\n";

      OS << "  const unsigned NoBypass = 0;\n";
      for (unsigned J = 0, BPN = BPs.size(); J < BPN; ++J)
        OS << "  const unsigned " << BPs[J]->getName() << " = 1 << " << J
           << ";\n";

      OS << "} // end namespace " << Name << "Bypass\n";
    }
  }
}

std::string const
PrinterLLVM::subtargetGetBeginStageTable(std::string const &TargetName) const {
  return "\nextern const llvm::InstrStage " + TargetName + "Stages[] = {\n" +
         "  { 0, 0, 0, llvm::InstrStage::Required }, // No itinerary\n";
}

std::string const PrinterLLVM::subtargetGetBeginOperandCycleTable(
    std::string const &TargetName) const {
  return "extern const unsigned " + TargetName + "OperandCycles[] = {\n" +
         "  0, // No itinerary\n";
}

std::string const
PrinterLLVM::subtargetGetBeginBypassTable(std::string const &TargetName) const {
  return "extern const unsigned " + TargetName + "ForwardingPaths[] = {\n" +
         " 0, // No itinerary\n";
}

std::string const PrinterLLVM::subtargetGetEndStageTable() const {
  return "  { 0, 0, 0, llvm::InstrStage::Required } // End stages\n};\n";
}

std::string const PrinterLLVM::subtargetGetEndOperandCycleTable() const {
  return "  0 // End operand cycles\n};\n";
}

std::string const PrinterLLVM::subtargetGetEndBypassTable() const {
  return " 0 // End bypass tables\n};\n";
}

// subtargetFormItineraryStageString - Compose a string containing the stage
// data initialization for the specified itinerary.  N is the number
// of stages.
void PrinterLLVM::subtargetFormItineraryStageString(std::string const &Name,
                                                    Record *ItinData,
                                                    std::string &ItinString,
                                                    unsigned &NStages) const {
  // Get states list
  RecVec StageList = ItinData->getValueAsListOfDefs("Stages");

  // For each stage
  unsigned const N = NStages = StageList.size();
  for (unsigned I = 0; I < N;) {
    // Next stage
    const Record *Stage = StageList[I];

    // Form string as ,{ cycles, u1 | u2 | ... | un, timeinc, kind }
    int const Cycles = Stage->getValueAsInt("Cycles");
    ItinString += "  { " + itostr(Cycles) + ", ";

    // Get unit list
    RecVec UnitList = Stage->getValueAsListOfDefs("Units");

    // For each unit
    for (unsigned J = 0, M = UnitList.size(); J < M;) {
      // Add name and bitwise or
      ItinString += Name + "FU::" + UnitList[J]->getName().str();
      if (++J < M)
        ItinString += " | ";
    }

    int const TimeInc = Stage->getValueAsInt("TimeInc");
    ItinString += ", " + itostr(TimeInc);

    int const Kind = Stage->getValueAsInt("Kind");
    ItinString += ", (llvm::InstrStage::ReservationKinds)" + itostr(Kind);

    // Close off stage
    ItinString += " }";
    if (++I < N)
      ItinString += ", ";
  }
}

// FormItineraryOperandCycleString - Compose a string containing the
// operand cycle initialization for the specified itinerary.  N is the
// number of operands that has cycles specified.
void PrinterLLVM::subtargetFormItineraryOperandCycleString(
    Record *ItinData, std::string &ItinString, unsigned &NOperandCycles) const {
  // Get operand cycle list
  std::vector<int64_t> OperandCycleList =
      ItinData->getValueAsListOfInts("OperandCycles");

  // For each operand cycle
  NOperandCycles = OperandCycleList.size();
  ListSeparator LS;
  for (int OCycle : OperandCycleList) {
    // Next operand cycle
    ItinString += LS;
    ItinString += "  " + itostr(OCycle);
  }
}

void PrinterLLVM::subtargetFormItineraryBypassString(
    const std::string &Name, Record *ItinData, std::string &ItinString,
    unsigned NOperandCycles) const {
  RecVec BypassList = ItinData->getValueAsListOfDefs("Bypasses");
  unsigned const N = BypassList.size();
  unsigned I = 0;
  ListSeparator LS;
  for (; I < N; ++I) {
    ItinString += LS;
    ItinString += Name + "Bypass::" + BypassList[I]->getName().str();
  }
  for (; I < NOperandCycles; ++I) {
    ItinString += LS;
    ItinString += " 0";
  }
}

std::string
PrinterLLVM::subtargetGetStageEntryPartI(std::string const &ItinStageString,
                                         unsigned StageCount) const {
  return ItinStageString + ", // " + itostr(StageCount);
}
std::string PrinterLLVM::subtargetGetStageEntryPartII(unsigned StageCount,
                                                      unsigned NStages) const {
  return "-" + itostr(StageCount + NStages - 1);
}
std::string PrinterLLVM::subtargetGetStageEntryPartIII() const { return "\n"; }

std::string PrinterLLVM::subtargetGetOperandCycleEntryPartI(
    std::string const &ItinOperandCycleString) const {
  return ItinOperandCycleString + ", // ";
}

std::string PrinterLLVM::subtargetGetOperandCycleEntryPartII(
    unsigned OperandCycleCount, unsigned NOperandCycles) const {
  return "-" + itostr(OperandCycleCount + NOperandCycles - 1);
}

std::string PrinterLLVM::subtargetGetOperandCycleEntryPartIII(
    std::string const &OperandIdxComment) const {
  return OperandIdxComment + "\n";
}

std::string PrinterLLVM::subtargetGetOperandCycleEntryPartIV(
    std::string const &ItinBypassString,
    std::string const &OperandIdxComment) const {
  return ItinBypassString + ", // " + OperandIdxComment + "\n";
}

void PrinterLLVM::subtargetEmitProcessorItineraryTable(
    std::string const &ItinsDefName, std::vector<InstrItinerary> &ItinList,
    CodeGenSchedModels const &SchedModels) const {
  OS << "\n";
  OS << "static const llvm::InstrItinerary ";

  // Begin processor itinerary table
  OS << ItinsDefName << "[] = {\n";

  // For each itinerary class in CodeGenSchedClass::Index order.
  for (unsigned J = 0, M = ItinList.size(); J < M; ++J) {
    InstrItinerary const &Intinerary = ItinList[J];

    // Emit Itinerary in the form of
    // { firstStage, lastStage, firstCycle, lastCycle } // index
    OS << "  { " << Intinerary.NumMicroOps << ", " << Intinerary.FirstStage
       << ", " << Intinerary.LastStage << ", " << Intinerary.FirstOperandCycle
       << ", " << Intinerary.LastOperandCycle << " }"
       << ", // " << J << " " << SchedModels.getSchedClass(J).Name << "\n";
  }
  // End processor itinerary table
  OS << "  { 0, uint16_t(~0U), uint16_t(~0U), uint16_t(~0U), uint16_t(~0U) }"
        "// end marker\n";
  OS << "};\n";
}

void PrinterLLVM::subtargetEmitPreOperandTableComment() const {
  OS << "\n// ===============================================================\n"
     << "// Data tables for the new per-operand machine model.\n";
}

// Emit SchedClass tables for all processors and associated global tables.
void PrinterLLVM::subtargetEmitSchedClassTables(
    SchedClassTablesT &SchedTables, std::string const &TargetName,
    CodeGenSchedModels const &SchedModels) const {
  // Emit global WriteProcResTable.
  OS << "\n// {ProcResourceIdx, Cycles}\n"
     << "extern const llvm::MCWriteProcResEntry " << TargetName
     << "WriteProcResTable[] = {\n"
     << "  { 0,  0}, // Invalid\n";
  for (unsigned WPRIdx = 1, WPREnd = SchedTables.WriteProcResources.size();
       WPRIdx != WPREnd; ++WPRIdx) {
    MCWriteProcResEntry const &WPREntry =
        SchedTables.WriteProcResources[WPRIdx];
    OS << "  {" << format("%2d", WPREntry.ProcResourceIdx) << ", "
       << format("%2d", WPREntry.Cycles) << "}";
    if (WPRIdx + 1 < WPREnd)
      OS << ',';
    OS << " // #" << WPRIdx << '\n';
  }
  OS << "}; // " << TargetName << "WriteProcResTable\n";

  // Emit global WriteLatencyTable.
  OS << "\n// {Cycles, WriteResourceID}\n"
     << "extern const llvm::MCWriteLatencyEntry " << TargetName
     << "WriteLatencyTable[] = {\n"
     << "  { 0,  0}, // Invalid\n";
  for (unsigned WLIdx = 1, WLEnd = SchedTables.WriteLatencies.size();
       WLIdx != WLEnd; ++WLIdx) {
    MCWriteLatencyEntry &WLEntry = SchedTables.WriteLatencies[WLIdx];
    OS << "  {" << format("%2d", WLEntry.Cycles) << ", "
       << format("%2d", WLEntry.WriteResourceID) << "}";
    if (WLIdx + 1 < WLEnd)
      OS << ',';
    OS << " // #" << WLIdx << " " << SchedTables.WriterNames[WLIdx] << '\n';
  }
  OS << "}; // " << TargetName << "WriteLatencyTable\n";

  // Emit global ReadAdvanceTable.
  OS << "\n// {UseIdx, WriteResourceID, Cycles}\n"
     << "extern const llvm::MCReadAdvanceEntry " << TargetName
     << "ReadAdvanceTable[] = {\n"
     << "  {0,  0,  0}, // Invalid\n";
  for (unsigned RAIdx = 1, RAEnd = SchedTables.ReadAdvanceEntries.size();
       RAIdx != RAEnd; ++RAIdx) {
    MCReadAdvanceEntry &RAEntry = SchedTables.ReadAdvanceEntries[RAIdx];
    OS << "  {" << RAEntry.UseIdx << ", "
       << format("%2d", RAEntry.WriteResourceID) << ", "
       << format("%2d", RAEntry.Cycles) << "}";
    if (RAIdx + 1 < RAEnd)
      OS << ',';
    OS << " // #" << RAIdx << '\n';
  }
  OS << "}; // " << TargetName << "ReadAdvanceTable\n";

  // Emit a SchedClass table for each processor.
  for (CodeGenSchedModels::ProcIter PI = SchedModels.procModelBegin(),
                                    PM = SchedModels.procModelEnd();
       PI != PM; ++PI) {
    if (!PI->hasInstrSchedModel())
      continue;

    std::vector<MCSchedClassDesc> &SCTab =
        SchedTables.ProcSchedClasses[1 + (PI - SchedModels.procModelBegin())];

    OS << "\n// {Name, NumMicroOps, BeginGroup, EndGroup, RetireOOO,"
       << " WriteProcResIdx,#, WriteLatencyIdx,#, ReadAdvanceIdx,#}\n";
    OS << "static const llvm::MCSchedClassDesc " << PI->ModelName
       << "SchedClasses[] = {\n";

    // The first class is always invalid. We no way to distinguish it except by
    // name and position.
    assert(SchedModels.getSchedClass(0).Name == "NoInstrModel" &&
           "invalid class not first");
    OS << "  {DBGFIELD(\"InvalidSchedClass\")  "
       << MCSchedClassDesc::InvalidNumMicroOps
       << ", false, false, false, 0, 0,  0, 0,  0, 0},\n";

    for (unsigned SCIdx = 1, SCEnd = SCTab.size(); SCIdx != SCEnd; ++SCIdx) {
      MCSchedClassDesc &MCDesc = SCTab[SCIdx];
      const CodeGenSchedClass &SchedClass = SchedModels.getSchedClass(SCIdx);
      OS << "  {DBGFIELD(\"" << SchedClass.Name << "\") ";
      if (SchedClass.Name.size() < 18)
        OS.indent(18 - SchedClass.Name.size());
      OS << MCDesc.NumMicroOps << ", " << (MCDesc.BeginGroup ? "true" : "false")
         << ", " << (MCDesc.EndGroup ? "true" : "false") << ", "
         << (MCDesc.RetireOOO ? "true" : "false") << ", "
         << format("%2d", MCDesc.WriteProcResIdx) << ", "
         << MCDesc.NumWriteProcResEntries << ", "
         << format("%2d", MCDesc.WriteLatencyIdx) << ", "
         << MCDesc.NumWriteLatencyEntries << ", "
         << format("%2d", MCDesc.ReadAdvanceIdx) << ", "
         << MCDesc.NumReadAdvanceEntries << "}, // #" << SCIdx << '\n';
    }
    OS << "}; // " << PI->ModelName << "SchedClasses\n";
  }
}

unsigned PrinterLLVM::subtargetEmitRegisterFileTables(
    CodeGenProcModel const &ProcModel) const {
  // Print the RegisterCost table first.
  OS << "\n// {RegisterClassID, Register Cost, AllowMoveElimination }\n";
  OS << "static const llvm::MCRegisterCostEntry " << ProcModel.ModelName
     << "RegisterCosts"
     << "[] = {\n";

  for (const CodeGenRegisterFile &RF : ProcModel.RegisterFiles) {
    // Skip register files with a default cost table.
    if (RF.hasDefaultCosts())
      continue;
    // Add entries to the cost table.
    for (const CodeGenRegisterCost &RC : RF.Costs) {
      OS << "  { ";
      Record *Rec = RC.RCDef;
      if (Rec->getValue("Namespace"))
        OS << Rec->getValueAsString("Namespace") << "::";
      OS << Rec->getName() << "RegClassID, " << RC.Cost << ", "
         << RC.AllowMoveElimination << "},\n";
    }
  }
  OS << "};\n";

  // Now generate a table with register file info.
  OS << "\n // {Name, #PhysRegs, #CostEntries, IndexToCostTbl, "
     << "MaxMovesEliminatedPerCycle, AllowZeroMoveEliminationOnly }\n";
  OS << "static const llvm::MCRegisterFileDesc " << ProcModel.ModelName
     << "RegisterFiles"
     << "[] = {\n"
     << "  { \"InvalidRegisterFile\", 0, 0, 0, 0, 0 },\n";
  unsigned CostTblIndex = 0;

  for (const CodeGenRegisterFile &RD : ProcModel.RegisterFiles) {
    OS << "  { ";
    OS << '"' << RD.Name << '"' << ", " << RD.NumPhysRegs << ", ";
    unsigned NumCostEntries = RD.Costs.size();
    OS << NumCostEntries << ", " << CostTblIndex << ", "
       << RD.MaxMovesEliminatedPerCycle << ", "
       << RD.AllowZeroMoveEliminationOnly << "},\n";
    CostTblIndex += NumCostEntries;
  }
  OS << "};\n";

  return CostTblIndex;
}

void PrinterLLVM::subtargetEmitMCExtraProcInfoTableHeader(
    std::string const &ProcModelName) const {
  OS << "\nstatic const llvm::MCExtraProcessorInfo " << ProcModelName
     << "ExtraInfo = {\n  ";
}

void PrinterLLVM::subtargetEmitMCExtraProcInfoTableEnd() const { OS << "};\n"; }

void PrinterLLVM::subtargetEmitReorderBufferSize(
    int64_t ReorderBufferSize) const {
  OS << ReorderBufferSize << ", // ReorderBufferSize\n  ";
}

void PrinterLLVM::subtargetEmitMaxRetirePerCycle(
    int64_t MaxRetirePerCycle) const {
  OS << MaxRetirePerCycle << ", // MaxRetirePerCycle\n  ";
}

void PrinterLLVM::subtargetEmitRegisterFileInfo(
    CodeGenProcModel const &ProcModel, unsigned NumRegisterFiles,
    unsigned NumCostEntries) const {
  if (NumRegisterFiles)
    OS << ProcModel.ModelName << "RegisterFiles,\n  " << (1 + NumRegisterFiles);
  else
    OS << "nullptr,\n  0";

  OS << ", // Number of register files.\n  ";
  if (NumCostEntries)
    OS << ProcModel.ModelName << "RegisterCosts,\n  ";
  else
    OS << "nullptr,\n  ";
  OS << NumCostEntries << ", // Number of register cost entries.\n";
}

void PrinterLLVM::subtargetEmitResourceDescriptorLoadQueue(
    unsigned QueueID) const {
  OS << "  " << QueueID << ", // Resource Descriptor for the Load Queue\n";
}

void PrinterLLVM::subtargetEmitResourceDescriptorStoreQueue(
    unsigned QueueID) const {
  OS << "  " << QueueID << ", // Resource Descriptor for the Store Queue\n";
}

void PrinterLLVM::subtargetEmitProcessorResourceSubUnits(
    const CodeGenProcModel &ProcModel,
    CodeGenSchedModels const &SchedModels) const {
  OS << "\nstatic const unsigned " << ProcModel.ModelName
     << "ProcResourceSubUnits[] = {\n"
     << "  0,  // Invalid\n";

  for (unsigned I = 0, E = ProcModel.ProcResourceDefs.size(); I < E; ++I) {
    Record *PRDef = ProcModel.ProcResourceDefs[I];
    if (!PRDef->isSubClassOf("ProcResGroup"))
      continue;
    RecVec const ResUnits = PRDef->getValueAsListOfDefs("Resources");
    for (Record *RUDef : ResUnits) {
      Record *const RU =
          SchedModels.findProcResUnits(RUDef, ProcModel, PRDef->getLoc());
      for (unsigned J = 0; J < RU->getValueAsInt("NumUnits"); ++J) {
        OS << "  " << ProcModel.getProcResourceIdx(RU) << ", ";
      }
    }
    OS << "  // " << PRDef->getName() << "\n";
  }
  OS << "};\n";
}

void PrinterLLVM::subtargetEmitMCProcResourceDescHeader(
    std::string const &ProcModelName) const {
  OS << "\n// {Name, NumUnits, SuperIdx, BufferSize, SubUnitsIdxBegin}\n";
  OS << "static const llvm::MCProcResourceDesc " << ProcModelName
     << "ProcResources"
     << "[] = {\n"
     << "  {\"InvalidUnit\", 0, 0, 0, 0},\n";
}

void PrinterLLVM::subtargetEmitMCProcResourceDescEnd() const { OS << "};\n"; }

void PrinterLLVM::subtargetEmitMCProcResourceDesc(
    Record const *PRDef, Record const *SuperDef,
    std::string const &ProcModelName, unsigned SubUnitsOffset,
    unsigned SuperIdx, unsigned NumUnits, int BufferSize, unsigned I,
    unsigned const SubUnitsBeginOffset) const {
  // Emit the ProcResourceDesc
  OS << "  {\"" << PRDef->getName() << "\", ";
  if (PRDef->getName().size() < 15)
    OS.indent(15 - PRDef->getName().size());
  OS << NumUnits << ", " << SuperIdx << ", " << BufferSize << ", ";
  if (SubUnitsBeginOffset != SubUnitsOffset) {
    OS << ProcModelName << "ProcResourceSubUnits + " << SubUnitsBeginOffset;
  } else {
    OS << "nullptr";
  }
  OS << "}, // #" << I + 1;
  if (SuperDef)
    OS << ", Super=" << SuperDef->getName();
  OS << "\n";
}

// Emit either the value defined in the TableGen Record, or the default
// value defined in the C++ header. The Record is null if the processor does not
// define a model.
void PrinterLLVM::subtargetEmitProcessorProp(Record const *R,
                                             StringRef const Name,
                                             char Separator) const {
  OS << "  ";
  int const V = R ? R->getValueAsInt(Name) : -1;
  if (V >= 0)
    OS << V << Separator << " // " << Name;
  else
    OS << "MCSchedModel::Default" << Name << Separator;
  OS << '\n';
}

void PrinterLLVM::subtargetEmitProcModelHeader(
    std::string const &ModelName) const {
  OS << "\n";
  OS << "static const llvm::MCSchedModel " << ModelName << " = {\n";
}

void PrinterLLVM::subtargetEmitProcModel(
    CodeGenProcModel const &PM, CodeGenSchedModels const &SchedModels) const {
  bool const PostRAScheduler =
      (PM.ModelDef ? PM.ModelDef->getValueAsBit("PostRAScheduler") : false);

  OS << "  " << (PostRAScheduler ? "true" : "false") << ", // "
     << "PostRAScheduler\n";

  bool const CompleteModel =
      (PM.ModelDef ? PM.ModelDef->getValueAsBit("CompleteModel") : false);

  OS << "  " << (CompleteModel ? "true" : "false") << ", // "
     << "CompleteModel\n";

  OS << "  " << PM.Index << ", // Processor ID\n";
  if (PM.hasInstrSchedModel())
    OS << "  " << PM.ModelName << "ProcResources"
       << ",\n"
       << "  " << PM.ModelName << "SchedClasses"
       << ",\n"
       << "  " << PM.ProcResourceDefs.size() + 1 << ",\n"
       << "  " << (SchedModels.schedClassEnd() - SchedModels.schedClassBegin())
       << ",\n";
  else
    OS << "  nullptr, nullptr, 0, 0,"
       << " // No instruction-level machine model.\n";
  if (PM.hasItineraries())
    OS << "  " << PM.ItinsDef->getName() << ",\n";
  else
    OS << "  nullptr, // No Itinerary\n";
  if (PM.hasExtraProcessorInfo())
    OS << "  &" << PM.ModelName << "ExtraInfo,\n";
  else
    OS << "  nullptr // No extra processor descriptor\n";
  OS << "};\n";
}

void PrinterLLVM::subtargetEmitResolveVariantSchedClassImplHdr() const {
  OS << "unsigned resolveVariantSchedClassImpl(unsigned SchedClass,\n"
     << "    const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID) {\n";
}

void PrinterLLVM::subtargetEmitResolveVariantSchedClassImplEnd() const {
  OS << "}\n";
}

void PrinterLLVM::subtargetEmitSchedClassSwitch() const {
  OS << "  switch (SchedClass) {\n";
}

void PrinterLLVM::subtargetEmitSchedClassCase(unsigned VC,
                                              std::string const &SCName) const {
  OS << "  case " << VC << ": // " << SCName << '\n';
}

void PrinterLLVM::subtargetEmitSchedClassProcGuard(
    unsigned Pi, bool OnlyExpandMCInstPredicates,
    std::string const &ModelName) const {
  OS << "    ";

  // Emit a guard on the processor ID.
  if (Pi != 0) {
    OS << (OnlyExpandMCInstPredicates ? "if (CPUID == "
                                      : "if (SchedModel->getProcessorID() == ");
    OS << Pi << ") ";
    OS << "{ // " << ModelName << '\n';
  }
}

// Indent <= -1 (default = -1) means previous PE indent level.
void PrinterLLVM::subtargetEmitPredicates(
    CodeGenSchedTransition const &T, CodeGenSchedClass const &SC,
    bool (*IsTruePredicate)(Record const *Rec), int Indent) const {
  if (Indent > -1)
    PE->setIndentLevel(Indent);
  std::string Buffer;
  raw_string_ostream SS(Buffer);

  // If not all predicates are MCTrue, then we need an if-stmt.
  unsigned const NumNonTruePreds =
      T.PredTerm.size() - count_if(T.PredTerm, IsTruePredicate);

  SS.indent(PE->getIndentLevel() * 2);

  if (NumNonTruePreds) {
    bool FirstNonTruePredicate = true;
    SS << "if (";

    PE->setIndentLevel(PE->getIndentLevel() + 2);

    for (const Record *Rec : T.PredTerm) {
      // Skip predicates that evaluate to "true".
      if (IsTruePredicate(Rec))
        continue;

      if (FirstNonTruePredicate) {
        FirstNonTruePredicate = false;
      } else {
        SS << "\n";
        SS.indent(PE->getIndentLevel() * 2);
        SS << "&& ";
      }

      if (Rec->isSubClassOf("MCSchedPredicate")) {
        PE->expandPredicate(SS, Rec->getValueAsDef("Pred"));
        continue;
      }

      // Expand this legacy predicate and wrap it around braces if there is more
      // than one predicate to expand.
      SS << ((NumNonTruePreds > 1) ? "(" : "")
         << Rec->getValueAsString("Predicate")
         << ((NumNonTruePreds > 1) ? ")" : "");
    }

    SS << ")\n"; // end of if-stmt
    PE->decreaseIndentLevel();
    SS.indent(PE->getIndentLevel() * 2);
    PE->decreaseIndentLevel();
  }

  SS << "return " << T.ToClassIdx << "; // " << SC.Name << '\n';
  OS << Buffer;
}

void PrinterLLVM::subtargetEmitProcTransitionEnd() const { OS << "    }\n"; }

void PrinterLLVM::subtargetEmitSchedClassCaseEnd(
    CodeGenSchedClass const &SC) const {
  if (SC.isInferred())
    OS << "    return " << SC.Index << ";\n";
  OS << "    break;\n";
}

void PrinterLLVM::subtargetEmitSchedClassSwitchEnd() const { OS << "  };\n"; }

// Used by method `SubtargetEmitter::emitSchedModelHelpersImpl()` to generate
// epilogue code for the auto-generated helper.
void PrinterLLVM::subtargetEmitSchedModelHelperEpilogue(
    bool ShouldReturnZero) const {
  if (ShouldReturnZero) {
    OS << "  // Don't know how to resolve this scheduling class.\n"
       << "  return 0;\n";
    return;
  }

  OS << "  report_fatal_error(\"Expected a variant SchedClass\");\n";
}

void PrinterLLVM::subtargetEmitGenMCSubtargetInfoClass(
    std::string const &TargetName, bool OverrideGetHwMode) const {
  OS << "struct " << TargetName
     << "GenMCSubtargetInfo : public MCSubtargetInfo {\n";
  OS << "  " << TargetName << "GenMCSubtargetInfo(const Triple &TT,\n"
     << "    StringRef CPU, StringRef TuneCPU, StringRef FS,\n"
     << "    ArrayRef<SubtargetFeatureKV> PF,\n"
     << "    ArrayRef<SubtargetSubTypeKV> PD,\n"
     << "    const MCWriteProcResEntry *WPR,\n"
     << "    const MCWriteLatencyEntry *WL,\n"
     << "    const MCReadAdvanceEntry *RA, const InstrStage *IS,\n"
     << "    const unsigned *OC, const unsigned *FP) :\n"
     << "      MCSubtargetInfo(TT, CPU, TuneCPU, FS, PF, PD,\n"
     << "                      WPR, WL, RA, IS, OC, FP) { }\n\n"
     << "  unsigned resolveVariantSchedClass(unsigned SchedClass,\n"
     << "      const MCInst *MI, const MCInstrInfo *MCII,\n"
     << "      unsigned CPUID) const override {\n"
     << "    return " << TargetName << "_MC"
     << "::resolveVariantSchedClassImpl(SchedClass, MI, MCII, CPUID);\n";
  OS << "  }\n";
  if (OverrideGetHwMode)
    OS << "  unsigned getHwMode() const override;\n";
  OS << "};\n";
}

void PrinterLLVM::subtargetEmitMCSubtargetInfoImpl(
    std::string const &TargetName, unsigned NumFeatures, unsigned NumProcs,
    bool SchedModelHasItin) const {
  OS << "\nstatic inline MCSubtargetInfo *create" << TargetName
     << "MCSubtargetInfoImpl("
     << "const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS) {\n";
  OS << "  return new " << TargetName
     << "GenMCSubtargetInfo(TT, CPU, TuneCPU, FS, ";
  if (NumFeatures)
    OS << TargetName << "FeatureKV, ";
  else
    OS << "std::nullopt, ";
  if (NumProcs)
    OS << TargetName << "SubTypeKV, ";
  else
    OS << "None, ";
  OS << '\n';
  OS.indent(22);
  OS << TargetName << "WriteProcResTable, " << TargetName
     << "WriteLatencyTable, " << TargetName << "ReadAdvanceTable, ";
  OS << '\n';
  OS.indent(22);
  if (SchedModelHasItin) {
    OS << TargetName << "Stages, " << TargetName << "OperandCycles, "
       << TargetName << "ForwardingPaths";
  } else
    OS << "nullptr, nullptr, nullptr";
  OS << ");\n}\n\n";
}

void PrinterLLVM::subtargetEmitIncludeSTIDesc() const {
  OS << "#include \"llvm/Support/Debug.h\"\n";
  OS << "#include \"llvm/Support/raw_ostream.h\"\n\n";
}

void PrinterLLVM::subtargetEmitDFAPacketizerClass(
    std::string const &TargetName, std::string const &ClassName,
    bool OverrideGetHwMode) const {
  OS << "class DFAPacketizer;\n";
  OS << "namespace " << TargetName << "_MC {\n"
     << "unsigned resolveVariantSchedClassImpl(unsigned SchedClass,"
     << " const MCInst *MI, const MCInstrInfo *MCII, unsigned CPUID);\n"
     << "} // end namespace " << TargetName << "_MC\n\n";
  OS << "struct " << ClassName << " : public TargetSubtargetInfo {\n"
     << "  explicit " << ClassName << "(const Triple &TT, StringRef CPU, "
     << "StringRef TuneCPU, StringRef FS);\n"
     << "public:\n"
     << "  unsigned resolveSchedClass(unsigned SchedClass, "
     << " const MachineInstr *DefMI,"
     << " const TargetSchedModel *SchedModel) const override;\n"
     << "  unsigned resolveVariantSchedClass(unsigned SchedClass,"
     << " const MCInst *MI, const MCInstrInfo *MCII,"
     << " unsigned CPUID) const override;\n"
     << "  DFAPacketizer *createDFAPacketizer(const InstrItineraryData *IID)"
     << " const;\n";
  if (OverrideGetHwMode)
    OS << "  unsigned getHwMode() const override;\n";
}

void PrinterLLVM::subtargetEmitDFASubtargetInfoImpl(
    std::string const &TargetName, std::string const &ClassName,
    unsigned NumFeatures, unsigned NumProcs, bool SchedModelHasItin) const {
  OS << ClassName << "::" << ClassName << "(const Triple &TT, StringRef CPU, "
     << "StringRef TuneCPU, StringRef FS)\n"
     << "  : TargetSubtargetInfo(TT, CPU, TuneCPU, FS, ";
  if (NumFeatures)
    OS << "makeArrayRef(" << TargetName << "FeatureKV, " << NumFeatures
       << "), ";
  else
    OS << "std::nullopt, ";
  if (NumProcs)
    OS << "makeArrayRef(" << TargetName << "SubTypeKV, " << NumProcs << "), ";
  else
    OS << "None, ";
  OS << '\n';
  OS.indent(24);
  OS << TargetName << "WriteProcResTable, " << TargetName
     << "WriteLatencyTable, " << TargetName << "ReadAdvanceTable, ";
  OS << '\n';
  OS.indent(24);
  if (SchedModelHasItin) {
    OS << TargetName << "Stages, " << TargetName << "OperandCycles, "
       << TargetName << "ForwardingPaths";
  } else
    OS << "nullptr, nullptr, nullptr";
  OS << ") {}\n\n";
}

void PrinterLLVM::subtargetEmitDFAPacketizerClassEnd() const { OS << "};\n"; }

void PrinterLLVM::subtargetEmitSTICtor() const {
  OS << "#include \"llvm/CodeGen/TargetSchedule.h\"\n\n";
}

void PrinterLLVM::subtargetEmitExternKVArrays(std::string const &TargetName,
                                              bool SchedModelsHasItin) const {
  OS << "extern const llvm::SubtargetFeatureKV " << TargetName
     << "FeatureKV[];\n";
  OS << "extern const llvm::SubtargetSubTypeKV " << TargetName
     << "SubTypeKV[];\n";
  OS << "extern const llvm::MCWriteProcResEntry " << TargetName
     << "WriteProcResTable[];\n";
  OS << "extern const llvm::MCWriteLatencyEntry " << TargetName
     << "WriteLatencyTable[];\n";
  OS << "extern const llvm::MCReadAdvanceEntry " << TargetName
     << "ReadAdvanceTable[];\n";

  if (SchedModelsHasItin) {
    OS << "extern const llvm::InstrStage " << TargetName << "Stages[];\n";
    OS << "extern const unsigned " << TargetName << "OperandCycles[];\n";
    OS << "extern const unsigned " << TargetName << "ForwardingPaths[];\n";
  }
}

void PrinterLLVM::subtargetEmitClassDefs(std::string const &TargetName,
                                         std::string const &ClassName,
                                         unsigned NumFeatures,
                                         unsigned NumProcs,
                                         bool SchedModelsHasItin) const {
  OS << ClassName << "::" << ClassName << "(const Triple &TT, StringRef CPU, "
     << "StringRef TuneCPU, StringRef FS)\n"
     << "  : TargetSubtargetInfo(TT, CPU, TuneCPU, FS, ";
  if (NumFeatures)
    OS << "makeArrayRef(" << TargetName << "FeatureKV, " << NumFeatures
       << "), ";
  else
    OS << "std::nullopt, ";
  if (NumProcs)
    OS << "ArrayRef(" << TargetName << "SubTypeKV, " << NumProcs << "), ";
  else
    OS << "None, ";
  OS << '\n';
  OS.indent(24);
  OS << TargetName << "WriteProcResTable, "
     << TargetName << "WriteLatencyTable, "
     << TargetName << "ReadAdvanceTable, ";
  OS << '\n';
  OS.indent(24);
  if (SchedModelsHasItin) {
    OS << TargetName << "Stages, "
       << TargetName << "OperandCycles, "
       << TargetName << "ForwardingPaths";
  } else
    OS << "nullptr, nullptr, nullptr";
  OS << ") {}\n\n";
}

void PrinterLLVM::subtargetEmitResolveSchedClassHdr(
    std::string const &ClassName) const {
  OS << "unsigned " << ClassName
     << "\n::resolveSchedClass(unsigned SchedClass, const MachineInstr *MI,"
     << " const TargetSchedModel *SchedModel) const {\n";
}

void PrinterLLVM::subtargetEmitResolveSchedClassEnd(
    std::string const &ClassName) const {
  OS << "} // " << ClassName << "::resolveSchedClass\n\n";
}

void PrinterLLVM::subtargetEmitResolveVariantSchedClass(
    std::string const &TargetName, std::string const &ClassName) const {
  OS << "unsigned " << ClassName
     << "\n::resolveVariantSchedClass(unsigned SchedClass, const MCInst *MI,"
     << " const MCInstrInfo *MCII, unsigned CPUID) const {\n"
     << "  return " << TargetName << "_MC"
     << "::resolveVariantSchedClassImpl(SchedClass, MI, MCII, CPUID);\n"
     << "} // " << ClassName << "::resolveVariantSchedClass\n\n";
}

void PrinterLLVM::subtargetEmitPredicateProlog(
    const RecordKeeper &Records) const {
  std::string Buffer;
  raw_string_ostream Stream(Buffer);

  // Collect all the PredicateProlog records and print them to the output
  // stream.
  std::vector<Record *> Prologs =
      Records.getAllDerivedDefinitions("PredicateProlog");
  llvm::sort(Prologs, LessRecord());
  for (Record *P : Prologs)
    Stream << P->getValueAsString("Code") << '\n';

  OS << Buffer;
}

void PrinterLLVM::subtargetEmitParseFeaturesFunction(
    std::string const &TargetName,
    std::vector<Record *> const &Features) const {
  OS << "// ParseSubtargetFeatures - Parses features string setting specified\n"
     << "// subtarget options.\n"
     << "void llvm::";
  OS << TargetName;
  OS << "Subtarget::ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, "
     << "StringRef FS) {\n"
     << "  LLVM_DEBUG(dbgs() << \"\\nFeatures:\" << FS);\n"
     << "  LLVM_DEBUG(dbgs() << \"\\nCPU:\" << CPU);\n"
     << "  LLVM_DEBUG(dbgs() << \"\\nTuneCPU:\" << TuneCPU << \"\\n\\n\");\n";

  if (Features.empty()) {
    OS << "}\n";
    return;
  }

  OS << "  InitMCProcessorInfo(CPU, TuneCPU, FS);\n"
     << "  const FeatureBitset &Bits = getFeatureBits();\n";

  for (Record *R : Features) {
    // Next record
    StringRef const Instance = R->getName();
    StringRef const Value = R->getValueAsString("Value");
    StringRef const Attribute = R->getValueAsString("Attribute");

    if (Value == "true" || Value == "false")
      OS << "  if (Bits[" << TargetName << "::" << Instance << "]) "
         << Attribute << " = " << Value << ";\n";
    else
      OS << "  if (Bits[" << TargetName << "::" << Instance << "] && "
         << Attribute << " < " << Value << ") " << Attribute << " = " << Value
         << ";\n";
  }

  OS << "}\n";
}

void PrinterLLVM::subtargetEmitExpandedSTIPreds(
    StringRef const &TargetName, std::string const &ClassName,
    CodeGenSchedModels const &SchedModels) {
  initNewPE(TargetName);
  PE->setClassPrefix(ClassName);
  PE->setExpandDefinition(true);
  PE->setByRef(false);
  PE->setIndentLevel(0);

  for (const STIPredicateFunction &Fn : SchedModels.getSTIPredicates())
    PE->expandSTIPredicate(OS, Fn);
}

void PrinterLLVM::subtargetPrepareSchedClassPreds(
    StringRef const &TargetName, bool OnlyExpandMCInstPredicates) {
  initNewPE(TargetName);
  PE->setByRef(false);
  PE->setExpandForMC(OnlyExpandMCInstPredicates);
}

void PrinterLLVM::subtargetEmitExpandedSTIPredsMCAnaDecl(
    StringRef const &TargetName, CodeGenSchedModels const &SchedModels) {
  initNewPE(TargetName);
  PE->setExpandForMC(true);
  PE->setByRef(true);
  for (const STIPredicateFunction &Fn : SchedModels.getSTIPredicates())
    PE->expandSTIPredicate(OS, Fn);
}

void PrinterLLVM::subtargetEmitExpandedSTIPredsMCAnaDefs(
    StringRef const &TargetName, std::string const &ClassPrefix,
    CodeGenSchedModels const &SchedModels) const {
  // Predicate expander was initialized before.
  PE->setExpandDefinition(true);
  PE->setClassPrefix(ClassPrefix);
  PE->setIndentLevel(0);
  for (const STIPredicateFunction &Fn : SchedModels.getSTIPredicates())
    PE->expandSTIPredicate(OS, Fn);
}

void PrinterLLVM::subtargetEmitExpandedSTIPredsHeader(
    StringRef const &TargetName, CodeGenSchedModels const &SchedModels) {
  initNewPE(TargetName);
  PE->setByRef(false);
  for (const STIPredicateFunction &Fn : SchedModels.getSTIPredicates())
    PE->expandSTIPredicate(OS, Fn);
}

void PrinterLLVM::subtargetEmitStageAndSycleTables(
    std::string const &StageTable, std::string const &OperandCycleTable,
    std::string const &BypassTable) const {
  OS << StageTable;
  OS << OperandCycleTable;
  OS << BypassTable;
}

//---------------------------
// Backend: InstrInfoEmitter
//---------------------------

void PrinterLLVM::instrInfoEmitSourceFileHeader() const {
  emitSourceFileHeader("Target Instruction Enum Values and Descriptors", OS);
}

void PrinterLLVM::instrInfoSetOperandInfoStr(
    std::string &Res, Record const *OpR, CGIOperandList::OperandInfo const &Op,
    CGIOperandList::ConstraintInfo const &Constraint) const {
  if (OpR->isSubClassOf("RegisterOperand"))
    OpR = OpR->getValueAsDef("RegClass");
  if (OpR->isSubClassOf("RegisterClass"))
    Res += getQualifiedName(OpR) + "RegClassID, ";
  else if (OpR->isSubClassOf("PointerLikeRegClass"))
    Res += utostr(OpR->getValueAsInt("RegClassKind")) + ", ";
  else
    // -1 means the operand does not have a fixed register class.
    Res += "-1, ";

  // Fill in applicable flags.
  Res += "0";

  // Ptr value whose register class is resolved via callback.
  if (OpR->isSubClassOf("PointerLikeRegClass"))
    Res += "|(1<<MCOI::LookupPtrRegClass)";

  // Predicate operands.  Check to see if the original unexpanded operand
  // was of type PredicateOp.
  if (Op.Rec->isSubClassOf("PredicateOp"))
    Res += "|(1<<MCOI::Predicate)";

  // Optional def operands.  Check to see if the original unexpanded operand
  // was of type OptionalDefOperand.
  if (Op.Rec->isSubClassOf("OptionalDefOperand"))
    Res += "|(1<<MCOI::OptionalDef)";

  // Branch target operands.  Check to see if the original unexpanded
  // operand was of type BranchTargetOperand.
  if (Op.Rec->isSubClassOf("BranchTargetOperand"))
    Res += "|(1<<MCOI::BranchTarget)";

  // Fill in operand type.
  Res += ", ";
  assert(!Op.OperandType.empty() && "Invalid operand type.");
  Res += Op.OperandType;

  // Fill in constraint info.
  Res += ", ";

  if (Constraint.isNone())
    Res += "0";
  else if (Constraint.isEarlyClobber())
    Res += "MCOI_EARLY_CLOBBER";
  else {
    assert(Constraint.isTied());
    Res += "MCOI_TIED_TO(" + utostr(Constraint.getTiedOperand()) + ")";
  }
}

void PrinterLLVM::instrInfoPrintDefList(
    const std::vector<Record *> &Uses, unsigned Num,
    std::string (*GetQualifiedName)(Record const *R)) const {
  OS << "static const MCPhysReg ImplicitList" << Num << "[] = { ";
  for (auto [Idx, U] : enumerate(Uses))
    OS << (Idx ? ", " : "") << getQualifiedName(U);
  OS << " };\n";
}

void PrinterLLVM::instrInfoEmitOperandInfoTable(
    std::vector<std::string> const &OperandInfo, unsigned N) const {
  OS << "static const MCOperandInfo OperandInfo" << N << "[] = { ";
  for (const std::string &Info : OperandInfo)
    OS << "{ " << Info << " }, ";
  OS << "};\n";
}

void PrinterLLVM::instrInfoEmitMCInstrDescHdr(std::string TargetName) const {
  OS << "\nextern const MCInstrDesc " << TargetName << "Insts[] = {\n";
}

void PrinterLLVM::instrInfoEmitMCInstrDescEnd() const { OS << "};\n\n"; }

void PrinterLLVM::instrInfoEmitRecord(CodeGenSchedModels const &SchedModels,
                                      CodeGenInstruction const &Inst,
                                      unsigned Num, int MinOperands) const {
  OS << "  { ";
  OS << Num << ",\t" << MinOperands << ",\t"
     << Inst.Operands.NumDefs << ",\t"
     << Inst.TheDef->getValueAsInt("Size") << ",\t"
     << SchedModels.getSchedClassIdx(Inst) << ",\t"
     << Inst.ImplicitUses.size() << ",\t"
     << Inst.ImplicitDefs.size() << ",\t0";
}

void PrinterLLVM::instrInfoEmitTargetIndepFlags(
    CodeGenInstruction const &Inst, bool GetAllowRegisterRenaming) const {
  // clang-format off
  if (Inst.isPreISelOpcode)    OS << "|(1ULL<<MCID::PreISelOpcode)";
  if (Inst.isPseudo)           OS << "|(1ULL<<MCID::Pseudo)";
  if (Inst.isMeta)             OS << "|(1ULL<<MCID::Meta)";
  if (Inst.isReturn)           OS << "|(1ULL<<MCID::Return)";
  if (Inst.isEHScopeReturn)    OS << "|(1ULL<<MCID::EHScopeReturn)";
  if (Inst.isBranch)           OS << "|(1ULL<<MCID::Branch)";
  if (Inst.isIndirectBranch)   OS << "|(1ULL<<MCID::IndirectBranch)";
  if (Inst.isCompare)          OS << "|(1ULL<<MCID::Compare)";
  if (Inst.isMoveImm)          OS << "|(1ULL<<MCID::MoveImm)";
  if (Inst.isMoveReg)          OS << "|(1ULL<<MCID::MoveReg)";
  if (Inst.isBitcast)          OS << "|(1ULL<<MCID::Bitcast)";
  if (Inst.isAdd)              OS << "|(1ULL<<MCID::Add)";
  if (Inst.isTrap)             OS << "|(1ULL<<MCID::Trap)";
  if (Inst.isSelect)           OS << "|(1ULL<<MCID::Select)";
  if (Inst.isBarrier)          OS << "|(1ULL<<MCID::Barrier)";
  if (Inst.hasDelaySlot)       OS << "|(1ULL<<MCID::DelaySlot)";
  if (Inst.isCall)             OS << "|(1ULL<<MCID::Call)";
  if (Inst.canFoldAsLoad)      OS << "|(1ULL<<MCID::FoldableAsLoad)";
  if (Inst.mayLoad)            OS << "|(1ULL<<MCID::MayLoad)";
  if (Inst.mayStore)           OS << "|(1ULL<<MCID::MayStore)";
  if (Inst.mayRaiseFPException) OS << "|(1ULL<<MCID::MayRaiseFPException)";
  if (Inst.isPredicable)       OS << "|(1ULL<<MCID::Predicable)";
  if (Inst.isConvertibleToThreeAddress) OS << "|(1ULL<<MCID::ConvertibleTo3Addr)";
  if (Inst.isCommutable)       OS << "|(1ULL<<MCID::Commutable)";
  if (Inst.isTerminator)       OS << "|(1ULL<<MCID::Terminator)";
  if (Inst.isReMaterializable) OS << "|(1ULL<<MCID::Rematerializable)";
  if (Inst.isNotDuplicable)    OS << "|(1ULL<<MCID::NotDuplicable)";
  if (Inst.Operands.hasOptionalDef) OS << "|(1ULL<<MCID::HasOptionalDef)";
  if (Inst.usesCustomInserter) OS << "|(1ULL<<MCID::UsesCustomInserter)";
  if (Inst.hasPostISelHook)    OS << "|(1ULL<<MCID::HasPostISelHook)";
  if (Inst.Operands.isVariadic)OS << "|(1ULL<<MCID::Variadic)";
  if (Inst.hasSideEffects)     OS << "|(1ULL<<MCID::UnmodeledSideEffects)";
  if (Inst.isAsCheapAsAMove)   OS << "|(1ULL<<MCID::CheapAsAMove)";
  if (!GetAllowRegisterRenaming || Inst.hasExtraSrcRegAllocReq)
    OS << "|(1ULL<<MCID::ExtraSrcRegAllocReq)";
  if (!GetAllowRegisterRenaming || Inst.hasExtraDefRegAllocReq)
    OS << "|(1ULL<<MCID::ExtraDefRegAllocReq)";
  if (Inst.isRegSequence) OS << "|(1ULL<<MCID::RegSequence)";
  if (Inst.isExtractSubreg) OS << "|(1ULL<<MCID::ExtractSubreg)";
  if (Inst.isInsertSubreg) OS << "|(1ULL<<MCID::InsertSubreg)";
  if (Inst.isConvergent) OS << "|(1ULL<<MCID::Convergent)";
  if (Inst.variadicOpsAreDefs) OS << "|(1ULL<<MCID::VariadicOpsAreDefs)";
  if (Inst.isAuthenticated) OS << "|(1ULL<<MCID::Authenticated)";
  // clang-format on
}

void PrinterLLVM::instrInfoEmitTSFFlags(uint64_t Value) const {
  OS << ", 0x";
  OS.write_hex(Value);
  OS << "ULL, ";
}

void PrinterLLVM::instrInfoEmitUseDefsLists(
    std::map<std::vector<Record *>, unsigned> &EmittedLists,
    std::vector<Record *> const &ImplicitOps) const {
  if (ImplicitOps.empty())
    OS << "nullptr, ";
  else
    OS << "ImplicitList" << EmittedLists[ImplicitOps] << ", ";
}

void PrinterLLVM::instrInfoEmitOperandInfo(
    std::vector<std::string> const &OperandInfo,
    OperandInfoMapTy const &OpInfo) const {
  if (OperandInfo.empty())
    OS << "nullptr";
  else
    OS << "OperandInfo" << OpInfo.find(OperandInfo)->second;
}

void PrinterLLVM::instrInfoEmitRecordEnd(unsigned InstNum,
                                         std::string const &InstName) const {
  OS << " },  // Inst #" << InstNum << " = " << InstName << "\n";
}

void PrinterLLVM::instrInfoEmitStringLiteralDef(
    std::string const &TargetName,
    SequenceToOffsetTable<std::string> InstrNames) const {
  InstrNames.emitStringLiteralDef(OS, Twine("extern const char ") + TargetName +
                                          "InstrNameData[]");
}

void PrinterLLVM::instrInfoEmitInstrNameIndices(
    std::string const &TargetName,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
    SequenceToOffsetTable<std::string> const &InstrNames) const {
  OS << "extern const unsigned " << TargetName << "InstrNameIndices[] = {";
  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    // Newline every eight entries.
    if (Num % 8 == 0)
      OS << "\n    ";
    OS << InstrNames.get(std::string(Inst->TheDef->getName())) << "U, ";
    ++Num;
  }
  OS << "\n};\n\n";
}

void PrinterLLVM::instrInfoEmitInstrDeprFeatures(
    std::string const &TargetName, std::string const &TargetNamespace,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
    SequenceToOffsetTable<std::string> const &InstrNames) const {
  OS << "extern const uint8_t " << TargetName
     << "InstrDeprecationFeatures[] = {";
  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    if (Num % 8 == 0)
      OS << "\n    ";
    if (!Inst->HasComplexDeprecationPredicate &&
        !Inst->DeprecatedReason.empty())
      OS << TargetNamespace << "::" << Inst->DeprecatedReason << ", ";
    else
      OS << "uint8_t(-1), ";
    ++Num;
  }
  OS << "\n};\n\n";
}

void PrinterLLVM::instrInfoEmitInstrComplexDeprInfos(
    std::string const &TargetName,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {
  OS << "extern const MCInstrInfo::ComplexDeprecationPredicate " << TargetName
     << "InstrComplexDeprecationInfos[] = {";
  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    if (Num % 8 == 0)
      OS << "\n    ";
    if (Inst->HasComplexDeprecationPredicate)
      // Emit a function pointer to the complex predicate method.
      OS << "&get" << Inst->DeprecatedReason << "DeprecationInfo, ";
    else
      OS << "nullptr, ";
    ++Num;
  }
  OS << "\n};\n\n";
}

void PrinterLLVM::instrInfoEmitMCInstrInfoInitRoutine(
    std::string const &TargetName, unsigned NumberedInstrSize,
    bool HasDeprecationFeatures, bool HasComplexDeprecationInfos) const {
  OS << "static inline void Init" << TargetName
     << "MCInstrInfo(MCInstrInfo *II) {\n";
  OS << "  II->InitMCInstrInfo(" << TargetName << "Insts, " << TargetName
     << "InstrNameIndices, " << TargetName << "InstrNameData, ";
  if (HasDeprecationFeatures)
    OS << TargetName << "InstrDeprecationFeatures, ";
  else
    OS << "nullptr, ";
  if (HasComplexDeprecationInfos)
    OS << TargetName << "InstrComplexDeprecationInfos, ";
  else
    OS << "nullptr, ";
  OS << NumberedInstrSize << ");\n}\n\n";
}

void PrinterLLVM::instrInfoEmitClassStruct(std::string const &ClassName) const {
  OS << "struct " << ClassName << " : public TargetInstrInfo {\n"
     << "  explicit " << ClassName
     << "(int CFSetupOpcode = -1, int CFDestroyOpcode = -1, int CatchRetOpcode "
        "= -1, int ReturnOpcode = -1);\n"
     << "  ~" << ClassName << "() override = default;\n";
  OS << "\n};\n";
}

void PrinterLLVM::instrInfoEmitTIIHelperMethod(StringRef const &TargetName,
                                               Record const *Rec,
                                               bool ExpandDefinition) const {
  OS << (ExpandDefinition ? "" : "static ") << "bool ";
  if (ExpandDefinition)
    OS << TargetName << "InstrInfo::";
  OS << Rec->getValueAsString("FunctionName");
  OS << "(const MachineInstr &MI)";
  if (!ExpandDefinition) {
    OS << ";\n";
    return;
  }

  OS << " {\n";
  OS.indent(PE->getIndentLevel() * 2);
  PE->expandStatement(OS, Rec->getValueAsDef("Body"));
  OS << "\n}\n\n";
}

void PrinterLLVM::instrInfoEmitExternArrays(
    std::string const &TargetName, bool HasDeprecationFeatures,
    bool HasComplexDeprecationInfos) const {
  OS << "extern const MCInstrDesc " << TargetName << "Insts[];\n";
  OS << "extern const unsigned " << TargetName << "InstrNameIndices[];\n";
  OS << "extern const char " << TargetName << "InstrNameData[];\n";
  if (HasDeprecationFeatures)
    OS << "extern const uint8_t " << TargetName
       << "InstrDeprecationFeatures[];\n";
  if (HasComplexDeprecationInfos)
    OS << "extern const MCInstrInfo::ComplexDeprecationPredicate " << TargetName
       << "InstrComplexDeprecationInfos[];\n";
}

void PrinterLLVM::instrInfoEmitMCInstrInfoInit(
    std::string const &TargetName, std::string const &ClassName,
    unsigned NumberedInstrSize, bool HasDeprecationFeatures,
    bool HasComplexDeprecationInfos) const {
  OS << ClassName << "::" << ClassName
     << "(int CFSetupOpcode, int CFDestroyOpcode, int CatchRetOpcode, int "
        "ReturnOpcode)\n"
     << "  : TargetInstrInfo(CFSetupOpcode, CFDestroyOpcode, CatchRetOpcode, "
        "ReturnOpcode) {\n"
     << "  InitMCInstrInfo(" << TargetName << "Insts, " << TargetName
     << "InstrNameIndices, " << TargetName << "InstrNameData, ";
  if (HasDeprecationFeatures)
    OS << TargetName << "InstrDeprecationFeatures, ";
  else
    OS << "nullptr, ";
  if (HasComplexDeprecationInfos)
    OS << TargetName << "InstrComplexDeprecationInfos, ";
  else
    OS << "nullptr, ";
  OS << NumberedInstrSize << ");\n}\n";
}

void PrinterLLVM::instrInfoEmitOperandEnum(
    std::map<std::string, unsigned> const &Operands) const {
  OS << "enum {\n";
  for (const auto &Op : Operands)
    OS << "  " << Op.first << " = " << Op.second << ",\n";

  OS << "  OPERAND_LAST";
  OS << "\n};\n";
}

void PrinterLLVM::instrInfoEmitGetNamedOperandIdx(
    std::map<std::string, unsigned> const &Operands,
    OpNameMapTy const &OperandMap) const {
  OS << "LLVM_READONLY\n";
  OS << "int16_t getNamedOperandIdx(uint16_t Opcode, uint16_t NamedIdx) {\n";
  if (!Operands.empty()) {
    OS << "  static const int16_t OperandMap [][" << Operands.size()
       << "] = {\n";
    for (const auto &Entry : OperandMap) {
      const std::map<unsigned, unsigned> &OpList = Entry.first;
      OS << "{";

      // Emit a row of the OperandMap table
      for (unsigned I = 0, E = Operands.size(); I != E; ++I)
        OS << (OpList.count(I) == 0 ? -1 : (int)OpList.find(I)->second) << ", ";

      OS << "},\n";
    }
    OS << "};\n";

    OS << "  switch(Opcode) {\n";
    unsigned TableIndex = 0;
    for (const auto &Entry : OperandMap) {
      for (const std::string &Name : Entry.second)
        OS << "  case " << Name << ":\n";

      OS << "    return OperandMap[" << TableIndex++ << "][NamedIdx];\n";
    }
    OS << "  default: return -1;\n";
    OS << "  }\n";
  } else {
    // There are no operands, so no need to emit anything
    OS << "  return -1;\n";
  }
  OS << "}\n";
}

void PrinterLLVM::instrInfoEmitOpTypeEnumPartI() const {
  OS << "enum OperandType {\n";
}

void PrinterLLVM::instrInfoEmitOpTypeEnumPartII(StringRef const &OpName,
                                                unsigned EnumVal) const {
  OS << "  " << OpName << " = " << EnumVal << ",\n";
}

void PrinterLLVM::instrInfoEmitOpTypeEnumPartIII() const {
  OS << "  OPERAND_TYPE_LIST_END"
     << "\n};\n";
}

void PrinterLLVM::instrInfoEmitOpTypeOffsetTable(
    std::vector<int> OperandOffsets, unsigned OpRecSize,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {
  OS << ((OpRecSize <= UINT16_MAX) ? "  const uint16_t" : "  const uint32_t");
  OS << " Offsets[] = {\n";
  for (int I = 0, E = OperandOffsets.size(); I != E; ++I) {
    OS << "    /* " << NumberedInstructions[I]->TheDef->getName() << " */\n";
    OS << "    " << OperandOffsets[I] << ",\n";
  }
  OS << "  };\n";
}

void PrinterLLVM::instrInfoEmitOpcodeOpTypesTable(
    unsigned EnumVal, std::vector<Record *> const &OperandRecords,
    std::vector<int> OperandOffsets,
    ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const {
  OS << "\n  using namespace OpTypes;\n";
  OS << ((EnumVal <= INT8_MAX) ? "  const int8_t" : "  const int16_t");
  OS << " OpcodeOperandTypes[] = {\n    ";
  for (int I = 0, E = OperandRecords.size(), CurOffset = 0; I != E; ++I) {
    // We print each Opcode's operands in its own row.
    if (I == OperandOffsets[CurOffset]) {
      OS << "\n    /* " << NumberedInstructions[CurOffset]->TheDef->getName()
         << " */\n    ";
      while (OperandOffsets[++CurOffset] == I)
        OS << "/* " << NumberedInstructions[CurOffset]->TheDef->getName()
           << " */\n    ";
    }
    Record *OpR = OperandRecords[I];
    if ((OpR->isSubClassOf("Operand") || OpR->isSubClassOf("RegisterOperand") ||
         OpR->isSubClassOf("RegisterClass")) &&
        !OpR->isAnonymous())
      OS << OpR->getName();
    else
      OS << -1;
    OS << ", ";
  }
  OS << "\n  };\n";
}

void PrinterLLVM::instrInfoEmitGetOpTypeHdr() const {
  OS << "LLVM_READONLY\n";
  OS << "static int getOperandType(uint16_t Opcode, uint16_t OpIdx) {\n";
}

void PrinterLLVM::instrInfoEmitGetOpTypeReturn() const {
  OS << "  return OpcodeOperandTypes[Offsets[Opcode] + OpIdx];\n";
}

void PrinterLLVM::instrInfoEmitGetOpTypeUnreachable() const {
  OS << "  llvm_unreachable(\"No instructions defined\");\n";
}

void PrinterLLVM::instrInfoEmitGetOpTypeEnd() const { OS << "}\n"; }

void PrinterLLVM::instrInfoEmitGetMemOpSizeHdr() const {
  OS << "LLVM_READONLY\n";
  OS << "static int getMemOperandSize(int OpType) {\n";
  OS << "  switch (OpType) {\n";
}

void PrinterLLVM::instrInfoEmitGetOpMemSizeTbl(
    std::map<int, std::vector<StringRef>> const &SizeToOperandName) const {
  OS << "  default: return 0;\n";
  for (auto KV : SizeToOperandName) {
    for (const StringRef &OperandName : KV.second)
      OS << "  case OpTypes::" << OperandName << ":\n";
    OS << "    return " << KV.first << ";\n\n";
  }
  OS << "  }\n}\n";
}

std::string
PrinterLLVM::instrInfoGetInstMapEntry(StringRef const &Namespace,
                                      StringRef const &InstrName) const {
  return Namespace.str() + "::" + InstrName.str();
}

void PrinterLLVM::instrInfoEmitGetLogicalOpSizeHdr() const {
  OS << "LLVM_READONLY static unsigned\n";
  OS << "getLogicalOperandSize(uint16_t Opcode, uint16_t LogicalOpIdx) {\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpSizeTable(
    size_t LogicalOpListSize,
    std::vector<const std::vector<unsigned> *> const &LogicalOpSizeList) const {
  OS << "  static const unsigned SizeMap[][" << LogicalOpListSize << "] = {\n";
  for (auto &R : LogicalOpSizeList) {
    const auto &Row = *R;
    OS << "   {";
    int I;
    for (I = 0; I < static_cast<int>(Row.size()); ++I) {
      OS << Row[I] << ", ";
    }
    for (; I < static_cast<int>(LogicalOpListSize); ++I) {
      OS << "0, ";
    }
    OS << "}, ";
    OS << "\n";
  }
  OS << "  };\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpSizeSwitch(
    std::map<unsigned, std::vector<std::string>> InstMap) const {
  OS << "  switch (Opcode) {\n";
  OS << "  default: return LogicalOpIdx;\n";
  for (auto &P : InstMap) {
    auto OpMapIdx = P.first;
    const auto &Insts = P.second;
    for (const auto &Inst : Insts) {
      OS << "  case " << Inst << ":\n";
    }
    OS << "    return SizeMap[" << OpMapIdx << "][LogicalOpIdx];\n";
  }
  OS << "  }\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpSizeReturn() const {
  OS << "  return LogicalOpIdx;\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpSizeEnd() const { OS << "}\n"; }

void PrinterLLVM::instrInfoEmitGetLogicalOpIdx() const {
  OS << "LLVM_READONLY static inline unsigned\n";
  OS << "getLogicalOperandIdx(uint16_t Opcode, uint16_t LogicalOpIdx) {\n";
  OS << "  auto S = 0U;\n";
  OS << "  for (auto i = 0U; i < LogicalOpIdx; ++i)\n";
  OS << "    S += getLogicalOperandSize(Opcode, i);\n";
  OS << "  return S;\n";
  OS << "}\n";
}

std::string
PrinterLLVM::instrInfoGetOpTypeListEntry(StringRef const &Namespace,
                                         StringRef const &OpName) const {
  return Namespace.str() + "::OpTypes::" + OpName.str();
}

void PrinterLLVM::instrInfoEmitGetLogicalOpTypeHdr() const {
  OS << "LLVM_READONLY static int\n";
  OS << "getLogicalOperandType(uint16_t Opcode, uint16_t LogicalOpIdx) {\n";
}
void PrinterLLVM::instrInfoEmitGetLogicalOpTypeTable(
    size_t OpTypeListSize,
    std::vector<const std::vector<std::string> *> const &LogicalOpTypeList)
    const {
  OS << "  static const int TypeMap[][" << OpTypeListSize << "] = {\n";
  for (int R = 0, Rs = LogicalOpTypeList.size(); R < Rs; ++R) {
    const auto &Row = *LogicalOpTypeList[R];
    OS << "   {";
    int I, S = Row.size();
    for (I = 0; I < S; ++I) {
      if (I > 0)
        OS << ", ";
      OS << Row[I];
    }
    for (; I < static_cast<int>(OpTypeListSize); ++I) {
      if (I > 0)
        OS << ", ";
      OS << "-1";
    }
    OS << "}";
    if (R != Rs - 1)
      OS << ",";
    OS << "\n";
  }
  OS << "  };\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpTypeSwitch(
    std::map<unsigned, std::vector<std::string>> InstMap) const {
  OS << "  switch (Opcode) {\n";
  OS << "  default: return -1;\n";
  for (auto &P : InstMap) {
    auto OpMapIdx = P.first;
    const auto &Insts = P.second;
    for (const auto &Inst : Insts) {
      OS << "  case " << Inst << ":\n";
    }
    OS << "    return TypeMap[" << OpMapIdx << "][LogicalOpIdx];\n";
  }
  OS << "  }\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpTypeReturn() const {
  OS << "  return -1;\n";
}

void PrinterLLVM::instrInfoEmitGetLogicalOpTypeEnd() const { OS << "}\n"; }

void PrinterLLVM::instrInfoEmitDeclareMCInstFeatureClasses() const {
  OS << "class MCInst;\n";
  OS << "class FeatureBitset;\n\n";
}

void PrinterLLVM::instrInfoEmitPredFcnDecl(RecVec const &TIIPredicates) const {
  for (const Record *Rec : TIIPredicates) {
    OS << "bool " << Rec->getValueAsString("FunctionName")
       << "(const MCInst &MI);\n";
  }

  OS << "void verifyInstructionPredicates(unsigned Opcode, const FeatureBitset "
        "&Features);\n";
}

void PrinterLLVM::instrInfoEmitPredFcnImpl(StringRef const &TargetName,
                                           RecVec const &TIIPredicates) {
  initNewPE(TargetName);
  PE->setExpandForMC(true);
  for (const Record *Rec : TIIPredicates) {
    OS << "bool " << Rec->getValueAsString("FunctionName");
    OS << "(const MCInst &MI) {\n";

    OS.indent(PE->getIndentLevel() * 2);
    PE->expandStatement(OS, Rec->getValueAsDef("Body"));
    OS << "\n}\n\n";
  }
}

void PrinterLLVM::instrInfoEmitInstrPredVerifierIncludes() const {
  OS << "#include <sstream>\n\n";
}

void PrinterLLVM::instrInfoEmitSubtargetFeatureBitEnumeration(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {
  // Emit the subtarget feature enumeration.
  SubtargetFeatureInfo::emitSubtargetFeatureBitEnumeration(SubtargetFeatures,
                                                           OS);
}

void PrinterLLVM::instrInfoEmitEmitSTFNameTable(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {
  OS << "#ifndef NDEBUG\n";
  SubtargetFeatureInfo::emitNameTable(SubtargetFeatures, OS);
  OS << "#endif // NDEBUG\n\n";
}

void PrinterLLVM::instrInfoEmitFeatureBitsEnum(
    std::vector<std::vector<Record *>> const &FeatureBitsets) const {
  OS << "// Feature bitsets.\n"
     << "enum : " << getMinimalTypeForRange(FeatureBitsets.size()) << " {\n"
     << "  CEFBS_None,\n";
  for (const auto &FeatureBitset : FeatureBitsets) {
    if (FeatureBitset.empty())
      continue;
    OS << "  " << getNameForFeatureBitset(FeatureBitset) << ",\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::instrInfoEmitFeatureBitsArray(
    std::vector<std::vector<Record *>> const &FeatureBitsets,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        &SubtargetFeatures) const {
  OS << "static constexpr FeatureBitset FeatureBitsets[] = {\n"
     << "  {}, // CEFBS_None\n";
  for (const auto &FeatureBitset : FeatureBitsets) {
    if (FeatureBitset.empty())
      continue;
    OS << "  {";
    for (const auto &Feature : FeatureBitset) {
      const auto &I = SubtargetFeatures.find(Feature);
      assert(I != SubtargetFeatures.end() && "Didn't import predicate?");
      OS << I->second.getEnumBitName() << ", ";
    }
    OS << "},\n";
  }
  OS << "};\n";
}

void PrinterLLVM::instrInfoEmitPredVerifier(
    std::vector<std::vector<Record *>> const &FeatureBitsets,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        &SubtargetFeatures,
    CodeGenTarget const &Target) const {
  OS << "void verifyInstructionPredicates(\n"
     << "    unsigned Opcode, const FeatureBitset &Features) {\n";
  emitIfNotDef("NDEBUG", true);
  OS << "  static " << getMinimalTypeForRange(FeatureBitsets.size())
     << " RequiredFeaturesRefs[] = {\n";
  unsigned InstIdx = 0;
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue()) {
    OS << "    CEFBS";
    unsigned NumPredicates = 0;
    for (Record *Predicate : Inst->TheDef->getValueAsListOfDefs("Predicates")) {
      const auto &I = SubtargetFeatures.find(Predicate);
      if (I != SubtargetFeatures.end()) {
        OS << '_' << I->second.TheDef->getName();
        NumPredicates++;
      }
    }
    if (!NumPredicates)
      OS << "_None";
    OS << ", // " << Inst->TheDef->getName() << " = " << InstIdx << "\n";
    InstIdx++;
  }
  OS << "  };\n\n";
  OS << "  assert(Opcode < " << InstIdx << ");\n";
  OS << "  FeatureBitset AvailableFeatures = "
        "computeAvailableFeatures(Features);\n";
  OS << "  const FeatureBitset &RequiredFeatures = "
        "FeatureBitsets[RequiredFeaturesRefs[Opcode]];\n";
  OS << "  FeatureBitset MissingFeatures =\n"
     << "      (AvailableFeatures & RequiredFeatures) ^\n"
     << "      RequiredFeatures;\n"
     << "  if (MissingFeatures.any()) {\n"
     << "    std::ostringstream Msg;\n"
     << "    Msg << \"Attempting to emit \" << &" << Target.getName()
     << "InstrNameData[" << Target.getName() << "InstrNameIndices[Opcode]]\n"
     << "        << \" instruction but the \";\n"
     << "    for (unsigned i = 0, e = MissingFeatures.size(); i != e; ++i)\n"
     << "      if (MissingFeatures.test(i))\n"
     << "        Msg << SubtargetFeatureNames[i] << \" \";\n"
     << "    Msg << \"predicate(s) are not met\";\n"
     << "    report_fatal_error(Msg.str().c_str());\n"
     << "  }\n";
  emitIfNotDef("NDEBUG", false);
  OS << "}\n";
}

void PrinterLLVM::instrInfoEmitEnums(
    CodeGenTarget const &Target, StringRef const &Namespace,
    CodeGenSchedModels const &SchedModels) const {
  emitIncludeToggle("GET_INSTRINFO_ENUM", true);

  emitNamespace("llvm", true);
  // We must emit the PHI opcode first...
  emitNamespace(Namespace.str(), true);
  unsigned Num = 0;
  OS << "  enum {\n";
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue())
    OS << "    " << Inst->TheDef->getName() << "\t= " << Num++ << ",\n";
  OS << "    INSTRUCTION_LIST_END = " << Num << "\n";
  OS << "  };\n\n";
  emitNamespace(Namespace.str(), false);
  emitNamespace("llvm", false);
  emitIncludeToggle("GET_INSTRINFO_ENUM", false);

  emitIncludeToggle("GET_INSTRINFO_SCHED_ENUM", true);
  emitNamespace("llvm", true);
  emitNamespace(Namespace.str(), true);
  emitNamespace("Sched", true);
  Num = 0;
  OS << "  enum {\n";
  for (const auto &Class : SchedModels.explicit_classes())
    OS << "    " << Class.Name << "\t= " << Num++ << ",\n";
  OS << "    SCHED_LIST_END = " << Num << "\n";
  OS << "  };\n";
  emitNamespace("Sched", false);
  emitNamespace(Namespace.str(), false);
  emitNamespace("llvm", false);

  emitIncludeToggle("GET_INSTRINFO_SCHED_ENUM", false);
}

void PrinterLLVM::instrInfoEmitTIIPredicates(StringRef const &TargetName,
                                             RecVec const &TIIPredicates,
                                             bool ExpandDefinition) {
  initNewPE(TargetName);
  PE->setExpandForMC(false);

  for (const Record *Rec : TIIPredicates) {
    instrInfoEmitTIIHelperMethod(TargetName, Rec, ExpandDefinition);
  }
}

void PrinterLLVM::instrInfoEmitComputeAssemblerAvailableFeatures(
    StringRef const &TargetName,
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> &SubtargetFeatures)
    const {
  SubtargetFeatureInfo::emitComputeAssemblerAvailableFeatures(
      TargetName, "", "computeAvailableFeatures", SubtargetFeatures, OS);
}

//--------------------------
// Backend: AsmMatcher
//--------------------------

void PrinterLLVM::asmMatcherEmitSourceFileHeader(
    std::string const &Desc) const {
  emitSourceFileHeader(Desc, OS);
}

void PrinterLLVM::asmMatcherEmitDeclarations(bool HasOptionalOperands,
                                             bool ReportMultipleNearMisses,
                                             bool HasOperandInfos) const {
  OS << "  // This should be included into the middle of the declaration of\n";
  OS << "  // your subclasses implementation of MCTargetAsmParser.\n";
  OS << "  FeatureBitset ComputeAvailableFeatures(const FeatureBitset &FB) "
        "const;\n";
  if (HasOptionalOperands) {
    OS << "  void convertToMCInst(unsigned Kind, MCInst &Inst, "
       << "unsigned Opcode,\n"
       << "                       const OperandVector &Operands,\n"
       << "                       const SmallBitVector "
          "&OptionalOperandsMask);\n";
  } else {
    OS << "  void convertToMCInst(unsigned Kind, MCInst &Inst, "
       << "unsigned Opcode,\n"
       << "                       const OperandVector &Operands);\n";
  }
  OS << "  void convertToMapAndConstraints(unsigned Kind,\n                ";
  OS << "           const OperandVector &Operands) override;\n";
  OS << "  unsigned MatchInstructionImpl(const OperandVector &Operands,\n"
     << "                                MCInst &Inst,\n";
  if (ReportMultipleNearMisses)
    OS << "                                SmallVectorImpl<NearMissInfo> "
          "*NearMisses,\n";
  else
    OS << "                                uint64_t &ErrorInfo,\n"
       << "                                FeatureBitset &MissingFeatures,\n";
  OS << "                                bool matchingInlineAsm,\n"
     << "                                unsigned VariantID = 0);\n";
  if (!ReportMultipleNearMisses)
    OS << "  unsigned MatchInstructionImpl(const OperandVector &Operands,\n"
       << "                                MCInst &Inst,\n"
       << "                                uint64_t &ErrorInfo,\n"
       << "                                bool matchingInlineAsm,\n"
       << "                                unsigned VariantID = 0) {\n"
       << "    FeatureBitset MissingFeatures;\n"
       << "    return MatchInstructionImpl(Operands, Inst, ErrorInfo, "
          "MissingFeatures,\n"
       << "                                matchingInlineAsm, VariantID);\n"
       << "  }\n\n";

  if (HasOperandInfos) {
    OS << "  OperandMatchResultTy MatchOperandParserImpl(\n";
    OS << "    OperandVector &Operands,\n";
    OS << "    StringRef Mnemonic,\n";
    OS << "    bool ParseForAllFeatures = false);\n";

    OS << "  OperandMatchResultTy tryCustomParseOperand(\n";
    OS << "    OperandVector &Operands,\n";
    OS << "    unsigned MCK);\n\n";
  }
}

void PrinterLLVM::asmMatcherEmitOperandDiagTypes(
    std::set<StringRef> const Types) const {
  for (StringRef Type : Types)
    OS << "  Match_" << Type << ",\n";
  OS << "  END_OPERAND_DIAGNOSTIC_TYPES\n";
}

/// emitGetSubtargetFeatureName - Emit the helper function to get the
/// user-level name for a subtarget feature.
void PrinterLLVM::asmMatcherEmitGetSubtargetFeatureName(
    std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
        SubtargetFeatures) const {
  OS << "// User-level names for subtarget features that participate in\n"
     << "// instruction matching.\n"
     << "static const char *getSubtargetFeatureName(uint64_t Val) {\n";
  if (!SubtargetFeatures.empty()) {
    OS << "  switch(Val) {\n";
    for (const auto &SF : SubtargetFeatures) {
      const SubtargetFeatureInfo &SFI = SF.second;
      // FIXME: Totally just a placeholder name to get the algorithm working.
      OS << "  case " << SFI.getEnumBitName() << ": return \""
         << SFI.TheDef->getValueAsString("PredicateName") << "\";\n";
    }
    OS << "  default: return \"(unknown)\";\n";
    OS << "  }\n";
  } else {
    // Nothing to emit, so skip the switch
    OS << "  return \"(unknown)\";\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionI(
    StringRef const &TargetName, StringRef const &ClassName,
    std::string const &TargetOperandClass, bool HasOptionalOperands,
    size_t MaxNumOperands) const {
  if (HasOptionalOperands) {
    *CvtOS << "void " << TargetName << ClassName << "::\n"
           << "convertToMCInst(unsigned Kind, MCInst &Inst, "
           << "unsigned Opcode,\n"
           << "                const OperandVector &Operands,\n"
           << "                const SmallBitVector &OptionalOperandsMask) {\n";
  } else {
    *CvtOS << "void " << TargetName << ClassName << "::\n"
           << "convertToMCInst(unsigned Kind, MCInst &Inst, "
           << "unsigned Opcode,\n"
           << "                const OperandVector &Operands) {\n";
  }
  *CvtOS << "  assert(Kind < CVT_NUM_SIGNATURES && \"Invalid signature!\");\n";
  *CvtOS << "  const uint8_t *Converter = ConversionTable[Kind];\n";
  if (HasOptionalOperands) {
    *CvtOS << "  unsigned DefaultsOffset[" << (MaxNumOperands + 1)
           << "] = { 0 };\n";
    *CvtOS << "  assert(OptionalOperandsMask.size() == " << (MaxNumOperands)
           << ");\n";
    *CvtOS << "  for (unsigned i = 0, NumDefaults = 0; i < " << (MaxNumOperands)
           << "; ++i) {\n";
    *CvtOS << "    DefaultsOffset[i + 1] = NumDefaults;\n";
    *CvtOS << "    NumDefaults += (OptionalOperandsMask[i] ? 1 : 0);\n";
    *CvtOS << "  }\n";
  }
  *CvtOS << "  unsigned OpIdx;\n";
  *CvtOS << "  Inst.setOpcode(Opcode);\n";
  *CvtOS << "  for (const uint8_t *p = Converter; *p; p += 2) {\n";
  if (HasOptionalOperands) {
    *CvtOS << "    OpIdx = *(p + 1) - DefaultsOffset[*(p + 1)];\n";
  } else {
    *CvtOS << "    OpIdx = *(p + 1);\n";
  }
  *CvtOS << "    switch (*p) {\n";
  *CvtOS << "    default: llvm_unreachable(\"invalid conversion entry!\");\n";
  *CvtOS << "    case CVT_Reg:\n";
  *CvtOS << "      static_cast<" << TargetOperandClass
         << " &>(*Operands[OpIdx]).addRegOperands(Inst, 1);\n";
  *CvtOS << "      break;\n";
  *CvtOS << "    case CVT_Tied: {\n";
  *CvtOS << "      assert(OpIdx < (size_t)(std::end(TiedAsmOperandTable) -\n";
  *CvtOS
      << "                              std::begin(TiedAsmOperandTable)) &&\n";
  *CvtOS << "             \"Tied operand not found\");\n";
  *CvtOS << "      unsigned TiedResOpnd = TiedAsmOperandTable[OpIdx][0];\n";
  *CvtOS << "      if (TiedResOpnd != (uint8_t)-1)\n";
  *CvtOS << "        Inst.addOperand(Inst.getOperand(TiedResOpnd));\n";
  *CvtOS << "      break;\n";
  *CvtOS << "    }\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionII(
    std::string const &EnumName, StringRef const &AsmMatchConverter) const {
  *CvtOS << "    case CVT_" << EnumName << ":\n"
         << "      " << AsmMatchConverter << "(Inst, Operands);\n"
         << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionIII(
    std::string const &EnumName, std::string const TargetOperandClass,
    bool HasOptionalOperands, MatchableInfo::AsmOperand const &Op,
    MatchableInfo::ResOperand const &OpInfo) const {
  *CvtOS << "    case " << EnumName << ":\n";
  if (Op.Class->IsOptional) {
    // If optional operand is not present in actual instruction then we
    // should call its DefaultMethod before RenderMethod
    assert(HasOptionalOperands);
    *CvtOS << "      if (OptionalOperandsMask[*(p + 1) - 1]) {\n"
           << "        " << Op.Class->DefaultMethod << "()"
           << "->" << Op.Class->RenderMethod << "(Inst, "
           << OpInfo.MINumOperands << ");\n"
           << "      } else {\n"
           << "        static_cast<" << TargetOperandClass
           << " &>(*Operands[OpIdx])." << Op.Class->RenderMethod << "(Inst, "
           << OpInfo.MINumOperands << ");\n"
           << "      }\n";
  } else {
    *CvtOS << "      static_cast<" << TargetOperandClass
           << " &>(*Operands[OpIdx])." << Op.Class->RenderMethod << "(Inst, "
           << OpInfo.MINumOperands << ");\n";
  }
  *CvtOS << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionIV(
    std::string const &EnumName, int64_t Val) const {
  *CvtOS << "    case " << EnumName << ":\n"
         << "      Inst.addOperand(MCOperand::createImm(" << Val << "));\n"
         << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionV(
    std::string const &EnumName, std::string const &Reg) const {
  *CvtOS << "    case " << EnumName << ":\n"
         << "      Inst.addOperand(MCOperand::createReg(" << Reg << "));\n"
         << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitConversionFunctionVI() const {
  *CvtOS << "    }\n  }\n}\n\n";
}

void PrinterLLVM::asmMatcherEmitOperandFunctionI(
    StringRef const &TargetName, StringRef const &ClassName) const {
  *OpOS << "void " << TargetName << ClassName << "::\n"
        << "convertToMapAndConstraints(unsigned Kind,\n";
  OpOS->indent(27);
  *OpOS << "const OperandVector &Operands) {\n"
        << "  assert(Kind < CVT_NUM_SIGNATURES && \"Invalid signature!\");\n"
        << "  unsigned NumMCOperands = 0;\n"
        << "  const uint8_t *Converter = ConversionTable[Kind];\n"
        << "  for (const uint8_t *p = Converter; *p; p += 2) {\n"
        << "    switch (*p) {\n"
        << "    default: llvm_unreachable(\"invalid conversion entry!\");\n"
        << "    case CVT_Reg:\n"
        << "      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);\n"
        << "      Operands[*(p + 1)]->setConstraint(\"r\");\n"
        << "      ++NumMCOperands;\n"
        << "      break;\n"
        << "    case CVT_Tied:\n"
        << "      ++NumMCOperands;\n"
        << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitOperandFunctionII(
    std::string const &EnumName, MatchableInfo::AsmOperand const &Op,
    MatchableInfo::ResOperand const &OpInfo) const {
  // Add a handler for the operand number lookup.
  *OpOS << "    case " << EnumName << ":\n"
        << "      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);\n";

  if (Op.Class->isRegisterClass())
    *OpOS << "      Operands[*(p + 1)]->setConstraint(\"r\");\n";
  else
    *OpOS << "      Operands[*(p + 1)]->setConstraint(\"m\");\n";
  *OpOS << "      NumMCOperands += " << OpInfo.MINumOperands << ";\n"
        << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitOperandFunctionIII(
    std::string const &EnumName) const {
  *OpOS << "    case " << EnumName << ":\n"
        << "      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);\n"
        << "      Operands[*(p + 1)]->setConstraint(\"\");\n"
        << "      ++NumMCOperands;\n"
        << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitOperandFunctionIV(
    std::string const &EnumName) const {
  *OpOS << "    case " << EnumName << ":\n"
        << "      Operands[*(p + 1)]->setMCOperandNum(NumMCOperands);\n"
        << "      Operands[*(p + 1)]->setConstraint(\"m\");\n"
        << "      ++NumMCOperands;\n"
        << "      break;\n";
}

void PrinterLLVM::asmMatcherEmitOperandFunctionV() const {
  *OpOS << "    }\n  }\n}\n\n";
}

void PrinterLLVM::asmMatcherEmitTiedOperandEnum(
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {
  OS << "enum {\n";
  for (auto &KV : TiedOperandsEnumMap) {
    OS << "  " << KV.second << ",\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitTiedOpTable(
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {
  OS << "static const uint8_t TiedAsmOperandTable[][3] = {\n";
  for (auto &KV : TiedOperandsEnumMap) {
    OS << "  /* " << KV.second << " */ { " << utostr(std::get<0>(KV.first))
       << ", " << utostr(std::get<1>(KV.first)) << ", "
       << utostr(std::get<2>(KV.first)) << " },\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitTiedOpEmptyTable() const {
  OS << "static const uint8_t TiedAsmOperandTable[][3] = "
        "{ /* empty  */ {0, 0, 0} };\n\n";
}

void PrinterLLVM::asmMatcherEmitOperandConvKindEnum(
    SmallSetVector<CachedHashString, 16> OperandConversionKinds) const {
  OS << "enum OperatorConversionKind {\n";
  for (const auto &Converter : OperandConversionKinds)
    OS << "  " << Converter << ",\n";
  OS << "  CVT_NUM_CONVERTERS\n";
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitInstrConvKindEnum(
    SmallSetVector<CachedHashString, 16> InstructionConversionKinds) const {
  OS << "enum InstructionConversionKind {\n";
  for (const auto &Signature : InstructionConversionKinds)
    OS << "  " << Signature << ",\n";
  OS << "  CVT_NUM_SIGNATURES\n";
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitConversionTable(
    size_t MaxRowLength,
    std::vector<std::vector<uint8_t>> const ConversionTable,
    SmallSetVector<CachedHashString, 16> InstructionConversionKinds,
    SmallSetVector<CachedHashString, 16> OperandConversionKinds,
    std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
        TiedOperandsEnumMap) const {
  OS << "static const uint8_t ConversionTable[CVT_NUM_SIGNATURES]["
     << MaxRowLength << "] = {\n";

  for (unsigned Row = 0, ERow = ConversionTable.size(); Row != ERow; ++Row) {
    assert(ConversionTable[Row].size() % 2 == 0 && "bad conversion row!");
    OS << "  // " << InstructionConversionKinds[Row] << "\n";
    OS << "  { ";
    for (unsigned I = 0, E = ConversionTable[Row].size(); I != E; I += 2) {
      OS << OperandConversionKinds[ConversionTable[Row][I]] << ", ";
      if (OperandConversionKinds[ConversionTable[Row][I]] !=
          CachedHashString("CVT_Tied")) {
        OS << (unsigned)(ConversionTable[Row][I + 1]) << ", ";
        continue;
      }

      // For a tied operand, emit a reference to the TiedAsmOperandTable
      // that contains the operand to copy, and the parsed operands to
      // check for their tied constraints.
      auto Key = std::make_tuple((uint8_t)ConversionTable[Row][I + 1],
                                 (uint8_t)ConversionTable[Row][I + 2],
                                 (uint8_t)ConversionTable[Row][I + 3]);
      auto TiedOpndEnum = TiedOperandsEnumMap.find(Key);
      assert(TiedOpndEnum != TiedOperandsEnumMap.end() &&
             "No record for tied operand pair");
      OS << TiedOpndEnum->second << ", ";
      I += 2;
    }
    OS << "CVT_Done },\n";
  }

  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherWriteCvtOSToOS() const { OS << CvtOS->str(); }

void PrinterLLVM::asmMatcherWriteOpOSToOS() const { OS << OpOS->str(); }

void PrinterLLVM::asmMatcherEmitMatchClassKindEnum(
    std::forward_list<ClassInfo> const &Infos) const {
  OS << "/// MatchClassKind - The kinds of classes which participate in\n"
     << "/// instruction matching.\n";
  OS << "enum MatchClassKind {\n";
  OS << "  InvalidMatchClass = 0,\n";
  OS << "  OptionalMatchClass = 1,\n";
  ClassInfo::ClassInfoKind LastKind = ClassInfo::Token;
  StringRef LastName = "OptionalMatchClass";
  for (const auto &CI : Infos) {
    if (LastKind == ClassInfo::Token && CI.Kind != ClassInfo::Token) {
      OS << "  MCK_LAST_TOKEN = " << LastName << ",\n";
    } else if (LastKind < ClassInfo::UserClass0 &&
               CI.Kind >= ClassInfo::UserClass0) {
      OS << "  MCK_LAST_REGISTER = " << LastName << ",\n";
    }
    LastKind = (ClassInfo::ClassInfoKind)CI.Kind;
    LastName = CI.Name;

    OS << "  " << CI.Name << ", // ";
    if (CI.Kind == ClassInfo::Token) {
      OS << "'" << CI.ValueName << "'\n";
    } else if (CI.isRegisterClass()) {
      if (!CI.ValueName.empty())
        OS << "register class '" << CI.ValueName << "'\n";
      else
        OS << "derived register class\n";
    } else {
      OS << "user defined class '" << CI.ValueName << "'\n";
    }
  }
  OS << "  NumMatchClassKinds\n";
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchClassDiagStrings(
    AsmMatcherInfo const &Info) const {
  OS << "static const char *getMatchKindDiag(" << Info.Target.getName()
     << "AsmParser::" << Info.Target.getName()
     << "MatchResultTy MatchResult) {\n";
  OS << "  switch (MatchResult) {\n";

  for (const auto &CI : Info.Classes) {
    if (!CI.DiagnosticString.empty()) {
      assert(!CI.DiagnosticType.empty() &&
             "DiagnosticString set without DiagnosticType");
      OS << "  case " << Info.Target.getName() << "AsmParser::Match_"
         << CI.DiagnosticType << ":\n";
      OS << "    return \"" << CI.DiagnosticString << "\";\n";
    }
  }

  OS << "  default:\n";
  OS << "    return nullptr;\n";

  OS << "  }\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitRegisterMatchErrorFunc(
    AsmMatcherInfo &Info) const {
  OS << "static unsigned getDiagKindFromRegisterClass(MatchClassKind "
        "RegisterClass) {\n";
  if (none_of(Info.Classes, [](const ClassInfo &CI) {
        return CI.isRegisterClass() && !CI.DiagnosticType.empty();
      })) {
    OS << "  return MCTargetAsmParser::Match_InvalidOperand;\n";
  } else {
    OS << "  switch (RegisterClass) {\n";
    for (const auto &CI : Info.Classes) {
      if (CI.isRegisterClass() && !CI.DiagnosticType.empty()) {
        OS << "  case " << CI.Name << ":\n";
        OS << "    return " << Info.Target.getName() << "AsmParser::Match_"
           << CI.DiagnosticType << ";\n";
      }
    }

    OS << "  default:\n";
    OS << "    return MCTargetAsmParser::Match_InvalidOperand;\n";

    OS << "  }\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitIsSubclassI() const {
  OS << "/// isSubclass - Compute whether \\p A is a subclass of \\p B.\n";
  OS << "static bool isSubclass(MatchClassKind A, MatchClassKind B) {\n";
  OS << "  if (A == B)\n";
  OS << "    return true;\n\n";
}

bool PrinterLLVM::asmMatcherEmitIsSubclassII(bool EmittedSwitch,
                                             std::string const &Name) const {
  bool ESTmp = EmittedSwitch;
  if (!EmittedSwitch) {
    OS << "  switch (A) {\n";
    OS << "  default:\n";
    OS << "    return false;\n";
    ESTmp = true;
  }

  OS << "\n  case " << Name << ":\n";
  return ESTmp;
}

void PrinterLLVM::asmMatcherEmitIsSubclassIII(StringRef const &Class) const {
  OS << "    return B == " << Class << ";\n";
}

void PrinterLLVM::asmMatcherEmitIsSubclassIV(
    std::vector<StringRef> const &SuperClasses) const {
  if (!SuperClasses.empty()) {
    OS << "    switch (B) {\n";
    OS << "    default: return false;\n";
    for (StringRef SC : SuperClasses)
      OS << "    case " << SC << ": return true;\n";
    OS << "    }\n";
  } else {
    // No case statement to emit
    OS << "    return false;\n";
  }
}

void PrinterLLVM::asmMatcherEmitIsSubclassV(bool EmittedSwitch) const {
  if (EmittedSwitch)
    OS << "  }\n";
  else
    OS << "  return false;\n";

  OS << "}\n\n";
}

/// emitValidateOperandClass - Emit the function to validate an operand class.
void PrinterLLVM::asmMatcherEmitValidateOperandClass(
    AsmMatcherInfo &Info) const {
  OS << "static unsigned validateOperandClass(MCParsedAsmOperand &GOp, "
     << "MatchClassKind Kind) {\n";
  OS << "  " << Info.Target.getName() << "Operand &Operand = ("
     << Info.Target.getName() << "Operand &)GOp;\n";

  // The InvalidMatchClass is not to match any operand.
  OS << "  if (Kind == InvalidMatchClass)\n";
  OS << "    return MCTargetAsmParser::Match_InvalidOperand;\n\n";

  // Check for Token operands first.
  // FIXME: Use a more specific diagnostic type.
  OS << "  if (Operand.isToken() && Kind <= MCK_LAST_TOKEN)\n";
  OS << "    return isSubclass(matchTokenString(Operand.getToken()), Kind) ?\n"
     << "             MCTargetAsmParser::Match_Success :\n"
     << "             MCTargetAsmParser::Match_InvalidOperand;\n\n";

  // Check the user classes. We don't care what order since we're only
  // actually matching against one of them.
  OS << "  switch (Kind) {\n"
        "  default: break;\n";
  for (const auto &CI : Info.Classes) {
    if (!CI.isUserClass())
      continue;

    OS << "  // '" << CI.ClassName << "' class\n";
    OS << "  case " << CI.Name << ": {\n";
    OS << "    DiagnosticPredicate DP(Operand." << CI.PredicateMethod
       << "());\n";
    OS << "    if (DP.isMatch())\n";
    OS << "      return MCTargetAsmParser::Match_Success;\n";
    if (!CI.DiagnosticType.empty()) {
      OS << "    if (DP.isNearMatch())\n";
      OS << "      return " << Info.Target.getName() << "AsmParser::Match_"
         << CI.DiagnosticType << ";\n";
      OS << "    break;\n";
    } else
      OS << "    break;\n";
    OS << "    }\n";
  }
  OS << "  } // end switch (Kind)\n\n";

  // Check for register operands, including sub-classes.
  OS << "  if (Operand.isReg()) {\n";
  OS << "    MatchClassKind OpKind;\n";
  OS << "    switch (Operand.getReg()) {\n";
  OS << "    default: OpKind = InvalidMatchClass; break;\n";
  for (const auto &RC : Info.RegisterClasses)
    OS << "    case " << RC.first->getValueAsString("Namespace")
       << "::" << RC.first->getName() << ": OpKind = " << RC.second->Name
       << "; break;\n";
  OS << "    }\n";
  OS << "    return isSubclass(OpKind, Kind) ? "
     << "(unsigned)MCTargetAsmParser::Match_Success :\n                     "
     << "                 getDiagKindFromRegisterClass(Kind);\n  }\n\n";

  // Expected operand is a register, but actual is not.
  OS << "  if (Kind > MCK_LAST_TOKEN && Kind <= MCK_LAST_REGISTER)\n";
  OS << "    return getDiagKindFromRegisterClass(Kind);\n\n";

  // Generic fallthrough match failure case for operands that don't have
  // specialized diagnostic types.
  OS << "  return MCTargetAsmParser::Match_InvalidOperand;\n";
  OS << "}\n\n";
}

// Emit a function mapping match classes to strings, for debugging.
void PrinterLLVM::asmMatcherEmitMatchClassKindNames(
    std::forward_list<ClassInfo> &Infos) const {
  OS << "#ifndef NDEBUG\n";
  OS << "const char *getMatchClassName(MatchClassKind Kind) {\n";
  OS << "  switch (Kind) {\n";

  OS << "  case InvalidMatchClass: return \"InvalidMatchClass\";\n";
  OS << "  case OptionalMatchClass: return \"OptionalMatchClass\";\n";
  for (const auto &CI : Infos) {
    OS << "  case " << CI.Name << ": return \"" << CI.Name << "\";\n";
  }
  OS << "  case NumMatchClassKinds: return \"NumMatchClassKinds\";\n";

  OS << "  }\n";
  OS << "  llvm_unreachable(\"unhandled MatchClassKind!\");\n";
  OS << "}\n\n";
  OS << "#endif // NDEBUG\n";
}

void PrinterLLVM::asmMatcherEmitAsmTiedOperandConstraints(
    CodeGenTarget &Target, AsmMatcherInfo &Info) const {
  std::string AsmParserName =
      std::string(Info.AsmParser->getValueAsString("AsmParserClassName"));
  OS << "static bool ";
  OS << "checkAsmTiedOperandConstraints(const " << Target.getName()
     << AsmParserName << "&AsmParser,\n";
  OS << "                               unsigned Kind,\n";
  OS << "                               const OperandVector &Operands,\n";
  OS << "                               uint64_t &ErrorInfo) {\n";
  OS << "  assert(Kind < CVT_NUM_SIGNATURES && \"Invalid signature!\");\n";
  OS << "  const uint8_t *Converter = ConversionTable[Kind];\n";
  OS << "  for (const uint8_t *p = Converter; *p; p += 2) {\n";
  OS << "    switch (*p) {\n";
  OS << "    case CVT_Tied: {\n";
  OS << "      unsigned OpIdx = *(p + 1);\n";
  OS << "      assert(OpIdx < (size_t)(std::end(TiedAsmOperandTable) -\n";
  OS << "                              std::begin(TiedAsmOperandTable)) &&\n";
  OS << "             \"Tied operand not found\");\n";
  OS << "      unsigned OpndNum1 = TiedAsmOperandTable[OpIdx][1];\n";
  OS << "      unsigned OpndNum2 = TiedAsmOperandTable[OpIdx][2];\n";
  OS << "      if (OpndNum1 != OpndNum2) {\n";
  OS << "        auto &SrcOp1 = Operands[OpndNum1];\n";
  OS << "        auto &SrcOp2 = Operands[OpndNum2];\n";
  OS << "        if (!AsmParser.areEqualRegs(*SrcOp1, *SrcOp2)) {\n";
  OS << "          ErrorInfo = OpndNum2;\n";
  OS << "          return false;\n";
  OS << "        }\n";
  OS << "      }\n";
  OS << "      break;\n";
  OS << "    }\n";
  OS << "    default:\n";
  OS << "      break;\n";
  OS << "    }\n";
  OS << "  }\n";
  OS << "  return true;\n";
  OS << "}\n\n";
}

std::string PrinterLLVM::getNameForFeatureBitset(
    const std::vector<Record *> &FeatureBitset) const {
  std::string Name = "AMFBS";
  for (const auto &Feature : FeatureBitset)
    Name += ("_" + Feature->getName()).str();
  return Name;
}

void PrinterLLVM::asmMatcherEmitFeatureBitsetEnum(
    std::vector<std::vector<Record *>> const FeatureBitsets) const {
  OS << "// Feature bitsets.\n"
     << "enum : " << getMinimalTypeForRange(FeatureBitsets.size()) << " {\n"
     << "  AMFBS_None,\n";
  for (const auto &FeatureBitset : FeatureBitsets) {
    if (FeatureBitset.empty())
      continue;
    OS << "  " << getNameForFeatureBitset(FeatureBitset) << ",\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitFeatureBitsets(
    std::vector<std::vector<Record *>> const FeatureBitsets,
    AsmMatcherInfo const &Info) const {
  OS << "static constexpr FeatureBitset FeatureBitsets[] = {\n"
     << "  {}, // AMFBS_None\n";
  for (const auto &FeatureBitset : FeatureBitsets) {
    if (FeatureBitset.empty())
      continue;
    OS << "  {";
    for (const auto &Feature : FeatureBitset) {
      const auto &I = Info.SubtargetFeatures.find(Feature);
      assert(I != Info.SubtargetFeatures.end() && "Didn't import predicate?");
      OS << I->second.getEnumBitName() << ", ";
    }
    OS << "},\n";
  }
  OS << "};\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchEntryStruct(
    unsigned MaxMnemonicIndex, unsigned NumConverters, size_t MaxNumOperands,
    std::vector<std::vector<Record *>> const FeatureBitsets,
    AsmMatcherInfo const &Info) const {
  OS << "  struct MatchEntry {\n";
  OS << "    " << getMinimalTypeForRange(MaxMnemonicIndex) << " Mnemonic;\n";
  OS << "    uint16_t Opcode;\n";
  OS << "    " << getMinimalTypeForRange(NumConverters) << " ConvertFn;\n";
  OS << "    " << getMinimalTypeForRange(FeatureBitsets.size())
     << " RequiredFeaturesIdx;\n";
  OS << "    "
     << getMinimalTypeForRange(
            std::distance(Info.Classes.begin(), Info.Classes.end()))
     << " Classes[" << MaxNumOperands << "];\n";
  OS << "    StringRef getMnemonic() const {\n";
  OS << "      return StringRef(MnemonicTable + Mnemonic + 1,\n";
  OS << "                       MnemonicTable[Mnemonic]);\n";
  OS << "    }\n";
  OS << "  };\n";
  OS << "  // Predicate for searching for an opcode.\n";
  OS << "  struct LessOpcode {\n";
  OS << "    bool operator()(const MatchEntry &LHS, StringRef RHS) {\n";
  OS << "      return LHS.getMnemonic() < RHS;\n";
  OS << "    }\n";
  OS << "    bool operator()(StringRef LHS, const MatchEntry &RHS) {\n";
  OS << "      return LHS < RHS.getMnemonic();\n";
  OS << "    }\n";
  OS << "    bool operator()(const MatchEntry &LHS, const MatchEntry &RHS) {\n";
  OS << "      return LHS.getMnemonic() < RHS.getMnemonic();\n";
  OS << "    }\n";
  OS << "  };\n";
}

void PrinterLLVM::asmMatcherEmitMatchFunction(
    CodeGenTarget const &Target, Record const *AsmParser,
    StringRef const &ClassName, bool HasMnemonicFirst, bool HasOptionalOperands,
    bool ReportMultipleNearMisses, bool HasMnemonicAliases,
    size_t MaxNumOperands, bool HasDeprecation,
    unsigned int VariantCount) const {
  OS << "unsigned " << Target.getName() << ClassName << "::\n"
     << "MatchInstructionImpl(const OperandVector &Operands,\n";
  OS << "                     MCInst &Inst,\n";
  if (ReportMultipleNearMisses)
    OS << "                     SmallVectorImpl<NearMissInfo> *NearMisses,\n";
  else
    OS << "                     uint64_t &ErrorInfo,\n"
       << "                     FeatureBitset &MissingFeatures,\n";
  OS << "                     bool matchingInlineAsm, unsigned VariantID) {\n";

  if (!ReportMultipleNearMisses) {
    OS << "  // Eliminate obvious mismatches.\n";
    OS << "  if (Operands.size() > " << (MaxNumOperands + HasMnemonicFirst)
       << ") {\n";
    OS << "    ErrorInfo = " << (MaxNumOperands + HasMnemonicFirst) << ";\n";
    OS << "    return Match_InvalidOperand;\n";
    OS << "  }\n\n";
  }

  // Emit code to get the available features.
  OS << "  // Get the current feature set.\n";
  OS << "  const FeatureBitset &AvailableFeatures = "
        "getAvailableFeatures();\n\n";

  OS << "  // Get the instruction mnemonic, which is the first token.\n";
  if (HasMnemonicFirst) {
    OS << "  StringRef Mnemonic = ((" << Target.getName()
       << "Operand &)*Operands[0]).getToken();\n\n";
  } else {
    OS << "  StringRef Mnemonic;\n";
    OS << "  if (Operands[0]->isToken())\n";
    OS << "    Mnemonic = ((" << Target.getName()
       << "Operand &)*Operands[0]).getToken();\n\n";
  }

  if (HasMnemonicAliases) {
    OS << "  // Process all MnemonicAliases to remap the mnemonic.\n";
    OS << "  applyMnemonicAliases(Mnemonic, AvailableFeatures, VariantID);\n\n";
  }

  // Emit code to compute the class list for this operand vector.
  if (!ReportMultipleNearMisses) {
    OS << "  // Some state to try to produce better error messages.\n";
    OS << "  bool HadMatchOtherThanFeatures = false;\n";
    OS << "  bool HadMatchOtherThanPredicate = false;\n";
    OS << "  unsigned RetCode = Match_InvalidOperand;\n";
    OS << "  MissingFeatures.set();\n";
    OS << "  // Set ErrorInfo to the operand that mismatches if it is\n";
    OS << "  // wrong for all instances of the instruction.\n";
    OS << "  ErrorInfo = ~0ULL;\n";
  }

  if (HasOptionalOperands) {
    OS << "  SmallBitVector OptionalOperandsMask(" << MaxNumOperands << ");\n";
  }

  // Emit code to search the table.
  OS << "  // Find the appropriate table for this asm variant.\n";
  OS << "  const MatchEntry *Start, *End;\n";
  OS << "  switch (VariantID) {\n";
  OS << "  default: llvm_unreachable(\"invalid variant!\");\n";
  for (unsigned VC = 0; VC != VariantCount; ++VC) {
    Record *AsmVariant = Target.getAsmParserVariant(VC);
    int AsmVariantNo = AsmVariant->getValueAsInt("Variant");
    OS << "  case " << AsmVariantNo << ": Start = std::begin(MatchTable" << VC
       << "); End = std::end(MatchTable" << VC << "); break;\n";
  }
  OS << "  }\n";

  OS << "  // Search the table.\n";
  if (HasMnemonicFirst) {
    OS << "  auto MnemonicRange = "
          "std::equal_range(Start, End, Mnemonic, LessOpcode());\n\n";
  } else {
    OS << "  auto MnemonicRange = std::make_pair(Start, End);\n";
    OS << "  unsigned SIndex = Mnemonic.empty() ? 0 : 1;\n";
    OS << "  if (!Mnemonic.empty())\n";
    OS << "    MnemonicRange = "
          "std::equal_range(Start, End, Mnemonic.lower(), LessOpcode());\n\n";
  }

  OS << "  DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"AsmMatcher: found \" "
        "<<\n"
     << "  std::distance(MnemonicRange.first, MnemonicRange.second) <<\n"
     << "  \" encodings with mnemonic '\" << Mnemonic << \"'\\n\");\n\n";

  OS << "  // Return a more specific error code if no mnemonics match.\n";
  OS << "  if (MnemonicRange.first == MnemonicRange.second)\n";
  OS << "    return Match_MnemonicFail;\n\n";

  OS << "  for (const MatchEntry *it = MnemonicRange.first, "
     << "*ie = MnemonicRange.second;\n";
  OS << "       it != ie; ++it) {\n";
  OS << "    const FeatureBitset &RequiredFeatures = "
        "FeatureBitsets[it->RequiredFeaturesIdx];\n";
  OS << "    bool HasRequiredFeatures =\n";
  OS << "      (AvailableFeatures & RequiredFeatures) == RequiredFeatures;\n";
  OS << "    DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Trying to match "
        "opcode \"\n";
  OS << "                                          << MII.getName(it->Opcode) "
        "<< \"\\n\");\n";

  if (ReportMultipleNearMisses) {
    OS << "    // Some state to record ways in which this instruction did not "
          "match.\n";
    OS << "    NearMissInfo OperandNearMiss = NearMissInfo::getSuccess();\n";
    OS << "    NearMissInfo FeaturesNearMiss = NearMissInfo::getSuccess();\n";
    OS << "    NearMissInfo EarlyPredicateNearMiss = "
          "NearMissInfo::getSuccess();\n";
    OS << "    NearMissInfo LatePredicateNearMiss = "
          "NearMissInfo::getSuccess();\n";
    OS << "    bool MultipleInvalidOperands = false;\n";
  }

  if (HasMnemonicFirst) {
    OS << "    // equal_range guarantees that instruction mnemonic matches.\n";
    OS << "    assert(Mnemonic == it->getMnemonic());\n";
  }

  // Emit check that the subclasses match.
  if (!ReportMultipleNearMisses)
    OS << "    bool OperandsValid = true;\n";
  if (HasOptionalOperands) {
    OS << "    OptionalOperandsMask.reset(0, " << MaxNumOperands << ");\n";
  }
  OS << "    for (unsigned FormalIdx = " << (HasMnemonicFirst ? "0" : "SIndex")
     << ", ActualIdx = " << (HasMnemonicFirst ? "1" : "SIndex")
     << "; FormalIdx != " << MaxNumOperands << "; ++FormalIdx) {\n";
  OS << "      auto Formal = "
     << "static_cast<MatchClassKind>(it->Classes[FormalIdx]);\n";
  OS << "      DEBUG_WITH_TYPE(\"asm-matcher\",\n";
  OS << "                      dbgs() << \"  Matching formal operand class \" "
        "<< getMatchClassName(Formal)\n";
  OS << "                             << \" against actual operand at index \" "
        "<< ActualIdx);\n";
  OS << "      if (ActualIdx < Operands.size())\n";
  OS << "        DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \" (\";\n";
  OS << "                        Operands[ActualIdx]->print(dbgs()); dbgs() << "
        "\"): \");\n";
  OS << "      else\n";
  OS << "        DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \": \");\n";
  OS << "      if (ActualIdx >= Operands.size()) {\n";
  OS << "        DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"actual operand "
        "index out of range\\n\");\n";
  if (ReportMultipleNearMisses) {
    OS << "        bool ThisOperandValid = (Formal == "
       << "InvalidMatchClass) || "
          "isSubclass(Formal, OptionalMatchClass);\n";
    OS << "        if (!ThisOperandValid) {\n";
    OS << "          if (!OperandNearMiss) {\n";
    OS << "            // Record info about match failure for later use.\n";
    OS << "            DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"recording "
          "too-few-operands near miss\\n\");\n";
    OS << "            OperandNearMiss =\n";
    OS << "                NearMissInfo::getTooFewOperands(Formal, "
          "it->Opcode);\n";
    OS << "          } else if (OperandNearMiss.getKind() != "
          "NearMissInfo::NearMissTooFewOperands) {\n";
    OS << "            // If more than one operand is invalid, give up on this "
          "match entry.\n";
    OS << "            DEBUG_WITH_TYPE(\n";
    OS << "                \"asm-matcher\",\n";
    OS << "                dbgs() << \"second invalid operand, giving up on "
          "this opcode\\n\");\n";
    OS << "            MultipleInvalidOperands = true;\n";
    OS << "            break;\n";
    OS << "          }\n";
    OS << "        } else {\n";
    OS << "          DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"but formal "
          "operand not required\\n\");\n";
    OS << "        }\n";
    OS << "        continue;\n";
  } else {
    OS << "        if (Formal == InvalidMatchClass) {\n";
    if (HasOptionalOperands) {
      OS << "          OptionalOperandsMask.set(FormalIdx, " << MaxNumOperands
         << ");\n";
    }
    OS << "          break;\n";
    OS << "        }\n";
    OS << "        if (isSubclass(Formal, OptionalMatchClass)) {\n";
    if (HasOptionalOperands) {
      OS << "          OptionalOperandsMask.set(FormalIdx);\n";
    }
    OS << "          continue;\n";
    OS << "        }\n";
    OS << "        OperandsValid = false;\n";
    OS << "        ErrorInfo = ActualIdx;\n";
    OS << "        break;\n";
  }
  OS << "      }\n";
  OS << "      MCParsedAsmOperand &Actual = *Operands[ActualIdx];\n";
  OS << "      unsigned Diag = validateOperandClass(Actual, Formal);\n";
  OS << "      if (Diag == Match_Success) {\n";
  OS << "        DEBUG_WITH_TYPE(\"asm-matcher\",\n";
  OS << "                        dbgs() << \"match success using generic "
        "matcher\\n\");\n";
  OS << "        ++ActualIdx;\n";
  OS << "        continue;\n";
  OS << "      }\n";
  OS << "      // If the generic handler indicates an invalid operand\n";
  OS << "      // failure, check for a special case.\n";
  OS << "      if (Diag != Match_Success) {\n";
  OS << "        unsigned TargetDiag = validateTargetOperandClass(Actual, "
        "Formal);\n";
  OS << "        if (TargetDiag == Match_Success) {\n";
  OS << "          DEBUG_WITH_TYPE(\"asm-matcher\",\n";
  OS << "                          dbgs() << \"match success using target "
        "matcher\\n\");\n";
  OS << "          ++ActualIdx;\n";
  OS << "          continue;\n";
  OS << "        }\n";
  OS << "        // If the target matcher returned a specific error code use\n";
  OS << "        // that, else use the one from the generic matcher.\n";
  OS << "        if (TargetDiag != Match_InvalidOperand && "
        "HasRequiredFeatures)\n";
  OS << "          Diag = TargetDiag;\n";
  OS << "      }\n";
  OS << "      // If current formal operand wasn't matched and it is optional\n"
     << "      // then try to match next formal operand\n";
  OS << "      if (Diag == Match_InvalidOperand "
     << "&& isSubclass(Formal, OptionalMatchClass)) {\n";
  if (HasOptionalOperands) {
    OS << "        OptionalOperandsMask.set(FormalIdx);\n";
  }
  OS << "        DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"ignoring "
        "optional operand\\n\");\n";
  OS << "        continue;\n";
  OS << "      }\n";

  if (ReportMultipleNearMisses) {
    OS << "      if (!OperandNearMiss) {\n";
    OS << "        // If this is the first invalid operand we have seen, "
          "record some\n";
    OS << "        // information about it.\n";
    OS << "        DEBUG_WITH_TYPE(\n";
    OS << "            \"asm-matcher\",\n";
    OS << "            dbgs()\n";
    OS << "                << \"operand match failed, recording near-miss with "
          "diag code \"\n";
    OS << "                << Diag << \"\\n\");\n";
    OS << "        OperandNearMiss =\n";
    OS << "            NearMissInfo::getMissedOperand(Diag, Formal, "
          "it->Opcode, ActualIdx);\n";
    OS << "        ++ActualIdx;\n";
    OS << "      } else {\n";
    OS << "        // If more than one operand is invalid, give up on this "
          "match entry.\n";
    OS << "        DEBUG_WITH_TYPE(\n";
    OS << "            \"asm-matcher\",\n";
    OS << "            dbgs() << \"second operand mismatch, skipping this "
          "opcode\\n\");\n";
    OS << "        MultipleInvalidOperands = true;\n";
    OS << "        break;\n";
    OS << "      }\n";
    OS << "    }\n\n";
  } else {
    OS << "      // If this operand is broken for all of the instances of "
          "this\n";
    OS << "      // mnemonic, keep track of it so we can report loc info.\n";
    OS << "      // If we already had a match that only failed due to a\n";
    OS << "      // target predicate, that diagnostic is preferred.\n";
    OS << "      if (!HadMatchOtherThanPredicate &&\n";
    OS << "          (it == MnemonicRange.first || ErrorInfo <= ActualIdx)) "
          "{\n";
    OS << "        if (HasRequiredFeatures && (ErrorInfo != ActualIdx || Diag "
          "!= Match_InvalidOperand))\n";
    OS << "          RetCode = Diag;\n";
    OS << "        ErrorInfo = ActualIdx;\n";
    OS << "      }\n";
    OS << "      // Otherwise, just reject this instance of the mnemonic.\n";
    OS << "      OperandsValid = false;\n";
    OS << "      break;\n";
    OS << "    }\n\n";
  }

  if (ReportMultipleNearMisses)
    OS << "    if (MultipleInvalidOperands) {\n";
  else
    OS << "    if (!OperandsValid) {\n";
  OS << "      DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Opcode result: "
        "multiple \"\n";
  OS << "                                               \"operand mismatches, "
        "ignoring \"\n";
  OS << "                                               \"this opcode\\n\");\n";
  OS << "      continue;\n";
  OS << "    }\n";

  // Emit check that the required features are available.
  OS << "    if (!HasRequiredFeatures) {\n";
  if (!ReportMultipleNearMisses)
    OS << "      HadMatchOtherThanFeatures = true;\n";
  OS << "      FeatureBitset NewMissingFeatures = RequiredFeatures & "
        "~AvailableFeatures;\n";
  OS << "      DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Missing target "
        "features:\";\n";
  OS << "                      for (unsigned I = 0, E = "
        "NewMissingFeatures.size(); I != E; ++I)\n";
  OS << "                        if (NewMissingFeatures[I])\n";
  OS << "                          dbgs() << ' ' << I;\n";
  OS << "                      dbgs() << \"\\n\");\n";
  if (ReportMultipleNearMisses) {
    OS << "      FeaturesNearMiss = "
          "NearMissInfo::getMissedFeature(NewMissingFeatures);\n";
  } else {
    OS << "      if (NewMissingFeatures.count() <=\n"
          "          MissingFeatures.count())\n";
    OS << "        MissingFeatures = NewMissingFeatures;\n";
    OS << "      continue;\n";
  }
  OS << "    }\n";
  OS << "\n";
  OS << "    Inst.clear();\n\n";
  OS << "    Inst.setOpcode(it->Opcode);\n";
  // Verify the instruction with the target-specific match predicate function.
  OS << "    // We have a potential match but have not rendered the operands.\n"
     << "    // Check the target predicate to handle any context sensitive\n"
        "    // constraints.\n"
     << "    // For example, Ties that are referenced multiple times must be\n"
        "    // checked here to ensure the input is the same for each match\n"
        "    // constraints. If we leave it any later the ties will have been\n"
        "    // canonicalized\n"
     << "    unsigned MatchResult;\n"
     << "    if ((MatchResult = checkEarlyTargetMatchPredicate(Inst, "
        "Operands)) != Match_Success) {\n"
     << "      Inst.clear();\n";
  OS << "      DEBUG_WITH_TYPE(\n";
  OS << "          \"asm-matcher\",\n";
  OS << "          dbgs() << \"Early target match predicate failed with diag "
        "code \"\n";
  OS << "                 << MatchResult << \"\\n\");\n";
  if (ReportMultipleNearMisses) {
    OS << "      EarlyPredicateNearMiss = "
          "NearMissInfo::getMissedPredicate(MatchResult);\n";
  } else {
    OS << "      RetCode = MatchResult;\n"
       << "      HadMatchOtherThanPredicate = true;\n"
       << "      continue;\n";
  }
  OS << "    }\n\n";

  if (ReportMultipleNearMisses) {
    OS << "    // If we did not successfully match the operands, then we can't "
          "convert to\n";
    OS << "    // an MCInst, so bail out on this instruction variant now.\n";
    OS << "    if (OperandNearMiss) {\n";
    OS << "      // If the operand mismatch was the only problem, reprrt it as "
          "a near-miss.\n";
    OS << "      if (NearMisses && !FeaturesNearMiss && "
          "!EarlyPredicateNearMiss) {\n";
    OS << "        DEBUG_WITH_TYPE(\n";
    OS << "            \"asm-matcher\",\n";
    OS << "            dbgs()\n";
    OS << "                << \"Opcode result: one mismatched operand, adding "
          "near-miss\\n\");\n";
    OS << "        NearMisses->push_back(OperandNearMiss);\n";
    OS << "      } else {\n";
    OS << "        DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Opcode result: "
          "multiple \"\n";
    OS << "                                                 \"types of "
          "mismatch, so not \"\n";
    OS << "                                                 \"reporting "
          "near-miss\\n\");\n";
    OS << "      }\n";
    OS << "      continue;\n";
    OS << "    }\n\n";
  }

  OS << "    if (matchingInlineAsm) {\n";
  OS << "      convertToMapAndConstraints(it->ConvertFn, Operands);\n";
  if (!ReportMultipleNearMisses) {
    OS << "      if (!checkAsmTiedOperandConstraints(*this, it->ConvertFn, "
          "Operands, ErrorInfo))\n";
    OS << "        return Match_InvalidTiedOperand;\n";
    OS << "\n";
  }
  OS << "      return Match_Success;\n";
  OS << "    }\n\n";
  OS << "    // We have selected a definite instruction, convert the parsed\n"
     << "    // operands into the appropriate MCInst.\n";
  if (HasOptionalOperands) {
    OS << "    convertToMCInst(it->ConvertFn, Inst, it->Opcode, Operands,\n"
       << "                    OptionalOperandsMask);\n";
  } else {
    OS << "    convertToMCInst(it->ConvertFn, Inst, it->Opcode, Operands);\n";
  }
  OS << "\n";

  // Verify the instruction with the target-specific match predicate function.
  OS << "    // We have a potential match. Check the target predicate to\n"
     << "    // handle any context sensitive constraints.\n"
     << "    if ((MatchResult = checkTargetMatchPredicate(Inst)) !="
     << " Match_Success) {\n"
     << "      DEBUG_WITH_TYPE(\"asm-matcher\",\n"
     << "                      dbgs() << \"Target match predicate failed with "
        "diag code \"\n"
     << "                             << MatchResult << \"\\n\");\n"
     << "      Inst.clear();\n";
  if (ReportMultipleNearMisses) {
    OS << "      LatePredicateNearMiss = "
          "NearMissInfo::getMissedPredicate(MatchResult);\n";
  } else {
    OS << "      RetCode = MatchResult;\n"
       << "      HadMatchOtherThanPredicate = true;\n"
       << "      continue;\n";
  }
  OS << "    }\n\n";

  if (ReportMultipleNearMisses) {
    OS << "    int NumNearMisses = ((int)(bool)OperandNearMiss +\n";
    OS << "                         (int)(bool)FeaturesNearMiss +\n";
    OS << "                         (int)(bool)EarlyPredicateNearMiss +\n";
    OS << "                         (int)(bool)LatePredicateNearMiss);\n";
    OS << "    if (NumNearMisses == 1) {\n";
    OS << "      // We had exactly one type of near-miss, so add that to the "
          "list.\n";
    OS << "      assert(!OperandNearMiss && \"OperandNearMiss was handled "
          "earlier\");\n";
    OS << "      DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Opcode result: "
          "found one type of \"\n";
    OS << "                                            \"mismatch, so "
          "reporting a \"\n";
    OS << "                                            \"near-miss\\n\");\n";
    OS << "      if (NearMisses && FeaturesNearMiss)\n";
    OS << "        NearMisses->push_back(FeaturesNearMiss);\n";
    OS << "      else if (NearMisses && EarlyPredicateNearMiss)\n";
    OS << "        NearMisses->push_back(EarlyPredicateNearMiss);\n";
    OS << "      else if (NearMisses && LatePredicateNearMiss)\n";
    OS << "        NearMisses->push_back(LatePredicateNearMiss);\n";
    OS << "\n";
    OS << "      continue;\n";
    OS << "    } else if (NumNearMisses > 1) {\n";
    OS << "      // This instruction missed in more than one way, so ignore "
          "it.\n";
    OS << "      DEBUG_WITH_TYPE(\"asm-matcher\", dbgs() << \"Opcode result: "
          "multiple \"\n";
    OS << "                                               \"types of mismatch, "
          "so not \"\n";
    OS << "                                               \"reporting "
          "near-miss\\n\");\n";
    OS << "      continue;\n";
    OS << "    }\n";
  }

  // Call the post-processing function, if used.
  StringRef InsnCleanupFn = AsmParser->getValueAsString("AsmParserInstCleanup");
  if (!InsnCleanupFn.empty())
    OS << "    " << InsnCleanupFn << "(Inst);\n";

  if (HasDeprecation) {
    OS << "    std::string Info;\n";
    OS << "    if "
          "(!getParser().getTargetParser().getTargetOptions()."
          "MCNoDeprecatedWarn &&\n";
    OS << "        MII.getDeprecatedInfo(Inst, getSTI(), Info)) {\n";
    OS << "      SMLoc Loc = ((" << Target.getName()
       << "Operand &)*Operands[0]).getStartLoc();\n";
    OS << "      getParser().Warning(Loc, Info, std::nullopt);\n";
    OS << "    }\n";
  }

  if (!ReportMultipleNearMisses) {
    OS << "    if (!checkAsmTiedOperandConstraints(*this, it->ConvertFn, "
          "Operands, ErrorInfo))\n";
    OS << "      return Match_InvalidTiedOperand;\n";
    OS << "\n";
  }

  OS << "    DEBUG_WITH_TYPE(\n";
  OS << "        \"asm-matcher\",\n";
  OS << "        dbgs() << \"Opcode result: complete match, selecting this "
        "opcode\\n\");\n";
  OS << "    return Match_Success;\n";
  OS << "  }\n\n";

  if (ReportMultipleNearMisses) {
    OS << "  // No instruction variants matched exactly.\n";
    OS << "  return Match_NearMisses;\n";
  } else {
    OS << "  // Okay, we had no match.  Try to return a useful error code.\n";
    OS << "  if (HadMatchOtherThanPredicate || !HadMatchOtherThanFeatures)\n";
    OS << "    return RetCode;\n\n";
    OS << "  ErrorInfo = 0;\n";
    OS << "  return Match_MissingFeature;\n";
  }
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitMnemonicSpellChecker(
    CodeGenTarget const &Target, unsigned VariantCount) const {
  OS << "static std::string " << Target.getName()
     << "MnemonicSpellCheck(StringRef S, const FeatureBitset &FBS,"
     << " unsigned VariantID) {\n";
  if (!VariantCount)
    OS << "  return \"\";";
  else {
    OS << "  const unsigned MaxEditDist = 2;\n";
    OS << "  std::vector<StringRef> Candidates;\n";
    OS << "  StringRef Prev = \"\";\n\n";

    OS << "  // Find the appropriate table for this asm variant.\n";
    OS << "  const MatchEntry *Start, *End;\n";
    OS << "  switch (VariantID) {\n";
    OS << "  default: llvm_unreachable(\"invalid variant!\");\n";
    for (unsigned VC = 0; VC != VariantCount; ++VC) {
      Record *AsmVariant = Target.getAsmParserVariant(VC);
      int AsmVariantNo = AsmVariant->getValueAsInt("Variant");
      OS << "  case " << AsmVariantNo << ": Start = std::begin(MatchTable" << VC
         << "); End = std::end(MatchTable" << VC << "); break;\n";
    }
    OS << "  }\n\n";
    OS << "  for (auto I = Start; I < End; I++) {\n";
    OS << "    // Ignore unsupported instructions.\n";
    OS << "    const FeatureBitset &RequiredFeatures = "
          "FeatureBitsets[I->RequiredFeaturesIdx];\n";
    OS << "    if ((FBS & RequiredFeatures) != RequiredFeatures)\n";
    OS << "      continue;\n";
    OS << "\n";
    OS << "    StringRef T = I->getMnemonic();\n";
    OS << "    // Avoid recomputing the edit distance for the same string.\n";
    OS << "    if (T.equals(Prev))\n";
    OS << "      continue;\n";
    OS << "\n";
    OS << "    Prev = T;\n";
    OS << "    unsigned Dist = S.edit_distance(T, false, MaxEditDist);\n";
    OS << "    if (Dist <= MaxEditDist)\n";
    OS << "      Candidates.push_back(T);\n";
    OS << "  }\n";
    OS << "\n";
    OS << "  if (Candidates.empty())\n";
    OS << "    return \"\";\n";
    OS << "\n";
    OS << "  std::string Res = \", did you mean: \";\n";
    OS << "  unsigned i = 0;\n";
    OS << "  for (; i < Candidates.size() - 1; i++)\n";
    OS << "    Res += Candidates[i].str() + \", \";\n";
    OS << "  return Res + Candidates[i].str() + \"?\";\n";
  }
  OS << "}\n";
  OS << "\n";
}

void PrinterLLVM::asmMatcherEmitMnemonicChecker(CodeGenTarget const &Target,
                                                unsigned VariantCount,
                                                bool HasMnemonicFirst,
                                                bool HasMnemonicAliases) const {
  OS << "static bool " << Target.getName()
     << "CheckMnemonic(StringRef Mnemonic,\n";
  OS << "                                "
     << "const FeatureBitset &AvailableFeatures,\n";
  OS << "                                "
     << "unsigned VariantID) {\n";

  if (!VariantCount) {
    OS << "  return false;\n";
  } else {
    if (HasMnemonicAliases) {
      OS << "  // Process all MnemonicAliases to remap the mnemonic.\n";
      OS << "  applyMnemonicAliases(Mnemonic, AvailableFeatures, VariantID);";
      OS << "\n\n";
    }
    OS << "  // Find the appropriate table for this asm variant.\n";
    OS << "  const MatchEntry *Start, *End;\n";
    OS << "  switch (VariantID) {\n";
    OS << "  default: llvm_unreachable(\"invalid variant!\");\n";
    for (unsigned VC = 0; VC != VariantCount; ++VC) {
      Record *AsmVariant = Target.getAsmParserVariant(VC);
      int AsmVariantNo = AsmVariant->getValueAsInt("Variant");
      OS << "  case " << AsmVariantNo << ": Start = std::begin(MatchTable" << VC
         << "); End = std::end(MatchTable" << VC << "); break;\n";
    }
    OS << "  }\n\n";

    OS << "  // Search the table.\n";
    if (HasMnemonicFirst) {
      OS << "  auto MnemonicRange = "
            "std::equal_range(Start, End, Mnemonic, LessOpcode());\n\n";
    } else {
      OS << "  auto MnemonicRange = std::make_pair(Start, End);\n";
      OS << "  unsigned SIndex = Mnemonic.empty() ? 0 : 1;\n";
      OS << "  if (!Mnemonic.empty())\n";
      OS << "    MnemonicRange = "
         << "std::equal_range(Start, End, Mnemonic.lower(), LessOpcode());\n\n";
    }

    OS << "  if (MnemonicRange.first == MnemonicRange.second)\n";
    OS << "    return false;\n\n";

    OS << "  for (const MatchEntry *it = MnemonicRange.first, "
       << "*ie = MnemonicRange.second;\n";
    OS << "       it != ie; ++it) {\n";
    OS << "    const FeatureBitset &RequiredFeatures =\n";
    OS << "      FeatureBitsets[it->RequiredFeaturesIdx];\n";
    OS << "    if ((AvailableFeatures & RequiredFeatures) == ";
    OS << "RequiredFeatures)\n";
    OS << "      return true;\n";
    OS << "  }\n";
    OS << "  return false;\n";
  }
  OS << "}\n";
  OS << "\n";
}

void PrinterLLVM::asmMatcherEmitIncludes() const {
  OS << "#include \"llvm/Support/Debug.h\"\n";
  OS << "#include \"llvm/Support/Format.h\"\n\n";
}

void PrinterLLVM::asmMatcherEmitMnemonicTable(
    StringToOffsetTable &StringTable) const {
  OS << "static const char MnemonicTable[] =\n";
  StringTable.EmitString(OS);
  OS << ";\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchTable(CodeGenTarget const &Target,
                                           AsmMatcherInfo &Info,
                                           StringToOffsetTable &StringTable,
                                           unsigned VariantCount) const {
  for (unsigned VC = 0; VC != VariantCount; ++VC) {
    Record *AsmVariant = Target.getAsmParserVariant(VC);
    int AsmVariantNo = AsmVariant->getValueAsInt("Variant");

    OS << "static const MatchEntry MatchTable" << VC << "[] = {\n";

    for (const auto &MI : Info.Matchables) {
      if (MI->AsmVariantID != AsmVariantNo)
        continue;

      // Store a pascal-style length byte in the mnemonic.
      std::string LenMnemonic =
          char(MI->Mnemonic.size()) + MI->Mnemonic.lower();
      OS << "  { " << StringTable.GetOrAddStringOffset(LenMnemonic, false)
         << " /* " << MI->Mnemonic << " */, " << Target.getInstNamespace()
         << "::" << MI->getResultInst()->TheDef->getName() << ", "
         << MI->ConversionFnKind << ", ";

      // Write the required features mask.
      OS << "AMFBS";
      if (MI->RequiredFeatures.empty())
        OS << "_None";
      else
        for (unsigned I = 0, E = MI->RequiredFeatures.size(); I != E; ++I)
          OS << '_' << MI->RequiredFeatures[I]->TheDef->getName();

      OS << ", { ";
      ListSeparator LS;
      for (const MatchableInfo::AsmOperand &Op : MI->AsmOperands)
        OS << LS << Op.Class->Name;
      OS << " }, },\n";
    }

    OS << "};\n\n";
  }
}

void PrinterLLVM::asmMatcherEmitCustomOperandParsing(
    unsigned MaxMask, CodeGenTarget &Target, AsmMatcherInfo const &Info,
    StringRef ClassName, StringToOffsetTable &StringTable,
    unsigned MaxMnemonicIndex, unsigned MaxFeaturesIndex, bool HasMnemonicFirst,
    Record const &AsmParser) const {
  // Emit the static custom operand parsing table;
  emitNamespace("", true);
  OS << "  struct OperandMatchEntry {\n";
  OS << "    " << getMinimalTypeForRange(MaxMnemonicIndex) << " Mnemonic;\n";
  OS << "    " << getMinimalTypeForRange(MaxMask) << " OperandMask;\n";
  OS << "    "
     << getMinimalTypeForRange(
            std::distance(Info.Classes.begin(), Info.Classes.end()))
     << " Class;\n";
  OS << "    " << getMinimalTypeForRange(MaxFeaturesIndex)
     << " RequiredFeaturesIdx;\n\n";
  OS << "    StringRef getMnemonic() const {\n";
  OS << "      return StringRef(MnemonicTable + Mnemonic + 1,\n";
  OS << "                       MnemonicTable[Mnemonic]);\n";
  OS << "    }\n";
  OS << "  };\n\n";

  OS << "  // Predicate for searching for an opcode.\n";
  OS << "  struct LessOpcodeOperand {\n";
  OS << "    bool operator()(const OperandMatchEntry &LHS, StringRef RHS) {\n";
  OS << "      return LHS.getMnemonic()  < RHS;\n";
  OS << "    }\n";
  OS << "    bool operator()(StringRef LHS, const OperandMatchEntry &RHS) {\n";
  OS << "      return LHS < RHS.getMnemonic();\n";
  OS << "    }\n";
  OS << "    bool operator()(const OperandMatchEntry &LHS,";
  OS << " const OperandMatchEntry &RHS) {\n";
  OS << "      return LHS.getMnemonic() < RHS.getMnemonic();\n";
  OS << "    }\n";
  OS << "  };\n";

  emitNamespace("", false);

  OS << "static const OperandMatchEntry OperandMatchTable["
     << Info.OperandMatchInfo.size() << "] = {\n";

  OS << "  /* Operand List Mnemonic, Mask, Operand Class, Features */\n";
  for (const OperandMatchEntry &OMI : Info.OperandMatchInfo) {
    const MatchableInfo &II = *OMI.MI;

    OS << "  { ";

    // Store a pascal-style length byte in the mnemonic.
    std::string LenMnemonic = char(II.Mnemonic.size()) + II.Mnemonic.lower();
    OS << StringTable.GetOrAddStringOffset(LenMnemonic, false) << " /* "
       << II.Mnemonic << " */, ";

    OS << OMI.OperandMask;
    OS << " /* ";
    ListSeparator LS;
    for (int I = 0, E = 31; I != E; ++I)
      if (OMI.OperandMask & (1 << I))
        OS << LS << I;
    OS << " */, ";

    OS << OMI.CI->Name;

    // Write the required features mask.
    OS << ", AMFBS";
    if (II.RequiredFeatures.empty())
      OS << "_None";
    else
      for (unsigned I = 0, E = II.RequiredFeatures.size(); I != E; ++I)
        OS << '_' << II.RequiredFeatures[I]->TheDef->getName();

    OS << " },\n";
  }
  OS << "};\n\n";

  // Emit the operand class switch to call the correct custom parser for
  // the found operand class.
  OS << "OperandMatchResultTy " << Target.getName() << ClassName << "::\n"
     << "tryCustomParseOperand(OperandVector"
     << " &Operands,\n                      unsigned MCK) {\n\n"
     << "  switch(MCK) {\n";

  for (const auto &CI : Info.Classes) {
    if (CI.ParserMethod.empty())
      continue;
    OS << "  case " << CI.Name << ":\n"
       << "    return " << CI.ParserMethod << "(Operands);\n";
  }

  OS << "  default:\n";
  OS << "    return MatchOperand_NoMatch;\n";
  OS << "  }\n";
  OS << "  return MatchOperand_NoMatch;\n";
  OS << "}\n\n";

  // Emit the static custom operand parser. This code is very similar with
  // the other matcher. Also use MatchResultTy here just in case we go for
  // a better error handling.
  OS << "OperandMatchResultTy " << Target.getName() << ClassName << "::\n"
     << "MatchOperandParserImpl(OperandVector"
     << " &Operands,\n                       StringRef Mnemonic,\n"
     << "                       bool ParseForAllFeatures) {\n";

  // Emit code to get the available features.
  OS << "  // Get the current feature set.\n";
  OS << "  const FeatureBitset &AvailableFeatures = "
        "getAvailableFeatures();\n\n";

  OS << "  // Get the next operand index.\n";
  OS << "  unsigned NextOpNum = Operands.size()"
     << (HasMnemonicFirst ? " - 1" : "") << ";\n";

  // Emit code to search the table.
  OS << "  // Search the table.\n";
  if (HasMnemonicFirst) {
    OS << "  auto MnemonicRange =\n";
    OS << "    std::equal_range(std::begin(OperandMatchTable), "
          "std::end(OperandMatchTable),\n";
    OS << "                     Mnemonic, LessOpcodeOperand());\n\n";
  } else {
    OS << "  auto MnemonicRange = std::make_pair(std::begin(OperandMatchTable),"
          " std::end(OperandMatchTable));\n";
    OS << "  if (!Mnemonic.empty())\n";
    OS << "    MnemonicRange =\n";
    OS << "      std::equal_range(std::begin(OperandMatchTable), "
          "std::end(OperandMatchTable),\n";
    OS << "                       Mnemonic, LessOpcodeOperand());\n\n";
  }

  OS << "  if (MnemonicRange.first == MnemonicRange.second)\n";
  OS << "    return MatchOperand_NoMatch;\n\n";

  OS << "  for (const OperandMatchEntry *it = MnemonicRange.first,\n"
     << "       *ie = MnemonicRange.second; it != ie; ++it) {\n";

  OS << "    // equal_range guarantees that instruction mnemonic matches.\n";
  OS << "    assert(Mnemonic == it->getMnemonic());\n\n";

  // Emit check that the required features are available.
  OS << "    // check if the available features match\n";
  OS << "    const FeatureBitset &RequiredFeatures = "
        "FeatureBitsets[it->RequiredFeaturesIdx];\n";
  OS << "    if (!ParseForAllFeatures && (AvailableFeatures & "
        "RequiredFeatures) != RequiredFeatures)\n";
  OS << "      continue;\n\n";

  // Emit check to ensure the operand number matches.
  OS << "    // check if the operand in question has a custom parser.\n";
  OS << "    if (!(it->OperandMask & (1 << NextOpNum)))\n";
  OS << "      continue;\n\n";

  // Emit call to the custom parser method
  StringRef ParserName = AsmParser.getValueAsString("OperandParserMethod");
  if (ParserName.empty())
    ParserName = "tryCustomParseOperand";
  OS << "    // call custom parse method to handle the operand\n";
  OS << "    OperandMatchResultTy Result = " << ParserName
     << "(Operands, it->Class);\n";
  OS << "    if (Result != MatchOperand_NoMatch)\n";
  OS << "      return Result;\n";
  OS << "  }\n\n";

  OS << "  // Okay, we had no match.\n";
  OS << "  return MatchOperand_NoMatch;\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchRegisterName(
    Record const *AsmParser,
    std::vector<StringMatcher::StringPair> const Matches) const {
  OS << "static unsigned MatchRegisterName(StringRef Name) {\n";

  bool IgnoreDuplicates =
      AsmParser->getValueAsBit("AllowDuplicateRegisterNames");
  StringMatcher("Name", Matches, OS, PRINTER_LANG_CPP)
      .Emit(0, IgnoreDuplicates);

  OS << "  return 0;\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchRegisterAltName(
    Record const *AsmParser,
    std::vector<StringMatcher::StringPair> const Matches) const {
  OS << "static unsigned MatchRegisterAltName(StringRef Name) {\n";

  bool IgnoreDuplicates =
      AsmParser->getValueAsBit("AllowDuplicateRegisterNames");
  StringMatcher("Name", Matches, OS, PRINTER_LANG_CPP)
      .Emit(0, IgnoreDuplicates);

  OS << "  return 0;\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitMatchTokenString(
    std::vector<StringMatcher::StringPair> const Matches) const {
  OS << "static MatchClassKind matchTokenString(StringRef Name) {\n";

  StringMatcher("Name", Matches, OS, PRINTER_LANG_CPP).Emit();

  OS << "  return InvalidMatchClass;\n";
  OS << "}\n\n";
}

void PrinterLLVM::asmMatcherEmitMnemonicAliasVariant(
    std::vector<StringMatcher::StringPair> const &Cases,
    unsigned Indent) const {
  StringMatcher("Mnemonic", Cases, OS, PRINTER_LANG_CPP).Emit(Indent);
}

void PrinterLLVM::asmMatcherAppendMnemonicAlias(Record const *R,
                                                std::string const &FeatureMask,
                                                std::string &MatchCode) const {
  if (!MatchCode.empty())
    MatchCode += "else ";
  MatchCode += "if (" + FeatureMask + ")\n";
  MatchCode += "  Mnemonic = \"";
  MatchCode += R->getValueAsString("ToMnemonic").lower();
  MatchCode += "\";\n";
}

void PrinterLLVM::asmMatcherAppendMnemonic(Record const *R,
                                           std::string &MatchCode) const {
  if (!MatchCode.empty())
    MatchCode += "else\n  ";
  MatchCode += "Mnemonic = \"";
  MatchCode += R->getValueAsString("ToMnemonic").lower();
  MatchCode += "\";\n";
}

void PrinterLLVM::asmMatcherAppendMnemonicAliasEnd(
    std::string &MatchCode) const {
  MatchCode += "return;";
}

void PrinterLLVM::asmMatcherEmitApplyMnemonicAliasesI() const {
  OS << "static void applyMnemonicAliases(StringRef &Mnemonic, "
        "const FeatureBitset &Features, unsigned VariantID) {\n";
  OS << "  switch (VariantID) {\n";
}

void PrinterLLVM::asmMatcherEmitApplyMnemonicAliasesII(
    int AsmParserVariantNo) const {
  OS << "  case " << AsmParserVariantNo << ":\n";
}

void PrinterLLVM::asmMatcherEmitApplyMnemonicAliasesIII() const {
  OS << "    break;\n";
}

void PrinterLLVM::asmMatcherEmitApplyMnemonicAliasesIV() const {
  OS << "  }\n";
}

void PrinterLLVM::asmMatcherEmitApplyMnemonicAliasesV() const { OS << "}\n\n"; }

void PrinterLLVM::asmMatcherEmitSTFBitEnum(AsmMatcherInfo &Info) const {
  SubtargetFeatureInfo::emitSubtargetFeatureBitEnumeration(
      Info.SubtargetFeatures, OS);
}

void PrinterLLVM::asmMatcherEmitComputeAssemblerAvailableFeatures(
    AsmMatcherInfo &Info, StringRef const &ClassName) const {
  SubtargetFeatureInfo::emitComputeAssemblerAvailableFeatures(
      Info.Target.getName(), ClassName, "ComputeAvailableFeatures",
      Info.SubtargetFeatures, OS);
}

void PrinterLLVM::searchableTablesWriteFiles() const {}

void PrinterLLVM::searchableTablesEmitGenericEnum(
    const GenericEnum &Enum) const {
  OS << "enum " << Enum.Name << " {\n";
  for (const auto &Entry : Enum.Entries)
    OS << "  " << Entry->first << " = " << Entry->second << ",\n";
  OS << "};\n";
}

void PrinterLLVM::searchableTablesEmitGenericTable(
    const GenericTable &Enum) const {}

void PrinterLLVM::searchableTablesEmitIfdef(const std::string Guard,
                                            StreamType ST) const {
  OS << "#ifdef " << Guard << "\n";
}

void PrinterLLVM::searchableTablesEmitEndif(StreamType ST) const {
  OS << "#endif\n\n";
}

void PrinterLLVM::searchableTablesEmitUndef(std::string const &Guard) const {
  OS << "#undef " << Guard << "\n";
}

std::string PrinterLLVM::searchableTablesSearchableFieldType(
    const GenericTable &Table, const SearchIndex &Index,
    const GenericField &Field, TypeContext Ctx) const {
  if (isa<StringRecTy>(Field.RecType)) {
    if (Ctx == TypeInStaticStruct)
      return "const char *";
    if (Ctx == TypeInTempStruct)
      return "std::string";
    return "StringRef";
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

std::string PrinterLLVM::searchableTablesPrimaryRepresentation(
    SMLoc Loc, const GenericField &Field, Init *I,
    StringRef const &InstrinsicEnumName) const {
  if (StringInit *SI = dyn_cast<StringInit>(I)) {
    if (Field.IsCode || SI->hasCodeFormat())
      return std::string(SI->getValue());
    else
      return SI->getAsString();
  } else if (BitsInit *BI = dyn_cast<BitsInit>(I))
    return "0x" + utohexstr(getAsInt(BI));
  else if (BitInit *BI = dyn_cast<BitInit>(I))
    return BI->getValue() ? "true" : "false";
  else if (Field.IsIntrinsic)
    return "Intrinsic::" + InstrinsicEnumName.str();
  else if (Field.IsInstruction)
    return I->getAsString();
  else if (Field.Enum) {
    auto *Entry = Field.Enum->EntryMap[cast<DefInit>(I)->getDef()];
    if (!Entry)
      PrintFatalError(Loc,
                      Twine("Entry for field '") + Field.Name + "' is null");
    return std::string(Entry->first);
  }
  PrintFatalError(Loc, Twine("invalid field type for field '") + Field.Name +
                           "'; expected: bit, bits, string, or code");
}

void PrinterLLVM::searchableTablesEmitLookupDeclaration(
    const GenericTable &Table, const SearchIndex &Index, StreamType ST) {
  OS << "const " << Table.CppTypeName << " *" << Index.Name << "(";

  ListSeparator LS;
  for (const auto &Field : Index.Fields)
    OS << LS
       << searchableTablesSearchableFieldType(Table, Index, Field,
                                              TypeInArgument)
       << " " << Field.Name;
  OS << ")";
  if (ST == ST_DECL_OS)
    OS << ";\n";
  else if (ST == ST_IMPL_OS)
    OS << " {\n";
}

void PrinterLLVM::searchableTablesEmitIndexTypeStruct(
    const GenericTable &Table, const SearchIndex &Index) const {
  OS << "  struct IndexType {\n";
  for (const auto &Field : Index.Fields) {
    OS << "    "
       << searchableTablesSearchableFieldType(Table, Index, Field,
                                              TypeInStaticStruct)
       << " " << Field.Name << ";\n";
  }
  OS << "    unsigned _index;\n";
  OS << "  };\n";
}

void PrinterLLVM::searchableTablesEmitIndexArrayI() const {
  OS << "  static const struct IndexType Index[] = {\n";
}

void PrinterLLVM::searchableTablesEmitIndexArrayII() const { OS << "    { "; }

void PrinterLLVM::searchableTablesEmitIndexArrayIII(ListSeparator &LS,
                                                    std::string Repr) const {
  OS << LS << Repr;
}

void PrinterLLVM::searchableTablesEmitIndexArrayIV(
    std::pair<Record *, unsigned> const &Entry) const {
  OS << ", " << Entry.second << " },\n";
}
void PrinterLLVM::searchableTablesEmitIndexArrayV() const { OS << "  };\n\n"; }

void PrinterLLVM::searchableTablesEmitIsContiguousCase(
    StringRef const &IndexName, const GenericTable &Table,
    const SearchIndex &Index, bool IsPrimary) {
    OS << "  auto Table = ArrayRef(" << IndexName << ");\n";
    OS << "  size_t Idx = " << Index.Fields[0].Name << ";\n";
    OS << "  return Idx >= Table.size() ? nullptr : ";
    if (IsPrimary)
      OS << "&Table[Idx]";
    else
      OS << "&" << Table.Name << "[Table[Idx]._index]";
    OS << ";\n";
    OS << "}\n";
}

void PrinterLLVM::searchableTablesEmitIfFieldCase(
    const GenericField &Field, std::string const &FirstRepr,
    std::string const &LastRepr) const {
  OS << "  if ((" << Field.Name << " < " << FirstRepr << ") ||\n";
  OS << "      (" << Field.Name << " > " << LastRepr << "))\n";
  OS << "    return nullptr;\n\n";
}

void PrinterLLVM::searchableTablesEmitKeyTypeStruct(
    const GenericTable &Table, const SearchIndex &Index) const {
  OS << "  struct KeyType {\n";
  for (const auto &Field : Index.Fields) {
    OS << "    "
       << searchableTablesSearchableFieldType(Table, Index, Field,
                                              TypeInTempStruct)
       << " " << Field.Name << ";\n";
  }
  OS << "  };\n";
}

void PrinterLLVM::searchableTablesEmitKeyArray(const GenericTable &Table,
                                               const SearchIndex &Index,
                                               bool IsPrimary) const {
  OS << "  KeyType Key = {";
  ListSeparator LS;
  for (const auto &Field : Index.Fields) {
    OS << LS << Field.Name;
    if (isa<StringRecTy>(Field.RecType)) {
      OS << ".upper()";
      if (IsPrimary)
        PrintFatalError(Index.Loc,
                        Twine("In table '") + Table.Name +
                            "', use a secondary lookup method for "
                            "case-insensitive comparison of field '" +
                            Field.Name + "'");
    }
  }
  OS << "};\n";
}

void PrinterLLVM::searchableTablesEmitIndexLamda(
    const SearchIndex &Index, StringRef const &IndexName,
    StringRef const &IndexTypeName) const {
  OS << "  auto Table = ArrayRef(" << IndexName << ");\n";
  OS << "  auto Idx = std::lower_bound(Table.begin(), Table.end(), Key,\n";
  OS << "    [](const " << IndexTypeName << " &LHS, const KeyType &RHS) {\n";

  for (const auto &Field : Index.Fields) {
    if (isa<StringRecTy>(Field.RecType)) {
      OS << "      int Cmp" << Field.Name << " = StringRef(LHS." << Field.Name
         << ").compare(RHS." << Field.Name << ");\n";
      OS << "      if (Cmp" << Field.Name << " < 0) return true;\n";
      OS << "      if (Cmp" << Field.Name << " > 0) return false;\n";
    } else if (Field.Enum) {
      // Explicitly cast to unsigned, because the signedness of enums is
      // compiler-dependent.
      OS << "      if ((unsigned)LHS." << Field.Name << " < (unsigned)RHS."
         << Field.Name << ")\n";
      OS << "        return true;\n";
      OS << "      if ((unsigned)LHS." << Field.Name << " > (unsigned)RHS."
         << Field.Name << ")\n";
      OS << "        return false;\n";
    } else {
      OS << "      if (LHS." << Field.Name << " < RHS." << Field.Name << ")\n";
      OS << "        return true;\n";
      OS << "      if (LHS." << Field.Name << " > RHS." << Field.Name << ")\n";
      OS << "        return false;\n";
    }
  }
}

void PrinterLLVM::searchableTablesEmitReturns(const GenericTable &Table,
                                              const SearchIndex &Index,
                                              bool IsPrimary) {
  OS << "  if (Idx == Table.end()";

  for (const auto &Field : Index.Fields)
    OS << " ||\n      Key." << Field.Name << " != Idx->" << Field.Name;
  OS << ")\n    return nullptr;\n";

  if (IsPrimary)
    OS << "  return &*Idx;\n";
  else
    OS << "  return &" << Table.Name << "[Idx->_index];\n";

  OS << "}\n";
}

void PrinterLLVM::searchableTablesEmitMapI(const GenericTable &Table) const {
  OS << "constexpr " << Table.CppTypeName << " " << Table.Name << "[] = {\n";
}

void PrinterLLVM::searchableTablesEmitMapII() const { OS << "  { "; }

void PrinterLLVM::searchableTablesEmitMapIII(const GenericTable &Table,
                                             ListSeparator &LS,
                                             GenericField const &Field,
                                             StringRef &IntrinsicEnum,
                                             Record *Entry) const {
  OS << LS
     << searchableTablesPrimaryRepresentation(Table.Locs[0], Field,
                                              Entry->getValueInit(Field.Name),
                                              IntrinsicEnum);
}

void PrinterLLVM::searchableTablesEmitMapIV(unsigned i) const {
  OS << " }, // " << i << "\n";
}

void PrinterLLVM::searchableTablesEmitMapV() { OS << "  };\n\n"; }
} // end namespace llvm
