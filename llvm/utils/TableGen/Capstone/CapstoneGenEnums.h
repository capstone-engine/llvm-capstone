// Enums Generation Module
// TODO use `typedef enum`s instead of macros

#ifndef LLVM_UTILS_TABLEGEN_CAPSTONEGENENUMS_H
#define LLVM_UTILS_TABLEGEN_CAPSTONEGENENUMS_H

#include "CapstoneGenInfo.h"
#include "CodeGenInstruction.h"
#include "llvm/TableGen/Error.h"

const unsigned MAX_SUBTARGET_WORDS = 4;
const unsigned MAX_SUBTARGET_FEATURES = MAX_SUBTARGET_WORDS * 64;

void CapstoneGenInfo::Enumeration(
    raw_ostream &OS, DenseMap<Record *, unsigned> &FeatureMap) const {
  // Get all records of class and sort
  std::vector<Record *> DefList =
      this->RK.getAllDerivedDefinitions("SubtargetFeature");
  llvm::sort(DefList, LessRecord());

  unsigned N = DefList.size();
  if (N == 0)
    return;
  if (N + 1 > MAX_SUBTARGET_FEATURES)
    PrintFatalError(
        "Too many subtarget features! Bump MAX_SUBTARGET_FEATURES.");

  // For each record
  for (unsigned i = 0; i < N; ++i) {
    // Next record
    Record *Def = DefList[i];

    // Get and emit name
    OS << "#define " << this->Target.getName() << "_" << Def->getName() << " "
       << i << "ULL\n";

    // Save the index for this feature.
    FeatureMap[Def] = i;
  }
}

void CapstoneGenInfo::runEnums(raw_ostream &OS, CodeGenTarget &Target,
                               CodeGenRegBank &Bank) {
  const auto &Registers = Bank.getRegisters();

  // Register enums are stored as uint16_t in the tables. Make sure we'll fit.
  assert(Registers.size() <= 0xffff && "Too many regs to fit in tables");

  StringRef Namespace = Registers.front().TheDef->getValueAsString("Namespace");

  OS << "\n#ifdef GET_REGINFO_ENUM\n";
  OS << "#undef GET_REGINFO_ENUM\n\n";

  for (const auto &Reg : Registers)
    OS << "#define " << Namespace << "_" << Reg.getName() << " "
       << Reg.EnumValue << "\n";
  assert(Registers.size() == Registers.back().EnumValue &&
         "Register enum value mismatch!");
  OS << "#define " << Namespace << "_"
     << "NUM_TARGET_REGS " << Registers.size() + 1 << "\n";
  OS << "\n";

  const auto &RegisterClasses = Bank.getRegClasses();
  if (!RegisterClasses.empty()) {

    // RegisterClass enums are stored as uint16_t in the tables.
    assert(RegisterClasses.size() <= 0xffff &&
           "Too many register classes to fit in tables");

    OS << "\n// Register classes\n\n";
    for (const auto &RC : RegisterClasses)
      OS << "#define " << Namespace << "_" << RC.getName() << "RegClassID"
         << " " << RC.EnumValue << "\n";
    OS << "\n";
  }
  OS << "#endif // GET_REGINFO_ENUM\n\n";

  // instruction info
  OS << "#ifdef GET_INSTRINFO_ENUM\n";
  OS << "#undef GET_INSTRINFO_ENUM\n";

  if (Namespace.empty())
    PrintFatalError("No instructions defined!");

  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue())
    OS << "#define " << Namespace << "_" << Inst->TheDef->getName() << "\t "
       << Num++ << "\n";

  OS << "#endif // GET_INSTRINFO_ENUM\n\n";

  OS << "#ifdef GET_REGINFO_EXTRA\n";
  OS << "#undef GET_REGINFO_EXTRA\n";

  const std::vector<Record *> &RegAltNameIndices =
      Target.getRegAltNameIndices();
  // If the only definition is the default NoRegAltName, we don't need to
  // emit anything.
  if (RegAltNameIndices.size() > 1) {
    OS << "\n// Register alternate name indices\n\n";
    OS << "enum {\n";
    for (unsigned i = 0, e = RegAltNameIndices.size(); i != e; ++i)
      OS << "  " << Namespace << "_" << RegAltNameIndices[i]->getName()
         << ",\t// " << i << "\n  ";
    OS << Namespace << "_"
       << "NUM_TARGET_REG_ALT_NAMES = " << RegAltNameIndices.size() << "\n";
    OS << "};\n";
  }

  auto &SubRegIndices = Bank.getSubRegIndices();
  if (!SubRegIndices.empty()) {
    OS << "\n// Subregister indices\n\n";
    OS << "enum {\n  NoSubRegister,\n";
    unsigned i = 0;
    for (const auto &Idx : SubRegIndices)
      OS << "  " << Namespace << "_" << Idx.getName() << ",\t// " << ++i
         << "\n  ";
    OS << Namespace << "_"
       << "NUM_TARGET_SUBREGS\n};\n";
  }

  OS << "#endif // GET_REGINFO_EXTRA\n\n";
}

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEGENENUMS_H
