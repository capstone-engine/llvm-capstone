//===- InstrInfoEmitter.cpp - Generate a Instruction Set Desc. --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend is responsible for emitting a description of the target
// instruction set for the code generator.
//
//===----------------------------------------------------------------------===//

#include "InstrInfoEmitterTypes.h"
#include "Printer.h"

using namespace llvm;

cl::OptionCategory InstrInfoEmitterCat("Options for -gen-instr-info");
static cl::opt<bool> ExpandMIOperandInfo(
    "instr-info-expand-mi-operand-info",
    cl::desc("Expand operand's MIOperandInfo DAG into suboperands"),
    cl::cat(InstrInfoEmitterCat), cl::init(true));

namespace {

class InstrInfoEmitter {
  RecordKeeper &Records;
  PrinterLLVM &PI;
  CodeGenDAGPatterns CDP;
  const CodeGenSchedModels &SchedModels;

public:
  InstrInfoEmitter(RecordKeeper &R, PrinterLLVM &PI):
    Records(R), PI(PI), CDP(R), SchedModels(CDP.getTargetInfo().getSchedModels()) {}

  // run - Output the instruction set description.
  void run();

private:
  void emitEnums();

  /// Generate member functions in the target-specific GenInstrInfo class.
  ///
  /// This method is used to custom expand TIIPredicate definitions.
  /// See file llvm/Target/TargetInstPredicates.td for a description of what is
  /// a TIIPredicate and how to use it.
  void emitTIIHelperMethods(StringRef TargetName,
                            bool ExpandDefinition = true);

  /// Expand TIIPredicate definitions to functions that accept a const MCInst
  /// reference.
  void emitMCIIHelperMethods(StringRef TargetName);

  /// Write verifyInstructionPredicates methods.
  void emitFeatureVerifier(const CodeGenTarget &Target);
  void emitRecord(const CodeGenInstruction &Inst, unsigned Num,
                  Record *InstrInfo,
                  std::map<std::vector<Record*>, unsigned> &EL,
                  const OperandInfoMapTy &OpInf);
  void emitOperandTypeMappings(
      const CodeGenTarget &Target,
      ArrayRef<const CodeGenInstruction *> NumberedInstructions);
  void initOperandMapData(
            ArrayRef<const CodeGenInstruction *> NumberedInstructions,
            StringRef Namespace,
            std::map<std::string, unsigned> &Operands,
            OpNameMapTy &OperandMap);
  void emitOperandNameMappings(const CodeGenTarget &Target,
            ArrayRef<const CodeGenInstruction*> NumberedInstructions);

  void emitLogicalOperandSizeMappings(
      StringRef Namespace,
      ArrayRef<const CodeGenInstruction *> NumberedInstructions);
  void emitLogicalOperandTypeMappings(
      StringRef Namespace,
      ArrayRef<const CodeGenInstruction *> NumberedInstructions);

  // Operand information.
  void EmitOperandInfo(OperandInfoMapTy &OperandInfoIDs);
  std::vector<std::string> GetOperandInfo(const CodeGenInstruction &Inst);
};

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Operand Info Emission.
//===----------------------------------------------------------------------===//

std::vector<std::string>
InstrInfoEmitter::GetOperandInfo(const CodeGenInstruction &Inst) {
  std::vector<std::string> Result;

  for (auto &Op : Inst.Operands) {
    // Handle aggregate operands and normal operands the same way by expanding
    // either case into a list of operands for this op.
    std::vector<CGIOperandList::OperandInfo> OperandList;

    // This might be a multiple operand thing.  Targets like X86 have
    // registers in their multi-operand operands.  It may also be an anonymous
    // operand, which has a single operand, but no declared class for the
    // operand.
    DagInit *MIOI = Op.MIOperandInfo;

    if (!MIOI || MIOI->getNumArgs() == 0) {
      // Single, anonymous, operand.
      OperandList.push_back(Op);
    } else {
      for (unsigned j = 0, e = Op.MINumOperands; j != e; ++j) {
        OperandList.push_back(Op);

        auto *OpR = cast<DefInit>(MIOI->getArg(j))->getDef();
        OperandList.back().Rec = OpR;
      }
    }

    for (unsigned j = 0, e = OperandList.size(); j != e; ++j) {
      Record *OpR = OperandList[j].Rec;
      std::string Res;

      PI.instrInfoSetOperandInfoStr(Res, OpR, Op, Op.Constraints[j]);
      Result.push_back(Res);
    }
  }

  return Result;
}

void InstrInfoEmitter::EmitOperandInfo(
                                       OperandInfoMapTy &OperandInfoIDs) {
  // ID #0 is for no operand info.
  unsigned OperandListNum = 0;
  OperandInfoIDs[std::vector<std::string>()] = ++OperandListNum;

  PI.emitString("\n");
  const CodeGenTarget &Target = CDP.getTargetInfo();
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue()) {
    std::vector<std::string> OperandInfo = GetOperandInfo(*Inst);
    unsigned &N = OperandInfoIDs[OperandInfo];
    if (N != 0) continue;

    N = ++OperandListNum;
    PI.instrInfoEmitOperandInfoTable(OperandInfo, N);
  }
}

/// Initialize data structures for generating operand name mappings.
///
/// \param Operands [out] A map used to generate the OpName enum with operand
///        names as its keys and operand enum values as its values.
/// \param OperandMap [out] A map for representing the operand name mappings for
///        each instructions.  This is used to generate the OperandMap table as
///        well as the getNamedOperandIdx() function.
void InstrInfoEmitter::initOperandMapData(
        ArrayRef<const CodeGenInstruction *> NumberedInstructions,
        StringRef Namespace,
        std::map<std::string, unsigned> &Operands,
        OpNameMapTy &OperandMap) {
  unsigned NumOperands = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    if (!Inst->TheDef->getValueAsBit("UseNamedOperandTable"))
      continue;
    std::map<unsigned, unsigned> OpList;
    for (const auto &Info : Inst->Operands) {
      StrUintMapIter I = Operands.find(Info.Name);

      if (I == Operands.end()) {
        I = Operands.insert(Operands.begin(),
                    std::pair<std::string, unsigned>(Info.Name, NumOperands++));
      }
      OpList[I->second] = Info.MIOperandNo;
    }
    OperandMap[OpList].push_back(
            PI.instrInfoGetInstMapEntry(Namespace, Inst->TheDef->getName()));
  }
}

/// Generate a table and function for looking up the indices of operands by
/// name.
///
/// This code generates:
/// - An enum in the llvm::TargetNamespace::OpName namespace, with one entry
///   for each operand name.
/// - A 2-dimensional table called OperandMap for mapping OpName enum values to
///   operand indices.
/// - A function called getNamedOperandIdx(uint16_t Opcode, uint16_t NamedIdx)
///   for looking up the operand index for an instruction, given a value from
///   OpName enum
void InstrInfoEmitter::emitOperandNameMappings(
           const CodeGenTarget &Target,
           ArrayRef<const CodeGenInstruction*> NumberedInstructions) {
  StringRef Namespace = Target.getInstNamespace();
  std::string OpNameNS = "OpName";
  // Map of operand names to their enumeration value.  This will be used to
  // generate the OpName enum.
  std::map<std::string, unsigned> Operands;
  OpNameMapTy OperandMap;

  initOperandMapData(NumberedInstructions, Namespace, Operands, OperandMap);

  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_ENUM", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.emitNamespace(OpNameNS, true);
  PI.instrInfoEmitOperandEnum(Operands);
  PI.emitNamespace(OpNameNS, false);
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_ENUM", false);

  PI.emitIncludeToggle("GET_INSTRINFO_NAMED_OPS", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.instrInfoEmitGetNamedOperandIdx(Operands, OperandMap);
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_NAMED_OPS", false);
}

/// Generate an enum for all the operand types for this target, under the
/// llvm::TargetNamespace::OpTypes namespace.
/// Operand types are all definitions derived of the Operand Target.td class.
void InstrInfoEmitter::emitOperandTypeMappings(
    const CodeGenTarget &Target,
    ArrayRef<const CodeGenInstruction *> NumberedInstructions) {

  StringRef Namespace = Target.getInstNamespace();
  std::vector<Record *> Operands = Records.getAllDerivedDefinitions("Operand");
  std::vector<Record *> RegisterOperands =
      Records.getAllDerivedDefinitions("RegisterOperand");
  std::vector<Record *> RegisterClasses =
      Records.getAllDerivedDefinitions("RegisterClass");

  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_TYPES_ENUM", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.emitNamespace("OpTypes", true);
  PI.instrInfoEmitOpTypeEnumPartI();

  unsigned EnumVal = 0;
  for (const std::vector<Record *> *RecordsToAdd :
       {&Operands, &RegisterOperands, &RegisterClasses}) {
    for (const Record *Op : *RecordsToAdd) {
      if (!Op->isAnonymous())
        PI.instrInfoEmitOpTypeEnumPartII(Op->getName(), EnumVal);
      ++EnumVal;
    }
  }

  PI.instrInfoEmitOpTypeEnumPartIII();
  PI.emitNamespace("OpTypes", false);
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_TYPES_ENUM", false);

  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_TYPE", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.instrInfoEmitGetOpTypeHdr();

  // TODO: Factor out duplicate operand lists to compress the tables.
  if (!NumberedInstructions.empty()) {
    std::vector<int> OperandOffsets;
    std::vector<Record *> OperandRecords;
    int CurrentOffset = 0;
    for (const CodeGenInstruction *Inst : NumberedInstructions) {
      OperandOffsets.push_back(CurrentOffset);
      for (const auto &Op : Inst->Operands) {
        const DagInit *MIOI = Op.MIOperandInfo;
        if (!ExpandMIOperandInfo || !MIOI || MIOI->getNumArgs() == 0) {
          // Single, anonymous, operand.
          OperandRecords.push_back(Op.Rec);
          ++CurrentOffset;
        } else {
          for (Init *Arg : MIOI->getArgs()) {
            OperandRecords.push_back(cast<DefInit>(Arg)->getDef());
            ++CurrentOffset;
          }
        }
      }
    }

    // Emit the table of offsets (indexes) into the operand type table.
    // Size the unsigned integer offset to save space.
    assert(OperandRecords.size() <= UINT32_MAX &&
           "Too many operands for offset table");
    PI.instrInfoEmitOpTypeOffsetTable(OperandOffsets, OperandRecords.size(), NumberedInstructions);

    // Add an entry for the end so that we don't need to special case it below.
    OperandOffsets.push_back(OperandRecords.size());

    // Emit the actual operand types in a flat table.
    // Size the signed integer operand type to save space.
    assert(EnumVal <= INT16_MAX &&
           "Too many operand types for operand types table");
    PI.instrInfoEmitOpcodeOpTypesTable(EnumVal,
                                       OperandRecords,
                                       OperandOffsets,
                                       NumberedInstructions);
    PI.instrInfoEmitGetOpTypeReturn();
  } else {
    PI.instrInfoEmitGetOpTypeUnreachable();
  }
  PI.instrInfoEmitGetOpTypeEnd();
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_OPERAND_TYPE", false);

  PI.emitIncludeToggle("GET_INSTRINFO_MEM_OPERAND_SIZE", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.instrInfoEmitGetMemOpSizeHdr();
  std::map<int, std::vector<StringRef>> SizeToOperandName;
  for (const Record *Op : Operands) {
    if (!Op->isSubClassOf("X86MemOperand"))
      continue;
    if (int Size = Op->getValueAsInt("Size"))
      SizeToOperandName[Size].push_back(Op->getName());
  }
  PI.instrInfoEmitGetOpMemSizeTbl(SizeToOperandName);
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_MEM_OPERAND_SIZE", false);
}

void InstrInfoEmitter::emitLogicalOperandSizeMappings(
    StringRef Namespace,
    ArrayRef<const CodeGenInstruction *> NumberedInstructions) {
  std::map<std::vector<unsigned>, unsigned> LogicalOpSizeMap;

  std::map<unsigned, std::vector<std::string>> InstMap;

  size_t LogicalOpListSize = 0U;
  std::vector<unsigned> LogicalOpList;
  for (const auto *Inst : NumberedInstructions) {
    if (!Inst->TheDef->getValueAsBit("UseLogicalOperandMappings"))
      continue;

    LogicalOpList.clear();
    llvm::transform(Inst->Operands, std::back_inserter(LogicalOpList),
                    [](const CGIOperandList::OperandInfo &Op) -> unsigned {
                      auto *MIOI = Op.MIOperandInfo;
                      if (!MIOI || MIOI->getNumArgs() == 0)
                        return 1;
                      return MIOI->getNumArgs();
                    });
    LogicalOpListSize = std::max(LogicalOpList.size(), LogicalOpListSize);

    auto I =
        LogicalOpSizeMap.insert({LogicalOpList, LogicalOpSizeMap.size()}).first;
    InstMap[I->second].push_back(
            PI.instrInfoGetInstMapEntry(Namespace, Inst->TheDef->getName()));
  }

  PI.emitIncludeToggle("GET_INSTRINFO_LOGICAL_OPERAND_SIZE_MAP", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.instrInfoEmitGetLogicalOpSizeHdr();
  if (!InstMap.empty()) {
    std::vector<const std::vector<unsigned> *> LogicalOpSizeList(
        LogicalOpSizeMap.size());
    for (auto &P : LogicalOpSizeMap) {
      LogicalOpSizeList[P.second] = &P.first;
    }
    PI.instrInfoEmitGetLogicalOpSizeTable(LogicalOpListSize, LogicalOpSizeList);

    PI.instrInfoEmitGetLogicalOpSizeSwitch(InstMap);
  } else {
    PI.instrInfoEmitGetLogicalOpSizeReturn();
  }
  PI.instrInfoEmitGetLogicalOpSizeEnd();

  PI.instrInfoEmitGetLogicalOpIdx();

  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_LOGICAL_OPERAND_SIZE_MAP", false);
}

void InstrInfoEmitter::emitLogicalOperandTypeMappings(
    StringRef Namespace,
    ArrayRef<const CodeGenInstruction *> NumberedInstructions) {
  std::map<std::vector<std::string>, unsigned> LogicalOpTypeMap;

  std::map<unsigned, std::vector<std::string>> InstMap;

  size_t OpTypeListSize = 0U;
  std::vector<std::string> LogicalOpTypeList;
  for (const auto *Inst : NumberedInstructions) {
    if (!Inst->TheDef->getValueAsBit("UseLogicalOperandMappings"))
      continue;

    LogicalOpTypeList.clear();
    for (const auto &Op : Inst->Operands) {
      auto *OpR = Op.Rec;
      if ((OpR->isSubClassOf("Operand") ||
           OpR->isSubClassOf("RegisterOperand") ||
           OpR->isSubClassOf("RegisterClass")) &&
          !OpR->isAnonymous()) {
        LogicalOpTypeList.push_back(
            PI.instrInfoGetOpTypeListEntry(Namespace, Op.Rec->getName()));
      } else {
        LogicalOpTypeList.push_back("-1");
      }
    }
    OpTypeListSize = std::max(LogicalOpTypeList.size(), OpTypeListSize);

    auto I =
        LogicalOpTypeMap.insert({LogicalOpTypeList, LogicalOpTypeMap.size()})
            .first;
    InstMap[I->second].push_back(
        PI.instrInfoGetInstMapEntry(Namespace, Inst->TheDef->getName()));
  }

  PI.emitIncludeToggle("GET_INSTRINFO_LOGICAL_OPERAND_TYPE_MAP", true);
  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Namespace.str(), true);
  PI.instrInfoEmitGetLogicalOpTypeHdr();
  if (!InstMap.empty()) {
    std::vector<const std::vector<std::string> *> LogicalOpTypeList(
        LogicalOpTypeMap.size());
    for (auto &P : LogicalOpTypeMap) {
      LogicalOpTypeList[P.second] = &P.first;
    }
  PI.instrInfoEmitGetLogicalOpTypeTable(OpTypeListSize,
                                        LogicalOpTypeList);
  PI.instrInfoEmitGetLogicalOpTypeSwitch(InstMap);
  } else {
    PI.instrInfoEmitGetLogicalOpTypeReturn();
  }
  PI.instrInfoEmitGetLogicalOpTypeEnd();
  PI.emitNamespace(Namespace.str(), false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_INSTRINFO_LOGICAL_OPERAND_TYPE_MAP", false);
}

void InstrInfoEmitter::emitMCIIHelperMethods(
                                             StringRef TargetName) {
  RecVec TIIPredicates = Records.getAllDerivedDefinitions("TIIPredicate");

  PI.emitIncludeToggle("GET_INSTRINFO_MC_HELPER_DECLS", true);
  PI.emitNamespace("llvm", true);
  PI.instrInfoEmitDeclareMCInstFeatureClasses();

  PI.emitNamespace(TargetName.str() + "_MC", true);

  PI.instrInfoEmitPredFcnDecl(TIIPredicates);

  PI.emitNamespace(TargetName.str() + "_MC", false);
  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_INSTRINFO_MC_HELPER_DECLS", false);

  PI.emitIncludeToggle("GET_INSTRINFO_MC_HELPERS", true);

  PI.emitNamespace("llvm", true);
  PI.emitNamespace(TargetName.str() + "_MC", true);

  PI.instrInfoEmitPredFcnImpl(TargetName, TIIPredicates);

  PI.emitNamespace(TargetName.str() + "_MC", false);
  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_INSTRINFO_MC_HELPERS", false);
}

void InstrInfoEmitter::emitFeatureVerifier(
                                           const CodeGenTarget &Target) {
  const auto &All = SubtargetFeatureInfo::getAll(Records);
  std::map<Record *, SubtargetFeatureInfo, LessRecordByID> SubtargetFeatures;
  SubtargetFeatures.insert(All.begin(), All.end());

  PI.emitIncludeToggle("ENABLE_INSTR_PREDICATE_VERIFIER", true);
  PI.instrInfoEmitInstrPredVerifierIncludes();

  PI.emitNamespace("llvm", true);
  PI.emitNamespace(Target.getName().str() + "_MC", true);

  // Emit the subtarget feature enumeration.
  PI.instrInfoEmitSubtargetFeatureBitEnumeration(SubtargetFeatures);

  // Emit the name table for error messages.
  PI.instrInfoEmitEmitSTFNameTable(SubtargetFeatures);

  // Emit the available features compute function.
  PI.instrInfoEmitComputeAssemblerAvailableFeatures(Target.getName(),
                                                    SubtargetFeatures);

  std::vector<std::vector<Record *>> FeatureBitsets;
  for (const CodeGenInstruction *Inst : Target.getInstructionsByEnumValue()) {
    FeatureBitsets.emplace_back();
    for (Record *Predicate : Inst->TheDef->getValueAsListOfDefs("Predicates")) {
      const auto &I = SubtargetFeatures.find(Predicate);
      if (I != SubtargetFeatures.end())
        FeatureBitsets.back().push_back(I->second.TheDef);
    }
  }

  llvm::sort(FeatureBitsets, [&](const std::vector<Record *> &A,
                                 const std::vector<Record *> &B) {
    if (A.size() < B.size())
      return true;
    if (A.size() > B.size())
      return false;
    for (auto Pair : zip(A, B)) {
      if (std::get<0>(Pair)->getName() < std::get<1>(Pair)->getName())
        return true;
      if (std::get<0>(Pair)->getName() > std::get<1>(Pair)->getName())
        return false;
    }
    return false;
  });
  FeatureBitsets.erase(
      std::unique(FeatureBitsets.begin(), FeatureBitsets.end()),
      FeatureBitsets.end());
  PI.emitIfNotDef("NDEBUG", true);
  PI.instrInfoEmitFeatureBitsEnum(FeatureBitsets);
  PI.instrInfoEmitFeatureBitsArray(FeatureBitsets, SubtargetFeatures);
  PI.emitIfNotDef("NDEBUG", false);

  // Emit the predicate verifier.
  PI.instrInfoEmitPredVerifier(FeatureBitsets, SubtargetFeatures, Target);
  PI.emitNamespace(Target.getName().str() + "_MC", false);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("ENABLE_INSTR_PREDICATE_VERIFIER", false);
}

void InstrInfoEmitter::emitTIIHelperMethods(
                                            StringRef TargetName,
                                            bool ExpandDefinition) {
  RecVec TIIPredicates = Records.getAllDerivedDefinitions("TIIPredicate");
  if (TIIPredicates.empty())
    return;

  PI.instrInfoEmitTIIPredicates(TargetName,
                                TIIPredicates,
                                ExpandDefinition);
}

//===----------------------------------------------------------------------===//
// Main Output.
//===----------------------------------------------------------------------===//

// run - Emit the main instruction description records for the target...
void InstrInfoEmitter::run() {
  PI.instrInfoEmitSourceFileHeader();
  emitEnums();

  PI.emitIncludeToggle("GET_INSTRINFO_MC_DESC", true);
  PI.emitNamespace("llvm", true);

  CodeGenTarget &Target = CDP.getTargetInfo();
  const std::string &TargetName = std::string(Target.getName());
  Record *InstrInfo = Target.getInstructionSet();

  // Keep track of all of the def lists we have emitted already.
  std::map<std::vector<Record*>, unsigned> EmittedLists;
  unsigned ListNumber = 0;

  // Emit all of the instruction's implicit uses and defs.
  Records.startTimer("Emit uses/defs");
  for (const CodeGenInstruction *II : Target.getInstructionsByEnumValue()) {
    std::vector<Record *> ImplicitOps = II->ImplicitUses;
    llvm::append_range(ImplicitOps, II->ImplicitDefs);
    if (!ImplicitOps.empty()) {
      unsigned &IL = EmittedLists[ImplicitOps];
      if (!IL) {
        IL = ++ListNumber;
        PI.instrInfoPrintDefList(ImplicitOps, IL, getQualifiedName);
      }
    }
  }

  OperandInfoMapTy OperandInfoIDs;

  // Emit all of the operand info records.
  Records.startTimer("Emit operand info");
  EmitOperandInfo(OperandInfoIDs);

  // Emit all of the MCInstrDesc records in ascending ENUM ordering.
  Records.startTimer("Emit InstrDesc records");
  PI.instrInfoEmitMCInstrDescHdr(TargetName);
  ArrayRef<const CodeGenInstruction*> NumberedInstructions =
    Target.getInstructionsByEnumValue();

  SequenceToOffsetTable<std::string> InstrNames;
  // CAPSTONE: Do not reverse NumberedInstructions.
  // We access them via the opcode. They must ascend for this.
  unsigned Num = 0;
  for (const CodeGenInstruction *Inst : NumberedInstructions) {
    // Keep a list of the instruction names.
    InstrNames.add(std::string(Inst->TheDef->getName()));
    // Emit the record into the table.
    emitRecord(*Inst, Num++, InstrInfo, EmittedLists, OperandInfoIDs);
  }
  PI.instrInfoEmitMCInstrDescEnd();
  // Emit the array of instruction names.
  Records.startTimer("Emit instruction names");
  InstrNames.layout();
  PI.instrInfoEmitStringLiteralDef(TargetName, InstrNames);

  PI.instrInfoEmitInstrNameIndices(TargetName, NumberedInstructions, InstrNames);

  bool HasDeprecationFeatures =
      llvm::any_of(NumberedInstructions, [](const CodeGenInstruction *Inst) {
        return !Inst->HasComplexDeprecationPredicate &&
               !Inst->DeprecatedReason.empty();
      });
  if (HasDeprecationFeatures) {
    PI.instrInfoEmitInstrDeprFeatures(TargetName,
                                      Target.getInstNamespace().str(),
                                      NumberedInstructions,
                                      InstrNames);
  }

  bool HasComplexDeprecationInfos =
      llvm::any_of(NumberedInstructions, [](const CodeGenInstruction *Inst) {
        return Inst->HasComplexDeprecationPredicate;
      });
  if (HasComplexDeprecationInfos) {
      PI.instrInfoEmitInstrComplexDeprInfos(TargetName,
                                            NumberedInstructions);
  }

  // MCInstrInfo initialization routine.
  Records.startTimer("Emit initialization routine");
  PI.instrInfoEmitMCInstrInfoInitRoutine(TargetName,
                                         NumberedInstructions.size(),
                                         HasDeprecationFeatures,
                                         HasComplexDeprecationInfos);

  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_INSTRINFO_MC_DESC", false);

  // Create a TargetInstrInfo subclass to hide the MC layer initialization.
  PI.emitIncludeToggle("GET_INSTRINFO_HEADER", true);

  std::string ClassName = TargetName + "GenInstrInfo";
  PI.emitNamespace("llvm", true);
  PI.instrInfoEmitClassStruct(ClassName);
  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_INSTRINFO_HEADER", false);

  PI.emitIncludeToggle("GET_INSTRINFO_HELPER_DECLS", true);
  emitTIIHelperMethods(TargetName, /* ExpandDefinition = */ false);
  PI.emitString("\n");
  PI.emitIncludeToggle("GET_INSTRINFO_HELPER_DECLS", false);

  PI.emitIncludeToggle("GET_INSTRINFO_HELPERS", true);
  emitTIIHelperMethods(TargetName, /* ExpandDefinition = */ true);
  PI.emitIncludeToggle("GET_INSTRINFO_HELPERS", false);

  PI.emitIncludeToggle("GET_INSTRINFO_CTOR_DTOR", true);

  PI.emitNamespace("llvm", true);
  PI.instrInfoEmitExternArrays(TargetName,
                               HasDeprecationFeatures,
                               HasComplexDeprecationInfos);
  PI.instrInfoEmitMCInstrInfoInit(TargetName,
                                  ClassName,
                                  NumberedInstructions.size(),
                                  HasDeprecationFeatures,
                                  HasComplexDeprecationInfos);
  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_INSTRINFO_CTOR_DTOR", false);

  Records.startTimer("Emit operand name mappings");
  emitOperandNameMappings(Target, NumberedInstructions);

  Records.startTimer("Emit operand type mappings");
  emitOperandTypeMappings(Target, NumberedInstructions);

  Records.startTimer("Emit logical operand size mappings");
  emitLogicalOperandSizeMappings(TargetName, NumberedInstructions);

  Records.startTimer("Emit logical operand type mappings");
  emitLogicalOperandTypeMappings(TargetName, NumberedInstructions);

  Records.startTimer("Emit helper methods");
  emitMCIIHelperMethods(TargetName);

  Records.startTimer("Emit verifier methods");
  emitFeatureVerifier(Target);
}

void InstrInfoEmitter::emitRecord(const CodeGenInstruction &Inst, unsigned Num,
                                  Record *InstrInfo,
                         std::map<std::vector<Record*>, unsigned> &EmittedLists,
                                  const OperandInfoMapTy &OpInfo) {
  int MinOperands = 0;
  if (!Inst.Operands.empty())
    // Each logical operand can be multiple MI operands.
    MinOperands = Inst.Operands.back().MIOperandNo +
                  Inst.Operands.back().MINumOperands;

  PI.instrInfoEmitRecord(SchedModels, Inst, Num, MinOperands);

  CodeGenTarget &Target = CDP.getTargetInfo();

  // Emit all of the target independent flags...
  PI.instrInfoEmitTargetIndepFlags(Inst, Target.getAllowRegisterRenaming());

  // Emit all of the target-specific flags...
  BitsInit *TSF = Inst.TheDef->getValueAsBitsInit("TSFlags");
  if (!TSF)
    PrintFatalError(Inst.TheDef->getLoc(), "no TSFlags?");
  uint64_t Value = 0;
  for (unsigned i = 0, e = TSF->getNumBits(); i != e; ++i) {
    if (const auto *Bit = dyn_cast<BitInit>(TSF->getBit(i)))
      Value |= uint64_t(Bit->getValue()) << i;
    else
      PrintFatalError(Inst.TheDef->getLoc(),
                      "Invalid TSFlags bit in " + Inst.TheDef->getName());
  }
  PI.instrInfoEmitTSFFlags(Value);

  // Emit the implicit use/def list...
  std::vector<Record *> ImplicitOps = Inst.ImplicitUses;
  llvm::append_range(ImplicitOps, Inst.ImplicitDefs);
  PI.instrInfoEmitUseDefsLists(EmittedLists, ImplicitOps);

  // Emit the operand info.
  std::vector<std::string> OperandInfo = GetOperandInfo(Inst);
  PI.instrInfoEmitOperandInfo(OperandInfo, OpInfo);

  PI.instrInfoEmitRecordEnd(Num, Inst.TheDef->getName().str());
}

// emitEnums - Print out enum values for all of the instructions.
void InstrInfoEmitter::emitEnums() {
  const CodeGenTarget &Target = CDP.getTargetInfo();

  // We must emit the PHI opcode first...
  StringRef Namespace = Target.getInstNamespace();
  if (Namespace.empty())
    PrintFatalError("No instructions defined!");
  PI.instrInfoEmitEnums(Target, Namespace, SchedModels);
}

namespace llvm {

void EmitInstrInfo(RecordKeeper &RK, raw_ostream &OS) {
  formatted_raw_ostream FOS(OS);
  PrinterLLVM *PI;
  PrinterLanguage const PL = PrinterLLVM::getLanguage();

  if (PL == PRINTER_LANG_CPP) {
    PI = new PrinterLLVM(FOS);
  } else if (PL == PRINTER_LANG_CAPSTONE_C) {
    PI = new PrinterCapstone(FOS);
  } else {
    llvm_unreachable("InstrInfoEmitter does not support the given output language.");
  }

  RK.startTimer("Analyze DAG patterns");
  InstrInfoEmitter(RK, *PI).run();
  RK.startTimer("Emit map table");
  EmitMapTable(RK, OS);
  delete PI;
}

} // end namespace llvm
