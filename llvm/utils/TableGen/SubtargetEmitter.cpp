//===- SubtargetEmitter.cpp - Generate subtarget enumerations -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits subtarget enumerations.
//
//===----------------------------------------------------------------------===//

#include "Printer.h"
#include "PrinterTypes.h"
#include "SubtargetEmitterTypes.h"

using namespace llvm;

#define DEBUG_TYPE "subtarget-emitter"

namespace {

class SubtargetEmitter {

  SchedClassTablesT SchedClassTables;

  struct LessWriteProcResources {
    bool operator()(const MCWriteProcResEntry &LHS,
                    const MCWriteProcResEntry &RHS) {
      return LHS.ProcResourceIdx < RHS.ProcResourceIdx;
    }
  };

  const CodeGenTarget &TGT;
  RecordKeeper &Records;
  CodeGenSchedModels &SchedModels;
  std::string Target;
  PrinterLLVM &PI;

  void Enumeration(DenseMap<Record *, unsigned> &FeatureMap);
  void EmitSubtargetInfoMacroCalls();
  unsigned FeatureKeyValues(
                            const DenseMap<Record *, unsigned> &FeatureMap);
  unsigned CPUKeyValues(
                        const DenseMap<Record *, unsigned> &FeatureMap);
  void EmitStageAndOperandCycleData(
                                    std::vector<std::vector<InstrItinerary>>
                                      &ProcItinLists);
  void EmitItineraries(
                       std::vector<std::vector<InstrItinerary>>
                         &ProcItinLists);
  void EmitLoadStoreQueueInfo(const CodeGenProcModel &ProcModel);
  void EmitExtraProcessorInfo(const CodeGenProcModel &ProcModel);
  void EmitProcessorResources(const CodeGenProcModel &ProcModel);
  Record *FindWriteResources(const CodeGenSchedRW &SchedWrite,
                             const CodeGenProcModel &ProcModel);
  Record *FindReadAdvance(const CodeGenSchedRW &SchedRead,
                          const CodeGenProcModel &ProcModel);
  void ExpandProcResources(RecVec &PRVec, std::vector<int64_t> &Cycles,
                           const CodeGenProcModel &ProcModel);
  void GenSchedClassTables(const CodeGenProcModel &ProcModel,
                           SchedClassTablesT &SchedTables);
  void EmitSchedClassTables(SchedClassTablesT &SchedTables);
  void EmitProcessorModels();
  void EmitSchedModelHelpers(const std::string &ClassName);
  void emitSchedModelHelpersImpl(bool OnlyExpandMCInstPredicates = false);
  void emitGenMCSubtargetInfo();
  void EmitMCInstrAnalysisPredicateFunctions();

  void EmitSchedModel();
  void EmitHwModeCheck(const std::string &ClassName);
  void ParseFeaturesFunction();

public:
  SubtargetEmitter(RecordKeeper &R, CodeGenTarget &TGT, PrinterLLVM &PI)
      : TGT(TGT), Records(R), SchedModels(TGT.getSchedModels()),
        Target(TGT.getName()), PI(PI) {}

  void run();
};

} // end anonymous namespace

//
// Enumeration - Emit the specified class as an enumeration.
//
void SubtargetEmitter::Enumeration(
                                   DenseMap<Record *, unsigned> &FeatureMap) {
  // Get all records of class and sort
  std::vector<Record*> DefList =
    Records.getAllDerivedDefinitions("SubtargetFeature");
  llvm::sort(DefList, LessRecord());

  unsigned N = DefList.size();
  if (N == 0)
    return;
  if (N + 1 > MAX_SUBTARGET_FEATURES)
    PrintFatalError("Too many subtarget features! Bump MAX_SUBTARGET_FEATURES.");

  PI.emitNamespace(Target, true);
  PI.subtargetEmitFeatureEnum(FeatureMap, DefList, N);
  PI.emitNamespace(Target, false);
}

static void printFeatureMask(PrinterLLVM const &PI, RecVec &FeatureList,
                             const DenseMap<Record *, unsigned> &FeatureMap) {
  std::array<uint64_t, MAX_SUBTARGET_WORDS> Mask = {};
  for (const Record *Feature : FeatureList) {
    unsigned Bit = FeatureMap.lookup(Feature);
    Mask[Bit / 64] |= 1ULL << (Bit % 64);
  }
  PI.subtargetEmitPrintFeatureMask(Mask);
}

/// Emit some information about the SubtargetFeature as calls to a macro so
/// that they can be used from C++.
void SubtargetEmitter::EmitSubtargetInfoMacroCalls() {
  PI.emitIncludeToggle("GET_SUBTARGETINFO_MACRO", true, true, true);

  std::vector<Record *> FeatureList =
      Records.getAllDerivedDefinitions("SubtargetFeature");
  llvm::sort(FeatureList, LessRecordFieldName());

  for (const Record *Feature : FeatureList) {
    const StringRef Attribute = Feature->getValueAsString("Attribute");
    const StringRef Value = Feature->getValueAsString("Value");

    // Only handle boolean features for now, excluding BitVectors and enums.
    const bool IsBool = (Value == "false" || Value == "true") &&
                        !StringRef(Attribute).contains('[');
    if (!IsBool)
      continue;

    PI.subtargetEmitGetSTIMacro(Value, Attribute);
  }
  PI.emitIncludeToggle("GET_SUBTARGETINFO_MACRO", false, true, true);
}

//
// FeatureKeyValues - Emit data of all the subtarget features.  Used by the
// command line.
//
unsigned SubtargetEmitter::FeatureKeyValues(
    const DenseMap<Record *, unsigned> &FeatureMap) {
  // Gather and sort all the features
  std::vector<Record*> FeatureList =
                           Records.getAllDerivedDefinitions("SubtargetFeature");

  if (FeatureList.empty())
    return 0;

  llvm::sort(FeatureList, LessRecordFieldName());

  // Begin feature table
  PI.subtargetEmitFeatureKVHeader(Target);

  // For each feature
  unsigned NumFeatures = 0;
  for (const Record *Feature : FeatureList) {
    // Next feature
    StringRef Name = Feature->getName();
    StringRef CommandLineName = Feature->getValueAsString("Name");
    StringRef Desc = Feature->getValueAsString("Desc");

    if (CommandLineName.empty()) continue;

    PI.subtargetEmitFeatureKVPartI(Target, CommandLineName, Name, Desc);

    RecVec ImpliesList = Feature->getValueAsListOfDefs("Implies");

    printFeatureMask(PI, ImpliesList, FeatureMap);

    PI.subtargetEmitFeatureKVPartII();

    ++NumFeatures;
  }

  // End feature table
  PI.subtargetEmitFeatureKVEnd();

  return NumFeatures;
}

//
// CPUKeyValues - Emit data of all the subtarget processors.  Used by command
// line.
//
unsigned
SubtargetEmitter::CPUKeyValues(
                               const DenseMap<Record *, unsigned> &FeatureMap) {
  // Gather and sort processor information
  std::vector<Record*> ProcessorList =
                          Records.getAllDerivedDefinitions("Processor");
  llvm::sort(ProcessorList, LessRecordFieldName());

  // Begin processor table
  PI.subtargetEmitCPUKVHeader(Target);

  // For each processor
  for (Record *Processor : ProcessorList) {
    StringRef Name = Processor->getValueAsString("Name");
    RecVec FeatureList = Processor->getValueAsListOfDefs("Features");
    RecVec TuneFeatureList = Processor->getValueAsListOfDefs("TuneFeatures");

    PI.subtargetEmitCPUKVPartI(Name);

    printFeatureMask(PI, FeatureList, FeatureMap);
    PI.subtargetEmitCPUKVPartII();
    printFeatureMask(PI, TuneFeatureList, FeatureMap);

    // Emit the scheduler model pointer.
    const std::string &ProcModelName =
      SchedModels.getModelForProc(Processor).ModelName;
    PI.subtargetEmitCPUKVPartIII(ProcModelName);
  }

  // End processor table
  PI.subtargetEmitCPUKVEnd();
  return ProcessorList.size();
}

//
// EmitStageAndOperandCycleData - Generate unique itinerary stages and operand
// cycle tables. Create a list of InstrItinerary objects (ProcItinLists) indexed
// by CodeGenSchedClass::Index.
//
void SubtargetEmitter::
EmitStageAndOperandCycleData(std::vector<std::vector<InstrItinerary>>
                             &ProcItinLists) {

  PI.subtargetEmitFunctionalItinaryUnits(SchedModels);

  // Begin stages table
  std::string StageTable = PI.subtargetGetBeginStageTable(Target);

  // Begin operand cycle table
  std::string OperandCycleTable = PI.subtargetGetBeginOperandCycleTable(Target);

  // Begin pipeline bypass table
  std::string BypassTable = PI.subtargetGetBeginBypassTable(Target);

  // For each Itinerary across all processors, add a unique entry to the stages,
  // operand cycles, and pipeline bypass tables. Then add the new Itinerary
  // object with computed offsets to the ProcItinLists result.
  unsigned StageCount = 1, OperandCycleCount = 1;
  std::map<std::string, unsigned> ItinStageMap, ItinOperandMap;
  for (const CodeGenProcModel &ProcModel : SchedModels.procModels()) {
    // Add process itinerary to the list.
    ProcItinLists.resize(ProcItinLists.size()+1);

    // If this processor defines no itineraries, then leave the itinerary list
    // empty.
    std::vector<InstrItinerary> &ItinList = ProcItinLists.back();
    if (!ProcModel.hasItineraries())
      continue;

    StringRef Name = ProcModel.ItinsDef->getName();

    ItinList.resize(SchedModels.numInstrSchedClasses());
    assert(ProcModel.ItinDefList.size() == ItinList.size() && "bad Itins");

    for (unsigned SchedClassIdx = 0, SchedClassEnd = ItinList.size();
         SchedClassIdx < SchedClassEnd; ++SchedClassIdx) {

      // Next itinerary data
      Record *ItinData = ProcModel.ItinDefList[SchedClassIdx];

      // Get string and stage count
      std::string ItinStageString;
      unsigned NStages = 0;
      if (ItinData)
        PI.subtargetFormItineraryStageString(std::string(Name),
                                             ItinData,
                                             ItinStageString,
                                             NStages);

      // Get string and operand cycle count
      std::string ItinOperandCycleString;
      unsigned NOperandCycles = 0;
      std::string ItinBypassString;
      if (ItinData) {
        PI.subtargetFormItineraryOperandCycleString(ItinData,
                                                    ItinOperandCycleString,
                                                    NOperandCycles);

        PI.subtargetFormItineraryBypassString(std::string(Name),
                                              ItinData,
                                              ItinBypassString,
                                              NOperandCycles);
      }

      // Check to see if stage already exists and create if it doesn't
      uint16_t FindStage = 0;
      if (NStages > 0) {
        FindStage = ItinStageMap[ItinStageString];
        if (FindStage == 0) {
          // Emit as { cycles, u1 | u2 | ... | un, timeinc }, // indices
          StageTable += PI.subtargetGetStageEntryPartI(ItinStageString, StageCount);
          if (NStages > 1)
            StageTable += PI.subtargetGetStageEntryPartII(StageCount, NStages);
          StageTable += PI.subtargetGetStageEntryPartIII();
          // Record Itin class number.
          ItinStageMap[ItinStageString] = FindStage = StageCount;
          StageCount += NStages;
        }
      }

      // Check to see if operand cycle already exists and create if it doesn't
      uint16_t FindOperandCycle = 0;
      if (NOperandCycles > 0) {
        std::string ItinOperandString = ItinOperandCycleString+ItinBypassString;
        FindOperandCycle = ItinOperandMap[ItinOperandString];
        if (FindOperandCycle == 0) {
          // Emit as  cycle, // index
          OperandCycleTable += PI.subtargetGetOperandCycleEntryPartI(
              ItinOperandCycleString);
          std::string OperandIdxComment = itostr(OperandCycleCount);
          if (NOperandCycles > 1)
            OperandIdxComment += PI.subtargetGetOperandCycleEntryPartII(
                OperandCycleCount, NOperandCycles);
          OperandCycleTable += PI.subtargetGetOperandCycleEntryPartIII(
              OperandIdxComment);
          // Record Itin class number.
          ItinOperandMap[ItinOperandCycleString] =
            FindOperandCycle = OperandCycleCount;
          // Emit as bypass, // index
          BypassTable += PI.subtargetGetOperandCycleEntryPartIV(
              ItinBypassString, OperandIdxComment);
          OperandCycleCount += NOperandCycles;
        }
      }

      // Set up itinerary as location and location + stage count
      int16_t NumUOps = ItinData ? ItinData->getValueAsInt("NumMicroOps") : 0;
      InstrItinerary Intinerary = {
          NumUOps,
          FindStage,
          uint16_t(FindStage + NStages),
          FindOperandCycle,
          uint16_t(FindOperandCycle + NOperandCycles),
      };

      // Inject - empty slots will be 0, 0
      ItinList[SchedClassIdx] = Intinerary;
    }
  }

  // Closing stage
  StageTable += PI.subtargetGetEndStageTable();

  // Closing operand cycles
  OperandCycleTable += PI.subtargetGetEndOperandCycleTable();

  BypassTable += PI.subtargetGetEndBypassTable();

  // Emit tables.
  PI.subtargetEmitStageAndSycleTables(StageTable, OperandCycleTable, BypassTable);
}

//
// EmitProcessorData - Generate data for processor itineraries that were
// computed during EmitStageAndOperandCycleData(). ProcItinLists lists all
// Itineraries for each processor. The Itinerary lists are indexed on
// CodeGenSchedClass::Index.
//
void SubtargetEmitter::
EmitItineraries(
                std::vector<std::vector<InstrItinerary>> &ProcItinLists) {
  // Multiple processor models may share an itinerary record. Emit it once.
  SmallPtrSet<Record*, 8> ItinsDefSet;

  // For each processor's machine model
  std::vector<std::vector<InstrItinerary>>::iterator
      ProcItinListsIter = ProcItinLists.begin();
  for (CodeGenSchedModels::ProcIter PIM = SchedModels.procModelBegin(),
         PE = SchedModels.procModelEnd(); PIM != PE; ++PIM, ++ProcItinListsIter) {

    Record *ItinsDef = PIM->ItinsDef;
    if (!ItinsDefSet.insert(ItinsDef).second)
      continue;

    // Get the itinerary list for the processor.
    assert(ProcItinListsIter != ProcItinLists.end() && "bad iterator");
    std::vector<InstrItinerary> &ItinList = *ProcItinListsIter;

    // Empty itineraries aren't referenced anywhere in the tablegen output
    // so don't emit them.
    if (ItinList.empty())
      continue;

    PI.subtargetEmitProcessorItineraryTable(ItinsDef->getName().str(),
                                            ItinList,
                                            SchedModels);
  }
}

static void EmitRetireControlUnitInfo(const CodeGenProcModel &ProcModel,
                                      PrinterLLVM &PI) {
  int64_t ReorderBufferSize = 0, MaxRetirePerCycle = 0;
  if (Record *RCU = ProcModel.RetireControlUnit) {
    ReorderBufferSize =
        std::max(ReorderBufferSize, RCU->getValueAsInt("ReorderBufferSize"));
    MaxRetirePerCycle =
        std::max(MaxRetirePerCycle, RCU->getValueAsInt("MaxRetirePerCycle"));
  }

  PI.subtargetEmitReorderBufferSize(ReorderBufferSize);
  PI.subtargetEmitMaxRetirePerCycle(MaxRetirePerCycle);
}

void SubtargetEmitter::EmitLoadStoreQueueInfo(const CodeGenProcModel &ProcModel) {
  unsigned QueueID = 0;
  if (ProcModel.LoadQueue) {
    const Record *Queue = ProcModel.LoadQueue->getValueAsDef("QueueDescriptor");
    QueueID = 1 + std::distance(ProcModel.ProcResourceDefs.begin(),
                                find(ProcModel.ProcResourceDefs, Queue));
  }
  PI.subtargetEmitResourceDescriptorLoadQueue(QueueID);

  QueueID = 0;
  if (ProcModel.StoreQueue) {
    const Record *Queue =
        ProcModel.StoreQueue->getValueAsDef("QueueDescriptor");
    QueueID = 1 + std::distance(ProcModel.ProcResourceDefs.begin(),
                                find(ProcModel.ProcResourceDefs, Queue));
  }
  PI.subtargetEmitResourceDescriptorStoreQueue(QueueID);
}

void SubtargetEmitter::EmitExtraProcessorInfo(const CodeGenProcModel &ProcModel) {
  // Generate a table of register file descriptors (one entry per each user
  // defined register file), and a table of register costs.
  unsigned NumCostEntries;
  if (llvm::all_of(ProcModel.RegisterFiles, [](const CodeGenRegisterFile &RF) {
        return RF.hasDefaultCosts();
      }))
    NumCostEntries = 0;
  else
    NumCostEntries = PI.subtargetEmitRegisterFileTables(ProcModel);

  // Now generate a table for the extra processor info.
  PI.subtargetEmitMCExtraProcInfoTableHeader(ProcModel.ModelName);

  // Add information related to the retire control unit.
  EmitRetireControlUnitInfo(ProcModel, PI);

  // Add information related to the register files (i.e. where to find register
  // file descriptors and register costs).
  PI.subtargetEmitRegisterFileInfo(ProcModel, ProcModel.RegisterFiles.size(),
                                   NumCostEntries);

  // Add information about load/store queues.
  EmitLoadStoreQueueInfo(ProcModel);
  PI.subtargetEmitMCExtraProcInfoTableEnd();
}

void SubtargetEmitter::EmitProcessorResources(const CodeGenProcModel &ProcModel) {
  PI.subtargetEmitProcessorResourceSubUnits(ProcModel, SchedModels);

  PI.subtargetEmitMCProcResourceDescHeader(ProcModel.ModelName);

  unsigned SubUnitsOffset = 1;
  for (unsigned i = 0, e = ProcModel.ProcResourceDefs.size(); i < e; ++i) {
    Record *PRDef = ProcModel.ProcResourceDefs[i];

    Record *SuperDef = nullptr;
    unsigned SuperIdx = 0;
    unsigned NumUnits = 0;
    const unsigned SubUnitsBeginOffset = SubUnitsOffset;
    int BufferSize = PRDef->getValueAsInt("BufferSize");
    if (PRDef->isSubClassOf("ProcResGroup")) {
      RecVec ResUnits = PRDef->getValueAsListOfDefs("Resources");
      for (Record *RU : ResUnits) {
        NumUnits += RU->getValueAsInt("NumUnits");
        SubUnitsOffset += RU->getValueAsInt("NumUnits");
      }
    }
    else {
      // Find the SuperIdx
      if (PRDef->getValueInit("Super")->isComplete()) {
        SuperDef =
            SchedModels.findProcResUnits(PRDef->getValueAsDef("Super"),
                                         ProcModel, PRDef->getLoc());
        SuperIdx = ProcModel.getProcResourceIdx(SuperDef);
      }
      NumUnits = PRDef->getValueAsInt("NumUnits");
    }
    PI.subtargetEmitMCProcResourceDesc(PRDef,
                                       SuperDef,
                                       ProcModel.ModelName,
                                       SubUnitsOffset,
                                       SuperIdx,
                                       NumUnits,
                                       BufferSize,
                                       i, SubUnitsBeginOffset);
  }
  PI.subtargetEmitMCProcResourceDescEnd();
}

// Find the WriteRes Record that defines processor resources for this
// SchedWrite.
Record *SubtargetEmitter::FindWriteResources(
  const CodeGenSchedRW &SchedWrite, const CodeGenProcModel &ProcModel) {

  // Check if the SchedWrite is already subtarget-specific and directly
  // specifies a set of processor resources.
  if (SchedWrite.TheDef->isSubClassOf("SchedWriteRes"))
    return SchedWrite.TheDef;

  Record *AliasDef = nullptr;
  for (Record *A : SchedWrite.Aliases) {
    const CodeGenSchedRW &AliasRW =
      SchedModels.getSchedRW(A->getValueAsDef("AliasRW"));
    if (AliasRW.TheDef->getValueInit("SchedModel")->isComplete()) {
      Record *ModelDef = AliasRW.TheDef->getValueAsDef("SchedModel");
      if (&SchedModels.getProcModel(ModelDef) != &ProcModel)
        continue;
    }
    if (AliasDef)
      PrintFatalError(AliasRW.TheDef->getLoc(), "Multiple aliases "
                    "defined for processor " + ProcModel.ModelName +
                    " Ensure only one SchedAlias exists per RW.");
    AliasDef = AliasRW.TheDef;
  }
  if (AliasDef && AliasDef->isSubClassOf("SchedWriteRes"))
    return AliasDef;

  // Check this processor's list of write resources.
  Record *ResDef = nullptr;
  for (Record *WR : ProcModel.WriteResDefs) {
    if (!WR->isSubClassOf("WriteRes"))
      continue;
    if (AliasDef == WR->getValueAsDef("WriteType")
        || SchedWrite.TheDef == WR->getValueAsDef("WriteType")) {
      if (ResDef) {
        PrintFatalError(WR->getLoc(), "Resources are defined for both "
                      "SchedWrite and its alias on processor " +
                      ProcModel.ModelName);
      }
      ResDef = WR;
    }
  }
  // TODO: If ProcModel has a base model (previous generation processor),
  // then call FindWriteResources recursively with that model here.
  if (!ResDef) {
    PrintFatalError(ProcModel.ModelDef->getLoc(),
                    Twine("Processor does not define resources for ") +
                    SchedWrite.TheDef->getName());
  }
  return ResDef;
}

/// Find the ReadAdvance record for the given SchedRead on this processor or
/// return NULL.
Record *SubtargetEmitter::FindReadAdvance(const CodeGenSchedRW &SchedRead,
                                          const CodeGenProcModel &ProcModel) {
  // Check for SchedReads that directly specify a ReadAdvance.
  if (SchedRead.TheDef->isSubClassOf("SchedReadAdvance"))
    return SchedRead.TheDef;

  // Check this processor's list of aliases for SchedRead.
  Record *AliasDef = nullptr;
  for (Record *A : SchedRead.Aliases) {
    const CodeGenSchedRW &AliasRW =
      SchedModels.getSchedRW(A->getValueAsDef("AliasRW"));
    if (AliasRW.TheDef->getValueInit("SchedModel")->isComplete()) {
      Record *ModelDef = AliasRW.TheDef->getValueAsDef("SchedModel");
      if (&SchedModels.getProcModel(ModelDef) != &ProcModel)
        continue;
    }
    if (AliasDef)
      PrintFatalError(AliasRW.TheDef->getLoc(), "Multiple aliases "
                    "defined for processor " + ProcModel.ModelName +
                    " Ensure only one SchedAlias exists per RW.");
    AliasDef = AliasRW.TheDef;
  }
  if (AliasDef && AliasDef->isSubClassOf("SchedReadAdvance"))
    return AliasDef;

  // Check this processor's ReadAdvanceList.
  Record *ResDef = nullptr;
  for (Record *RA : ProcModel.ReadAdvanceDefs) {
    if (!RA->isSubClassOf("ReadAdvance"))
      continue;
    if (AliasDef == RA->getValueAsDef("ReadType")
        || SchedRead.TheDef == RA->getValueAsDef("ReadType")) {
      if (ResDef) {
        PrintFatalError(RA->getLoc(), "Resources are defined for both "
                      "SchedRead and its alias on processor " +
                      ProcModel.ModelName);
      }
      ResDef = RA;
    }
  }
  // TODO: If ProcModel has a base model (previous generation processor),
  // then call FindReadAdvance recursively with that model here.
  if (!ResDef && SchedRead.TheDef->getName() != "ReadDefault") {
    PrintFatalError(ProcModel.ModelDef->getLoc(),
                    Twine("Processor does not define resources for ") +
                    SchedRead.TheDef->getName());
  }
  return ResDef;
}

// Expand an explicit list of processor resources into a full list of implied
// resource groups and super resources that cover them.
void SubtargetEmitter::ExpandProcResources(RecVec &PRVec,
                                           std::vector<int64_t> &Cycles,
                                           const CodeGenProcModel &PM) {
  assert(PRVec.size() == Cycles.size() && "failed precondition");
  for (unsigned i = 0, e = PRVec.size(); i != e; ++i) {
    Record *PRDef = PRVec[i];
    RecVec SubResources;
    if (PRDef->isSubClassOf("ProcResGroup"))
      SubResources = PRDef->getValueAsListOfDefs("Resources");
    else {
      SubResources.push_back(PRDef);
      PRDef = SchedModels.findProcResUnits(PRDef, PM, PRDef->getLoc());
      for (Record *SubDef = PRDef;
           SubDef->getValueInit("Super")->isComplete();) {
        if (SubDef->isSubClassOf("ProcResGroup")) {
          // Disallow this for simplicitly.
          PrintFatalError(SubDef->getLoc(), "Processor resource group "
                          " cannot be a super resources.");
        }
        Record *SuperDef =
            SchedModels.findProcResUnits(SubDef->getValueAsDef("Super"), PM,
                                         SubDef->getLoc());
        PRVec.push_back(SuperDef);
        Cycles.push_back(Cycles[i]);
        SubDef = SuperDef;
      }
    }
    for (Record *PR : PM.ProcResourceDefs) {
      if (PR == PRDef || !PR->isSubClassOf("ProcResGroup"))
        continue;
      RecVec SuperResources = PR->getValueAsListOfDefs("Resources");
      RecIter SubI = SubResources.begin(), SubE = SubResources.end();
      for( ; SubI != SubE; ++SubI) {
        if (!is_contained(SuperResources, *SubI)) {
          break;
        }
      }
      if (SubI == SubE) {
        PRVec.push_back(PR);
        Cycles.push_back(Cycles[i]);
      }
    }
  }
}

// Generate the SchedClass table for this processor and update global
// tables. Must be called for each processor in order.
void SubtargetEmitter::GenSchedClassTables(const CodeGenProcModel &ProcModel,
                                           SchedClassTablesT &SchedTables) {
  SchedTables.ProcSchedClasses.resize(SchedTables.ProcSchedClasses.size() + 1);
  if (!ProcModel.hasInstrSchedModel())
    return;

  std::vector<MCSchedClassDesc> &SCTab = SchedTables.ProcSchedClasses.back();
  LLVM_DEBUG(dbgs() << "\n+++ SCHED CLASSES (GenSchedClassTables) +++\n");
  for (const CodeGenSchedClass &SC : SchedModels.schedClasses()) {
    LLVM_DEBUG(SC.dump(&SchedModels));

    SCTab.resize(SCTab.size() + 1);
    MCSchedClassDesc &SCDesc = SCTab.back();
    // SCDesc.Name is guarded by NDEBUG
    SCDesc.NumMicroOps = 0;
    SCDesc.BeginGroup = false;
    SCDesc.EndGroup = false;
    SCDesc.RetireOOO = false;
    SCDesc.WriteProcResIdx = 0;
    SCDesc.WriteLatencyIdx = 0;
    SCDesc.ReadAdvanceIdx = 0;

    // A Variant SchedClass has no resources of its own.
    bool HasVariants = false;
    for (const CodeGenSchedTransition &CGT :
           make_range(SC.Transitions.begin(), SC.Transitions.end())) {
      if (CGT.ProcIndex == ProcModel.Index) {
        HasVariants = true;
        break;
      }
    }
    if (HasVariants) {
      SCDesc.NumMicroOps = MCSchedClassDesc::VariantNumMicroOps;
      continue;
    }

    // Determine if the SchedClass is actually reachable on this processor. If
    // not don't try to locate the processor resources, it will fail.
    // If ProcIndices contains 0, this class applies to all processors.
    assert(!SC.ProcIndices.empty() && "expect at least one procidx");
    if (SC.ProcIndices[0] != 0) {
      if (!is_contained(SC.ProcIndices, ProcModel.Index))
        continue;
    }
    IdxVec Writes = SC.Writes;
    IdxVec Reads = SC.Reads;
    if (!SC.InstRWs.empty()) {
      // This class has a default ReadWrite list which can be overridden by
      // InstRW definitions.
      Record *RWDef = nullptr;
      for (Record *RW : SC.InstRWs) {
        Record *RWModelDef = RW->getValueAsDef("SchedModel");
        if (&ProcModel == &SchedModels.getProcModel(RWModelDef)) {
          RWDef = RW;
          break;
        }
      }
      if (RWDef) {
        Writes.clear();
        Reads.clear();
        SchedModels.findRWs(RWDef->getValueAsListOfDefs("OperandReadWrites"),
                            Writes, Reads);
      }
    }
    if (Writes.empty()) {
      // Check this processor's itinerary class resources.
      for (Record *I : ProcModel.ItinRWDefs) {
        RecVec Matched = I->getValueAsListOfDefs("MatchedItinClasses");
        if (is_contained(Matched, SC.ItinClassDef)) {
          SchedModels.findRWs(I->getValueAsListOfDefs("OperandReadWrites"),
                              Writes, Reads);
          break;
        }
      }
      if (Writes.empty()) {
        LLVM_DEBUG(dbgs() << ProcModel.ModelName
                          << " does not have resources for class " << SC.Name
                          << '\n');
        SCDesc.NumMicroOps = MCSchedClassDesc::InvalidNumMicroOps;
      }
    }
    // Sum resources across all operand writes.
    std::vector<MCWriteProcResEntry> WriteProcResources;
    std::vector<MCWriteLatencyEntry> WriteLatencies;
    std::vector<std::string> WriterNames;
    std::vector<MCReadAdvanceEntry> ReadAdvanceEntries;
    for (unsigned W : Writes) {
      IdxVec WriteSeq;
      SchedModels.expandRWSeqForProc(W, WriteSeq, /*IsRead=*/false,
                                     ProcModel);

      // For each operand, create a latency entry.
      MCWriteLatencyEntry WLEntry;
      WLEntry.Cycles = 0;
      unsigned WriteID = WriteSeq.back();
      WriterNames.push_back(SchedModels.getSchedWrite(WriteID).Name);
      // If this Write is not referenced by a ReadAdvance, don't distinguish it
      // from other WriteLatency entries.
      if (!SchedModels.hasReadOfWrite(
            SchedModels.getSchedWrite(WriteID).TheDef)) {
        WriteID = 0;
      }
      WLEntry.WriteResourceID = WriteID;

      for (unsigned WS : WriteSeq) {

        Record *WriteRes =
          FindWriteResources(SchedModels.getSchedWrite(WS), ProcModel);

        // Mark the parent class as invalid for unsupported write types.
        if (WriteRes->getValueAsBit("Unsupported")) {
          SCDesc.NumMicroOps = MCSchedClassDesc::InvalidNumMicroOps;
          break;
        }
        WLEntry.Cycles += WriteRes->getValueAsInt("Latency");
        SCDesc.NumMicroOps += WriteRes->getValueAsInt("NumMicroOps");
        SCDesc.BeginGroup |= WriteRes->getValueAsBit("BeginGroup");
        SCDesc.EndGroup |= WriteRes->getValueAsBit("EndGroup");
        SCDesc.BeginGroup |= WriteRes->getValueAsBit("SingleIssue");
        SCDesc.EndGroup |= WriteRes->getValueAsBit("SingleIssue");
        SCDesc.RetireOOO |= WriteRes->getValueAsBit("RetireOOO");

        // Create an entry for each ProcResource listed in WriteRes.
        RecVec PRVec = WriteRes->getValueAsListOfDefs("ProcResources");
        std::vector<int64_t> Cycles =
          WriteRes->getValueAsListOfInts("ResourceCycles");

        if (Cycles.empty()) {
          // If ResourceCycles is not provided, default to one cycle per
          // resource.
          Cycles.resize(PRVec.size(), 1);
        } else if (Cycles.size() != PRVec.size()) {
          // If ResourceCycles is provided, check consistency.
          PrintFatalError(
              WriteRes->getLoc(),
              Twine("Inconsistent resource cycles: !size(ResourceCycles) != "
                    "!size(ProcResources): ")
                  .concat(Twine(PRVec.size()))
                  .concat(" vs ")
                  .concat(Twine(Cycles.size())));
        }

        ExpandProcResources(PRVec, Cycles, ProcModel);

        for (unsigned PRIdx = 0, PREnd = PRVec.size();
             PRIdx != PREnd; ++PRIdx) {
          MCWriteProcResEntry WPREntry;
          WPREntry.ProcResourceIdx = ProcModel.getProcResourceIdx(PRVec[PRIdx]);
          assert(WPREntry.ProcResourceIdx && "Bad ProcResourceIdx");
          WPREntry.Cycles = Cycles[PRIdx];
          // If this resource is already used in this sequence, add the current
          // entry's cycles so that the same resource appears to be used
          // serially, rather than multiple parallel uses. This is important for
          // in-order machine where the resource consumption is a hazard.
          unsigned WPRIdx = 0, WPREnd = WriteProcResources.size();
          for( ; WPRIdx != WPREnd; ++WPRIdx) {
            if (WriteProcResources[WPRIdx].ProcResourceIdx
                == WPREntry.ProcResourceIdx) {
              WriteProcResources[WPRIdx].Cycles += WPREntry.Cycles;
              break;
            }
          }
          if (WPRIdx == WPREnd)
            WriteProcResources.push_back(WPREntry);
        }
      }
      WriteLatencies.push_back(WLEntry);
    }
    // Create an entry for each operand Read in this SchedClass.
    // Entries must be sorted first by UseIdx then by WriteResourceID.
    for (unsigned UseIdx = 0, EndIdx = Reads.size();
         UseIdx != EndIdx; ++UseIdx) {
      Record *ReadAdvance =
        FindReadAdvance(SchedModels.getSchedRead(Reads[UseIdx]), ProcModel);
      if (!ReadAdvance)
        continue;

      // Mark the parent class as invalid for unsupported write types.
      if (ReadAdvance->getValueAsBit("Unsupported")) {
        SCDesc.NumMicroOps = MCSchedClassDesc::InvalidNumMicroOps;
        break;
      }
      RecVec ValidWrites = ReadAdvance->getValueAsListOfDefs("ValidWrites");
      IdxVec WriteIDs;
      if (ValidWrites.empty())
        WriteIDs.push_back(0);
      else {
        for (Record *VW : ValidWrites) {
          WriteIDs.push_back(SchedModels.getSchedRWIdx(VW, /*IsRead=*/false));
        }
      }
      llvm::sort(WriteIDs);
      for(unsigned W : WriteIDs) {
        MCReadAdvanceEntry RAEntry;
        RAEntry.UseIdx = UseIdx;
        RAEntry.WriteResourceID = W;
        RAEntry.Cycles = ReadAdvance->getValueAsInt("Cycles");
        ReadAdvanceEntries.push_back(RAEntry);
      }
    }
    if (SCDesc.NumMicroOps == MCSchedClassDesc::InvalidNumMicroOps) {
      WriteProcResources.clear();
      WriteLatencies.clear();
      ReadAdvanceEntries.clear();
    }
    // Add the information for this SchedClass to the global tables using basic
    // compression.
    //
    // WritePrecRes entries are sorted by ProcResIdx.
    llvm::sort(WriteProcResources, LessWriteProcResources());

    SCDesc.NumWriteProcResEntries = WriteProcResources.size();
    std::vector<MCWriteProcResEntry>::iterator WPRPos =
      std::search(SchedTables.WriteProcResources.begin(),
                  SchedTables.WriteProcResources.end(),
                  WriteProcResources.begin(), WriteProcResources.end());
    if (WPRPos != SchedTables.WriteProcResources.end())
      SCDesc.WriteProcResIdx = WPRPos - SchedTables.WriteProcResources.begin();
    else {
      SCDesc.WriteProcResIdx = SchedTables.WriteProcResources.size();
      SchedTables.WriteProcResources.insert(WPRPos, WriteProcResources.begin(),
                                            WriteProcResources.end());
    }
    // Latency entries must remain in operand order.
    SCDesc.NumWriteLatencyEntries = WriteLatencies.size();
    std::vector<MCWriteLatencyEntry>::iterator WLPos =
      std::search(SchedTables.WriteLatencies.begin(),
                  SchedTables.WriteLatencies.end(),
                  WriteLatencies.begin(), WriteLatencies.end());
    if (WLPos != SchedTables.WriteLatencies.end()) {
      unsigned idx = WLPos - SchedTables.WriteLatencies.begin();
      SCDesc.WriteLatencyIdx = idx;
      for (unsigned i = 0, e = WriteLatencies.size(); i < e; ++i)
        if (SchedTables.WriterNames[idx + i].find(WriterNames[i]) ==
            std::string::npos) {
          SchedTables.WriterNames[idx + i] += std::string("_") + WriterNames[i];
        }
    }
    else {
      SCDesc.WriteLatencyIdx = SchedTables.WriteLatencies.size();
      llvm::append_range(SchedTables.WriteLatencies, WriteLatencies);
      llvm::append_range(SchedTables.WriterNames, WriterNames);
    }
    // ReadAdvanceEntries must remain in operand order.
    SCDesc.NumReadAdvanceEntries = ReadAdvanceEntries.size();
    std::vector<MCReadAdvanceEntry>::iterator RAPos =
      std::search(SchedTables.ReadAdvanceEntries.begin(),
                  SchedTables.ReadAdvanceEntries.end(),
                  ReadAdvanceEntries.begin(), ReadAdvanceEntries.end());
    if (RAPos != SchedTables.ReadAdvanceEntries.end())
      SCDesc.ReadAdvanceIdx = RAPos - SchedTables.ReadAdvanceEntries.begin();
    else {
      SCDesc.ReadAdvanceIdx = SchedTables.ReadAdvanceEntries.size();
      llvm::append_range(SchedTables.ReadAdvanceEntries, ReadAdvanceEntries);
    }
  }
}

void SubtargetEmitter::EmitProcessorModels() {
  // For each processor model.
  for (const CodeGenProcModel &PM : SchedModels.procModels()) {
    // Emit extra processor info if available.
    if (PM.hasExtraProcessorInfo())
      EmitExtraProcessorInfo(PM);
    // Emit processor resource table.
    if (PM.hasInstrSchedModel())
      EmitProcessorResources(PM);
    else if(!PM.ProcResourceDefs.empty())
      PrintFatalError(PM.ModelDef->getLoc(), "SchedMachineModel defines "
                    "ProcResources without defining WriteRes SchedWriteRes");

    // Begin processor itinerary properties
    PI.subtargetEmitProcModelHeader(PM.ModelName);
    PI.subtargetEmitProcessorProp(PM.ModelDef, "IssueWidth", ',');
    PI.subtargetEmitProcessorProp(PM.ModelDef, "MicroOpBufferSize", ',');
    PI.subtargetEmitProcessorProp(PM.ModelDef, "LoopMicroOpBufferSize", ',');
    PI.subtargetEmitProcessorProp(PM.ModelDef, "LoadLatency", ',');
    PI.subtargetEmitProcessorProp(PM.ModelDef, "HighLatency", ',');
    PI.subtargetEmitProcessorProp(PM.ModelDef, "MispredictPenalty", ',');

    PI.subtargetEmitProcModel(PM, SchedModels);
  }
}

//
// EmitSchedModel - Emits all scheduling model tables, folding common patterns.
//
void SubtargetEmitter::EmitSchedModel() {
  PI.subtargetEmitDBGMacrosBegin();

  if (SchedModels.hasItineraries()) {
    std::vector<std::vector<InstrItinerary>> ProcItinLists;
    // Emit the stage data
    EmitStageAndOperandCycleData(ProcItinLists);
    EmitItineraries(ProcItinLists);
  }
  PI.subtargetEmitPreOperandTableComment();

  SchedClassTablesT SchedTables;
  for (const CodeGenProcModel &ProcModel : SchedModels.procModels()) {
    GenSchedClassTables(ProcModel, SchedTables);
  }
  PI.subtargetEmitSchedClassTables(SchedTables, Target, SchedModels);

  PI.subtargetEmitDBGMacrosEnd();

  // Emit the processor machine model
  EmitProcessorModels();
}

static bool isTruePredicate(const Record *Rec) {
  return Rec->isSubClassOf("MCSchedPredicate") &&
         Rec->getValueAsDef("Pred")->isSubClassOf("MCTrue");
}

static bool hasMCSchedPredicates(const CodeGenSchedTransition &T) {
  return all_of(T.PredTerm, [](const Record *Rec) {
    return Rec->isSubClassOf("MCSchedPredicate");
  });
}

static void collectVariantClasses(const CodeGenSchedModels &SchedModels,
                                  IdxVec &VariantClasses,
                                  bool OnlyExpandMCInstPredicates) {
  for (const CodeGenSchedClass &SC : SchedModels.schedClasses()) {
    // Ignore non-variant scheduling classes.
    if (SC.Transitions.empty())
      continue;

    if (OnlyExpandMCInstPredicates) {
      // Ignore this variant scheduling class no transitions use any meaningful
      // MCSchedPredicate definitions.
      if (llvm::none_of(SC.Transitions, hasMCSchedPredicates))
        continue;
    }

    VariantClasses.push_back(SC.Index);
  }
}

static void collectProcessorIndices(const CodeGenSchedClass &SC,
                                    IdxVec &ProcIndices) {
  // A variant scheduling class may define transitions for multiple
  // processors.  This function identifies wich processors are associated with
  // transition rules specified by variant class `SC`.
  for (const CodeGenSchedTransition &T : SC.Transitions) {
    IdxVec PI;
    std::set_union(&T.ProcIndex, &T.ProcIndex + 1, ProcIndices.begin(),
                   ProcIndices.end(), std::back_inserter(PI));
    ProcIndices.swap(PI);
  }
}

static bool isAlwaysTrue(const CodeGenSchedTransition &T) {
  return llvm::all_of(T.PredTerm, isTruePredicate);
}

void SubtargetEmitter::emitSchedModelHelpersImpl(
    bool OnlyExpandMCInstPredicates) {
  IdxVec VariantClasses;
  collectVariantClasses(SchedModels, VariantClasses,
                        OnlyExpandMCInstPredicates);

  if (VariantClasses.empty()) {
    PI.subtargetEmitSchedModelHelperEpilogue(OnlyExpandMCInstPredicates);
    return;
  }

  // Construct a switch statement where the condition is a check on the
  // scheduling class identifier. There is a `case` for every variant class
  // defined by the processor models of this target.
  // Each `case` implements a number of rules to resolve (i.e. to transition from)
  // a variant scheduling class to another scheduling class.  Rules are
  // described by instances of CodeGenSchedTransition. Note that transitions may
  // not be valid for all processors.
  PI.subtargetEmitSchedClassSwitch();
  for (unsigned VC : VariantClasses) {
    IdxVec ProcIndices;
    const CodeGenSchedClass &SC = SchedModels.getSchedClass(VC);
    collectProcessorIndices(SC, ProcIndices);

    PI.subtargetEmitSchedClassCase(VC, SC.Name);

    PI.subtargetPrepareSchedClassPreds(Target, OnlyExpandMCInstPredicates);
    for (unsigned Pi : ProcIndices) {
      PI.subtargetEmitSchedClassProcGuard(Pi, OnlyExpandMCInstPredicates,
          (SchedModels.procModelBegin() + Pi)->ModelName);

      // Now emit transitions associated with processor PI.
      const CodeGenSchedTransition *FinalT = nullptr;
      for (const CodeGenSchedTransition &T : SC.Transitions) {
        if (Pi != 0 && T.ProcIndex != Pi)
          continue;

        // Emit only transitions based on MCSchedPredicate, if it's the case.
        // At least the transition specified by NoSchedPred is emitted,
        // which becomes the default transition for those variants otherwise
        // not based on MCSchedPredicate.
        // FIXME: preferably, llvm-mca should instead assume a reasonable
        // default when a variant transition is not based on MCSchedPredicate
        // for a given processor.
        if (OnlyExpandMCInstPredicates && !hasMCSchedPredicates(T))
          continue;

        // If transition is folded to 'return X' it should be the last one.
        if (isAlwaysTrue(T)) {
          FinalT = &T;
          continue;
        }
        PI.subtargetEmitPredicates(T, SchedModels.getSchedClass(T.ToClassIdx),
                                   isTruePredicate, 3);
      }
      if (FinalT)
        PI.subtargetEmitPredicates(*FinalT, SchedModels.getSchedClass(FinalT->ToClassIdx),
                                   isTruePredicate);

      PI.subtargetEmitProcTransitionEnd();

      if (Pi == 0)
        break;
    }

    PI.subtargetEmitSchedClassCaseEnd(SC);
  }

  PI.subtargetEmitSchedClassSwitchEnd();

  PI.subtargetEmitSchedModelHelperEpilogue(OnlyExpandMCInstPredicates);
}

void SubtargetEmitter::EmitSchedModelHelpers(const std::string &ClassName) {
  PI.subtargetEmitResolveSchedClassHdr(ClassName);

  // Emit the predicate prolog code.
  PI.subtargetEmitPredicateProlog(Records);

  // Emit target predicates.
  emitSchedModelHelpersImpl();

  PI.subtargetEmitResolveSchedClassEnd(ClassName);

  PI.subtargetEmitResolveVariantSchedClass(Target, ClassName);

  PI.subtargetEmitExpandedSTIPreds(Target, ClassName, SchedModels);
}

void SubtargetEmitter::EmitHwModeCheck(const std::string &ClassName) {
  const CodeGenHwModes &CGH = TGT.getHwModes();
  assert(CGH.getNumModeIds() > 0);
  if (CGH.getNumModeIds() == 1)
    return;

  PI.subtargetEmitHwModes(CGH, ClassName);
}

// Produces a subtarget specific function for parsing
// the subtarget features string.
void SubtargetEmitter::ParseFeaturesFunction() {
  std::vector<Record*> Features =
                       Records.getAllDerivedDefinitions("SubtargetFeature");
  llvm::sort(Features, LessRecord());
  PI.subtargetEmitParseFeaturesFunction(Target, Features);
}

void SubtargetEmitter::emitGenMCSubtargetInfo() {
  PI.emitNamespace(Target + "_MC", true);
  PI.subtargetEmitResolveVariantSchedClassImplHdr();
  emitSchedModelHelpersImpl(/* OnlyExpandMCPredicates */ true);
  PI.subtargetEmitResolveVariantSchedClassImplEnd();
  PI.emitNamespace(Target + "_MC", false);

  PI.subtargetEmitGenMCSubtargetInfoClass(Target, TGT.getHwModes().getNumModeIds() > 1);
  EmitHwModeCheck(Target + "GenMCSubtargetInfo");
}

void SubtargetEmitter::EmitMCInstrAnalysisPredicateFunctions() {
  PI.emitIncludeToggle("GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS", true);

  PI.subtargetEmitExpandedSTIPredsMCAnaDecl(Target, SchedModels);

  PI.emitIncludeToggle("GET_STIPREDICATE_DECLS_FOR_MC_ANALYSIS", false);

  PI.emitIncludeToggle("GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS", true);

  std::string ClassPrefix = Target + "MCInstrAnalysis";
  PI.subtargetEmitExpandedSTIPreds(Target, ClassPrefix, SchedModels);

  PI.emitIncludeToggle("GET_STIPREDICATE_DEFS_FOR_MC_ANALYSIS", false);
}

//
// SubtargetEmitter::run - Main subtarget enumeration emitter.
//
void SubtargetEmitter::run() {
  PI.subtargetEmitSourceFileHeader();

  PI.emitIncludeToggle("GET_SUBTARGETINFO_ENUM", true);

  DenseMap<Record *, unsigned> FeatureMap;

  PI.emitNamespace("llvm", true);
  Enumeration(FeatureMap);
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_SUBTARGETINFO_ENUM", false);

  EmitSubtargetInfoMacroCalls();
  PI.emitIncludeToggle("GET_SUBTARGETINFO_MC_DESC", true);

  PI.emitNamespace("llvm", true);
#if 0
  PI.emitNamespace("", true);
#endif
  unsigned NumFeatures = FeatureKeyValues(FeatureMap);
  EmitSchedModel();
  PI.emitString("\n");
  unsigned NumProcs = CPUKeyValues(FeatureMap);
  PI.emitString("\n");
#if 0
  PI.emitNamespace("", false);
#endif

  // MCInstrInfo initialization routine.
  emitGenMCSubtargetInfo();

  PI.subtargetEmitMCSubtargetInfoImpl(Target, NumFeatures, NumProcs, SchedModels.hasItineraries());

  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_SUBTARGETINFO_MC_DESC", false);

  PI.emitIncludeToggle("GET_SUBTARGETINFO_TARGET_DESC", true);

  PI.subtargetEmitIncludeSTIDesc();

  ParseFeaturesFunction();

  PI.emitIncludeToggle("GET_SUBTARGETINFO_TARGET_DESC", false);

  // Create a TargetSubtargetInfo subclass to hide the MC layer initialization.
  PI.emitIncludeToggle("GET_SUBTARGETINFO_HEADER", true);

  std::string ClassName = Target + "GenSubtargetInfo";
  PI.emitNamespace("llvm", true);
  PI.subtargetEmitDFAPacketizerClass(Target, ClassName, TGT.getHwModes().getNumModeIds() > 1);

  PI.subtargetEmitExpandedSTIPredsHeader(Target, SchedModels);
  PI.subtargetEmitDFASubtargetInfoImpl(Target, ClassName, NumFeatures, NumProcs,
    SchedModels.hasItineraries());
  PI.subtargetEmitDFAPacketizerClassEnd();

  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_SUBTARGETINFO_HEADER", false);

  PI.emitIncludeToggle("GET_SUBTARGETINFO_CTOR", true);

  PI.subtargetEmitSTICtor();
  PI.emitNamespace("llvm", true);
  PI.subtargetEmitExternKVArrays(Target, SchedModels.hasItineraries());

  PI.subtargetEmitClassDefs(Target,
                            ClassName,
                            NumFeatures,
                            NumProcs,
                            SchedModels.hasItineraries());

  EmitSchedModelHelpers(ClassName);
  EmitHwModeCheck(ClassName);

  PI.emitNamespace("llvm", false);

  PI.emitIncludeToggle("GET_SUBTARGETINFO_CTOR", false);

  EmitMCInstrAnalysisPredicateFunctions();
}

namespace llvm {

void EmitSubtarget(RecordKeeper &RK, raw_ostream &OS) {
  CodeGenTarget CGTarget(RK);

  PrinterLanguage const PL = PrinterLLVM::getLanguage();
  PrinterLLVM *PI;
  formatted_raw_ostream FOS(OS);
  switch(PL) {
  default:
    llvm_unreachable("Subtarget backend does not support the selected printer language.");
  case PRINTER_LANG_CPP:
    PI = new PrinterLLVM(FOS, CGTarget.getName().str());
    break;
  case PRINTER_LANG_CAPSTONE_C:
    PI = new PrinterCapstone(FOS, CGTarget.getName().str());
    break;
  }
  SubtargetEmitter(RK, CGTarget, *PI).run();
  delete PI;
}

} // end namespace llvm
