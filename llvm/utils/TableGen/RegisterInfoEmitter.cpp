//===- RegisterInfoEmitter.cpp - Generate a Register File Desc. -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend is responsible for emitting a description of a target
// register file for a code generator.  It uses instances of the Register,
// RegisterAliases, and RegisterClass classes to gather this information.
//
//===----------------------------------------------------------------------===//

#include "Printer.h"
#include "RegisterInfoEmitterTypes.h"

using namespace llvm;

cl::OptionCategory RegisterInfoCat("Options for -gen-register-info");

static cl::opt<bool>
    RegisterInfoDebug("register-info-debug", cl::init(false),
                      cl::desc("Dump register information to help debugging"),
                      cl::cat(RegisterInfoCat));

namespace {

class RegisterInfoEmitter {
  CodeGenTarget Target;
  RecordKeeper &Records;
  PrinterLLVM &PI;

public:
  RegisterInfoEmitter(RecordKeeper &R, PrinterLLVM &PI)
      : Target(R), Records(R), PI(PI) {
    CodeGenRegBank &RegBank = Target.getRegBank();
    RegBank.computeDerivedInfo();
  }

  // runMCDesc - Print out MC register descriptions.
  void runMCDesc(CodeGenTarget &Target, CodeGenRegBank &Bank);

  // runTargetHeader - Emit a header fragment for the register info emitter.
  void runTargetHeader(CodeGenTarget &Target,
                       CodeGenRegBank &Bank);

  // runTargetDesc - Output the target register and register file descriptions.
  void runTargetDesc(CodeGenTarget &Target,
                     CodeGenRegBank &Bank);

  // run - Output the register file description.
  void run();

  void debugDump(raw_ostream &OS);

private:
  void EmitRegMapping(const std::deque<CodeGenRegister> &Regs,
                      bool isCtor);
  void EmitRegMappingTables(const std::deque<CodeGenRegister> &Regs,
                            bool isCtor);
  void EmitRegUnitPressure(const CodeGenRegBank &RegBank,
                           const std::string &ClassName);
  void emitComposeSubRegIndices(CodeGenRegBank &RegBank,
                                const std::string &ClassName);
  void emitComposeSubRegIndexLaneMask(CodeGenRegBank &RegBank,
                                      const std::string &ClassName);
};

} // end anonymous namespace

void RegisterInfoEmitter::
EmitRegUnitPressure(const CodeGenRegBank &RegBank,
                    const std::string &ClassName) {
  unsigned NumRCs = RegBank.getRegClasses().size();
  unsigned NumSets = RegBank.getNumRegPressureSets();

  PI.regInfoEmitRegClassWeight(RegBank, ClassName);
  // Reasonable targets (not ARMv7) have unit weight for all units, so don't
  // bother generating a table.
  bool RegUnitsHaveUnitWeight = true;
  for (unsigned UnitIdx = 0, UnitEnd = RegBank.getNumNativeRegUnits();
       UnitIdx < UnitEnd; ++UnitIdx) {
    if (RegBank.getRegUnit(UnitIdx).Weight > 1)
      RegUnitsHaveUnitWeight = false;
  }
  PI.regInfoEmitRegUnitWeight(RegBank, ClassName, RegUnitsHaveUnitWeight);
  PI.regInfoEmitGetNumRegPressureSets(ClassName, NumSets);
  PI.regInfoEmitGetRegPressureTables(RegBank, ClassName, NumSets);

  SequenceToOffsetTable<std::vector<int>> PSetsSeqs(PI.getLanguage());

  // This table may be larger than NumRCs if some register units needed a list
  // of unit sets that did not correspond to a register class.
  unsigned NumRCUnitSets = RegBank.getNumRegClassPressureSetLists();
  std::vector<std::vector<int>> PSets(NumRCUnitSets);

  for (unsigned i = 0, e = NumRCUnitSets; i != e; ++i) {
    ArrayRef<unsigned> PSetIDs = RegBank.getRCPressureSetIDs(i);
    PSets[i].reserve(PSetIDs.size());
    for (unsigned PSetID : PSetIDs) {
      PSets[i].push_back(RegBank.getRegPressureSet(PSetID).Order);
    }
    llvm::sort(PSets[i]);
    PSetsSeqs.add(PSets[i]);
  }

  PSetsSeqs.layout();
  PI.regInfoEmitRCSetsTable(ClassName, NumRCs, PSetsSeqs, PSets);
  PI.regInfoEmitGetRegUnitPressureSets(PSetsSeqs, RegBank,
                                       ClassName, PSets);
}

static void finalizeDwarfRegNumsKeys(DwarfRegNumsVecTy &DwarfRegNums) {
  // Sort and unique to get a map-like vector. We want the last assignment to
  // match previous behaviour.
  llvm::stable_sort(DwarfRegNums, on_first<LessRecordRegister>());
  // Warn about duplicate assignments.
  const Record *LastSeenReg = nullptr;
  for (const auto &X : DwarfRegNums) {
    const auto &Reg = X.first;
    // The only way LessRecordRegister can return equal is if they're the same
    // string. Use simple equality instead.
    if (LastSeenReg && Reg->getName() == LastSeenReg->getName())
      PrintWarning(Reg->getLoc(), Twine("DWARF numbers for register ") +
                                      getQualifiedName(Reg) +
                                      "specified multiple times");
    LastSeenReg = Reg;
  }
  auto Last = std::unique(
      DwarfRegNums.begin(), DwarfRegNums.end(),
      [](const DwarfRegNumsMapPair &A, const DwarfRegNumsMapPair &B) {
        return A.first->getName() == B.first->getName();
      });
  DwarfRegNums.erase(Last, DwarfRegNums.end());
}

void RegisterInfoEmitter::EmitRegMappingTables(
    const std::deque<CodeGenRegister> &Regs, bool isCtor) {
  // Collect all information about dwarf register numbers
  DwarfRegNumsVecTy DwarfRegNums;

  // First, just pull all provided information to the map
  unsigned maxLength = 0;
  for (auto &RE : Regs) {
    Record *Reg = RE.TheDef;
    std::vector<int64_t> RegNums = Reg->getValueAsListOfInts("DwarfNumbers");
    maxLength = std::max((size_t)maxLength, RegNums.size());
    DwarfRegNums.emplace_back(Reg, std::move(RegNums));
  }
  finalizeDwarfRegNumsKeys(DwarfRegNums);

  if (!maxLength)
    return;

  // Now we know maximal length of number list. Append -1's, where needed
  for (auto &DwarfRegNum : DwarfRegNums)
    for (unsigned I = DwarfRegNum.second.size(), E = maxLength; I != E; ++I)
      DwarfRegNum.second.push_back(-1);

  StringRef Namespace = Regs.front().TheDef->getValueAsString("Namespace");
  PI.regInfoEmitInfoDwarfRegsRev(Namespace, DwarfRegNums, maxLength, isCtor);

  for (auto &RE : Regs) {
    Record *Reg = RE.TheDef;
    const RecordVal *V = Reg->getValue("DwarfAlias");
    if (!V || !V->getValue())
      continue;

    DefInit *DI = cast<DefInit>(V->getValue());
    Record *Alias = DI->getDef();
    const auto &AliasIter = llvm::lower_bound(
        DwarfRegNums, Alias, [](const DwarfRegNumsMapPair &A, const Record *B) {
          return LessRecordRegister()(A.first, B);
        });
    assert(AliasIter != DwarfRegNums.end() && AliasIter->first == Alias &&
           "Expected Alias to be present in map");
    const auto &RegIter = llvm::lower_bound(
        DwarfRegNums, Reg, [](const DwarfRegNumsMapPair &A, const Record *B) {
          return LessRecordRegister()(A.first, B);
        });
    assert(RegIter != DwarfRegNums.end() && RegIter->first == Reg &&
           "Expected Reg to be present in map");
    RegIter->second = AliasIter->second;
  }

  // Emit information about the dwarf register numbers.
  PI.regInfoEmitInfoDwarfRegs(Namespace, DwarfRegNums, maxLength, isCtor);
}

void RegisterInfoEmitter::EmitRegMapping(
    const std::deque<CodeGenRegister> &Regs, bool isCtor) {
  // Emit the initializer so the tables from EmitRegMappingTables get wired up
  // to the MCRegisterInfo object.
  unsigned maxLength = 0;
  for (auto &RE : Regs) {
    Record *Reg = RE.TheDef;
    maxLength = std::max((size_t)maxLength,
                         Reg->getValueAsListOfInts("DwarfNumbers").size());
  }

  StringRef Namespace = Regs.front().TheDef->getValueAsString("Namespace");
  PI.regInfoEmitInfoRegMapping(Namespace, maxLength, isCtor);
}

// Differentially encode a sequence of numbers into V. The starting value and
// terminating 0 are not added to V, so it will have the same size as List.
static
DiffVec &diffEncode(DiffVec &V, unsigned InitVal, SparseBitVector<> List) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);

  for (uint16_t Cur : List) {
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

template<typename Iter>
static
DiffVec &diffEncode(DiffVec &V, unsigned InitVal, Iter Begin, Iter End) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);
  for (Iter I = Begin; I != End; ++I) {
    uint16_t Cur = (*I)->EnumValue;
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

// Try to combine Idx's compose map into Vec if it is compatible.
// Return false if it's not possible.
static bool combine(const CodeGenSubRegIndex *Idx,
                    SmallVectorImpl<CodeGenSubRegIndex*> &Vec) {
  const CodeGenSubRegIndex::CompMap &Map = Idx->getComposites();
  for (const auto &I : Map) {
    CodeGenSubRegIndex *&Entry = Vec[I.first->EnumValue - 1];
    if (Entry && Entry != I.second)
      return false;
  }

  // All entries are compatible. Make it so.
  for (const auto &I : Map) {
    auto *&Entry = Vec[I.first->EnumValue - 1];
    assert((!Entry || Entry == I.second) &&
           "Expected EnumValue to be unique");
    Entry = I.second;
  }
  return true;
}

void
RegisterInfoEmitter::emitComposeSubRegIndices(CodeGenRegBank &RegBank,
                                              const std::string &ClName) {
  const auto &SubRegIndices = RegBank.getSubRegIndices();
  PI.regInfoEmitComposeSubRegIndicesImplHead(ClName);
  // Many sub-register indexes are composition-compatible, meaning that
  //
  //   compose(IdxA, IdxB) == compose(IdxA', IdxB)
  //
  // for many IdxA, IdxA' pairs. Not all sub-register indexes can be composed.
  // The illegal entries can be use as wildcards to compress the table further.

  // Map each Sub-register index to a compatible table row.
  SmallVector<unsigned, 4> RowMap;
  SmallVector<SmallVector<CodeGenSubRegIndex*, 4>, 4> Rows;

  auto SubRegIndicesSize =
      std::distance(SubRegIndices.begin(), SubRegIndices.end());
  for (const auto &Idx : SubRegIndices) {
    unsigned Found = ~0u;
    for (unsigned r = 0, re = Rows.size(); r != re; ++r) {
      if (combine(&Idx, Rows[r])) {
        Found = r;
        break;
      }
    }
    if (Found == ~0u) {
      Found = Rows.size();
      Rows.resize(Found + 1);
      Rows.back().resize(SubRegIndicesSize);
      combine(&Idx, Rows.back());
    }
    RowMap.push_back(Found);
  }

  // Output the row map if there is multiple rows.
  PI.regInfoEmitComposeSubRegIndicesImplBody(Rows, SubRegIndicesSize, RowMap);
}

void
RegisterInfoEmitter::emitComposeSubRegIndexLaneMask(CodeGenRegBank &RegBank,
                                                    const std::string &ClName) {
  // See the comments in computeSubRegLaneMasks() for our goal here.
  const auto &SubRegIndices = RegBank.getSubRegIndices();

  // Create a list of Mask+Rotate operations, with equivalent entries merged.
  SmallVector<unsigned, 4> SubReg2SequenceIndexMap;
  SmallVector<SmallVector<MaskRolPair, 1>, 4> Sequences;
  for (const auto &Idx : SubRegIndices) {
    const SmallVector<MaskRolPair, 1> &IdxSequence
      = Idx.CompositionLaneMaskTransform;

    unsigned Found = ~0u;
    unsigned SIdx = 0;
    unsigned NextSIdx;
    for (size_t s = 0, se = Sequences.size(); s != se; ++s, SIdx = NextSIdx) {
      SmallVectorImpl<MaskRolPair> &Sequence = Sequences[s];
      NextSIdx = SIdx + Sequence.size() + 1;
      if (Sequence == IdxSequence) {
        Found = SIdx;
        break;
      }
    }
    if (Found == ~0u) {
      Sequences.push_back(IdxSequence);
      Found = SIdx;
    }
    SubReg2SequenceIndexMap.push_back(Found);
  }

  PI.regInfoEmitLaneMaskComposeSeq(Sequences, SubReg2SequenceIndexMap, SubRegIndices);
  PI.regInfoEmitComposeSubRegIdxLaneMask(ClName, SubRegIndices);
  PI.regInfoEmitComposeSubRegIdxLaneMaskRev(ClName, SubRegIndices);

}

//
// runMCDesc - Print out MC register descriptions.
//
void
RegisterInfoEmitter::runMCDesc(CodeGenTarget &Target,
                               CodeGenRegBank &RegBank) {
  PI.regInfoEmitSourceFileHeader("MC Register Information");

  const auto &Regs = RegBank.getRegisters();

  auto &SubRegIndices = RegBank.getSubRegIndices();

  // Differentially encoded lists.
  SequenceToOffsetTable<DiffVec> DiffSeqs(PI.getLanguage());
  SmallVector<DiffVec, 4> SubRegLists(Regs.size());
  SmallVector<DiffVec, 4> SuperRegLists(Regs.size());
  SmallVector<DiffVec, 4> RegUnitLists(Regs.size());
  SmallVector<unsigned, 4> RegUnitInitScale(Regs.size());

  // List of lane masks accompanying register unit sequences.
  SequenceToOffsetTable<MaskVec> LaneMaskSeqs(PI.getLanguage());
  SmallVector<MaskVec, 4> RegUnitLaneMasks(Regs.size());

  SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> SubRegIdxSeqs(PI.getLanguage());
  SmallVector<SubRegIdxVec, 4> SubRegIdxLists(Regs.size());

  SequenceToOffsetTable<std::string> RegStrings(PI.getLanguage());

  // Precompute register lists for the SequenceToOffsetTable.
  unsigned i = 0;
  for (auto I = Regs.begin(), E = Regs.end(); I != E; ++I, ++i) {
    const auto &Reg = *I;
    RegStrings.add(std::string(Reg.getName()));

    // Compute the ordered sub-register list.
    SetVector<const CodeGenRegister*> SR;
    Reg.addSubRegsPreOrder(SR, RegBank);
    diffEncode(SubRegLists[i], Reg.EnumValue, SR.begin(), SR.end());
    DiffSeqs.add(SubRegLists[i]);

    // Compute the corresponding sub-register indexes.
    SubRegIdxVec &SRIs = SubRegIdxLists[i];
    for (const CodeGenRegister *S : SR)
      SRIs.push_back(Reg.getSubRegIndex(S));
    SubRegIdxSeqs.add(SRIs);

    // Super-registers are already computed.
    const RegVec &SuperRegList = Reg.getSuperRegs();
    diffEncode(SuperRegLists[i], Reg.EnumValue, SuperRegList.begin(),
               SuperRegList.end());
    DiffSeqs.add(SuperRegLists[i]);

    // Differentially encode the register unit list, seeded by register number.
    // First compute a scale factor that allows more diff-lists to be reused:
    //
    //   D0 -> (S0, S1)
    //   D1 -> (S2, S3)
    //
    // A scale factor of 2 allows D0 and D1 to share a diff-list. The initial
    // value for the differential decoder is the register number multiplied by
    // the scale.
    //
    // Check the neighboring registers for arithmetic progressions.
    unsigned ScaleA = ~0u, ScaleB = ~0u;
    SparseBitVector<> RUs = Reg.getNativeRegUnits();
    if (I != Regs.begin() &&
        std::prev(I)->getNativeRegUnits().count() == RUs.count())
      ScaleB = *RUs.begin() - *std::prev(I)->getNativeRegUnits().begin();
    if (std::next(I) != Regs.end() &&
        std::next(I)->getNativeRegUnits().count() == RUs.count())
      ScaleA = *std::next(I)->getNativeRegUnits().begin() - *RUs.begin();
    unsigned Scale = std::min(ScaleB, ScaleA);
    // Default the scale to 0 if it can't be encoded in 4 bits.
    if (Scale >= 16)
      Scale = 0;
    RegUnitInitScale[i] = Scale;
    DiffSeqs.add(diffEncode(RegUnitLists[i], Scale * Reg.EnumValue, RUs));

    const auto &RUMasks = Reg.getRegUnitLaneMasks();
    MaskVec &LaneMaskVec = RegUnitLaneMasks[i];
    assert(LaneMaskVec.empty());
    llvm::append_range(LaneMaskVec, RUMasks);
    // Terminator mask should not be used inside of the list.
#ifndef NDEBUG
    for (LaneBitmask M : LaneMaskVec) {
      assert(!M.all() && "terminator mask should not be part of the list");
    }
#endif
    LaneMaskSeqs.add(LaneMaskVec);
  }

  // Compute the final layout of the sequence table.
  DiffSeqs.layout();
  LaneMaskSeqs.layout();
  SubRegIdxSeqs.layout();

  PI.emitIncludeToggle("GET_REGINFO_MC_DESC", true);
  PI.emitNamespace("llvm", true);

  const std::string &TargetName = std::string(Target.getName());

  // Emit the shared table of differential lists.
  PI.regInfoEmitRegDiffLists(TargetName, DiffSeqs);

  // Emit the shared table of regunit lane mask sequences.
  PI.regInfoEmitLaneMaskLists(TargetName, LaneMaskSeqs);

  // Emit the table of sub-register indexes.
  PI.regInfoEmitSubRegIdxLists(TargetName, SubRegIdxSeqs);

  // Emit the table of sub-register index sizes.
  PI.regInfoEmitSubRegIdxSizes(TargetName, SubRegIndices);

  // Emit the string table.
  RegStrings.layout();
  PI.regInfoEmitSubRegStrTable(TargetName, RegStrings);

  // Emit the register descriptors now.
  PI.regInfoEmitRegDesc(LaneMaskSeqs,
          Regs, SubRegIdxSeqs, DiffSeqs,
          SubRegIdxLists, SubRegLists,
          SuperRegLists, RegUnitLists,
          RegUnitInitScale, RegUnitLaneMasks,
          RegStrings);

  // Emit the table of register unit roots. Each regunit has one or two root
  // registers.
  PI.regInfoEmitRegUnitRoots(TargetName, RegBank);

  const auto &RegisterClasses = RegBank.getRegClasses();

  // Loop over all of the register classes... emitting each one.
  PI.emitNamespace("", true, "Register classes...");

  SequenceToOffsetTable<std::string> RegClassStrings(PI.getLanguage());

  // Emit the register enum value arrays for each RegisterClass
  PI.regInfoEmitRegClasses(RegisterClasses, RegClassStrings, Target);
  PI.emitNamespace("", false);

  RegClassStrings.layout();
  PI.regInfoEmitStrLiteralRegClasses(TargetName, RegClassStrings);

  PI.regInfoEmitMCRegClassesTable(TargetName, RegisterClasses, RegClassStrings);

  EmitRegMappingTables(Regs, false);

  // Emit Reg encoding table
  PI.regInfoEmitRegEncodingTable(TargetName, Regs);

  // MCRegisterInfo initialization routine.
  PI.regInfoEmitMCRegInfoInit(TargetName, RegBank, Regs, RegisterClasses, SubRegIndices);

  EmitRegMapping(Regs, false);

  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_REGINFO_MC_DESC", false);
}

void
RegisterInfoEmitter::runTargetHeader(CodeGenTarget &Target,
                                     CodeGenRegBank &RegBank) {
  PI.regInfoEmitSourceFileHeader("Register Information Header Fragment");
  PI.emitIncludeToggle("GET_REGINFO_HEADER", true);

  const std::string &TargetName = std::string(Target.getName());
  std::string ClassName = TargetName + "GenRegisterInfo";
  PI.regInfoEmitHeaderIncludes();

  PI.emitNamespace("llvm", true);
  const auto &RegisterClasses = RegBank.getRegClasses();
  PI.regInfoEmitHeaderDecl(TargetName, ClassName, !RegBank.getSubRegIndices().empty(),
    llvm::any_of(RegisterClasses, [](const auto &RC) { return RC.getBaseClassOrder(); }));

  if (!RegisterClasses.empty()) {
    PI.emitNamespace(RegisterClasses.front().Namespace.str(), true, "Register classes");
    PI.regInfoEmitHeaderExternRegClasses(RegisterClasses);
    PI.emitNamespace(RegisterClasses.front().Namespace.str(), false);
  }
  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_REGINFO_HEADER", false);
}

//
// runTargetDesc - Output the target register and register file descriptions.
//
void
RegisterInfoEmitter::runTargetDesc(CodeGenTarget &Target,
                                   CodeGenRegBank &RegBank){
  PI.regInfoEmitSourceFileHeader("Target Register and Register Classes Information");

  PI.emitIncludeToggle("GET_REGINFO_TARGET_DESC", true);
  PI.emitNamespace("llvm", true);

  // Get access to MCRegisterClass data.
  PI.regInfoEmitExternRegClassesArr(Target.getName().str());

  // Start out by emitting each of the register classes.
  const auto &RegisterClasses = RegBank.getRegClasses();
  const auto &SubRegIndices = RegBank.getSubRegIndices();

  // Collect all registers belonging to any allocatable class.
  std::set<Record*> AllocatableRegs;

  // Collect allocatable registers.
  for (const auto &RC : RegisterClasses) {
    ArrayRef<Record*> Order = RC.getOrder();

    if (RC.Allocatable)
      AllocatableRegs.insert(Order.begin(), Order.end());
  }

  const CodeGenHwModes &CGH = Target.getHwModes();
  unsigned NumModes = CGH.getNumModeIds();

  // Build a shared array of value types.
  SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> VTSeqs(PI.getLanguage());
  for (unsigned M = 0; M < NumModes; ++M) {
    for (const auto &RC : RegisterClasses) {
      std::vector<MVT::SimpleValueType> S;
      for (const ValueTypeByHwMode &VVT : RC.VTs)
        S.push_back(VVT.get(M).SimpleTy);
      VTSeqs.add(S);
    }
  }
  VTSeqs.layout();
  PI.regInfoEmitVTSeqs(VTSeqs);

  // Emit SubRegIndex names, skipping 0.
  PI.regInfoEmitSubRegIdxTable(SubRegIndices);

  // Now that all of the structs have been emitted, emit the instances.
  if (!RegisterClasses.empty()) {
    PI.regInfoEmitRegClassInfoTable(RegisterClasses, VTSeqs, CGH, NumModes);

    // Emit register class bit mask tables. The first bit mask emitted for a
    // register class, RC, is the set of sub-classes, including RC itself.
    //
    // If RC has super-registers, also create a list of subreg indices and bit
    // masks, (Idx, Mask). The bit mask has a bit for every superreg regclass,
    // SuperRC, that satisfies:
    //
    //   For all SuperReg in SuperRC: SuperReg:Idx in RC
    //
    // The 0-terminated list of subreg indices starts at:
    //
    //   RC->getSuperRegIndices() = SuperRegIdxSeqs + ...
    //
    // The corresponding bitmasks follow the sub-class mask in memory. Each
    // mask has RCMaskWords uint32_t entries.
    //
    // Every bit mask present in the list has at least one bit set.

    SmallVector<IdxList, 8> SuperRegIdxLists(RegisterClasses.size());
    SequenceToOffsetTable<IdxList, deref<std::less<>>> SuperRegIdxSeqs(PI.getLanguage());
    BitVector MaskBV(RegisterClasses.size());

    PI.regInfoEmitSubClassMaskTable(RegisterClasses,
                                    SuperRegIdxLists,
                                    SuperRegIdxSeqs,
                                    SubRegIndices,
                                    MaskBV);

    SuperRegIdxSeqs.layout();
    PI.regInfoEmitSuperRegIdxSeqsTable(SuperRegIdxSeqs);

    // Emit NULL terminated super-class lists.
    PI.regInfoEmitSuperClassesTable(RegisterClasses);

    // Emit methods.
    PI.regInfoEmitRegClassMethods(RegisterClasses, Target.getName().str());

    // Now emit the actual value-initialized register class instances.
    PI.emitNamespace(RegisterClasses.front().Namespace.str(), true, "Register class instances");
    PI.regInfomitRegClassInstances(RegisterClasses,
                                   SuperRegIdxSeqs,
                                   SuperRegIdxLists,
                                   Target.getName().str());
    PI.emitNamespace(RegisterClasses.front().Namespace.str(), false);
  }

  PI.emitNamespace("", true);
  PI.regInfoEmitRegClassTable(RegisterClasses);
  PI.emitNamespace("", false);

  // Emit extra information about registers.
  const std::string &TargetName = std::string(Target.getName());
  const auto &Regs = RegBank.getRegisters();
  unsigned NumRegCosts = 1;
  for (const auto &Reg : Regs)
    NumRegCosts = std::max((size_t)NumRegCosts, Reg.CostPerUse.size());

  std::vector<unsigned> AllRegCostPerUse;
  llvm::BitVector InAllocClass(Regs.size() + 1, false);
  AllRegCostPerUse.insert(AllRegCostPerUse.end(), NumRegCosts, 0);

  // Populate the vector RegCosts with the CostPerUse list of the registers
  // in the order they are read. Have at most NumRegCosts entries for
  // each register. Fill with zero for values which are not explicitly given.
  for (const auto &Reg : Regs) {
    auto Costs = Reg.CostPerUse;
    AllRegCostPerUse.insert(AllRegCostPerUse.end(), Costs.begin(), Costs.end());
    if (NumRegCosts > Costs.size())
      AllRegCostPerUse.insert(AllRegCostPerUse.end(),
                              NumRegCosts - Costs.size(), 0);

    if (AllocatableRegs.count(Reg.TheDef))
      InAllocClass.set(Reg.EnumValue);
  }

  // Emit the cost values as a 1D-array after grouping them by their indices,
  // i.e. the costs for all registers corresponds to index 0, 1, 2, etc.
  // Size of the emitted array should be NumRegCosts * (Regs.size() + 1).
  PI.regInfoEmitCostPerUseTable(AllRegCostPerUse, NumRegCosts);
  PI.regInfoEmitInAllocatableClassTable(InAllocClass);
  PI.regInfoEmitRegExtraDesc(TargetName, NumRegCosts);
  // End of register descriptors...

  std::string ClassName = Target.getName().str() + "GenRegisterInfo";

  auto SubRegIndicesSize =
      std::distance(SubRegIndices.begin(), SubRegIndices.end());

  if (!SubRegIndices.empty()) {
    emitComposeSubRegIndices(RegBank, ClassName);
    emitComposeSubRegIndexLaneMask(RegBank, ClassName);
  }

  if (!SubRegIndices.empty()) {
    PI.regInfoEmitSubClassSubRegGetter(ClassName,
                                       SubRegIndicesSize,
                                       SubRegIndices,
                                       RegisterClasses,
                                       RegBank);
  }

  EmitRegUnitPressure(RegBank, ClassName);

  // Emit register base class mapper
  if (!RegisterClasses.empty()) {
    // Collect base classes
    SmallVector<const CodeGenRegisterClass*> BaseClasses;
    for (const auto &RC : RegisterClasses) {
      if (RC.getBaseClassOrder())
        BaseClasses.push_back(&RC);
    }
    if (!BaseClasses.empty()) {
      // Represent class indexes with uint8_t and allocate one index for nullptr
      assert(BaseClasses.size() <= UINT8_MAX && "Too many base register classes");

      // Apply order
      struct BaseClassOrdering {
        bool operator()(const CodeGenRegisterClass *LHS, const CodeGenRegisterClass *RHS) const {
          return std::pair(*LHS->getBaseClassOrder(), LHS->EnumValue)
               < std::pair(*RHS->getBaseClassOrder(), RHS->EnumValue);
        }
      };
      llvm::stable_sort(BaseClasses, BaseClassOrdering());

      // Build mapping for Regs (+1 for NoRegister)
      std::vector<uint8_t> Mapping(Regs.size() + 1, 0);
      for (int RCIdx = BaseClasses.size() - 1; RCIdx >= 0; --RCIdx) {
        for (const auto Reg : BaseClasses[RCIdx]->getMembers())
          Mapping[Reg->EnumValue] = RCIdx + 1;
      }

      PI.regInfoEmitRegBaseClassMapping(ClassName, BaseClasses, Mapping);
    }
  }

  // Emit the constructor of the class...
  PI.regInfoEmitExternTableDecl(TargetName);

  EmitRegMappingTables(Regs, true);

  PI.regInfoEmitRegClassInit(TargetName,
                             ClassName,
                             RegBank,
                             RegisterClasses,
                             Regs,
                             SubRegIndicesSize);

  EmitRegMapping(Regs, true);

  // Emit CalleeSavedRegs information.
  std::vector<Record*> CSRSets =
    Records.getAllDerivedDefinitions("CalleeSavedRegs");
  for (unsigned i = 0, e = CSRSets.size(); i != e; ++i) {
    Record *CSRSet = CSRSets[i];
    const SetTheory::RecVec *Regs = RegBank.getSets().expand(CSRSet);
    assert(Regs && "Cannot expand CalleeSavedRegs instance");

    // Emit the *_SaveList list of callee-saved registers.
    PI.regInfoEmitSaveListTable(CSRSet, Regs);

    // Emit the *_RegMask bit mask of call-preserved registers.
    BitVector Covered = RegBank.computeCoveredRegisters(*Regs);

    // Check for an optional OtherPreserved set.
    // Add those registers to RegMask, but not to SaveList.
    if (DagInit *OPDag =
        dyn_cast<DagInit>(CSRSet->getValueInit("OtherPreserved"))) {
      SetTheory::RecSet OPSet;
      RegBank.getSets().evaluate(OPDag, OPSet, CSRSet->getLoc());
      Covered |= RegBank.computeCoveredRegisters(
        ArrayRef<Record*>(OPSet.begin(), OPSet.end()));
    }

    // Add all constant physical registers to the preserved mask:
    SetTheory::RecSet ConstantSet;
    for (auto &Reg : RegBank.getRegisters()) {
      if (Reg.Constant)
        ConstantSet.insert(Reg.TheDef);
    }
    Covered |= RegBank.computeCoveredRegisters(
        ArrayRef<Record *>(ConstantSet.begin(), ConstantSet.end()));

    PI.regInfoEmitRegMaskTable(CSRSet->getName().str(), Covered);
  }
  PI.emitNewline(2);
  PI.regInfoEmitGetRegMasks(CSRSets, ClassName);

  const std::list<CodeGenRegisterCategory> &RegCategories =
      RegBank.getRegCategories();
  PI.regInfoEmitGPRCheck(ClassName, RegCategories);
  PI.regInfoEmitFixedRegCheck(ClassName, RegCategories);
  PI.regInfoEmitArgRegCheck(ClassName, RegCategories);

  PI.regInfoEmitIsConstantPhysReg(Regs, ClassName);
  PI.regInfoEmitGetRegMaskNames(CSRSets, ClassName);

  PI.regInfoEmitGetFrameLowering(TargetName);

  PI.emitNamespace("llvm", false);
  PI.emitIncludeToggle("GET_REGINFO_TARGET_DESC", false);
}

void RegisterInfoEmitter::run() {
  CodeGenRegBank &RegBank = Target.getRegBank();
  Records.startTimer("Print enums");

  PI.regInfoEmitSourceFileHeader("Target Register Enum Values");
  PI.regInfoEmitEnums(Target, RegBank);

  Records.startTimer("Print MC registers");
  runMCDesc(Target, RegBank);

  Records.startTimer("Print header fragment");
  runTargetHeader(Target, RegBank);

  Records.startTimer("Print target registers");
  runTargetDesc(Target, RegBank);

  if (RegisterInfoDebug)
    debugDump(errs());
}

void RegisterInfoEmitter::debugDump(raw_ostream &ErrOS) {
  CodeGenRegBank &RegBank = Target.getRegBank();
  const CodeGenHwModes &CGH = Target.getHwModes();
  unsigned NumModes = CGH.getNumModeIds();
  auto getModeName = [CGH] (unsigned M) -> StringRef {
    if (M == 0)
      return "Default";
    return CGH.getMode(M).Name;
  };

  for (const CodeGenRegisterClass &RC : RegBank.getRegClasses()) {
    ErrOS << "RegisterClass " << RC.getName() << ":\n";
    ErrOS << "\tSpillSize: {";
    for (unsigned M = 0; M != NumModes; ++M)
      ErrOS << ' ' << getModeName(M) << ':' << RC.RSI.get(M).SpillSize;
    ErrOS << " }\n\tSpillAlignment: {";
    for (unsigned M = 0; M != NumModes; ++M)
      ErrOS << ' ' << getModeName(M) << ':' << RC.RSI.get(M).SpillAlignment;
    ErrOS << " }\n\tNumRegs: " << RC.getMembers().size() << '\n';
    ErrOS << "\tLaneMask: " << PrintLaneMask(RC.LaneMask) << '\n';
    ErrOS << "\tHasDisjunctSubRegs: " << RC.HasDisjunctSubRegs << '\n';
    ErrOS << "\tCoveredBySubRegs: " << RC.CoveredBySubRegs << '\n';
    ErrOS << "\tAllocatable: " << RC.Allocatable << '\n';
    ErrOS << "\tAllocationPriority: " << unsigned(RC.AllocationPriority) << '\n';
    ErrOS << "\tRegs:";
    for (const CodeGenRegister *R : RC.getMembers()) {
      ErrOS << " " << R->getName();
    }
    ErrOS << '\n';
    ErrOS << "\tSubClasses:";
    const BitVector &SubClasses = RC.getSubClasses();
    for (const CodeGenRegisterClass &SRC : RegBank.getRegClasses()) {
      if (!SubClasses.test(SRC.EnumValue))
        continue;
      ErrOS << " " << SRC.getName();
    }
    ErrOS << '\n';
    ErrOS << "\tSuperClasses:";
    for (const CodeGenRegisterClass *SRC : RC.getSuperClasses()) {
      ErrOS << " " << SRC->getName();
    }
    ErrOS << '\n';
  }

  for (const CodeGenSubRegIndex &SRI : RegBank.getSubRegIndices()) {
    ErrOS << "SubRegIndex " << SRI.getName() << ":\n";
    ErrOS << "\tLaneMask: " << PrintLaneMask(SRI.LaneMask) << '\n';
    ErrOS << "\tAllSuperRegsCovered: " << SRI.AllSuperRegsCovered << '\n';
    ErrOS << "\tOffset, Size: " << SRI.Offset << ", " << SRI.Size << '\n';
  }

  for (const CodeGenRegister &R : RegBank.getRegisters()) {
    ErrOS << "Register " << R.getName() << ":\n";
    ErrOS << "\tCostPerUse: ";
    for (const auto &Cost : R.CostPerUse)
      ErrOS << Cost << " ";
    ErrOS << '\n';
    ErrOS << "\tCoveredBySubregs: " << R.CoveredBySubRegs << '\n';
    ErrOS << "\tHasDisjunctSubRegs: " << R.HasDisjunctSubRegs << '\n';
    for (std::pair<CodeGenSubRegIndex*,CodeGenRegister*> P : R.getSubRegs()) {
      ErrOS << "\tSubReg " << P.first->getName()
         << " = " << P.second->getName() << '\n';
    }
  }
}

namespace llvm {

void EmitRegisterInfo(RecordKeeper &RK, raw_ostream &OS) {
  formatted_raw_ostream FOS(OS);
  PrinterLanguage const PLang = PrinterLLVM::getLanguage();
  PrinterLLVM *PI = nullptr;
  switch (PLang) {
  default:
    PrintFatalNote(
        "RegisterInfo backend does not support the selected ouput language.");
    return;
  case PRINTER_LANG_CPP:
    PI = new PrinterLLVM(FOS);
    break;
  case PRINTER_LANG_CAPSTONE_C:
    PI = new PrinterCapstone(FOS);
    break;
  }
  RegisterInfoEmitter(RK, *PI).run();
  delete PI;
}

} // end namespace llvm
