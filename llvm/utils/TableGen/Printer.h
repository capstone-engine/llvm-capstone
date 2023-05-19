//===--------------- Printer.h - Printer Interface --------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_PRINTER_H
#define LLVM_UTILS_TABLEGEN_PRINTER_H

#include "AsmMatcherEmitterTypes.h"
#include "AsmWriterInst.h"
#include "CodeGenRegisters.h"
#include "CodeGenTarget.h"
#include "DecoderEmitterTypes.h"
#include "InstrInfoEmitterTypes.h"
#include "PrinterTypes.h"
#include "RegisterInfoEmitterTypes.h"
#include "SearchableTablesTypes.h"
#include "SubtargetEmitterTypes.h"
#include "SubtargetFeatureInfo.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"

typedef enum {
  ST_NONE,
  ST_DECL_OS,
  ST_IMPL_OS,
  ST_ENUM_OS,
} StreamType;

namespace llvm {

class PrinterBitVectorEmitter {
  BitVector Values;

public:
  virtual void add(unsigned V);
  virtual void print(raw_ostream &OS);
};

void printBitVectorAsHex(raw_ostream &OS, const BitVector &Bits,
                         unsigned Width);

class PrinterCapstone;

//==============================
//
// Implementation: LLVM
//
//==============================

/// Interface for printing the generated code.
/// Every string which will be in the generated code of a backend originates
/// from here.
///
/// It also is the only class which writes directly into the output stream for
/// the backend.
///
/// This class has methods for all classes of backends which emit generated
/// code. If a backend currently does not emit the code in a language you need
/// you simply inherit this class and implement the relevant methods.
///
/// Printer implementation of LLVM.
/// This is the default printer for all backends.
///
/// Output language: C++
class PrinterLLVM {
  friend PrinterCapstone;

private:
  formatted_raw_ostream &OS;

  //-------------------------
  // Backend: DecoderEmitter and Subtarget
  //-------------------------
  std::string TargetName;
  std::string PredicateNamespace;
  std::string GuardPrefix, GuardPostfix;
  std::string ReturnOK, ReturnFail;
  std::string Locals;

  //----------------------------
  // Backends: InstrInfo
  //           SubTargetInfo
  //----------------------------
  PredicateExpander *PE = nullptr;

  //--------------------------
  // Backend: AsmMatcher
  //--------------------------

  // Convert function stream.
  // Write the convert function to a separate stream, so we can drop it after
  // the enum. We'll build up the conversion handlers for the individual
  // operand types opportunistically as we encounter them.
  std::string *ConvertFnBody = new std::string;
  raw_string_ostream *CvtOS = new raw_string_ostream(*ConvertFnBody);
  // Operand lookup function stream.
  std::string *OperandFnBody = new std::string;
  raw_string_ostream *OpOS = new raw_string_ostream(*OperandFnBody);

public:
  PrinterLLVM(formatted_raw_ostream &OS);
  PrinterLLVM(formatted_raw_ostream &OS, std::string TargetName);

  virtual ~PrinterLLVM();

  // Backend: DecoderEmitter
  PrinterLLVM(formatted_raw_ostream &OS, std::string PredicateNamespace,
              std::string GPrefix, std::string GPostfix, std::string ROK,
              std::string RFail, std::string L, std::string Target);

  static PrinterLanguage getLanguage();

  virtual void flushOS() const { OS.flush(); }

  //------------------------------
  // PredicateExpander management
  //------------------------------
  void initNewPE(StringRef const &Target) {
    if (PE) {
      delete PE;
      PE = nullptr;
    }
    PE = getNewPE(Target);
  }
  virtual PredicateExpander *getNewPE(StringRef const &Target) const {
    return new PredicateExpanderLLVM(Target);
  }

  //--------------------------
  // General printing methods
  //--------------------------

  virtual void emitIncludeToggle(std::string const &Name, bool Begin,
                                 bool Newline = true,
                                 bool UndefAtEnd = false) const;
  virtual void emitNewline(unsigned Count) const {
    for (unsigned I = Count; I > 0; --I)
      OS << "\n";
  }
  virtual void emitIfNotDef(std::string const &Name, bool Begin) const {
    if (Begin) {
      OS << "#ifndef " << Name << "\n";
    } else {
      OS << "#endif // " << Name << "\n\n";
    }
  }
  virtual void emitString(std::string const &Str) const { OS << Str; }
  virtual void emitNamespace(std::string const &Name, bool Begin,
                             std::string const &Comment = "") const;

  //------------------------
  // Backend: RegisterInfo
  //------------------------

  virtual void regInfoEmitSourceFileHeader(std::string const &Desc) const;
  virtual void regInfoEmitEnums(CodeGenTarget const &Target,
                                CodeGenRegBank const &Bank) const;
  virtual void
  regInfoEmitRegDiffLists(std::string const TargetName,
                          SequenceToOffsetTable<DiffVec> const &DiffSeqs) const;
  virtual void regInfoEmitLaneMaskLists(
      std::string const TargetName,
      SequenceToOffsetTable<MaskVec> const &DiffSeqs) const;
  virtual void regInfoEmitSubRegIdxLists(
      std::string const TargetName,
      SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> const
          &SubRegIdxSeqs) const;
  virtual void regInfoEmitSubRegIdxSizes(
      std::string const TargetName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitSubRegStrTable(
      std::string const TargetName,
      SequenceToOffsetTable<std::string> const &RegStrings) const;
  virtual void regInfoEmitRegDesc(
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
      SequenceToOffsetTable<std::string> const &RegStrings) const;
  virtual void regInfoEmitRegUnitRoots(std::string const TargetName,
                                       CodeGenRegBank const &RegBank) const;
  virtual void
  regInfoEmitRegClasses(std::list<CodeGenRegisterClass> const &RegClasses,
                        SequenceToOffsetTable<std::string> &RegClassStrings,
                        CodeGenTarget const &Target) const;
  virtual void regInfoEmitStrLiteralRegClasses(
      std::string const TargetName,
      SequenceToOffsetTable<std::string> const &RegClassStrings) const;
  virtual void regInfoEmitMCRegClassesTable(
      std::string const TargetName,
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<std::string> &RegClassStrings) const;
  virtual void
  regInfoEmitRegEncodingTable(std::string const TargetName,
                              std::deque<CodeGenRegister> const &Regs) const;
  virtual void regInfoEmitMCRegInfoInit(
      std::string const TargetName, CodeGenRegBank const &RegBank,
      std::deque<CodeGenRegister> const &Regs,
      std::list<CodeGenRegisterClass> const &RegClasses,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitInfoDwarfRegs(StringRef const &Namespace,
                                        DwarfRegNumsVecTy &DwarfRegNums,
                                        unsigned MaxLength, bool IsCtor) const;
  virtual void regInfoEmitInfoDwarfRegsRev(StringRef const &Namespace,
                                           DwarfRegNumsVecTy &DwarfRegNums,
                                           unsigned MaxLength,
                                           bool IsCtor) const;
  virtual void regInfoEmitInfoRegMapping(StringRef const &Namespace,
                                         unsigned MaxLength, bool IsCtor) const;
  virtual void regInfoEmitHeaderIncludes() const;
  virtual void regInfoEmitHeaderExternRegClasses(
      std::list<CodeGenRegisterClass> const &RegClasses) const;
  virtual void regInfoEmitHeaderDecl(std::string const &TargetName,
                                     std::string const &ClassName,
                                     bool SubRegsPresent,
                                     bool DeclareGetPhysRegBaseClass) const;
  virtual void
  regInfoEmitExternRegClassesArr(std::string const &TargetName) const;
  virtual void regInfoEmitVTSeqs(
      SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs)
      const;
  virtual void regInfoEmitSubRegIdxTable(
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitRegClassInfoTable(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs,
      CodeGenHwModes const &CGH, unsigned NumModes) const;
  virtual void regInfoEmitSubClassMaskTable(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SmallVector<IdxList, 8> &SuperRegIdxLists,
      SequenceToOffsetTable<IdxList, deref<std::less<>>> &SuperRegIdxSeqs,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices,
      BitVector &MaskBV) const;
  virtual void regInfoEmitSuperRegIdxSeqsTable(
      SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs)
      const;
  virtual void regInfoEmitSuperClassesTable(
      std::list<CodeGenRegisterClass> const &RegClasses) const;
  virtual void
  regInfoEmitRegClassMethods(std::list<CodeGenRegisterClass> const &RegClasses,
                             std::string const &TargetName) const;
  virtual void regInfomitRegClassInstances(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs,
      SmallVector<IdxList, 8> const &SuperRegIdxLists,
      std::string const &TargetName) const;
  virtual void regInfoEmitRegClassTable(
      std::list<CodeGenRegisterClass> const &RegClasses) const;
  virtual void
  regInfoEmitCostPerUseTable(std::vector<unsigned> const &AllRegCostPerUse,
                             unsigned NumRegCosts) const;
  virtual void
  regInfoEmitInAllocatableClassTable(llvm::BitVector const &InAllocClass) const;
  virtual void regInfoEmitRegExtraDesc(std::string const &TargetName,
                                       unsigned NumRegCosts) const;
  virtual void regInfoEmitSubClassSubRegGetter(
      std::string const &ClassName, unsigned SubRegIndicesSize,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices,
      std::list<CodeGenRegisterClass> const &RegClasses,
      CodeGenRegBank &RegBank) const;
  virtual void regInfoEmitRegClassWeight(CodeGenRegBank const &RegBank,
                                         std::string const &ClassName) const;
  virtual void regInfoEmitRegUnitWeight(CodeGenRegBank const &RegBank,
                                        std::string const &ClassName,
                                        bool RegUnitsHaveUnitWeight) const;
  virtual void regInfoEmitGetNumRegPressureSets(std::string const &ClassName,
                                                unsigned NumSets) const;
  virtual void regInfoEmitGetRegPressureTables(CodeGenRegBank const &RegBank,
                                               std::string const &ClassName,
                                               unsigned NumSets) const;
  virtual void regInfoEmitRCSetsTable(
      std::string const &ClassName, unsigned NumRCs,
      SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
      std::vector<std::vector<int>> const &PSets) const;
  virtual void regInfoEmitGetRegUnitPressureSets(
      SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
      CodeGenRegBank const &RegBank, std::string const &ClassName,
      std::vector<std::vector<int>> const &PSets) const;
  virtual void regInfoEmitExternTableDecl(std::string const &TargetName) const;
  virtual void
  regInfoEmitRegClassInit(std::string const &TargetName,
                          std::string const &ClassName,
                          CodeGenRegBank const &RegBank,
                          std::list<CodeGenRegisterClass> const &RegClasses,
                          std::deque<CodeGenRegister> const &Regs,
                          unsigned SubRegIndicesSize) const;
  virtual void regInfoEmitSaveListTable(Record const *CSRSet,
                                        SetTheory::RecVec const *Regs) const;
  virtual void regInfoEmitRegMaskTable(std::string const &CSRSetName,
                                       BitVector &Covered) const;
  virtual void
  regInfoEmitIsConstantPhysReg(std::deque<CodeGenRegister> const &Regs,
                               std::string const &ClassName) const;
  virtual void regInfoEmitGetRegMasks(std::vector<Record *> const &CSRSets,
                                      std::string const &ClassName) const;
  virtual void regInfoEmitGPRCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const;
  virtual void regInfoEmitFixedRegCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const;
  virtual void regInfoEmitArgRegCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const;
  virtual void regInfoEmitGetRegMaskNames(std::vector<Record *> const &CSRSets,
                                          std::string const &ClassName) const;
  virtual void regInfoEmitGetFrameLowering(std::string const &TargetName) const;
  virtual void
  regInfoEmitComposeSubRegIndicesImplHead(std::string const &ClName) const;
  virtual void regInfoEmitComposeSubRegIndicesImplBody(
      SmallVector<SmallVector<CodeGenSubRegIndex *, 4>, 4> const &Rows,
      unsigned SubRegIndicesSize, SmallVector<unsigned, 4> const &RowMap) const;
  virtual void regInfoEmitLaneMaskComposeSeq(
      SmallVector<SmallVector<MaskRolPair, 1>, 4> const &Sequences,
      SmallVector<unsigned, 4> const &SubReg2SequenceIndexMap,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitComposeSubRegIdxLaneMask(
      std::string const &ClName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitComposeSubRegIdxLaneMaskRev(
      std::string const &ClName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const;
  virtual void regInfoEmitRegBaseClassMapping(
      std::string const &ClassName,
      SmallVector<const CodeGenRegisterClass *> const BaseClasses,
      std::vector<uint8_t> const Mapping) const;

  //-------------------------
  // Backend: DecoderEmitter
  //-------------------------

  // FilterChooser printing

  virtual void decoderEmitterEmitOpDecoder(raw_ostream &DecoderOS,
                                           const OperandInfo &Op) const;
  virtual void
  decoderEmitterEmitOpBinaryParser(raw_ostream &DecoderOS,
                                   const OperandInfo &OpInfo) const;
  virtual bool decoderEmitterEmitPredicateMatchAux(const Init &Val,
                                                   bool ParenIfBinOp,
                                                   raw_ostream &PredOS) const;
  // Emits code to check the Predicates member of an instruction are true.
  // Returns true if predicate matches were emitted, false otherwise.
  virtual bool decoderEmitterEmitPredicateMatch(raw_ostream &PredOS,
                                                const ListInit *Predicates,
                                                unsigned Opc) const;

  // DecoderEmitter printing

  virtual void decoderEmitterEmitFieldFromInstruction() const;
  virtual void decoderEmitterEmitInsertBits() const;
  virtual void decoderEmitterEmitDecodeInstruction(bool IsVarLenInst) const;
  // Emit the decoder state machine table.
  virtual void decoderEmitterEmitTable(
      DecoderTable &Table, unsigned BitWidth, StringRef Namespace,
      std::vector<EncodingAndInst> &NumberedEncodings) const;
  virtual void
  decoderEmitterEmitInstrLenTable(std::vector<unsigned> &InstrLen) const;
  virtual void decoderEmitterEmitPredicateFunction(PredicateSet &Predicates,
                                                   unsigned Indentation) const;
  virtual void decoderEmitterEmitDecoderFunction(DecoderSet &Decoders,
                                                 unsigned Indentation) const;
  virtual void decoderEmitterEmitIncludes() const;
  virtual void decoderEmitterEmitSourceFileHeader() const;

  //-------------------------
  // Backend: AsmWriter
  //-------------------------

  virtual void asmWriterEmitSourceFileHeader() const;
  virtual void asmWriterEmitGetMnemonic(std::string const &TargetName,
                                        StringRef const &ClassName) const;
  virtual void asmWriterEmitAsmStrs(
      SequenceToOffsetTable<std::string> const &StrTable) const;
  virtual void asmWriterEmitMnemonicDecodeTable(
      unsigned const OpcodeInfoBits, unsigned BitsLeft,
      unsigned const &AsmStrBits,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      std::vector<uint64_t> const &OpcodeInfo) const;
  virtual void asmWriterEmitPrintInstruction(
      std::string const &TargetName,
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits, StringRef const &ClassName,
      bool PassSubtarget) const;
  virtual void asmWriterEmitOpCases(
      std::vector<std::pair<std::string, AsmWriterOperand>> &OpsToPrint,
      bool PassSubtarget) const;
  virtual void asmWriterEmitInstrSwitch() const;
  virtual void asmWriterEmitCompoundClosure(unsigned Indent, bool Newline,
                                            bool Semicolon) const;
  virtual void
  asmWriterEmitInstruction(AsmWriterInst const &FirstInst,
                           std::vector<AsmWriterInst> const &SimilarInsts,
                           unsigned DifferingOperand, bool PassSubtarget) const;
  virtual void asmWriterEmitGetRegNameAssert(std::string const &TargetName,
                                             StringRef const &ClassName,
                                             bool HasAltNames,
                                             unsigned RegSize) const;
  virtual void asmWriterEmitStringLiteralDef(
      SequenceToOffsetTable<std::string> const &StringTable,
      StringRef const &AltName) const;
  virtual void
  asmWriterEmitAltIdxSwitch(bool HasAltNames,
                            std::vector<Record *> const &AltNameIndices,
                            StringRef const &Namespace) const;
  virtual void asmWriterEmitRegAsmOffsets(
      unsigned RegSizes, SmallVector<std::string, 4> const &AsmNames,
      SequenceToOffsetTable<std::string> const &StringTable,
      StringRef const &AltName) const;
  virtual char const *asmWriterGetPatCondKIgnore() const;
  virtual char const *asmWriterGetPatCondKRegClass() const;
  virtual char const *asmWriterGetPatCondKTiedReg() const;
  virtual char const *asmWriterGetPatCondKCustom() const;
  virtual char const *asmWriterGetPatCondKImm() const;
  virtual char const *asmWriterGetPatCondKNoReg() const;
  virtual char const *asmWriterGetPatCondKReg() const;
  virtual char const *asmWriterGetPatCondKFeature() const;
  virtual char const *asmWriterGetPatCondKEndOrFeature() const;
  virtual char const *asmWriterGetPatOpcStart() const;
  virtual char const *asmWriterGetCondPatStart() const;
  virtual std::string asmWriterGetCond(std::string const &Cond) const;
  virtual char const *asmWriterGetPatternFormat() const;
  virtual char const *asmWriterGetOpcodeFormat() const;
  virtual void asmWriterEmitPrintAliasInstrHeader(std::string const &TargetName,
                                                  StringRef const &ClassName,
                                                  bool PassSubtarget) const;
  virtual void asmWriterEmitPrintAliasInstrBodyRetFalse() const;
  virtual void asmWriterEmitPrintAliasInstrBody(
      raw_string_ostream &OpcodeO, raw_string_ostream &PatternO,
      raw_string_ostream &CondO,
      std::vector<std::pair<uint32_t, std::string>> const &AsmStrings,
      std::vector<const Record *> const &MCOpPredicates,
      std::string const &TargetName, StringRef const &ClassName,
      bool PassSubtarget) const;
  virtual void asmWriterEmitDeclValid(std::string const &TargetName,
                                      StringRef const &ClassName) const;
  virtual void asmWriterEmitPrintAliasOp(
      std::string const &TargetName, StringRef const &ClassName,
      std::vector<std::pair<std::string, bool>> const &PrintMethods,
      bool PassSubtarget) const;
  virtual void
  asmWriterEmitPrintMC(std::string const &TargetName,
                       StringRef const &ClassName,
                       std::vector<const Record *> const &MCOpPredicates) const;

  //-------------------------
  // Backend: Subtarget
  //-------------------------

  virtual void subtargetEmitSourceFileHeader() const;
  virtual void
  subtargetEmitFeatureEnum(DenseMap<Record *, unsigned> &FeatureMap,
                           std::vector<Record *> const &DefList,
                           unsigned N) const;
  virtual void subtargetEmitGetSTIMacro(StringRef const &Value,
                                        StringRef const &Attribute) const;
  virtual void subtargetEmitHwModes(CodeGenHwModes const &CGH,
                                    std::string const &ClassName) const;
  virtual void subtargetEmitFeatureKVHeader(std::string const &Target) const;
  virtual void subtargetEmitFeatureKVEnd() const;
  virtual void subtargetEmitFeatureKVPartI(std::string const &Target,
                                           StringRef const &CommandLineName,
                                           StringRef const &Name,
                                           StringRef const &Desc) const;
  virtual void subtargetEmitFeatureKVPartII() const;
  virtual void subtargetEmitPrintFeatureMask(
      std::array<uint64_t, MAX_SUBTARGET_WORDS> const &Mask) const;
  virtual void subtargetEmitCPUKVHeader(std::string const &Target) const;
  virtual void subtargetEmitCPUKVEnd() const;
  virtual void subtargetEmitCPUKVPartI(StringRef const &Name) const;
  virtual void subtargetEmitCPUKVPartII() const;
  virtual void
  subtargetEmitCPUKVPartIII(std::string const &ProcModelName) const;
  virtual void subtargetEmitDBGMacrosBegin() const;
  virtual void subtargetEmitDBGMacrosEnd() const;
  virtual void subtargetEmitFunctionalItinaryUnits(
      CodeGenSchedModels const &SchedModels) const;
  virtual std::string const
  subtargetGetBeginStageTable(std::string const &TargetName) const;
  virtual std::string const
  subtargetGetBeginOperandCycleTable(std::string const &TargetName) const;
  virtual std::string const
  subtargetGetBeginBypassTable(std::string const &TargetName) const;
  virtual std::string const subtargetGetEndStageTable() const;
  virtual std::string const subtargetGetEndOperandCycleTable() const;
  virtual std::string const subtargetGetEndBypassTable() const;
  virtual void subtargetFormItineraryStageString(std::string const &Name,
                                                 Record *ItinData,
                                                 std::string &ItinString,
                                                 unsigned &NStages) const;
  virtual void
  subtargetFormItineraryOperandCycleString(Record *ItinData,
                                           std::string &ItinString,
                                           unsigned &NOperandCycles) const;
  virtual void
  subtargetFormItineraryBypassString(const std::string &Name, Record *ItinData,
                                     std::string &ItinString,
                                     unsigned NOperandCycles) const;
  virtual std::string
  subtargetGetStageEntryPartI(std::string const &ItinStageString,
                              unsigned StageCount) const;
  virtual std::string subtargetGetStageEntryPartII(unsigned StageCount,
                                                   unsigned NStages) const;
  virtual std::string subtargetGetStageEntryPartIII() const;
  virtual std::string subtargetGetOperandCycleEntryPartI(
      std::string const &ItinOperandCycleString) const;
  virtual std::string
  subtargetGetOperandCycleEntryPartII(unsigned OperandCycleCount,
                                      unsigned NOperandCycles) const;
  virtual std::string subtargetGetOperandCycleEntryPartIII(
      std::string const &OperandIdxComment) const;
  virtual std::string subtargetGetOperandCycleEntryPartIV(
      std::string const &ItinBypassString,
      std::string const &OperandIdxComment) const;
  virtual void subtargetEmitProcessorItineraryTable(
      std::string const &ItinsDefName, std::vector<InstrItinerary> &ItinList,
      CodeGenSchedModels const &SchedModels) const;
  virtual void subtargetEmitPreOperandTableComment() const;
  virtual void
  subtargetEmitSchedClassTables(SchedClassTablesT &SchedTables,
                                std::string const &TargetName,
                                CodeGenSchedModels const &SchedModels) const;
  virtual unsigned
  subtargetEmitRegisterFileTables(CodeGenProcModel const &ProcModel) const;
  virtual void subtargetEmitMCExtraProcInfoTableHeader(
      std::string const &ProcModelName) const;
  virtual void subtargetEmitMCExtraProcInfoTableEnd() const;
  virtual void subtargetEmitReorderBufferSize(int64_t ReorderBufferSize) const;
  virtual void subtargetEmitMaxRetirePerCycle(int64_t MaxRetirePerCycle) const;
  virtual void subtargetEmitRegisterFileInfo(CodeGenProcModel const &ProcModel,
                                             unsigned NumRegisterFiles,
                                             unsigned NumCostEntries) const;
  virtual void subtargetEmitResourceDescriptorLoadQueue(unsigned QueueID) const;
  virtual void
  subtargetEmitResourceDescriptorStoreQueue(unsigned QueueID) const;
  virtual void subtargetEmitProcessorResourceSubUnits(
      const CodeGenProcModel &ProcModel,
      CodeGenSchedModels const &SchedModels) const;
  virtual void
  subtargetEmitMCProcResourceDescHeader(std::string const &ProcModelName) const;
  virtual void subtargetEmitMCProcResourceDescEnd() const;
  virtual void
  subtargetEmitMCProcResourceDesc(Record const *PRDef, Record const *SuperDef,
                                  std::string const &ProcModelName,
                                  unsigned SubUnitsOffset, unsigned SuperIdx,
                                  unsigned NumUnits, int BufferSize, unsigned I,
                                  unsigned const SubUnitsBeginOffset) const;
  virtual void subtargetEmitProcessorProp(Record const *R, StringRef const Name,
                                          char Separator) const;
  virtual void subtargetEmitProcModelHeader(std::string const &ModelName) const;
  virtual void
  subtargetEmitProcModel(CodeGenProcModel const &PM,
                         CodeGenSchedModels const &SchedModels) const;
  virtual void subtargetEmitResolveVariantSchedClassImplHdr() const;
  virtual void subtargetEmitResolveVariantSchedClassImplEnd() const;
  virtual void subtargetEmitSchedClassSwitch() const;
  virtual void subtargetEmitSchedClassSwitchEnd() const;
  virtual void subtargetEmitSchedClassCase(unsigned VC,
                                           std::string const &SCName) const;
  virtual void
  subtargetEmitSchedClassProcGuard(unsigned Pi, bool OnlyExpandMCInstPredicates,
                                   std::string const &ModelName) const;
  virtual void subtargetEmitPredicates(
      CodeGenSchedTransition const &T, CodeGenSchedClass const &SC,
      bool (*IsTruePredicate)(Record const *Rec), int Indent = -1) const;
  virtual void subtargetEmitProcTransitionEnd() const;
  virtual void
  subtargetEmitSchedClassCaseEnd(CodeGenSchedClass const &SC) const;
  virtual void
  subtargetEmitSchedModelHelperEpilogue(bool ShouldReturnZero) const;
  virtual void
  subtargetEmitGenMCSubtargetInfoClass(std::string const &TargetName,
                                       bool OverrideGetHwMode) const;
  virtual void subtargetEmitMCSubtargetInfoImpl(std::string const &TargetName,
                                                unsigned NumFeatures,
                                                unsigned NumProcs,
                                                bool SchedModelHasItin) const;
  virtual void subtargetEmitIncludeSTIDesc() const;
  virtual void subtargetEmitDFAPacketizerClass(std::string const &TargetName,
                                               std::string const &ClassName,
                                               bool OverrideGetHwMode) const;
  virtual void subtargetEmitDFAPacketizerClassEnd() const;
  virtual void subtargetEmitSTICtor() const;
  virtual void subtargetEmitExternKVArrays(std::string const &TargetName,
                                           bool SchedModelsHasItin) const;
  virtual void subtargetEmitClassDefs(std::string const &TargetName,
                                      std::string const &ClassName,
                                      unsigned NumFeatures, unsigned NumProcs,
                                      bool SchedModelsHasItin) const;
  virtual void
  subtargetEmitResolveSchedClassHdr(std::string const &ClassName) const;
  virtual void
  subtargetEmitResolveSchedClassEnd(std::string const &ClassName) const;
  virtual void
  subtargetEmitResolveVariantSchedClass(std::string const &TargetName,
                                        std::string const &ClassName) const;
  virtual void subtargetEmitPredicateProlog(const RecordKeeper &Records) const;
  virtual void subtargetEmitParseFeaturesFunction(
      std::string const &TargetName,
      std::vector<Record *> const &Features) const;
  virtual void
  subtargetEmitExpandedSTIPreds(StringRef const &TargetName,
                                std::string const &ClassName,
                                CodeGenSchedModels const &SchedModels);
  virtual void subtargetPrepareSchedClassPreds(StringRef const &TargetName,
                                               bool OnlyExpandMCInstPredicates);
  virtual void
  subtargetEmitExpandedSTIPredsMCAnaDecl(StringRef const &TargetName,
                                         CodeGenSchedModels const &SchedModels);
  virtual void subtargetEmitExpandedSTIPredsMCAnaDefs(
      StringRef const &TargetName, std::string const &ClassPrefix,
      CodeGenSchedModels const &SchedModels) const;
  virtual void
  subtargetEmitExpandedSTIPredsHeader(StringRef const &TargetName,
                                      CodeGenSchedModels const &SchedModels);
  virtual void
  subtargetEmitStageAndSycleTables(std::string const &StageTable,
                                   std::string const &OperandCycleTable,
                                   std::string const &BypassTable) const;
  virtual void subtargetEmitDFASubtargetInfoImpl(std::string const &TargetName,
                                                 std::string const &ClassName,
                                                 unsigned NumFeatures,
                                                 unsigned NumProcs,
                                                 bool SchedModelHasItin) const;
  virtual void asmMatcherEmitSTFBitEnum(AsmMatcherInfo &Info) const;
  virtual void asmMatcherEmitComputeAssemblerAvailableFeatures(
      AsmMatcherInfo &Info, StringRef const &ClassName) const;

  //---------------------------
  // Backend: InstrInfoEmitter
  //---------------------------

  virtual void instrInfoEmitSourceFileHeader() const;
  virtual void instrInfoSetOperandInfoStr(
      std::string &Res, Record const *OpR,
      CGIOperandList::OperandInfo const &Op,
      CGIOperandList::ConstraintInfo const &Constraint) const;
  virtual void
  instrInfoPrintDefList(const std::vector<Record *> &Uses, unsigned Num,
                        std::string (*GetQualifiedName)(Record const *R)) const;
  virtual void
  instrInfoEmitOperandInfoTable(std::vector<std::string> const &OperandInfo,
                                unsigned N) const;
  virtual void instrInfoEmitMCInstrDescHdr(std::string TargetName) const;
  virtual void instrInfoEmitMCInstrDescEnd() const;
  virtual void instrInfoEmitRecord(CodeGenSchedModels const &SchedModels,
                                   CodeGenInstruction const &Inst, unsigned Num,
                                   int MinOperands) const;
  virtual void
  instrInfoEmitTargetIndepFlags(CodeGenInstruction const &Inst,
                                bool GetAllowRegisterRenaming) const;
  virtual void instrInfoEmitTSFFlags(uint64_t Value) const;
  virtual void instrInfoEmitUseDefsLists(
      std::map<std::vector<Record *>, unsigned> &EmittedLists,
      std::vector<Record *> const &ImplicitOps) const;
  virtual void
  instrInfoEmitOperandInfo(std::vector<std::string> const &OperandInfo,
                           OperandInfoMapTy const &OpInfo) const;
  virtual void instrInfoEmitRecordEnd(unsigned InstNum,
                                      std::string const &InstName) const;
  virtual void instrInfoEmitStringLiteralDef(
      std::string const &TargetName,
      SequenceToOffsetTable<std::string> InstrNames) const;
  virtual void instrInfoEmitInstrNameIndices(
      std::string const &TargetName,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      SequenceToOffsetTable<std::string> const &InstrNames) const;
  virtual void instrInfoEmitInstrDeprFeatures(
      std::string const &TargetName, std::string const &TargetNamespace,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      SequenceToOffsetTable<std::string> const &InstrNames) const;
  virtual void instrInfoEmitInstrComplexDeprInfos(
      std::string const &TargetName,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const;
  virtual void instrInfoEmitMCInstrInfoInitRoutine(
      std::string const &TargetName, unsigned NumberedInstrSize,
      bool HasDeprecationFeatures, bool HasComplexDeprecationInfos) const;
  virtual void instrInfoEmitClassStruct(std::string const &ClassName) const;
  virtual void instrInfoEmitTIIHelperMethod(StringRef const &TargetName,
                                            Record const *Rec,
                                            bool ExpandDefinition) const;
  virtual void instrInfoEmitExternArrays(std::string const &TargetName,
                                         bool HasDeprecationFeatures,
                                         bool HasComplexDeprecationInfos) const;
  virtual void instrInfoEmitMCInstrInfoInit(
      std::string const &TargetName, std::string const &ClassName,
      unsigned NumberedInstrSize, bool HasDeprecationFeatures,
      bool HasComplexDeprecationInfos) const;
  virtual void instrInfoEmitOperandEnum(
      std::map<std::string, unsigned> const &Operands) const;
  virtual void instrInfoEmitGetNamedOperandIdx(
      std::map<std::string, unsigned> const &Operands,
      OpNameMapTy const &OperandMap) const;
  virtual void instrInfoEmitOpTypeEnumPartI() const;
  virtual void instrInfoEmitOpTypeEnumPartII(StringRef const &OpName,
                                             unsigned EnumVal) const;
  virtual void instrInfoEmitOpTypeEnumPartIII() const;
  virtual void instrInfoEmitOpTypeOffsetTable(
      std::vector<int> OperandOffsets, unsigned OpRecSize,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const;
  virtual void instrInfoEmitOpcodeOpTypesTable(
      unsigned EnumVal, std::vector<Record *> const &OperandRecords,
      std::vector<int> OperandOffsets,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions) const;
  virtual void instrInfoEmitGetOpTypeHdr() const;
  virtual void instrInfoEmitGetOpTypeReturn() const;
  virtual void instrInfoEmitGetOpTypeUnreachable() const;
  virtual void instrInfoEmitGetOpTypeEnd() const;
  virtual void instrInfoEmitGetMemOpSizeHdr() const;
  virtual void instrInfoEmitGetOpMemSizeTbl(
      std::map<int, std::vector<StringRef>> const &SizeToOperandName) const;
  virtual std::string
  instrInfoGetInstMapEntry(StringRef const &Namespace,
                           StringRef const &InstrName) const;
  virtual void instrInfoEmitGetLogicalOpSizeHdr() const;
  virtual void instrInfoEmitGetLogicalOpSizeTable(
      size_t LogicalOpListSize,
      std::vector<const std::vector<unsigned> *> const &LogicalOpSizeList)
      const;
  virtual void instrInfoEmitGetLogicalOpSizeSwitch(
      std::map<unsigned, std::vector<std::string>> InstMap) const;
  virtual void instrInfoEmitGetLogicalOpSizeReturn() const;
  virtual void instrInfoEmitGetLogicalOpSizeEnd() const;
  virtual void instrInfoEmitGetLogicalOpIdx() const;
  virtual std::string
  instrInfoGetOpTypeListEntry(StringRef const &Namespace,
                              StringRef const &OpName) const;
  virtual void instrInfoEmitGetLogicalOpTypeHdr() const;
  virtual void instrInfoEmitGetLogicalOpTypeTable(
      size_t LogicalOpTypeListSize,
      std::vector<const std::vector<std::string> *> const &LogicalOpTypeList)
      const;
  virtual void instrInfoEmitGetLogicalOpTypeSwitch(
      std::map<unsigned, std::vector<std::string>> InstMap) const;
  virtual void instrInfoEmitGetLogicalOpTypeReturn() const;
  virtual void instrInfoEmitGetLogicalOpTypeEnd() const;
  virtual void instrInfoEmitDeclareMCInstFeatureClasses() const;
  virtual void instrInfoEmitPredFcnDecl(RecVec const &TIIPredicates) const;
  virtual void instrInfoEmitPredFcnImpl(StringRef const &TargetName,
                                        RecVec const &TIIPredicates);
  virtual void instrInfoEmitInstrPredVerifierIncludes() const;
  virtual void instrInfoEmitSubtargetFeatureBitEnumeration(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const;
  virtual void instrInfoEmitEmitSTFNameTable(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const;
  virtual void instrInfoEmitFeatureBitsEnum(
      std::vector<std::vector<Record *>> const &FeatureBitsets) const;
  virtual void instrInfoEmitFeatureBitsArray(
      std::vector<std::vector<Record *>> const &FeatureBitsets,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          &SubtargetFeatures) const;
  virtual void instrInfoEmitPredVerifier(
      std::vector<std::vector<Record *>> const &FeatureBitsets,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          &SubtargetFeatures,
      CodeGenTarget const &Target) const;
  virtual void instrInfoEmitEnums(CodeGenTarget const &Target,
                                  StringRef const &Namespace,
                                  CodeGenSchedModels const &SchedModels) const;
  virtual void instrInfoEmitTIIPredicates(StringRef const &TargetName,
                                          RecVec const &TIIPredicates,
                                          bool ExpandDefinition);
  virtual void instrInfoEmitComputeAssemblerAvailableFeatures(
      StringRef const &TargetName,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const;

  //--------------------------
  // Backend: AsmMatcher
  //--------------------------

  virtual void asmMatcherEmitSourceFileHeader(std::string const &Desc) const;
  virtual void asmMatcherEmitDeclarations(bool HasOptionalOperands,
                                          bool ReportMultipleNearMisses,
                                          bool HasOperandInfos) const;
  virtual void
  asmMatcherEmitOperandDiagTypes(std::set<StringRef> const Types) const;

  virtual void asmMatcherEmitGetSubtargetFeatureName(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          SubtargetFeatures) const;
  virtual void asmMatcherEmitConversionFunctionI(
      StringRef const &TargetName, StringRef const &ClassName,
      std::string const &TargetOperandClass, bool HasOptionalOperands,
      size_t MaxNumOperands) const;
  virtual void
  asmMatcherEmitConversionFunctionII(std::string const &EnumName,
                                     StringRef const &AsmMatchConverter) const;
  virtual void asmMatcherEmitConversionFunctionIII(
      std::string const &EnumName, std::string const TargetOperandClass,
      bool HasOptionalOperands, MatchableInfo::AsmOperand const &Op,
      MatchableInfo::ResOperand const &OpInfo) const;
  virtual void asmMatcherEmitConversionFunctionIV(std::string const &EnumName,
                                                  int64_t Val) const;
  virtual void asmMatcherEmitConversionFunctionV(std::string const &EnumName,
                                                 std::string const &Reg) const;
  virtual void asmMatcherEmitConversionFunctionVI() const;
  virtual void asmMatcherWriteCvtOSToOS() const;
  virtual void asmMatcherEmitOperandFunctionI(StringRef const &TargetName,
                                              StringRef const &ClassName) const;
  virtual void asmMatcherEmitOperandFunctionII(
      std::string const &EnumName, MatchableInfo::AsmOperand const &Op,
      MatchableInfo::ResOperand const &OpInfo) const;
  virtual void
  asmMatcherEmitOperandFunctionIII(std::string const &EnumName) const;
  virtual void
  asmMatcherEmitOperandFunctionIV(std::string const &EnumName) const;
  virtual void asmMatcherEmitOperandFunctionV() const;
  virtual void asmMatcherEmitTiedOperandEnum(
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const;
  virtual void asmMatcherWriteOpOSToOS() const;
  virtual void asmMatcherEmitTiedOpTable(
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const;
  virtual void asmMatcherEmitTiedOpEmptyTable() const;
  virtual void asmMatcherEmitOperandConvKindEnum(
      SmallSetVector<CachedHashString, 16> OperandConversionKinds) const;
  virtual void asmMatcherEmitInstrConvKindEnum(
      SmallSetVector<CachedHashString, 16> InstructionConversionKinds) const;
  virtual void asmMatcherEmitConversionTable(
      size_t MaxRowLength,
      std::vector<std::vector<uint8_t>> const ConversionTable,
      SmallSetVector<CachedHashString, 16> InstructionConversionKinds,
      SmallSetVector<CachedHashString, 16> OperandConversionKinds,
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const;
  virtual void asmMatcherEmitMatchClassKindEnum(
      std::forward_list<ClassInfo> const &Infos) const;
  virtual void
  asmMatcherEmitMatchClassDiagStrings(AsmMatcherInfo const &Info) const;
  virtual void asmMatcherEmitRegisterMatchErrorFunc(AsmMatcherInfo &Info) const;
  virtual void asmMatcherEmitIsSubclassI() const;
  virtual bool asmMatcherEmitIsSubclassII(bool EmittedSwitch,
                                          std::string const &Name) const;
  virtual void asmMatcherEmitIsSubclassIII(StringRef const &Name) const;
  virtual void
  asmMatcherEmitIsSubclassIV(std::vector<StringRef> const &SuperClasses) const;
  virtual void asmMatcherEmitIsSubclassV(bool EmittedSwitch) const;
  virtual void asmMatcherEmitValidateOperandClass(AsmMatcherInfo &Info) const;
  virtual void
  asmMatcherEmitMatchClassKindNames(std::forward_list<ClassInfo> &Infos) const;
  virtual void
  asmMatcherEmitAsmTiedOperandConstraints(CodeGenTarget &Target,
                                          AsmMatcherInfo &Info) const;
  virtual std::string
  getNameForFeatureBitset(const std::vector<Record *> &FeatureBitset) const;
  virtual void asmMatcherEmitFeatureBitsetEnum(
      std::vector<std::vector<Record *>> const FeatureBitsets) const;
  virtual void asmMatcherEmitFeatureBitsets(
      std::vector<std::vector<Record *>> const FeatureBitsets,
      AsmMatcherInfo const &Info) const;
  virtual void asmMatcherEmitMatchEntryStruct(
      unsigned MaxMnemonicIndex, unsigned NumConverters, size_t MaxNumOperands,
      std::vector<std::vector<Record *>> const FeatureBitsets,
      AsmMatcherInfo const &Info) const;
  virtual void asmMatcherEmitMatchFunction(
      CodeGenTarget const &Target, Record const *AsmParser,
      StringRef const &ClassName, bool HasMnemonicFirst,
      bool HasOptionalOperands, bool ReportMultipleNearMisses,
      bool HasMnemonicAliases, size_t MaxNumOperands, bool HasDeprecation,
      unsigned int VariantCount) const;
  virtual void asmMatcherEmitMnemonicSpellChecker(CodeGenTarget const &Target,
                                                  unsigned VariantCount) const;
  virtual void asmMatcherEmitMnemonicChecker(CodeGenTarget const &Target,
                                             unsigned VariantCount,
                                             bool HasMnemonicFirst,
                                             bool HasMnemonicAliases) const;
  virtual void asmMatcherEmitCustomOperandParsing(
      unsigned MaxMask, CodeGenTarget &Target, AsmMatcherInfo const &Info,
      StringRef ClassName, StringToOffsetTable &StringTable,
      unsigned MaxMnemonicIndex, unsigned MaxFeaturesIndex,
      bool HasMnemonicFirst, Record const &AsmParser) const;
  virtual void asmMatcherEmitIncludes() const;
  virtual void
  asmMatcherEmitMnemonicTable(StringToOffsetTable &StringTable) const;
  virtual void asmMatcherEmitMatchTable(CodeGenTarget const &Target,
                                        AsmMatcherInfo &Info,
                                        StringToOffsetTable &StringTable,
                                        unsigned VariantCount) const;
  virtual void asmMatcherEmitMatchRegisterName(
      Record const *AsmParser,
      std::vector<StringMatcher::StringPair> const Matches) const;
  virtual void asmMatcherEmitMatchTokenString(
      std::vector<StringMatcher::StringPair> const Matches) const;
  virtual void asmMatcherEmitMatchRegisterAltName(
      Record const *AsmParser,
      std::vector<StringMatcher::StringPair> const Matches) const;
  virtual void asmMatcherEmitMnemonicAliasVariant(
      std::vector<StringMatcher::StringPair> const &Cases,
      unsigned Indent) const;
  virtual void asmMatcherAppendMnemonicAlias(Record const *R,
                                             std::string const &FeatureMask,
                                             std::string &MatchCode) const;
  virtual void asmMatcherAppendMnemonic(Record const *R,
                                        std::string &MatchCode) const;
  virtual void asmMatcherAppendMnemonicAliasEnd(std::string &MatchCode) const;
  virtual void asmMatcherEmitApplyMnemonicAliasesI() const;
  virtual void
  asmMatcherEmitApplyMnemonicAliasesII(int AsmParserVariantNo) const;
  virtual void asmMatcherEmitApplyMnemonicAliasesIII() const;
  virtual void asmMatcherEmitApplyMnemonicAliasesIV() const;
  virtual void asmMatcherEmitApplyMnemonicAliasesV() const;

  //---------------------------
  // Backend: SearchableTables
  //---------------------------

  virtual void searchableTablesWriteFiles() const;
  virtual void searchableTablesEmitGenericEnum(const GenericEnum &Enum) const;
  virtual void searchableTablesEmitGenericTable(const GenericTable &Enum) const;
  virtual void searchableTablesEmitIfdef(const std::string Guard,
                                         StreamType ST = ST_NONE) const;
  virtual void searchableTablesEmitEndif(StreamType ST = ST_NONE) const;
  virtual void searchableTablesEmitUndef(std::string const &Guard) const;
  virtual void searchableTablesEmitLookupDeclaration(const GenericTable &Table,
                                                     const SearchIndex &Index,
                                                     StreamType ST);
  virtual std::string searchableTablesPrimaryRepresentation(
      SMLoc Loc, const GenericField &Field, Init *I,
      StringRef const &InstrinsicEnumName) const;
  virtual std::string searchableTablesSearchableFieldType(
      const GenericTable &Table, const SearchIndex &Index,
      const GenericField &Field, TypeContext Ctx) const;
  virtual void
  searchableTablesEmitKeyTypeStruct(const GenericTable &Table,
                                    const SearchIndex &Index) const;

  virtual void
  searchableTablesEmitIfFieldCase(const GenericField &Field,
                                  std::string const &FirstRepr,
                                  std::string const &LastRepr) const;
  virtual void searchableTablesEmitIsContiguousCase(StringRef const &IndexName,
                                                    const GenericTable &Table,
                                                    const SearchIndex &Index,
                                                    bool IsPrimary) const;
  virtual void searchableTablesEmitIndexArrayV() const;
  virtual void searchableTablesEmitIndexArrayIV(
      std::pair<Record *, unsigned> const &Entry) const;
  virtual void searchableTablesEmitIndexArrayIII(ListSeparator &LS,
                                                 std::string Repr) const;
  virtual void searchableTablesEmitIndexArrayII() const;
  virtual void searchableTablesEmitIndexArrayI() const;
  virtual void
  searchableTablesEmitIndexTypeStruct(const GenericTable &Table,
                                      const SearchIndex &Index) const;
  virtual void searchableTablesEmitReturns(const GenericTable &Table,
                                           const SearchIndex &Index,
                                           bool IsPrimary) const;
  virtual void
  searchableTablesEmitIndexLamda(const SearchIndex &Index,
                                 StringRef const &IndexName,
                                 StringRef const &IndexTypeName) const;
  virtual void searchableTablesEmitKeyArray(const GenericTable &Table,
                                            const SearchIndex &Index,
                                            bool IsPrimary) const;
  virtual void searchableTablesEmitMapI(const GenericTable &Table) const;
  virtual void searchableTablesEmitMapII() const;
  virtual void searchableTablesEmitMapIII(const GenericTable &Table,
                                          ListSeparator &LS,
                                          GenericField const &Field,
                                          StringRef &Intrinsic,
                                          Record *Entry) const;
  virtual void searchableTablesEmitMapIV(unsigned i) const;
  virtual void searchableTablesEmitMapV();
};

//==============================
//
// Implementation: Capstone
//
//==============================

/// Printer implementation of Capstone.
///
/// Output language: C
class PrinterCapstone : public PrinterLLVM {
  bool DoNotEmit = false;

public:
  using PrinterLLVM::PrinterLLVM;
  virtual void flushOS() const override { OS.flush(); }

  //--------------------------
  // General printing methods
  //--------------------------

  void emitNamespace(std::string const &Name, bool Begin,
                     std::string const &Comment) const override;
  void emitIfNotDef(std::string const &Name, bool Begin) const override;
  void emitIncludeToggle(std::string const &Name, bool Begin,
                         bool Newline = true,
                         bool UndefAtEnd = false) const override;

  //------------------------
  // Backend: RegisterInfo
  //------------------------

  void regInfoEmitSourceFileHeader(std::string const &Desc) const override;
  void regInfoEmitEnums(CodeGenTarget const &Target,
                        CodeGenRegBank const &Bank) const override;
  void regInfoEmitRegDiffLists(
      std::string const TargetName,
      SequenceToOffsetTable<DiffVec> const &DiffSeqs) const override;
  void regInfoEmitLaneMaskLists(
      std::string const TargetName,
      SequenceToOffsetTable<MaskVec> const &DiffSeqs) const override;
  void regInfoEmitSubRegIdxLists(
      std::string const TargetName,
      SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> const
          &SubRegIdxSeqs) const override;
  void regInfoEmitSubRegIdxSizes(
      std::string const TargetName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void regInfoEmitSubRegStrTable(
      std::string const TargetName,
      SequenceToOffsetTable<std::string> const &RegStrings) const override;
  void regInfoEmitRegDesc(
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
      SequenceToOffsetTable<std::string> const &RegStrings) const override;
  void regInfoEmitRegUnitRoots(std::string const TargetName,
                               CodeGenRegBank const &RegBank) const override;
  void
  regInfoEmitRegClasses(std::list<CodeGenRegisterClass> const &RegClasses,
                        SequenceToOffsetTable<std::string> &RegClassStrings,
                        CodeGenTarget const &Target) const override;
  void regInfoEmitStrLiteralRegClasses(
      std::string const TargetName,
      SequenceToOffsetTable<std::string> const &RegClassStrings) const override;
  void regInfoEmitMCRegClassesTable(
      std::string const TargetName,
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<std::string> &RegClassStrings) const override;
  void regInfoEmitRegEncodingTable(
      std::string const TargetName,
      std::deque<CodeGenRegister> const &Regs) const override;
  void regInfoEmitMCRegInfoInit(
      std::string const TargetName, CodeGenRegBank const &RegBank,
      std::deque<CodeGenRegister> const &Regs,
      std::list<CodeGenRegisterClass> const &RegClasses,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void regInfoEmitInfoDwarfRegs(StringRef const &Namespace,
                                DwarfRegNumsVecTy &DwarfRegNums,
                                unsigned MaxLength, bool IsCtor) const override;
  void regInfoEmitInfoDwarfRegsRev(StringRef const &Namespace,
                                   DwarfRegNumsVecTy &DwarfRegNums,
                                   unsigned MaxLength,
                                   bool IsCtor) const override;
  void regInfoEmitInfoRegMapping(StringRef const &Namespace, unsigned MaxLength,
                                 bool IsCtor) const override;
  void regInfoEmitHeaderIncludes() const override;
  void regInfoEmitHeaderExternRegClasses(
      std::list<CodeGenRegisterClass> const &RegClasses) const override;
  void regInfoEmitHeaderDecl(std::string const &TargetName,
                             std::string const &ClassName, bool SubRegsPresent,
                             bool DeclareGetPhysRegBaseClass) const override;
  void
  regInfoEmitExternRegClassesArr(std::string const &TargetName) const override;
  void regInfoEmitVTSeqs(
      SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs)
      const override;
  void regInfoEmitSubRegIdxTable(
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void regInfoEmitRegClassInfoTable(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<std::vector<MVT::SimpleValueType>> const &VTSeqs,
      CodeGenHwModes const &CGH, unsigned NumModes) const override;
  void regInfoEmitSubClassMaskTable(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SmallVector<IdxList, 8> &SuperRegIdxLists,
      SequenceToOffsetTable<IdxList, deref<std::less<>>> &SuperRegIdxSeqs,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices,
      BitVector &MaskBV) const override;
  void regInfoEmitSuperRegIdxSeqsTable(
      SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs)
      const override;
  void regInfoEmitSuperClassesTable(
      std::list<CodeGenRegisterClass> const &RegClasses) const override;
  void
  regInfoEmitRegClassMethods(std::list<CodeGenRegisterClass> const &RegClasses,
                             std::string const &TargetName) const override;
  void regInfomitRegClassInstances(
      std::list<CodeGenRegisterClass> const &RegClasses,
      SequenceToOffsetTable<IdxList, deref<std::less<>>> const &SuperRegIdxSeqs,
      SmallVector<IdxList, 8> const &SuperRegIdxLists,
      std::string const &TargetName) const override;
  void regInfoEmitRegClassTable(
      std::list<CodeGenRegisterClass> const &RegClasses) const override;
  void regInfoEmitCostPerUseTable(std::vector<unsigned> const &AllRegCostPerUse,
                                  unsigned NumRegCosts) const override;
  void regInfoEmitInAllocatableClassTable(
      llvm::BitVector const &InAllocClass) const override;
  void regInfoEmitRegExtraDesc(std::string const &TargetName,
                               unsigned NumRegCosts) const override;
  void regInfoEmitSubClassSubRegGetter(
      std::string const &ClassName, unsigned SubRegIndicesSize,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices,
      std::list<CodeGenRegisterClass> const &RegClasses,
      CodeGenRegBank &RegBank) const override;
  void regInfoEmitRegClassWeight(CodeGenRegBank const &RegBank,
                                 std::string const &ClassName) const override;
  void regInfoEmitRegUnitWeight(CodeGenRegBank const &RegBank,
                                std::string const &ClassName,
                                bool RegUnitsHaveUnitWeight) const override;
  void regInfoEmitGetNumRegPressureSets(std::string const &ClassName,
                                        unsigned NumSets) const override;
  void regInfoEmitGetRegPressureTables(CodeGenRegBank const &RegBank,
                                       std::string const &ClassName,
                                       unsigned NumSets) const override;
  void regInfoEmitRCSetsTable(
      std::string const &ClassName, unsigned NumRCs,
      SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
      std::vector<std::vector<int>> const &PSets) const override;
  void regInfoEmitGetRegUnitPressureSets(
      SequenceToOffsetTable<std::vector<int>> const &PSetsSeqs,
      CodeGenRegBank const &RegBank, std::string const &ClassName,
      std::vector<std::vector<int>> const &PSets) const override;
  void regInfoEmitExternTableDecl(std::string const &TargetName) const override;
  void
  regInfoEmitRegClassInit(std::string const &TargetName,
                          std::string const &ClassName,
                          CodeGenRegBank const &RegBank,
                          std::list<CodeGenRegisterClass> const &RegClasses,
                          std::deque<CodeGenRegister> const &Regs,
                          unsigned SubRegIndicesSize) const override;
  void regInfoEmitSaveListTable(Record const *CSRSet,
                                SetTheory::RecVec const *Regs) const override;
  void regInfoEmitRegMaskTable(std::string const &CSRSetName,
                               BitVector &Covered) const override;
  void regInfoEmitGetRegMasks(std::vector<Record *> const &CSRSets,
                              std::string const &ClassName) const override;
  void regInfoEmitGPRCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const override;
  void regInfoEmitFixedRegCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const override;
  void regInfoEmitArgRegCheck(
      std::string const &ClassName,
      std::list<CodeGenRegisterCategory> const &RegCategories) const override;
  void regInfoEmitGetRegMaskNames(std::vector<Record *> const &CSRSets,
                                  std::string const &ClassName) const override;
  void
  regInfoEmitGetFrameLowering(std::string const &TargetName) const override;
  void regInfoEmitComposeSubRegIndicesImplHead(
      std::string const &ClName) const override;
  void regInfoEmitComposeSubRegIndicesImplBody(
      SmallVector<SmallVector<CodeGenSubRegIndex *, 4>, 4> const &Rows,
      unsigned SubRegIndicesSize,
      SmallVector<unsigned, 4> const &RowMap) const override;
  void regInfoEmitLaneMaskComposeSeq(
      SmallVector<SmallVector<MaskRolPair, 1>, 4> const &Sequences,
      SmallVector<unsigned, 4> const &SubReg2SequenceIndexMap,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void regInfoEmitComposeSubRegIdxLaneMask(
      std::string const &ClName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void regInfoEmitComposeSubRegIdxLaneMaskRev(
      std::string const &ClName,
      std::deque<CodeGenSubRegIndex> const &SubRegIndices) const override;
  void
  regInfoEmitIsConstantPhysReg(std::deque<CodeGenRegister> const &Regs,
                               std::string const &ClassName) const override;

  //-------------------------
  // Backend: DecoderEmitter
  //-------------------------

  void decoderEmitterEmitOpDecoder(raw_ostream &DecoderOS,
                                   const OperandInfo &Op) const override;
  void
  decoderEmitterEmitOpBinaryParser(raw_ostream &DecoderOS,
                                   const OperandInfo &OpInfo) const override;
  bool decoderEmitterEmitPredicateMatchAux(const Init &Val, bool ParenIfBinOp,
                                           raw_ostream &OS) const override;
  bool decoderEmitterEmitPredicateMatch(raw_ostream &PredOS,
                                        const ListInit *Predicates,
                                        unsigned Opc) const override;
  void decoderEmitterEmitFieldFromInstruction() const override;
  void decoderEmitterEmitInsertBits() const override;
  void decoderEmitterEmitDecodeInstruction(bool IsVarLenInst) const override;
  void decoderEmitterEmitTable(
      DecoderTable &Table, unsigned BitWidth, StringRef Namespace,
      std::vector<EncodingAndInst> &NumberedEncodings) const override;
  void decoderEmitterEmitInstrLenTable(
      std::vector<unsigned> &InstrLen) const override;
  void decoderEmitterEmitPredicateFunction(PredicateSet &Predicates,
                                           unsigned Indentation) const override;
  void decoderEmitterEmitDecoderFunction(DecoderSet &Decoders,
                                         unsigned Indentation) const override;
  void decoderEmitterEmitIncludes() const override;
  void decoderEmitterEmitSourceFileHeader() const override;

  //-------------------------
  // Backend: AsmWriter
  //-------------------------

  void asmWriterEmitSourceFileHeader() const override;
  void asmWriterEmitGetMnemonic(std::string const &TargetName,
                                StringRef const &ClassName) const override;
  void asmWriterEmitAsmStrs(
      SequenceToOffsetTable<std::string> const &StrTable) const override;
  void asmWriterEmitMnemonicDecodeTable(
      unsigned const OpcodeInfoBits, unsigned BitsLeft,
      unsigned const &AsmStrBits,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      std::vector<uint64_t> const &OpcodeInfo) const override;
  void asmWriterEmitPrintInstruction(
      std::string const &TargetName,
      std::vector<std::vector<std::string>> &TableDrivenOperandPrinters,
      unsigned &BitsLeft, unsigned &AsmStrBits, StringRef const &ClassName,
      bool PassSubtarget) const override;
  void asmWriterEmitOpCases(
      std::vector<std::pair<std::string, AsmWriterOperand>> &OpsToPrint,
      bool PassSubtarget) const override;
  void asmWriterEmitInstrSwitch() const override;
  void asmWriterEmitCompoundClosure(unsigned Indent, bool Newline,
                                    bool Semicolon) const override;
  void asmWriterEmitInstruction(AsmWriterInst const &FirstInst,
                                std::vector<AsmWriterInst> const &SimilarInsts,
                                unsigned DifferingOperand,
                                bool PassSubtarget) const override;
  void asmWriterEmitGetRegNameAssert(std::string const &TargetName,
                                     StringRef const &ClassName,
                                     bool HasAltNames,
                                     unsigned RegSize) const override;
  void asmWriterEmitStringLiteralDef(
      SequenceToOffsetTable<std::string> const &StringTable,
      StringRef const &AltName) const override;
  void asmWriterEmitAltIdxSwitch(bool HasAltNames,
                                 std::vector<Record *> const &AltNameIndices,
                                 StringRef const &Namespace) const override;
  void asmWriterEmitRegAsmOffsets(
      unsigned RegSizes, SmallVector<std::string, 4> const &AsmNames,
      SequenceToOffsetTable<std::string> const &StringTable,
      StringRef const &AltName) const override;
  char const *asmWriterGetPatCondKIgnore() const override;
  char const *asmWriterGetPatCondKRegClass() const override;
  char const *asmWriterGetPatCondKTiedReg() const override;
  char const *asmWriterGetPatCondKCustom() const override;
  char const *asmWriterGetPatCondKImm() const override;
  char const *asmWriterGetPatCondKNoReg() const override;
  char const *asmWriterGetPatCondKReg() const override;
  char const *asmWriterGetPatCondKFeature() const override;
  char const *asmWriterGetPatCondKEndOrFeature() const override;
  char const *asmWriterGetPatOpcStart() const override;
  char const *asmWriterGetCondPatStart() const override;
  std::string asmWriterGetCond(std::string const &Cond) const override;
  char const *asmWriterGetPatternFormat() const override;
  char const *asmWriterGetOpcodeFormat() const override;
  void asmWriterEmitPrintAliasInstrHeader(std::string const &TargetName,
                                          StringRef const &ClassName,
                                          bool PassSubtarget) const override;
  void asmWriterEmitPrintAliasInstrBodyRetFalse() const override;
  void asmWriterEmitPrintAliasInstrBody(
      raw_string_ostream &OpcodeO, raw_string_ostream &PatternO,
      raw_string_ostream &CondO,
      std::vector<std::pair<uint32_t, std::string>> const &AsmStrings,
      std::vector<const Record *> const &MCOpPredicates,
      std::string const &TargetName, StringRef const &ClassName,
      bool PassSubtarget) const override;
  void asmWriterEmitDeclValid(std::string const &TargetName,
                              StringRef const &ClassName) const override;
  void asmWriterEmitPrintAliasOp(
      std::string const &TargetName, StringRef const &ClassName,
      std::vector<std::pair<std::string, bool>> const &PrintMethods,
      bool PassSubtarget) const override;
  void asmWriterEmitPrintMC(
      std::string const &TargetName, StringRef const &ClassName,
      std::vector<const Record *> const &MCOpPredicates) const override;

  //-------------------------
  // Backend: Subtarget
  //-------------------------

  void subtargetEmitSourceFileHeader() const override;
  void subtargetEmitFeatureEnum(DenseMap<Record *, unsigned> &FeatureMap,
                                std::vector<Record *> const &DefList,
                                unsigned N) const override;
  void subtargetEmitGetSTIMacro(StringRef const &Value,
                                StringRef const &Attribute) const override;
  void subtargetEmitHwModes(CodeGenHwModes const &CGH,
                            std::string const &ClassName) const override;
  void subtargetEmitFeatureKVHeader(std::string const &Target) const override;
  void subtargetEmitFeatureKVEnd() const override;
  void subtargetEmitFeatureKVPartI(std::string const &Target,
                                   StringRef const &CommandLineName,
                                   StringRef const &Name,
                                   StringRef const &Desc) const override;
  void subtargetEmitFeatureKVPartII() const override;
  void subtargetEmitPrintFeatureMask(
      std::array<uint64_t, MAX_SUBTARGET_WORDS> const &Mask) const override;
  void subtargetEmitCPUKVHeader(std::string const &Target) const override;
  void subtargetEmitCPUKVEnd() const override;
  void subtargetEmitCPUKVPartI(StringRef const &Name) const override;
  void subtargetEmitCPUKVPartII() const override;
  void
  subtargetEmitCPUKVPartIII(std::string const &ProcModelName) const override;
  void subtargetEmitDBGMacrosBegin() const override;
  void subtargetEmitDBGMacrosEnd() const override;
  void subtargetEmitFunctionalItinaryUnits(
      CodeGenSchedModels const &SchedModels) const override;
  std::string const
  subtargetGetBeginStageTable(std::string const &TargetName) const override;
  std::string const subtargetGetBeginOperandCycleTable(
      std::string const &TargetName) const override;
  std::string const
  subtargetGetBeginBypassTable(std::string const &TargetName) const override;
  std::string const subtargetGetEndStageTable() const override;
  std::string const subtargetGetEndOperandCycleTable() const override;
  std::string const subtargetGetEndBypassTable() const override;
  void subtargetFormItineraryStageString(std::string const &Name,
                                         Record *ItinData,
                                         std::string &ItinString,
                                         unsigned &NStages) const override;
  void subtargetFormItineraryOperandCycleString(
      Record *ItinData, std::string &ItinString,
      unsigned &NOperandCycles) const override;
  void
  subtargetFormItineraryBypassString(const std::string &Name, Record *ItinData,
                                     std::string &ItinString,
                                     unsigned NOperandCycles) const override;
  std::string subtargetGetStageEntryPartI(std::string const &ItinStageString,
                                          unsigned StageCount) const override;
  std::string subtargetGetStageEntryPartII(unsigned StageCount,
                                           unsigned NStages) const override;
  std::string subtargetGetStageEntryPartIII() const override;
  std::string subtargetGetOperandCycleEntryPartI(
      std::string const &ItinOperandCycleString) const override;
  std::string
  subtargetGetOperandCycleEntryPartII(unsigned OperandCycleCount,
                                      unsigned NOperandCycles) const override;
  std::string subtargetGetOperandCycleEntryPartIII(
      std::string const &OperandIdxComment) const override;
  std::string subtargetGetOperandCycleEntryPartIV(
      std::string const &ItinBypassString,
      std::string const &OperandIdxComment) const override;
  void subtargetEmitProcessorItineraryTable(
      std::string const &ItinsDefName, std::vector<InstrItinerary> &ItinList,
      CodeGenSchedModels const &SchedModels) const override;
  void subtargetEmitPreOperandTableComment() const override;
  void subtargetEmitSchedClassTables(
      SchedClassTablesT &SchedTables, std::string const &TargetName,
      CodeGenSchedModels const &SchedModels) const override;
  unsigned subtargetEmitRegisterFileTables(
      CodeGenProcModel const &ProcModel) const override;
  void subtargetEmitMCExtraProcInfoTableHeader(
      std::string const &ProcModelName) const override;
  void subtargetEmitMCExtraProcInfoTableEnd() const override;
  void subtargetEmitReorderBufferSize(int64_t ReorderBufferSize) const override;
  void subtargetEmitMaxRetirePerCycle(int64_t MaxRetirePerCycle) const override;
  void subtargetEmitRegisterFileInfo(CodeGenProcModel const &ProcModel,
                                     unsigned NumRegisterFiles,
                                     unsigned NumCostEntries) const override;
  void
  subtargetEmitResourceDescriptorLoadQueue(unsigned QueueID) const override;
  void
  subtargetEmitResourceDescriptorStoreQueue(unsigned QueueID) const override;
  void subtargetEmitProcessorResourceSubUnits(
      const CodeGenProcModel &ProcModel,
      CodeGenSchedModels const &SchedModels) const override;
  void subtargetEmitMCProcResourceDescHeader(
      std::string const &ProcModelName) const override;
  void subtargetEmitMCProcResourceDescEnd() const override;
  void subtargetEmitMCProcResourceDesc(
      Record const *PRDef, Record const *SuperDef,
      std::string const &ProcModelName, unsigned SubUnitsOffset,
      unsigned SuperIdx, unsigned NumUnits, int BufferSize, unsigned I,
      unsigned const SubUnitsBeginOffset) const override;
  void subtargetEmitProcessorProp(Record const *R, StringRef const Name,
                                  char Separator) const override;
  void
  subtargetEmitProcModelHeader(std::string const &ModelName) const override;
  void
  subtargetEmitProcModel(CodeGenProcModel const &PM,
                         CodeGenSchedModels const &SchedModels) const override;
  void subtargetEmitResolveVariantSchedClassImplHdr() const override;
  void subtargetEmitResolveVariantSchedClassImplEnd() const override;
  void subtargetEmitSchedClassSwitch() const override;
  void subtargetEmitSchedClassSwitchEnd() const override;
  void subtargetEmitSchedClassCase(unsigned VC,
                                   std::string const &SCName) const override;
  void
  subtargetEmitSchedClassProcGuard(unsigned Pi, bool OnlyExpandMCInstPredicates,
                                   std::string const &ModelName) const override;
  void subtargetEmitPredicates(CodeGenSchedTransition const &T,
                               CodeGenSchedClass const &SC,
                               bool (*IsTruePredicate)(Record const *Rec),
                               int Indent = -1) const override;
  void subtargetEmitProcTransitionEnd() const override;
  void
  subtargetEmitSchedClassCaseEnd(CodeGenSchedClass const &SC) const override;
  void
  subtargetEmitSchedModelHelperEpilogue(bool ShouldReturnZero) const override;
  void
  subtargetEmitGenMCSubtargetInfoClass(std::string const &TargetName,
                                       bool OverrideGetHwMode) const override;
  void subtargetEmitMCSubtargetInfoImpl(std::string const &TargetName,
                                        unsigned NumFeatures, unsigned NumProcs,
                                        bool SchedModelHasItin) const override;
  void subtargetEmitIncludeSTIDesc() const override;
  void subtargetEmitDFAPacketizerClass(std::string const &TargetName,
                                       std::string const &ClassName,
                                       bool OverrideGetHwMode) const override;
  void subtargetEmitDFAPacketizerClassEnd() const override;
  void subtargetEmitSTICtor() const override;
  void subtargetEmitExternKVArrays(std::string const &TargetName,
                                   bool SchedModelsHasItin) const override;
  void subtargetEmitClassDefs(std::string const &TargetName,
                              std::string const &ClassName,
                              unsigned NumFeatures, unsigned NumProcs,
                              bool SchedModelsHasItin) const override;
  void subtargetEmitResolveSchedClassHdr(
      std::string const &ClassName) const override;
  void subtargetEmitResolveSchedClassEnd(
      std::string const &ClassName) const override;
  void subtargetEmitResolveVariantSchedClass(
      std::string const &TargetName,
      std::string const &ClassName) const override;
  void subtargetEmitPredicateProlog(const RecordKeeper &Records) const override;
  void subtargetEmitParseFeaturesFunction(
      std::string const &TargetName,
      std::vector<Record *> const &Features) const override;
  void
  subtargetEmitExpandedSTIPreds(StringRef const &TargetName,
                                std::string const &ClassName,
                                CodeGenSchedModels const &SchedModels) override;
  void
  subtargetPrepareSchedClassPreds(StringRef const &TargetName,
                                  bool OnlyExpandMCInstPredicates) override;
  void subtargetEmitExpandedSTIPredsMCAnaDecl(
      StringRef const &TargetName,
      CodeGenSchedModels const &SchedModels) override;
  void subtargetEmitExpandedSTIPredsMCAnaDefs(
      StringRef const &TargetName, std::string const &ClassPrefix,
      CodeGenSchedModels const &SchedModels) const override;
  void subtargetEmitExpandedSTIPredsHeader(
      StringRef const &TargetName,
      CodeGenSchedModels const &SchedModels) override;
  void subtargetEmitStageAndSycleTables(
      std::string const &StageTable, std::string const &OperandCycleTable,
      std::string const &BypassTable) const override;
  void subtargetEmitDFASubtargetInfoImpl(std::string const &TargetName,
                                         std::string const &ClassName,
                                         unsigned NumFeatures,
                                         unsigned NumProcs,
                                         bool SchedModelHasItin) const override;

  //---------------------------
  // Backend: InstrInfoEmitter
  //---------------------------

  void instrInfoEmitSourceFileHeader() const override;
  void instrInfoSetOperandInfoStr(
      std::string &Res, Record const *OpR,
      CGIOperandList::OperandInfo const &Op,
      CGIOperandList::ConstraintInfo const &Constraint) const override;
  void instrInfoPrintDefList(
      const std::vector<Record *> &Uses, unsigned Num,
      std::string (*GetQualifiedName)(Record const *R)) const override;
  void
  instrInfoEmitOperandInfoTable(std::vector<std::string> const &OperandInfo,
                                unsigned N) const override;
  void instrInfoEmitMCInstrDescHdr(std::string TargetName) const override;
  void instrInfoEmitMCInstrDescEnd() const override;
  void instrInfoEmitRecord(CodeGenSchedModels const &SchedModels,
                           CodeGenInstruction const &Inst, unsigned Num,
                           int MinOperands) const override;
  void
  instrInfoEmitTargetIndepFlags(CodeGenInstruction const &Inst,
                                bool GetAllowRegisterRenaming) const override;
  void instrInfoEmitTSFFlags(uint64_t Value) const override;
  void instrInfoEmitUseDefsLists(
      std::map<std::vector<Record *>, unsigned> &EmittedLists,
      std::vector<Record *> const &ImplicitOps) const override;
  void instrInfoEmitOperandInfo(std::vector<std::string> const &OperandInfo,
                                OperandInfoMapTy const &OpInfo) const override;
  void instrInfoEmitRecordEnd(unsigned InstNum,
                              std::string const &InstName) const override;
  void instrInfoEmitStringLiteralDef(
      std::string const &TargetName,
      SequenceToOffsetTable<std::string> InstrNames) const override;
  void instrInfoEmitInstrNameIndices(
      std::string const &TargetName,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      SequenceToOffsetTable<std::string> const &InstrNames) const override;
  void instrInfoEmitInstrDeprFeatures(
      std::string const &TargetName, std::string const &TargetNamespace,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions,
      SequenceToOffsetTable<std::string> const &InstrNames) const override;
  void
  instrInfoEmitInstrComplexDeprInfos(std::string const &TargetName,
                                     ArrayRef<const CodeGenInstruction *> const
                                         &NumberedInstructions) const override;
  void instrInfoEmitMCInstrInfoInitRoutine(
      std::string const &TargetName, unsigned NumberedInstrSize,
      bool HasDeprecationFeatures,
      bool HasComplexDeprecationInfos) const override;
  void instrInfoEmitClassStruct(std::string const &ClassName) const override;
  void instrInfoEmitTIIHelperMethod(StringRef const &TargetName,
                                    Record const *Rec,
                                    bool ExpandDefinition) const override;
  void
  instrInfoEmitExternArrays(std::string const &TargetName,
                            bool HasDeprecationFeatures,
                            bool HasComplexDeprecationInfos) const override;
  void instrInfoEmitMCInstrInfoInit(
      std::string const &TargetName, std::string const &ClassName,
      unsigned NumberedInstrSize, bool HasDeprecationFeatures,
      bool HasComplexDeprecationInfos) const override;
  void instrInfoEmitOperandEnum(
      std::map<std::string, unsigned> const &Operands) const override;
  void instrInfoEmitGetNamedOperandIdx(
      std::map<std::string, unsigned> const &Operands,
      OpNameMapTy const &OperandMap) const override;
  void instrInfoEmitOpTypeEnumPartI() const override;
  void instrInfoEmitOpTypeEnumPartII(StringRef const &OpName,
                                     unsigned EnumVal) const override;
  void instrInfoEmitOpTypeEnumPartIII() const override;
  void instrInfoEmitOpTypeOffsetTable(std::vector<int> OperandOffsets,
                                      unsigned OpRecSize,
                                      ArrayRef<const CodeGenInstruction *> const
                                          &NumberedInstructions) const override;
  void instrInfoEmitOpcodeOpTypesTable(
      unsigned EnumVal, std::vector<Record *> const &OperandRecords,
      std::vector<int> OperandOffsets,
      ArrayRef<const CodeGenInstruction *> const &NumberedInstructions)
      const override;
  void instrInfoEmitGetOpTypeHdr() const override;
  void instrInfoEmitGetOpTypeReturn() const override;
  void instrInfoEmitGetOpTypeUnreachable() const override;
  void instrInfoEmitGetOpTypeEnd() const override;
  void instrInfoEmitGetMemOpSizeHdr() const override;
  void instrInfoEmitGetOpMemSizeTbl(std::map<int, std::vector<StringRef>> const
                                        &SizeToOperandName) const override;
  std::string
  instrInfoGetInstMapEntry(StringRef const &Namespace,
                           StringRef const &InstrName) const override;
  void instrInfoEmitGetLogicalOpSizeHdr() const override;
  void instrInfoEmitGetLogicalOpSizeTable(
      size_t LogicalOpListSize,
      std::vector<const std::vector<unsigned> *> const &LogicalOpSizeList)
      const override;
  void instrInfoEmitGetLogicalOpSizeSwitch(
      std::map<unsigned, std::vector<std::string>> InstMap) const override;
  void instrInfoEmitGetLogicalOpSizeReturn() const override;
  void instrInfoEmitGetLogicalOpSizeEnd() const override;
  void instrInfoEmitGetLogicalOpIdx() const override;
  std::string
  instrInfoGetOpTypeListEntry(StringRef const &Namespace,
                              StringRef const &OpName) const override;
  void instrInfoEmitGetLogicalOpTypeHdr() const override;
  void instrInfoEmitGetLogicalOpTypeTable(
      size_t LogicalOpTypeListSize,
      std::vector<const std::vector<std::string> *> const &LogicalOpTypeList)
      const override;
  void instrInfoEmitGetLogicalOpTypeSwitch(
      std::map<unsigned, std::vector<std::string>> InstMap) const override;
  void instrInfoEmitGetLogicalOpTypeReturn() const override;
  void instrInfoEmitGetLogicalOpTypeEnd() const override;
  void instrInfoEmitDeclareMCInstFeatureClasses() const override;
  void instrInfoEmitPredFcnDecl(RecVec const &TIIPredicates) const override;
  void instrInfoEmitPredFcnImpl(StringRef const &TargetName,
                                RecVec const &TIIPredicates) override;
  void instrInfoEmitInstrPredVerifierIncludes() const override;
  void instrInfoEmitSubtargetFeatureBitEnumeration(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const override;
  void instrInfoEmitEmitSTFNameTable(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const override;
  void instrInfoEmitFeatureBitsEnum(
      std::vector<std::vector<Record *>> const &FeatureBitsets) const override;
  void instrInfoEmitFeatureBitsArray(
      std::vector<std::vector<Record *>> const &FeatureBitsets,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          &SubtargetFeatures) const override;
  void instrInfoEmitPredVerifier(
      std::vector<std::vector<Record *>> const &FeatureBitsets,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          &SubtargetFeatures,
      CodeGenTarget const &Target) const override;
  void instrInfoEmitEnums(CodeGenTarget const &Target,
                          StringRef const &Namespace,
                          CodeGenSchedModels const &SchedModels) const override;
  void instrInfoEmitTIIPredicates(StringRef const &TargetName,
                                  RecVec const &TIIPredicates,
                                  bool ExpandDefinition) override;
  void instrInfoEmitComputeAssemblerAvailableFeatures(
      StringRef const &TargetName,
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID>
          &SubtargetFeatures) const override;

  //--------------------------
  // Backend: AsmMatcher
  //--------------------------

  void asmMatcherEmitSourceFileHeader(std::string const &Desc) const override;
  void asmMatcherEmitDeclarations(bool HasOptionalOperands,
                                  bool ReportMultipleNearMisses,
                                  bool HasOperandInfos) const override;
  void asmMatcherEmitOperandDiagTypes(
      std::set<StringRef> const Types) const override;

  void asmMatcherEmitGetSubtargetFeatureName(
      std::map<Record *, SubtargetFeatureInfo, LessRecordByID> const
          SubtargetFeatures) const override;
  void asmMatcherEmitConversionFunctionI(StringRef const &TargetName,
                                         StringRef const &ClassName,
                                         std::string const &TargetOperandClass,
                                         bool HasOptionalOperands,
                                         size_t MaxNumOperands) const override;
  void asmMatcherEmitConversionFunctionII(
      std::string const &EnumName,
      StringRef const &AsmMatchConverter) const override;
  void asmMatcherEmitConversionFunctionIII(
      std::string const &EnumName, std::string const TargetOperandClass,
      bool HasOptionalOperands, MatchableInfo::AsmOperand const &Op,
      MatchableInfo::ResOperand const &OpInfo) const override;
  void asmMatcherEmitConversionFunctionIV(std::string const &EnumName,
                                          int64_t Val) const override;
  void asmMatcherEmitConversionFunctionV(std::string const &EnumName,
                                         std::string const &Reg) const override;
  void asmMatcherEmitConversionFunctionVI() const override;
  void asmMatcherWriteCvtOSToOS() const override;
  void
  asmMatcherEmitOperandFunctionI(StringRef const &TargetName,
                                 StringRef const &ClassName) const override;
  void asmMatcherEmitOperandFunctionII(
      std::string const &EnumName, MatchableInfo::AsmOperand const &Op,
      MatchableInfo::ResOperand const &OpInfo) const override;
  void
  asmMatcherEmitOperandFunctionIII(std::string const &EnumName) const override;
  void
  asmMatcherEmitOperandFunctionIV(std::string const &EnumName) const override;
  void asmMatcherEmitOperandFunctionV() const override;
  void asmMatcherEmitTiedOperandEnum(
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const override;
  void asmMatcherWriteOpOSToOS() const override;
  void asmMatcherEmitTiedOpTable(
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const override;
  void asmMatcherEmitTiedOpEmptyTable() const override;
  void asmMatcherEmitOperandConvKindEnum(
      SmallSetVector<CachedHashString, 16> OperandConversionKinds)
      const override;
  void asmMatcherEmitInstrConvKindEnum(
      SmallSetVector<CachedHashString, 16> InstructionConversionKinds)
      const override;
  void asmMatcherEmitConversionTable(
      size_t MaxRowLength,
      std::vector<std::vector<uint8_t>> const ConversionTable,
      SmallSetVector<CachedHashString, 16> InstructionConversionKinds,
      SmallSetVector<CachedHashString, 16> OperandConversionKinds,
      std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
          TiedOperandsEnumMap) const override;
  void asmMatcherEmitMatchClassKindEnum(
      std::forward_list<ClassInfo> const &Infos) const override;
  void asmMatcherEmitMatchClassDiagStrings(
      AsmMatcherInfo const &Info) const override;
  void
  asmMatcherEmitRegisterMatchErrorFunc(AsmMatcherInfo &Info) const override;
  void asmMatcherEmitIsSubclassI() const override;
  bool asmMatcherEmitIsSubclassII(bool EmittedSwitch,
                                  std::string const &Name) const override;
  void asmMatcherEmitIsSubclassIII(StringRef const &Name) const override;
  void asmMatcherEmitIsSubclassIV(
      std::vector<StringRef> const &SuperClasses) const override;
  void asmMatcherEmitIsSubclassV(bool EmittedSwitch) const override;
  void asmMatcherEmitValidateOperandClass(AsmMatcherInfo &Info) const override;
  void asmMatcherEmitMatchClassKindNames(
      std::forward_list<ClassInfo> &Infos) const override;
  void
  asmMatcherEmitAsmTiedOperandConstraints(CodeGenTarget &Target,
                                          AsmMatcherInfo &Info) const override;
  std::string getNameForFeatureBitset(
      const std::vector<Record *> &FeatureBitset) const override;
  void asmMatcherEmitFeatureBitsetEnum(
      std::vector<std::vector<Record *>> const FeatureBitsets) const override;
  void asmMatcherEmitFeatureBitsets(
      std::vector<std::vector<Record *>> const FeatureBitsets,
      AsmMatcherInfo const &Info) const override;
  void asmMatcherEmitMatchEntryStruct(
      unsigned MaxMnemonicIndex, unsigned NumConverters, size_t MaxNumOperands,
      std::vector<std::vector<Record *>> const FeatureBitsets,
      AsmMatcherInfo const &Info) const override;
  void asmMatcherEmitMatchFunction(
      CodeGenTarget const &Target, Record const *AsmParser,
      StringRef const &ClassName, bool HasMnemonicFirst,
      bool HasOptionalOperands, bool ReportMultipleNearMisses,
      bool HasMnemonicAliases, size_t MaxNumOperands, bool HasDeprecation,
      unsigned int VariantCount) const override;
  void asmMatcherEmitMnemonicSpellChecker(CodeGenTarget const &Target,
                                          unsigned VariantCount) const override;
  void asmMatcherEmitMnemonicChecker(CodeGenTarget const &Target,
                                     unsigned VariantCount,
                                     bool HasMnemonicFirst,
                                     bool HasMnemonicAliases) const override;
  void asmMatcherEmitCustomOperandParsing(
      unsigned MaxMask, CodeGenTarget &Target, AsmMatcherInfo const &Info,
      StringRef ClassName, StringToOffsetTable &StringTable,
      unsigned MaxMnemonicIndex, unsigned MaxFeaturesIndex,
      bool HasMnemonicFirst, Record const &AsmParser) const override;
  void asmMatcherEmitIncludes() const override;
  void
  asmMatcherEmitMnemonicTable(StringToOffsetTable &StringTable) const override;
  void asmMatcherEmitMatchTable(CodeGenTarget const &Target,
                                AsmMatcherInfo &Info,
                                StringToOffsetTable &StringTable,
                                unsigned VariantCount) const override;
  void asmMatcherEmitMatchRegisterName(
      Record const *AsmParser,
      std::vector<StringMatcher::StringPair> const Matches) const override;
  void asmMatcherEmitMatchTokenString(
      std::vector<StringMatcher::StringPair> const Matches) const override;
  void asmMatcherEmitMatchRegisterAltName(
      Record const *AsmParser,
      std::vector<StringMatcher::StringPair> const Matches) const override;
  void asmMatcherEmitMnemonicAliasVariant(
      std::vector<StringMatcher::StringPair> const &Cases,
      unsigned Indent) const override;
  void asmMatcherAppendMnemonicAlias(Record const *R,
                                     std::string const &FeatureMask,
                                     std::string &MatchCode) const override;
  void asmMatcherAppendMnemonic(Record const *R,
                                std::string &MatchCode) const override;
  void asmMatcherAppendMnemonicAliasEnd(std::string &MatchCode) const override;
  void asmMatcherEmitApplyMnemonicAliasesI() const override;
  void
  asmMatcherEmitApplyMnemonicAliasesII(int AsmParserVariantNo) const override;
  void asmMatcherEmitApplyMnemonicAliasesIII() const override;
  void asmMatcherEmitApplyMnemonicAliasesIV() const override;
  void asmMatcherEmitApplyMnemonicAliasesV() const override;
  void asmMatcherEmitSTFBitEnum(AsmMatcherInfo &Info) const override;
  void asmMatcherEmitComputeAssemblerAvailableFeatures(
      AsmMatcherInfo &Info, StringRef const &ClassName) const override;

  //---------------------------
  // Backend: SearchableTables
  //---------------------------

  raw_string_ostream &searchableTablesGetOS(StreamType G) const;
  void searchableTablesWriteFiles() const override;
  void searchableTablesEmitGenericEnum(const GenericEnum &Enum) const override;
  void
  searchableTablesEmitGenericTable(const GenericTable &Enum) const override;
  void searchableTablesEmitIfdef(const std::string Guard,
                                 StreamType ST = ST_NONE) const override;
  void searchableTablesEmitEndif(StreamType ST = ST_NONE) const override;
  void searchableTablesEmitUndef(std::string const &Guard) const override;
  void searchableTablesEmitLookupDeclaration(const GenericTable &Table,
                                             const SearchIndex &Index,
                                             StreamType ST) override;
  std::string searchableTablesPrimaryRepresentation(
      SMLoc Loc, const GenericField &Field, Init *I,
      StringRef const &InstrinsicEnumName) const override;
  std::string searchableTablesSearchableFieldType(
      const GenericTable &Table, const SearchIndex &Index,
      const GenericField &Field, TypeContext Ctx) const override;
  void
  searchableTablesEmitKeyTypeStruct(const GenericTable &Table,
                                    const SearchIndex &Index) const override;

  void
  searchableTablesEmitIfFieldCase(const GenericField &Field,
                                  std::string const &FirstRepr,
                                  std::string const &LastRepr) const override;
  void searchableTablesEmitIsContiguousCase(StringRef const &IndexName,
                                            const GenericTable &Table,
                                            const SearchIndex &Index,
                                            bool IsPrimary) const override;
  void searchableTablesEmitIndexArrayV() const override;
  void searchableTablesEmitIndexArrayIV(
      std::pair<Record *, unsigned> const &Entry) const override;
  void searchableTablesEmitIndexArrayIII(ListSeparator &LS,
                                         std::string Repr) const override;
  void searchableTablesEmitIndexArrayII() const override;
  void searchableTablesEmitIndexArrayI() const override;
  void
  searchableTablesEmitIndexTypeStruct(const GenericTable &Table,
                                      const SearchIndex &Index) const override;
  void searchableTablesEmitReturns(const GenericTable &Table,
                                   const SearchIndex &Index,
                                   bool IsPrimary) const override;
  void
  searchableTablesEmitIndexLamda(const SearchIndex &Index,
                                 StringRef const &IndexName,
                                 StringRef const &IndexTypeName) const override;
  void searchableTablesEmitKeyArray(const GenericTable &Table,
                                    const SearchIndex &Index,
                                    bool IsPrimary) const override;
  void searchableTablesEmitMapI(const GenericTable &Table) const override;
  void searchableTablesEmitMapII() const override;
  void searchableTablesEmitMapIII(const GenericTable &Table, ListSeparator &LS,
                                  GenericField const &Field,
                                  StringRef &Intrinsic,
                                  Record *Entry) const override;
  void searchableTablesEmitMapIV(unsigned i) const override;
  void searchableTablesEmitMapV() override;
};

} // end namespace llvm

#endif // LLVM_UTILS_TABLEGEN_PRINTER_H
