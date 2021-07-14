//
// Created by Phosphorus15 on 2021/5/14.
// Machine Code Description Module
//

typedef SmallVector<uint16_t, 4> DiffVec;
typedef SmallVector<LaneBitmask, 4> MaskVec;
#include "../SequenceToOffsetTable.h"

static DiffVec &diffEncode(DiffVec &V, unsigned InitVal,
                           SparseBitVector<> List) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);

  for (uint16_t Cur : List) {
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

template <typename Iter>
static DiffVec &diffEncode(DiffVec &V, unsigned InitVal, Iter Begin, Iter End) {
  assert(V.empty() && "Clear DiffVec before diffEncode.");
  uint16_t Val = uint16_t(InitVal);
  for (Iter I = Begin; I != End; ++I) {
    uint16_t Cur = (*I)->EnumValue;
    V.push_back(Cur - Val);
    Val = Cur;
  }
  return V;
}

static void printDiff16(raw_ostream &OS, uint16_t Val) { OS << Val; }

static void printMask(raw_ostream &OS, LaneBitmask Val) {
  OS << "LaneBitmask(0x" << PrintLaneMask(Val) << ')';
}

static void printSubRegIndex(raw_ostream &OS, const CodeGenSubRegIndex *Idx) {
  OS << Idx->EnumValue;
}

static void printBitVectorAsHex(raw_ostream &OS, const BitVector &Bits,
                                unsigned Width) {
  assert(Width <= 32 && "Width too large");
  unsigned Digits = (Width + 3) / 4;
  for (unsigned i = 0, e = Bits.size(); i < e; i += Width) {
    unsigned Value = 0;
    for (unsigned j = 0; j != Width && i + j != e; ++j)
      Value |= Bits.test(i + j) << j;
    OS << format("0x%0*x, ", Digits, Value);
  }
}

class BitVectorEmitter {
  BitVector Values;

public:
  void add(unsigned v) {
    if (v >= Values.size())
      Values.resize(((v / 8) + 1) * 8); // Round up to the next byte.
    Values[v] = true;
  }

  void print(raw_ostream &OS) { printBitVectorAsHex(OS, Values, 8); }
};

void CapstoneGenInfo::runMCDesc(raw_ostream &OS, CodeGenTarget &Target,
                                CodeGenRegBank &RegBank) {

  OS << "\n#ifdef GET_REGINFO_MC_DESC\n";
  OS << "#undef GET_REGINFO_MC_DESC\n\n";

  const auto &Regs = RegBank.getRegisters();

  //  auto &SubRegIndices = RegBank.getSubRegIndices();
  // The lists of sub-registers and super-registers go in the same array.  That
  // allows us to share suffixes.
  typedef std::vector<const CodeGenRegister *> RegVec;

  // Differentially encoded lists.
  SequenceToOffsetTable<DiffVec> DiffSeqs;
  SmallVector<DiffVec, 4> SubRegLists(Regs.size());
  SmallVector<DiffVec, 4> SuperRegLists(Regs.size());
  SmallVector<DiffVec, 4> RegUnitLists(Regs.size());
  SmallVector<unsigned, 4> RegUnitInitScale(Regs.size());

  // List of lane masks accompanying register unit sequences.
  SequenceToOffsetTable<MaskVec> LaneMaskSeqs;
  SmallVector<MaskVec, 4> RegUnitLaneMasks(Regs.size());

  // Keep track of sub-register names as well. These are not differentially
  // encoded.
  typedef SmallVector<const CodeGenSubRegIndex *, 4> SubRegIdxVec;
  SequenceToOffsetTable<SubRegIdxVec, deref<std::less<>>> SubRegIdxSeqs;
  SmallVector<SubRegIdxVec, 4> SubRegIdxLists(Regs.size());

  SequenceToOffsetTable<std::string> RegStrings;

  // Precompute register lists for the SequenceToOffsetTable.
  unsigned i = 0;
  for (auto I = Regs.begin(), E = Regs.end(); I != E; ++I, ++i) {
    const auto &Reg = *I;
    RegStrings.add(std::string(Reg.getName()));

    // Compute the ordered sub-register list.
    SetVector<const CodeGenRegister *> SR;
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

  OS << "\n\n";

  const std::string &TargetName = std::string(Target.getName());

  // Emit the shared table of differential lists.
  OS << "static const MCPhysReg " << TargetName << "RegDiffLists[] = {\n";
  DiffSeqs.emit(OS, printDiff16);
  OS << "};\n\n";

  // Emit the shared table of regunit lane mask sequences.
  //  OS << "extern const LaneBitmask " << TargetName << "LaneMaskLists[] =
  //  {\n";
  //  LaneMaskSeqs.emit(OS, printMask, "LaneBitmask::getAll()");
  //  OS << "};\n\n";

  // Emit the table of sub-register indexes.
  OS << "static const uint16_t " << TargetName << "SubRegIdxLists[] = {\n";
  SubRegIdxSeqs.emit(OS, printSubRegIndex);
  OS << "};\n\n";

  // Emit the table of sub-register index sizes.
  //  OS << "extern const MCRegisterInfo::SubRegCoveredBits "
  //     << TargetName << "SubRegIdxRanges[] = {\n";
  //  OS << "  { " << (uint16_t)-1 << ", " << (uint16_t)-1 << " },\n";
  //  for (const auto &Idx : SubRegIndices) {
  //    OS << "  { " << Idx.Offset << ", " << Idx.Size << " },\t// "
  //       << Idx.getName() << "\n";
  //  }
  //  OS << "};\n\n";

  // Emit the string table.
  RegStrings.layout();
  RegStrings.emitStringLiteralDef(OS, Twine("static const char ") + TargetName +
                                          "RegStrings[]");

  OS << "static const MCRegisterDesc " << TargetName
     << "RegDesc[] = { // Descriptors\n";
  OS << "  { " << RegStrings.get("") << ", 0, 0, 0, 0, 0 },\n";

  // Emit the register descriptors now.
  i = 0;
  for (const auto &Reg : Regs) {
    OS << "  { " << RegStrings.get(std::string(Reg.getName())) << ", "
       << DiffSeqs.get(SubRegLists[i]) << ", " << DiffSeqs.get(SuperRegLists[i])
       << ", " << SubRegIdxSeqs.get(SubRegIdxLists[i]) << ", "
       << (DiffSeqs.get(RegUnitLists[i]) * 16 + RegUnitInitScale[i]) << ", "
       << LaneMaskSeqs.get(RegUnitLaneMasks[i]) << " },\n";
    ++i;
  }
  OS << "};\n\n"; // End of register descriptors...

  // Emit the table of register unit roots. Each regunit has one or two root
  // registers.
  //  OS << "static const MCPhysReg " << TargetName << "RegUnitRoots[][2] =
  //  {\n";
  //  for (unsigned i = 0, e = RegBank.getNumNativeRegUnits(); i != e; ++i) {
  //    ArrayRef<const CodeGenRegister*> Roots =
  //    RegBank.getRegUnit(i).getRoots();
  //    assert(!Roots.empty() && "All regunits must have a root register.");
  //    assert(Roots.size() <= 2 && "More than two roots not supported yet.");
  //    OS << "  { ";
  //    ListSeparator LS;
  //    for (const CodeGenRegister *R : Roots)
  //      OS << LS << getQualifiedName(R->TheDef);
  //    OS << " },\n";
  //  }
  //  OS << "};\n\n";

  const auto &RegisterClasses = RegBank.getRegClasses();

  SequenceToOffsetTable<std::string> RegClassStrings;

  // Emit the register enum value arrays for each RegisterClass
  for (const auto &RC : RegisterClasses) {
    ArrayRef<Record *> Order = RC.getOrder();

    // Give the register class a legal C name if it's anonymous.
    const std::string &Name = RC.getName();

    RegClassStrings.add(Name);

    StringRef Namespace = RC.Namespace;

    // Emit the register list now.
    OS << "  // " << Name << " Register Class...\n"
       << "  static const MCPhysReg " << Name << "[] = {\n    ";
    for (Record *Reg : Order) {
      OS << Namespace << "_" << Reg->getName().str() << ", ";
    }
    OS << "\n  };\n\n";

    OS << "  // " << Name << " Bit set.\n"
       << "  const uint8_t " << Name << "Bits[] = {\n    ";
    BitVectorEmitter BVE;
    for (Record *Reg : Order) {
      BVE.add(Target.getRegBank().getReg(Reg)->EnumValue);
    }
    BVE.print(OS);
    OS << "\n  };\n\n";
  }
  OS << "// end of register classes misc\n\n";

  RegClassStrings.layout();
  RegClassStrings.emitStringLiteralDef(
      OS, Twine("static const char ") + TargetName + "RegClassStrings[]");

  OS << "static const MCRegisterClass " << TargetName
     << "MCRegisterClasses[] = {\n";

  for (const auto &RC : RegisterClasses) {
    assert(isInt<8>(RC.CopyCost) && "Copy cost too large.");
    uint32_t RegSize = 0;
    if (RC.RSI.isSimple())
      RegSize = RC.RSI.getSimple().RegSize;
    OS << "  { " << RC.getName() << ", " << RC.getName() << "Bits, "
       << "sizeof(" << RC.getName() << "Bits)"
       << " },\n";
  }

  OS << "};\n\n";

  // FIXME we don't need this .. probably EmitRegMappingTables(OS, Regs, false);

  // Emit Reg encoding table
  //  OS << "extern const uint16_t " << TargetName;
  //  OS << "RegEncodingTable[] = {\n";
  //  // Add entry for NoRegister
  //  OS << "  0,\n";
  //  for (const auto &RE : Regs) {
  //    Record *Reg = RE.TheDef;
  //    BitsInit *BI = Reg->getValueAsBitsInit("HWEncoding");
  //    uint64_t Value = 0;
  //    for (unsigned b = 0, be = BI->getNumBits(); b != be; ++b) {
  //      if (BitInit *B = dyn_cast<BitInit>(BI->getBit(b)))
  //        Value |= (uint64_t)B->getValue() << b;
  //    }
  //    OS << "  " << Value << ",\n";
  //  }
  //  OS << "};\n";       // End of HW encoding table

  // MCRegisterInfo initialization routine.
  //  OS << "static inline void Init" << TargetName
  //     << "MCRegisterInfo(MCRegisterInfo *RI, unsigned RA, "
  //     << "unsigned DwarfFlavour = 0, unsigned EHFlavour = 0, unsigned PC = 0)
  //     "
  //        "{\n"
  //     << "  RI->InitMCRegisterInfo(" << TargetName << "RegDesc, "
  //     << Regs.size() + 1 << ", RA, PC, " << TargetName << "MCRegisterClasses,
  //     "
  //     << RegisterClasses.size() << ", " << TargetName << "RegUnitRoots, "
  //     << RegBank.getNumNativeRegUnits() << ", " << TargetName <<
  //     "RegDiffLists, "
  //     << TargetName << "LaneMaskLists, " << TargetName << "RegStrings, "
  //     << TargetName << "RegClassStrings, " << TargetName << "SubRegIdxLists,
  //     "
  //     << (std::distance(SubRegIndices.begin(), SubRegIndices.end()) + 1) <<
  //     ",\n"
  //     << TargetName << "SubRegIdxRanges, " << TargetName
  //     << "RegEncodingTable);\n\n";
  //
  //  //FIXME we don't need this.. probably EmitRegMapping(OS, Regs, false);
  //
  //  OS << "}\n\n";
  //
  //  OS << "} // end namespace llvm\n\n";
  OS << "#endif // GET_REGINFO_MC_DESC\n\n";
}
