/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Target Register Enum Values                                                *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/


#ifdef GET_REGINFO_ENUM
#undef GET_REGINFO_ENUM

namespace llvm {

class MCRegisterClass;
extern const MCRegisterClass AlphaMCRegisterClasses[];

namespace Alpha {
enum {
  NoRegister,
  F0 = 1,
  F1 = 2,
  F2 = 3,
  F3 = 4,
  F4 = 5,
  F5 = 6,
  F6 = 7,
  F7 = 8,
  F8 = 9,
  F9 = 10,
  F10 = 11,
  F11 = 12,
  F12 = 13,
  F13 = 14,
  F14 = 15,
  F15 = 16,
  F16 = 17,
  F17 = 18,
  F18 = 19,
  F19 = 20,
  F20 = 21,
  F21 = 22,
  F22 = 23,
  F23 = 24,
  F24 = 25,
  F25 = 26,
  F26 = 27,
  F27 = 28,
  F28 = 29,
  F29 = 30,
  F30 = 31,
  F31 = 32,
  R0 = 33,
  R1 = 34,
  R2 = 35,
  R3 = 36,
  R4 = 37,
  R5 = 38,
  R6 = 39,
  R7 = 40,
  R8 = 41,
  R9 = 42,
  R10 = 43,
  R11 = 44,
  R12 = 45,
  R13 = 46,
  R14 = 47,
  R15 = 48,
  R16 = 49,
  R17 = 50,
  R18 = 51,
  R19 = 52,
  R20 = 53,
  R21 = 54,
  R22 = 55,
  R23 = 56,
  R24 = 57,
  R25 = 58,
  R26 = 59,
  R27 = 60,
  R28 = 61,
  R29 = 62,
  R30 = 63,
  R31 = 64,
  NUM_TARGET_REGS // 65
};
} // end namespace Alpha

// Register classes

namespace Alpha {
enum {
  F4RCRegClassID = 0,
  F8RCRegClassID = 1,
  GPRCRegClassID = 2,

};
} // end namespace Alpha

// Register pressure sets enum.
namespace Alpha {
enum RegisterPressureSets {
  F4RC = 0,
  GPRC = 1,
};
} // end namespace Alpha

} // end namespace llvm

#endif // GET_REGINFO_ENUM

/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* MC Register Information                                                    *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifdef GET_REGINFO_MC_DESC
#undef GET_REGINFO_MC_DESC

namespace llvm {

extern const MCPhysReg AlphaRegDiffLists[] = {
  /* 0 */ 65535, 0,
};

extern const LaneBitmask AlphaLaneMaskLists[] = {
  /* 0 */ LaneBitmask(0x0000000000000000), LaneBitmask::getAll(),
};

extern const uint16_t AlphaSubRegIdxLists[] = {
  /* 0 */ 0,
};

extern const MCRegisterInfo::SubRegCoveredBits AlphaSubRegIdxRanges[] = {
  { 65535, 65535 },
};


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Woverlength-strings"
#endif
extern const char AlphaRegStrings[] = {
  /* 0 */ "F10\0"
  /* 4 */ "R10\0"
  /* 8 */ "F20\0"
  /* 12 */ "R20\0"
  /* 16 */ "F30\0"
  /* 20 */ "R30\0"
  /* 24 */ "F0\0"
  /* 27 */ "R0\0"
  /* 30 */ "F11\0"
  /* 34 */ "R11\0"
  /* 38 */ "F21\0"
  /* 42 */ "R21\0"
  /* 46 */ "F31\0"
  /* 50 */ "R31\0"
  /* 54 */ "F1\0"
  /* 57 */ "R1\0"
  /* 60 */ "F12\0"
  /* 64 */ "R12\0"
  /* 68 */ "F22\0"
  /* 72 */ "R22\0"
  /* 76 */ "F2\0"
  /* 79 */ "R2\0"
  /* 82 */ "F13\0"
  /* 86 */ "R13\0"
  /* 90 */ "F23\0"
  /* 94 */ "R23\0"
  /* 98 */ "F3\0"
  /* 101 */ "R3\0"
  /* 104 */ "F14\0"
  /* 108 */ "R14\0"
  /* 112 */ "F24\0"
  /* 116 */ "R24\0"
  /* 120 */ "F4\0"
  /* 123 */ "R4\0"
  /* 126 */ "F15\0"
  /* 130 */ "R15\0"
  /* 134 */ "F25\0"
  /* 138 */ "R25\0"
  /* 142 */ "F5\0"
  /* 145 */ "R5\0"
  /* 148 */ "F16\0"
  /* 152 */ "R16\0"
  /* 156 */ "F26\0"
  /* 160 */ "R26\0"
  /* 164 */ "F6\0"
  /* 167 */ "R6\0"
  /* 170 */ "F17\0"
  /* 174 */ "R17\0"
  /* 178 */ "F27\0"
  /* 182 */ "R27\0"
  /* 186 */ "F7\0"
  /* 189 */ "R7\0"
  /* 192 */ "F18\0"
  /* 196 */ "R18\0"
  /* 200 */ "F28\0"
  /* 204 */ "R28\0"
  /* 208 */ "F8\0"
  /* 211 */ "R8\0"
  /* 214 */ "F19\0"
  /* 218 */ "R19\0"
  /* 222 */ "F29\0"
  /* 226 */ "R29\0"
  /* 230 */ "F9\0"
  /* 233 */ "R9\0"
};
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

extern const MCRegisterDesc AlphaRegDesc[] = { // Descriptors
  { 3, 0, 0, 0, 0, 0 },
  { 24, 1, 1, 0, 1, 0 },
  { 54, 1, 1, 0, 1, 0 },
  { 76, 1, 1, 0, 1, 0 },
  { 98, 1, 1, 0, 1, 0 },
  { 120, 1, 1, 0, 1, 0 },
  { 142, 1, 1, 0, 1, 0 },
  { 164, 1, 1, 0, 1, 0 },
  { 186, 1, 1, 0, 1, 0 },
  { 208, 1, 1, 0, 1, 0 },
  { 230, 1, 1, 0, 1, 0 },
  { 0, 1, 1, 0, 1, 0 },
  { 30, 1, 1, 0, 1, 0 },
  { 60, 1, 1, 0, 1, 0 },
  { 82, 1, 1, 0, 1, 0 },
  { 104, 1, 1, 0, 1, 0 },
  { 126, 1, 1, 0, 1, 0 },
  { 148, 1, 1, 0, 1, 0 },
  { 170, 1, 1, 0, 1, 0 },
  { 192, 1, 1, 0, 1, 0 },
  { 214, 1, 1, 0, 1, 0 },
  { 8, 1, 1, 0, 1, 0 },
  { 38, 1, 1, 0, 1, 0 },
  { 68, 1, 1, 0, 1, 0 },
  { 90, 1, 1, 0, 1, 0 },
  { 112, 1, 1, 0, 1, 0 },
  { 134, 1, 1, 0, 1, 0 },
  { 156, 1, 1, 0, 1, 0 },
  { 178, 1, 1, 0, 1, 0 },
  { 200, 1, 1, 0, 1, 0 },
  { 222, 1, 1, 0, 1, 0 },
  { 16, 1, 1, 0, 1, 0 },
  { 46, 1, 1, 0, 1, 0 },
  { 27, 1, 1, 0, 1, 0 },
  { 57, 1, 1, 0, 1, 0 },
  { 79, 1, 1, 0, 1, 0 },
  { 101, 1, 1, 0, 1, 0 },
  { 123, 1, 1, 0, 1, 0 },
  { 145, 1, 1, 0, 1, 0 },
  { 167, 1, 1, 0, 1, 0 },
  { 189, 1, 1, 0, 1, 0 },
  { 211, 1, 1, 0, 1, 0 },
  { 233, 1, 1, 0, 1, 0 },
  { 4, 1, 1, 0, 1, 0 },
  { 34, 1, 1, 0, 1, 0 },
  { 64, 1, 1, 0, 1, 0 },
  { 86, 1, 1, 0, 1, 0 },
  { 108, 1, 1, 0, 1, 0 },
  { 130, 1, 1, 0, 1, 0 },
  { 152, 1, 1, 0, 1, 0 },
  { 174, 1, 1, 0, 1, 0 },
  { 196, 1, 1, 0, 1, 0 },
  { 218, 1, 1, 0, 1, 0 },
  { 12, 1, 1, 0, 1, 0 },
  { 42, 1, 1, 0, 1, 0 },
  { 72, 1, 1, 0, 1, 0 },
  { 94, 1, 1, 0, 1, 0 },
  { 116, 1, 1, 0, 1, 0 },
  { 138, 1, 1, 0, 1, 0 },
  { 160, 1, 1, 0, 1, 0 },
  { 182, 1, 1, 0, 1, 0 },
  { 204, 1, 1, 0, 1, 0 },
  { 226, 1, 1, 0, 1, 0 },
  { 20, 1, 1, 0, 1, 0 },
  { 50, 1, 1, 0, 1, 0 },
};

extern const MCPhysReg AlphaRegUnitRoots[][2] = {
  { Alpha::F0 },
  { Alpha::F1 },
  { Alpha::F2 },
  { Alpha::F3 },
  { Alpha::F4 },
  { Alpha::F5 },
  { Alpha::F6 },
  { Alpha::F7 },
  { Alpha::F8 },
  { Alpha::F9 },
  { Alpha::F10 },
  { Alpha::F11 },
  { Alpha::F12 },
  { Alpha::F13 },
  { Alpha::F14 },
  { Alpha::F15 },
  { Alpha::F16 },
  { Alpha::F17 },
  { Alpha::F18 },
  { Alpha::F19 },
  { Alpha::F20 },
  { Alpha::F21 },
  { Alpha::F22 },
  { Alpha::F23 },
  { Alpha::F24 },
  { Alpha::F25 },
  { Alpha::F26 },
  { Alpha::F27 },
  { Alpha::F28 },
  { Alpha::F29 },
  { Alpha::F30 },
  { Alpha::F31 },
  { Alpha::R0 },
  { Alpha::R1 },
  { Alpha::R2 },
  { Alpha::R3 },
  { Alpha::R4 },
  { Alpha::R5 },
  { Alpha::R6 },
  { Alpha::R7 },
  { Alpha::R8 },
  { Alpha::R9 },
  { Alpha::R10 },
  { Alpha::R11 },
  { Alpha::R12 },
  { Alpha::R13 },
  { Alpha::R14 },
  { Alpha::R15 },
  { Alpha::R16 },
  { Alpha::R17 },
  { Alpha::R18 },
  { Alpha::R19 },
  { Alpha::R20 },
  { Alpha::R21 },
  { Alpha::R22 },
  { Alpha::R23 },
  { Alpha::R24 },
  { Alpha::R25 },
  { Alpha::R26 },
  { Alpha::R27 },
  { Alpha::R28 },
  { Alpha::R29 },
  { Alpha::R30 },
  { Alpha::R31 },
};

namespace { // Register classes...
  // F4RC Register Class...
  const MCPhysReg F4RC[] = {
    Alpha::F0, Alpha::F1, Alpha::F10, Alpha::F11, Alpha::F12, Alpha::F13, Alpha::F14, Alpha::F15, Alpha::F16, Alpha::F17, Alpha::F18, Alpha::F19, Alpha::F20, Alpha::F21, Alpha::F22, Alpha::F23, Alpha::F24, Alpha::F25, Alpha::F26, Alpha::F27, Alpha::F28, Alpha::F29, Alpha::F30, Alpha::F2, Alpha::F3, Alpha::F4, Alpha::F5, Alpha::F6, Alpha::F7, Alpha::F8, Alpha::F9, Alpha::F31, 
  };

  // F4RC Bit set.
  const uint8_t F4RCBits[] = {
    0xfe, 0xff, 0xff, 0xff, 0x01, 
  };

  // F8RC Register Class...
  const MCPhysReg F8RC[] = {
    Alpha::F0, Alpha::F1, Alpha::F10, Alpha::F11, Alpha::F12, Alpha::F13, Alpha::F14, Alpha::F15, Alpha::F16, Alpha::F17, Alpha::F18, Alpha::F19, Alpha::F20, Alpha::F21, Alpha::F22, Alpha::F23, Alpha::F24, Alpha::F25, Alpha::F26, Alpha::F27, Alpha::F28, Alpha::F29, Alpha::F30, Alpha::F2, Alpha::F3, Alpha::F4, Alpha::F5, Alpha::F6, Alpha::F7, Alpha::F8, Alpha::F9, Alpha::F31, 
  };

  // F8RC Bit set.
  const uint8_t F8RCBits[] = {
    0xfe, 0xff, 0xff, 0xff, 0x01, 
  };

  // GPRC Register Class...
  const MCPhysReg GPRC[] = {
    Alpha::R0, Alpha::R1, Alpha::R2, Alpha::R3, Alpha::R4, Alpha::R5, Alpha::R6, Alpha::R7, Alpha::R8, Alpha::R16, Alpha::R17, Alpha::R18, Alpha::R19, Alpha::R20, Alpha::R21, Alpha::R22, Alpha::R23, Alpha::R24, Alpha::R25, Alpha::R28, Alpha::R27, Alpha::R26, Alpha::R29, Alpha::R9, Alpha::R10, Alpha::R11, Alpha::R12, Alpha::R13, Alpha::R14, Alpha::R15, Alpha::R30, Alpha::R31, 
  };

  // GPRC Bit set.
  const uint8_t GPRCBits[] = {
    0x00, 0x00, 0x00, 0x00, 0xfe, 0xff, 0xff, 0xff, 0x01, 
  };

} // end anonymous namespace


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Woverlength-strings"
#endif
extern const char AlphaRegClassStrings[] = {
  /* 0 */ "F4RC\0"
  /* 5 */ "F8RC\0"
  /* 10 */ "GPRC\0"
};
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

extern const MCRegisterClass AlphaMCRegisterClasses[] = {
  { F4RC, F4RCBits, 0, 32, sizeof(F4RCBits), Alpha::F4RCRegClassID, 32, 1, true },
  { F8RC, F8RCBits, 5, 32, sizeof(F8RCBits), Alpha::F8RCRegClassID, 64, 1, true },
  { GPRC, GPRCBits, 10, 32, sizeof(GPRCBits), Alpha::GPRCRegClassID, 64, 1, true },
};

// Alpha Dwarf<->LLVM register mappings.
extern const MCRegisterInfo::DwarfLLVMRegPair AlphaDwarfFlavour0Dwarf2L[] = {
  { 0U, Alpha::R0 },
  { 1U, Alpha::R1 },
  { 2U, Alpha::R2 },
  { 3U, Alpha::R3 },
  { 4U, Alpha::R4 },
  { 5U, Alpha::R5 },
  { 6U, Alpha::R6 },
  { 7U, Alpha::R7 },
  { 8U, Alpha::R8 },
  { 9U, Alpha::R9 },
  { 10U, Alpha::R10 },
  { 11U, Alpha::R11 },
  { 12U, Alpha::R12 },
  { 13U, Alpha::R13 },
  { 14U, Alpha::R14 },
  { 15U, Alpha::R15 },
  { 16U, Alpha::R16 },
  { 17U, Alpha::R17 },
  { 18U, Alpha::R18 },
  { 19U, Alpha::R19 },
  { 20U, Alpha::R20 },
  { 21U, Alpha::R21 },
  { 22U, Alpha::R22 },
  { 23U, Alpha::R23 },
  { 24U, Alpha::R24 },
  { 25U, Alpha::R25 },
  { 26U, Alpha::R26 },
  { 27U, Alpha::R27 },
  { 28U, Alpha::R28 },
  { 29U, Alpha::R29 },
  { 30U, Alpha::R30 },
  { 31U, Alpha::R31 },
  { 33U, Alpha::F0 },
  { 34U, Alpha::F1 },
  { 35U, Alpha::F2 },
  { 36U, Alpha::F3 },
  { 37U, Alpha::F4 },
  { 38U, Alpha::F5 },
  { 39U, Alpha::F6 },
  { 40U, Alpha::F7 },
  { 41U, Alpha::F8 },
  { 42U, Alpha::F9 },
  { 43U, Alpha::F10 },
  { 44U, Alpha::F11 },
  { 45U, Alpha::F12 },
  { 46U, Alpha::F13 },
  { 47U, Alpha::F14 },
  { 48U, Alpha::F15 },
  { 49U, Alpha::F16 },
  { 50U, Alpha::F17 },
  { 51U, Alpha::F18 },
  { 52U, Alpha::F19 },
  { 53U, Alpha::F20 },
  { 54U, Alpha::F21 },
  { 55U, Alpha::F22 },
  { 56U, Alpha::F23 },
  { 57U, Alpha::F24 },
  { 58U, Alpha::F25 },
  { 59U, Alpha::F26 },
  { 60U, Alpha::F27 },
  { 61U, Alpha::F28 },
  { 62U, Alpha::F29 },
  { 63U, Alpha::F30 },
  { 64U, Alpha::F31 },
};
extern const unsigned AlphaDwarfFlavour0Dwarf2LSize = std::size(AlphaDwarfFlavour0Dwarf2L);

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaEHFlavour0Dwarf2L[] = {
  { 0U, Alpha::R0 },
  { 1U, Alpha::R1 },
  { 2U, Alpha::R2 },
  { 3U, Alpha::R3 },
  { 4U, Alpha::R4 },
  { 5U, Alpha::R5 },
  { 6U, Alpha::R6 },
  { 7U, Alpha::R7 },
  { 8U, Alpha::R8 },
  { 9U, Alpha::R9 },
  { 10U, Alpha::R10 },
  { 11U, Alpha::R11 },
  { 12U, Alpha::R12 },
  { 13U, Alpha::R13 },
  { 14U, Alpha::R14 },
  { 15U, Alpha::R15 },
  { 16U, Alpha::R16 },
  { 17U, Alpha::R17 },
  { 18U, Alpha::R18 },
  { 19U, Alpha::R19 },
  { 20U, Alpha::R20 },
  { 21U, Alpha::R21 },
  { 22U, Alpha::R22 },
  { 23U, Alpha::R23 },
  { 24U, Alpha::R24 },
  { 25U, Alpha::R25 },
  { 26U, Alpha::R26 },
  { 27U, Alpha::R27 },
  { 28U, Alpha::R28 },
  { 29U, Alpha::R29 },
  { 30U, Alpha::R30 },
  { 31U, Alpha::R31 },
  { 33U, Alpha::F0 },
  { 34U, Alpha::F1 },
  { 35U, Alpha::F2 },
  { 36U, Alpha::F3 },
  { 37U, Alpha::F4 },
  { 38U, Alpha::F5 },
  { 39U, Alpha::F6 },
  { 40U, Alpha::F7 },
  { 41U, Alpha::F8 },
  { 42U, Alpha::F9 },
  { 43U, Alpha::F10 },
  { 44U, Alpha::F11 },
  { 45U, Alpha::F12 },
  { 46U, Alpha::F13 },
  { 47U, Alpha::F14 },
  { 48U, Alpha::F15 },
  { 49U, Alpha::F16 },
  { 50U, Alpha::F17 },
  { 51U, Alpha::F18 },
  { 52U, Alpha::F19 },
  { 53U, Alpha::F20 },
  { 54U, Alpha::F21 },
  { 55U, Alpha::F22 },
  { 56U, Alpha::F23 },
  { 57U, Alpha::F24 },
  { 58U, Alpha::F25 },
  { 59U, Alpha::F26 },
  { 60U, Alpha::F27 },
  { 61U, Alpha::F28 },
  { 62U, Alpha::F29 },
  { 63U, Alpha::F30 },
  { 64U, Alpha::F31 },
};
extern const unsigned AlphaEHFlavour0Dwarf2LSize = std::size(AlphaEHFlavour0Dwarf2L);

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaDwarfFlavour0L2Dwarf[] = {
  { Alpha::F0, 33U },
  { Alpha::F1, 34U },
  { Alpha::F2, 35U },
  { Alpha::F3, 36U },
  { Alpha::F4, 37U },
  { Alpha::F5, 38U },
  { Alpha::F6, 39U },
  { Alpha::F7, 40U },
  { Alpha::F8, 41U },
  { Alpha::F9, 42U },
  { Alpha::F10, 43U },
  { Alpha::F11, 44U },
  { Alpha::F12, 45U },
  { Alpha::F13, 46U },
  { Alpha::F14, 47U },
  { Alpha::F15, 48U },
  { Alpha::F16, 49U },
  { Alpha::F17, 50U },
  { Alpha::F18, 51U },
  { Alpha::F19, 52U },
  { Alpha::F20, 53U },
  { Alpha::F21, 54U },
  { Alpha::F22, 55U },
  { Alpha::F23, 56U },
  { Alpha::F24, 57U },
  { Alpha::F25, 58U },
  { Alpha::F26, 59U },
  { Alpha::F27, 60U },
  { Alpha::F28, 61U },
  { Alpha::F29, 62U },
  { Alpha::F30, 63U },
  { Alpha::F31, 64U },
  { Alpha::R0, 0U },
  { Alpha::R1, 1U },
  { Alpha::R2, 2U },
  { Alpha::R3, 3U },
  { Alpha::R4, 4U },
  { Alpha::R5, 5U },
  { Alpha::R6, 6U },
  { Alpha::R7, 7U },
  { Alpha::R8, 8U },
  { Alpha::R9, 9U },
  { Alpha::R10, 10U },
  { Alpha::R11, 11U },
  { Alpha::R12, 12U },
  { Alpha::R13, 13U },
  { Alpha::R14, 14U },
  { Alpha::R15, 15U },
  { Alpha::R16, 16U },
  { Alpha::R17, 17U },
  { Alpha::R18, 18U },
  { Alpha::R19, 19U },
  { Alpha::R20, 20U },
  { Alpha::R21, 21U },
  { Alpha::R22, 22U },
  { Alpha::R23, 23U },
  { Alpha::R24, 24U },
  { Alpha::R25, 25U },
  { Alpha::R26, 26U },
  { Alpha::R27, 27U },
  { Alpha::R28, 28U },
  { Alpha::R29, 29U },
  { Alpha::R30, 30U },
  { Alpha::R31, 31U },
};
extern const unsigned AlphaDwarfFlavour0L2DwarfSize = std::size(AlphaDwarfFlavour0L2Dwarf);

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaEHFlavour0L2Dwarf[] = {
  { Alpha::F0, 33U },
  { Alpha::F1, 34U },
  { Alpha::F2, 35U },
  { Alpha::F3, 36U },
  { Alpha::F4, 37U },
  { Alpha::F5, 38U },
  { Alpha::F6, 39U },
  { Alpha::F7, 40U },
  { Alpha::F8, 41U },
  { Alpha::F9, 42U },
  { Alpha::F10, 43U },
  { Alpha::F11, 44U },
  { Alpha::F12, 45U },
  { Alpha::F13, 46U },
  { Alpha::F14, 47U },
  { Alpha::F15, 48U },
  { Alpha::F16, 49U },
  { Alpha::F17, 50U },
  { Alpha::F18, 51U },
  { Alpha::F19, 52U },
  { Alpha::F20, 53U },
  { Alpha::F21, 54U },
  { Alpha::F22, 55U },
  { Alpha::F23, 56U },
  { Alpha::F24, 57U },
  { Alpha::F25, 58U },
  { Alpha::F26, 59U },
  { Alpha::F27, 60U },
  { Alpha::F28, 61U },
  { Alpha::F29, 62U },
  { Alpha::F30, 63U },
  { Alpha::F31, 64U },
  { Alpha::R0, 0U },
  { Alpha::R1, 1U },
  { Alpha::R2, 2U },
  { Alpha::R3, 3U },
  { Alpha::R4, 4U },
  { Alpha::R5, 5U },
  { Alpha::R6, 6U },
  { Alpha::R7, 7U },
  { Alpha::R8, 8U },
  { Alpha::R9, 9U },
  { Alpha::R10, 10U },
  { Alpha::R11, 11U },
  { Alpha::R12, 12U },
  { Alpha::R13, 13U },
  { Alpha::R14, 14U },
  { Alpha::R15, 15U },
  { Alpha::R16, 16U },
  { Alpha::R17, 17U },
  { Alpha::R18, 18U },
  { Alpha::R19, 19U },
  { Alpha::R20, 20U },
  { Alpha::R21, 21U },
  { Alpha::R22, 22U },
  { Alpha::R23, 23U },
  { Alpha::R24, 24U },
  { Alpha::R25, 25U },
  { Alpha::R26, 26U },
  { Alpha::R27, 27U },
  { Alpha::R28, 28U },
  { Alpha::R29, 29U },
  { Alpha::R30, 30U },
  { Alpha::R31, 31U },
};
extern const unsigned AlphaEHFlavour0L2DwarfSize = std::size(AlphaEHFlavour0L2Dwarf);

extern const uint16_t AlphaRegEncodingTable[] = {
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
};
static inline void InitAlphaMCRegisterInfo(MCRegisterInfo *RI, unsigned RA, unsigned DwarfFlavour = 0, unsigned EHFlavour = 0, unsigned PC = 0) {
  RI->InitMCRegisterInfo(AlphaRegDesc, 65, RA, PC, AlphaMCRegisterClasses, 3, AlphaRegUnitRoots, 64, AlphaRegDiffLists, AlphaLaneMaskLists, AlphaRegStrings, AlphaRegClassStrings, AlphaSubRegIdxLists, 1,
AlphaSubRegIdxRanges, AlphaRegEncodingTable);

  switch (DwarfFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    RI->mapDwarfRegsToLLVMRegs(AlphaDwarfFlavour0Dwarf2L, AlphaDwarfFlavour0Dwarf2LSize, false);
    break;
  }
  switch (EHFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    RI->mapDwarfRegsToLLVMRegs(AlphaEHFlavour0Dwarf2L, AlphaEHFlavour0Dwarf2LSize, true);
    break;
  }
  switch (DwarfFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    RI->mapLLVMRegsToDwarfRegs(AlphaDwarfFlavour0L2Dwarf, AlphaDwarfFlavour0L2DwarfSize, false);
    break;
  }
  switch (EHFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    RI->mapLLVMRegsToDwarfRegs(AlphaEHFlavour0L2Dwarf, AlphaEHFlavour0L2DwarfSize, true);
    break;
  }
}

} // end namespace llvm

#endif // GET_REGINFO_MC_DESC

/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Register Information Header Fragment                                       *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifdef GET_REGINFO_HEADER
#undef GET_REGINFO_HEADER

#include "llvm/CodeGen/TargetRegisterInfo.h"

namespace llvm {

class AlphaFrameLowering;

struct AlphaGenRegisterInfo : public TargetRegisterInfo {
  explicit AlphaGenRegisterInfo(unsigned RA, unsigned D = 0, unsigned E = 0,
      unsigned PC = 0, unsigned HwMode = 0);
  const RegClassWeight &getRegClassWeight(const TargetRegisterClass *RC) const override;
  unsigned getRegUnitWeight(unsigned RegUnit) const override;
  unsigned getNumRegPressureSets() const override;
  const char *getRegPressureSetName(unsigned Idx) const override;
  unsigned getRegPressureSetLimit(const MachineFunction &MF, unsigned Idx) const override;
  const int *getRegClassPressureSets(const TargetRegisterClass *RC) const override;
  const int *getRegUnitPressureSets(unsigned RegUnit) const override;
  ArrayRef<const char *> getRegMaskNames() const override;
  ArrayRef<const uint32_t *> getRegMasks() const override;
  bool isGeneralPurposeRegister(const MachineFunction &, MCRegister) const override;
  bool isFixedRegister(const MachineFunction &, MCRegister) const override;
  bool isArgumentRegister(const MachineFunction &, MCRegister) const override;
  bool isConstantPhysReg(MCRegister PhysReg) const override final;
  /// Devirtualized TargetFrameLowering.
  static const AlphaFrameLowering *getFrameLowering(
      const MachineFunction &MF);
};

namespace Alpha { // Register classes
  extern const TargetRegisterClass F4RCRegClass;
  extern const TargetRegisterClass F8RCRegClass;
  extern const TargetRegisterClass GPRCRegClass;
} // end namespace Alpha

} // end namespace llvm

#endif // GET_REGINFO_HEADER

/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|* Target Register and Register Classes Information                           *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#ifdef GET_REGINFO_TARGET_DESC
#undef GET_REGINFO_TARGET_DESC

namespace llvm {

extern const MCRegisterClass AlphaMCRegisterClasses[];

static const MVT::SimpleValueType VTLists[] = {
  /* 0 */ MVT::i64, MVT::Other,
  /* 2 */ MVT::f32, MVT::Other,
  /* 4 */ MVT::f64, MVT::Other,
};

static const char *SubRegIndexNameTable[] = { "" };


static const LaneBitmask SubRegIndexLaneMaskTable[] = {
  LaneBitmask::getAll(),
 };



static const TargetRegisterInfo::RegClassInfo RegClassInfos[] = {
  // Mode = 0 (Default)
  { 32, 32, 64, VTLists+2 },    // F4RC
  { 64, 64, 64, VTLists+4 },    // F8RC
  { 64, 64, 64, VTLists+0 },    // GPRC
};

static const TargetRegisterClass *const NullRegClasses[] = { nullptr };

static const uint32_t F4RCSubClassMask[] = {
  0x00000003, 
};

static const uint32_t F8RCSubClassMask[] = {
  0x00000002, 
};

static const uint32_t GPRCSubClassMask[] = {
  0x00000004, 
};

static const uint16_t SuperRegIdxSeqs[] = {
  /* 0 */ 0,
};

static const TargetRegisterClass *const F8RCSuperclasses[] = {
  &Alpha::F4RCRegClass,
  nullptr
};

namespace Alpha { // Register class instances
  extern const TargetRegisterClass F4RCRegClass = {
    &AlphaMCRegisterClasses[F4RCRegClassID],
    F4RCSubClassMask,
    SuperRegIdxSeqs + 0,
    LaneBitmask(0x0000000000000001),
    0,
    false,
    0x00, /* TSFlags */
    false, /* HasDisjunctSubRegs */
    false, /* CoveredBySubRegs */
    NullRegClasses,
    nullptr
  };

  extern const TargetRegisterClass F8RCRegClass = {
    &AlphaMCRegisterClasses[F8RCRegClassID],
    F8RCSubClassMask,
    SuperRegIdxSeqs + 0,
    LaneBitmask(0x0000000000000001),
    0,
    false,
    0x00, /* TSFlags */
    false, /* HasDisjunctSubRegs */
    false, /* CoveredBySubRegs */
    F8RCSuperclasses,
    nullptr
  };

  extern const TargetRegisterClass GPRCRegClass = {
    &AlphaMCRegisterClasses[GPRCRegClassID],
    GPRCSubClassMask,
    SuperRegIdxSeqs + 0,
    LaneBitmask(0x0000000000000001),
    0,
    false,
    0x00, /* TSFlags */
    false, /* HasDisjunctSubRegs */
    false, /* CoveredBySubRegs */
    NullRegClasses,
    nullptr
  };

} // end namespace Alpha

namespace {

  const TargetRegisterClass *const RegisterClasses[] = {
    &Alpha::F4RCRegClass,
    &Alpha::F8RCRegClass,
    &Alpha::GPRCRegClass,
  };
} // end anonymous namespace


static const uint8_t CostPerUseTable[] = { 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, };


static const bool InAllocatableClassTable[] = { 
false, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, };


static const TargetRegisterInfoDesc AlphaRegInfoDesc = { // Extra Descriptors
CostPerUseTable, 1, InAllocatableClassTable};

/// Get the weight in units of pressure for this register class.
const RegClassWeight &AlphaGenRegisterInfo::
getRegClassWeight(const TargetRegisterClass *RC) const {
  static const RegClassWeight RCWeightTable[] = {
    {1, 32},  	// F4RC
    {1, 32},  	// F8RC
    {1, 32},  	// GPRC
  };
  return RCWeightTable[RC->getID()];
}

/// Get the weight in units of pressure for this register unit.
unsigned AlphaGenRegisterInfo::
getRegUnitWeight(unsigned RegUnit) const {
  assert(RegUnit < 64 && "invalid register unit");
  // All register units have unit weight.
  return 1;
}


// Get the number of dimensions of register pressure.
unsigned AlphaGenRegisterInfo::getNumRegPressureSets() const {
  return 2;
}

// Get the name of this register unit pressure set.
const char *AlphaGenRegisterInfo::
getRegPressureSetName(unsigned Idx) const {
  static const char *PressureNameTable[] = {
    "F4RC",
    "GPRC",
  };
  return PressureNameTable[Idx];
}

// Get the register unit pressure limit for this dimension.
// This limit must be adjusted dynamically for reserved registers.
unsigned AlphaGenRegisterInfo::
getRegPressureSetLimit(const MachineFunction &MF, unsigned Idx) const {
  static const uint8_t PressureLimitTable[] = {
    32,  	// 0: F4RC
    32,  	// 1: GPRC
  };
  return PressureLimitTable[Idx];
}

/// Table of pressure sets per register class or unit.
static const int RCSetsTable[] = {
  /* 0 */ 0, -1,
  /* 2 */ 1, -1,
};

/// Get the dimensions of register pressure impacted by this register class.
/// Returns a -1 terminated array of pressure set IDs
const int *AlphaGenRegisterInfo::
getRegClassPressureSets(const TargetRegisterClass *RC) const {
  static const uint8_t RCSetStartTable[] = {
    0,0,2,};
  return &RCSetsTable[RCSetStartTable[RC->getID()]];
}

/// Get the dimensions of register pressure impacted by this register unit.
/// Returns a -1 terminated array of pressure set IDs
const int *AlphaGenRegisterInfo::
getRegUnitPressureSets(unsigned RegUnit) const {
  assert(RegUnit < 64 && "invalid register unit");
  static const uint8_t RUSetStartTable[] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,};
  return &RCSetsTable[RUSetStartTable[RegUnit]];
}

extern const MCRegisterDesc AlphaRegDesc[];
extern const MCPhysReg AlphaRegDiffLists[];
extern const LaneBitmask AlphaLaneMaskLists[];
extern const char AlphaRegStrings[];
extern const char AlphaRegClassStrings[];
extern const MCPhysReg AlphaRegUnitRoots[][2];
extern const uint16_t AlphaSubRegIdxLists[];
extern const MCRegisterInfo::SubRegCoveredBits AlphaSubRegIdxRanges[];
extern const uint16_t AlphaRegEncodingTable[];
// Alpha Dwarf<->LLVM register mappings.
extern const MCRegisterInfo::DwarfLLVMRegPair AlphaDwarfFlavour0Dwarf2L[];
extern const unsigned AlphaDwarfFlavour0Dwarf2LSize;

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaEHFlavour0Dwarf2L[];
extern const unsigned AlphaEHFlavour0Dwarf2LSize;

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaDwarfFlavour0L2Dwarf[];
extern const unsigned AlphaDwarfFlavour0L2DwarfSize;

extern const MCRegisterInfo::DwarfLLVMRegPair AlphaEHFlavour0L2Dwarf[];
extern const unsigned AlphaEHFlavour0L2DwarfSize;

AlphaGenRegisterInfo::
AlphaGenRegisterInfo(unsigned RA, unsigned DwarfFlavour, unsigned EHFlavour,
      unsigned PC, unsigned HwMode)
  : TargetRegisterInfo(&AlphaRegInfoDesc, RegisterClasses, RegisterClasses+3,
             SubRegIndexNameTable, SubRegIndexLaneMaskTable,
             LaneBitmask(0xFFFFFFFFFFFFFFFF), RegClassInfos, HwMode) {
  InitMCRegisterInfo(AlphaRegDesc, 65, RA, PC,
                     AlphaMCRegisterClasses, 3,
                     AlphaRegUnitRoots,
                     64,
                     AlphaRegDiffLists,
                     AlphaLaneMaskLists,
                     AlphaRegStrings,
                     AlphaRegClassStrings,
                     AlphaSubRegIdxLists,
                     1,
                     AlphaSubRegIdxRanges,
                     AlphaRegEncodingTable);

  switch (DwarfFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    mapDwarfRegsToLLVMRegs(AlphaDwarfFlavour0Dwarf2L, AlphaDwarfFlavour0Dwarf2LSize, false);
    break;
  }
  switch (EHFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    mapDwarfRegsToLLVMRegs(AlphaEHFlavour0Dwarf2L, AlphaEHFlavour0Dwarf2LSize, true);
    break;
  }
  switch (DwarfFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    mapLLVMRegsToDwarfRegs(AlphaDwarfFlavour0L2Dwarf, AlphaDwarfFlavour0L2DwarfSize, false);
    break;
  }
  switch (EHFlavour) {
  default:
    llvm_unreachable("Unknown DWARF flavour");
  case 0:
    mapLLVMRegsToDwarfRegs(AlphaEHFlavour0L2Dwarf, AlphaEHFlavour0L2DwarfSize, true);
    break;
  }
}



ArrayRef<const uint32_t *> AlphaGenRegisterInfo::getRegMasks() const {
  return std::nullopt;
}

bool AlphaGenRegisterInfo::
isGeneralPurposeRegister(const MachineFunction &MF, MCRegister PhysReg) const {
  return
      false;
}

bool AlphaGenRegisterInfo::
isFixedRegister(const MachineFunction &MF, MCRegister PhysReg) const {
  return
      false;
}

bool AlphaGenRegisterInfo::
isArgumentRegister(const MachineFunction &MF, MCRegister PhysReg) const {
  return
      false;
}

bool AlphaGenRegisterInfo::
isConstantPhysReg(MCRegister PhysReg) const {
  return
      false;
}

ArrayRef<const char *> AlphaGenRegisterInfo::getRegMaskNames() const {
  return std::nullopt;
}

const AlphaFrameLowering *
AlphaGenRegisterInfo::getFrameLowering(const MachineFunction &MF) {
  return static_cast<const AlphaFrameLowering *>(
      MF.getSubtarget().getFrameLowering());
}

} // end namespace llvm

#endif // GET_REGINFO_TARGET_DESC

