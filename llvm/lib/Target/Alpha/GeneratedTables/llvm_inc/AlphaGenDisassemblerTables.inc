/*===- TableGen'erated file -------------------------------------*- C++ -*-===*\
|*                                                                            *|
|*  * Alpha Disassembler                                                      *|
|*                                                                            *|
|* Automatically generated file, do not edit!                                 *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/

#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/LEB128.h"
#include "llvm/Support/raw_ostream.h"
#include <assert.h>

namespace llvm {

// Helper functions for extracting fields from encoded instructions.
// InsnType must either be integral or an APInt-like object that must:
// * be default-constructible and copy-constructible
// * be constructible from an APInt (this can be private)
// * Support insertBits(bits, startBit, numBits)
// * Support extractBitsAsZExtValue(numBits, startBit)
// * Support the ~, &, ==, and != operators with other objects of the same type
// * Support the != and bitwise & with uint64_t
// * Support put (<<) to raw_ostream&
template <typename InsnType>
#if defined(_MSC_VER) && !defined(__clang__)
__declspec(noinline)
#endif
static std::enable_if_t<std::is_integral<InsnType>::value, InsnType>
fieldFromInstruction(const InsnType &insn, unsigned startBit,
                     unsigned numBits) {
  assert(startBit + numBits <= 64 && "Cannot support >64-bit extractions!");
  assert(startBit + numBits <= (sizeof(InsnType) * 8) &&
         "Instruction field out of bounds!");
  InsnType fieldMask;
  if (numBits == sizeof(InsnType) * 8)
    fieldMask = (InsnType)(-1LL);
  else
    fieldMask = (((InsnType)1 << numBits) - 1) << startBit;
  return (insn & fieldMask) >> startBit;
}

template <typename InsnType>
static std::enable_if_t<!std::is_integral<InsnType>::value, uint64_t>
fieldFromInstruction(const InsnType &insn, unsigned startBit,
                     unsigned numBits) {
  return insn.extractBitsAsZExtValue(numBits, startBit);
}

// Helper function for inserting bits extracted from an encoded instruction into
// a field.
template <typename InsnType>
static std::enable_if_t<std::is_integral<InsnType>::value>
insertBits(InsnType &field, InsnType bits, unsigned startBit, unsigned numBits) {
  assert(startBit + numBits <= sizeof field * 8);
  field |= (InsnType)bits << startBit;
}

template <typename InsnType>
static std::enable_if_t<!std::is_integral<InsnType>::value>
insertBits(InsnType &field, uint64_t bits, unsigned startBit, unsigned numBits) {
  field.insertBits(bits, startBit, numBits);
}

static const uint8_t DecoderTable32[] = {
/* 0 */       MCD::OPC_ExtractField, 26, 6,  // Inst{31-26} ...
/* 3 */       MCD::OPC_FilterValue, 0, 0, 0, 0, // Skip to: 8
/* 8 */       MCD::OPC_FilterValue, 16, 215, 1, 0, // Skip to: 484
/* 13 */      MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 16 */      MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 32
/* 21 */      MCD::OPC_CheckField, 13, 3, 0, 206, 6, 0, // Skip to: 1770
/* 28 */      MCD::OPC_Decode, 136, 2, 0, // Opcode: ADDLr
/* 32 */      MCD::OPC_FilterValue, 2, 11, 0, 0, // Skip to: 48
/* 37 */      MCD::OPC_CheckField, 13, 3, 0, 190, 6, 0, // Skip to: 1770
/* 44 */      MCD::OPC_Decode, 149, 3, 0, // Opcode: S4ADDLr
/* 48 */      MCD::OPC_FilterValue, 9, 11, 0, 0, // Skip to: 64
/* 53 */      MCD::OPC_CheckField, 13, 3, 0, 174, 6, 0, // Skip to: 1770
/* 60 */      MCD::OPC_Decode, 189, 3, 0, // Opcode: SUBLr
/* 64 */      MCD::OPC_FilterValue, 11, 11, 0, 0, // Skip to: 80
/* 69 */      MCD::OPC_CheckField, 13, 3, 0, 158, 6, 0, // Skip to: 1770
/* 76 */      MCD::OPC_Decode, 153, 3, 0, // Opcode: S4SUBLr
/* 80 */      MCD::OPC_FilterValue, 15, 11, 0, 0, // Skip to: 96
/* 85 */      MCD::OPC_CheckField, 13, 3, 0, 142, 6, 0, // Skip to: 1770
/* 92 */      MCD::OPC_Decode, 173, 2, 0, // Opcode: CMPBGE
/* 96 */      MCD::OPC_FilterValue, 18, 11, 0, 0, // Skip to: 112
/* 101 */     MCD::OPC_CheckField, 13, 3, 0, 126, 6, 0, // Skip to: 1770
/* 108 */     MCD::OPC_Decode, 157, 3, 0, // Opcode: S8ADDLr
/* 112 */     MCD::OPC_FilterValue, 27, 11, 0, 0, // Skip to: 128
/* 117 */     MCD::OPC_CheckField, 13, 3, 0, 110, 6, 0, // Skip to: 1770
/* 124 */     MCD::OPC_Decode, 161, 3, 0, // Opcode: S8SUBLr
/* 128 */     MCD::OPC_FilterValue, 29, 11, 0, 0, // Skip to: 144
/* 133 */     MCD::OPC_CheckField, 13, 3, 0, 94, 6, 0, // Skip to: 1770
/* 140 */     MCD::OPC_Decode, 187, 2, 0, // Opcode: CMPULT
/* 144 */     MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 160
/* 149 */     MCD::OPC_CheckField, 13, 3, 0, 78, 6, 0, // Skip to: 1770
/* 156 */     MCD::OPC_Decode, 138, 2, 0, // Opcode: ADDQr
/* 160 */     MCD::OPC_FilterValue, 34, 11, 0, 0, // Skip to: 176
/* 165 */     MCD::OPC_CheckField, 13, 3, 0, 62, 6, 0, // Skip to: 1770
/* 172 */     MCD::OPC_Decode, 151, 3, 0, // Opcode: S4ADDQr
/* 176 */     MCD::OPC_FilterValue, 41, 11, 0, 0, // Skip to: 192
/* 181 */     MCD::OPC_CheckField, 13, 3, 0, 46, 6, 0, // Skip to: 1770
/* 188 */     MCD::OPC_Decode, 191, 3, 0, // Opcode: SUBQr
/* 192 */     MCD::OPC_FilterValue, 43, 11, 0, 0, // Skip to: 208
/* 197 */     MCD::OPC_CheckField, 13, 3, 0, 30, 6, 0, // Skip to: 1770
/* 204 */     MCD::OPC_Decode, 155, 3, 0, // Opcode: S4SUBQr
/* 208 */     MCD::OPC_FilterValue, 45, 11, 0, 0, // Skip to: 224
/* 213 */     MCD::OPC_CheckField, 13, 3, 0, 14, 6, 0, // Skip to: 1770
/* 220 */     MCD::OPC_Decode, 175, 2, 0, // Opcode: CMPEQ
/* 224 */     MCD::OPC_FilterValue, 50, 11, 0, 0, // Skip to: 240
/* 229 */     MCD::OPC_CheckField, 13, 3, 0, 254, 5, 0, // Skip to: 1770
/* 236 */     MCD::OPC_Decode, 159, 3, 0, // Opcode: S8ADDQr
/* 240 */     MCD::OPC_FilterValue, 59, 11, 0, 0, // Skip to: 256
/* 245 */     MCD::OPC_CheckField, 13, 3, 0, 238, 5, 0, // Skip to: 1770
/* 252 */     MCD::OPC_Decode, 163, 3, 0, // Opcode: S8SUBQr
/* 256 */     MCD::OPC_FilterValue, 61, 11, 0, 0, // Skip to: 272
/* 261 */     MCD::OPC_CheckField, 13, 3, 0, 222, 5, 0, // Skip to: 1770
/* 268 */     MCD::OPC_Decode, 185, 2, 0, // Opcode: CMPULE
/* 272 */     MCD::OPC_FilterValue, 77, 11, 0, 0, // Skip to: 288
/* 277 */     MCD::OPC_CheckField, 13, 3, 0, 206, 5, 0, // Skip to: 1770
/* 284 */     MCD::OPC_Decode, 179, 2, 0, // Opcode: CMPLT
/* 288 */     MCD::OPC_FilterValue, 109, 11, 0, 0, // Skip to: 304
/* 293 */     MCD::OPC_CheckField, 13, 3, 0, 190, 5, 0, // Skip to: 1770
/* 300 */     MCD::OPC_Decode, 177, 2, 0, // Opcode: CMPLE
/* 304 */     MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 314
/* 310 */     MCD::OPC_Decode, 135, 2, 0, // Opcode: ADDLi
/* 314 */     MCD::OPC_FilterValue, 130, 1, 4, 0, 0, // Skip to: 324
/* 320 */     MCD::OPC_Decode, 148, 3, 0, // Opcode: S4ADDLi
/* 324 */     MCD::OPC_FilterValue, 137, 1, 4, 0, 0, // Skip to: 334
/* 330 */     MCD::OPC_Decode, 188, 3, 0, // Opcode: SUBLi
/* 334 */     MCD::OPC_FilterValue, 139, 1, 4, 0, 0, // Skip to: 344
/* 340 */     MCD::OPC_Decode, 152, 3, 0, // Opcode: S4SUBLi
/* 344 */     MCD::OPC_FilterValue, 143, 1, 4, 0, 0, // Skip to: 354
/* 350 */     MCD::OPC_Decode, 174, 2, 0, // Opcode: CMPBGEi
/* 354 */     MCD::OPC_FilterValue, 146, 1, 4, 0, 0, // Skip to: 364
/* 360 */     MCD::OPC_Decode, 156, 3, 0, // Opcode: S8ADDLi
/* 364 */     MCD::OPC_FilterValue, 155, 1, 4, 0, 0, // Skip to: 374
/* 370 */     MCD::OPC_Decode, 160, 3, 0, // Opcode: S8SUBLi
/* 374 */     MCD::OPC_FilterValue, 157, 1, 4, 0, 0, // Skip to: 384
/* 380 */     MCD::OPC_Decode, 188, 2, 0, // Opcode: CMPULTi
/* 384 */     MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 394
/* 390 */     MCD::OPC_Decode, 137, 2, 0, // Opcode: ADDQi
/* 394 */     MCD::OPC_FilterValue, 162, 1, 4, 0, 0, // Skip to: 404
/* 400 */     MCD::OPC_Decode, 150, 3, 0, // Opcode: S4ADDQi
/* 404 */     MCD::OPC_FilterValue, 169, 1, 4, 0, 0, // Skip to: 414
/* 410 */     MCD::OPC_Decode, 190, 3, 0, // Opcode: SUBQi
/* 414 */     MCD::OPC_FilterValue, 171, 1, 4, 0, 0, // Skip to: 424
/* 420 */     MCD::OPC_Decode, 154, 3, 0, // Opcode: S4SUBQi
/* 424 */     MCD::OPC_FilterValue, 173, 1, 4, 0, 0, // Skip to: 434
/* 430 */     MCD::OPC_Decode, 176, 2, 0, // Opcode: CMPEQi
/* 434 */     MCD::OPC_FilterValue, 178, 1, 4, 0, 0, // Skip to: 444
/* 440 */     MCD::OPC_Decode, 158, 3, 0, // Opcode: S8ADDQi
/* 444 */     MCD::OPC_FilterValue, 187, 1, 4, 0, 0, // Skip to: 454
/* 450 */     MCD::OPC_Decode, 162, 3, 0, // Opcode: S8SUBQi
/* 454 */     MCD::OPC_FilterValue, 189, 1, 4, 0, 0, // Skip to: 464
/* 460 */     MCD::OPC_Decode, 186, 2, 0, // Opcode: CMPULEi
/* 464 */     MCD::OPC_FilterValue, 205, 1, 4, 0, 0, // Skip to: 474
/* 470 */     MCD::OPC_Decode, 180, 2, 0, // Opcode: CMPLTi
/* 474 */     MCD::OPC_FilterValue, 237, 1, 10, 5, 0, // Skip to: 1770
/* 480 */     MCD::OPC_Decode, 178, 2, 0, // Opcode: CMPLEi
/* 484 */     MCD::OPC_FilterValue, 17, 111, 1, 0, // Skip to: 856
/* 489 */     MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 492 */     MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 508
/* 497 */     MCD::OPC_CheckField, 13, 3, 0, 242, 4, 0, // Skip to: 1770
/* 504 */     MCD::OPC_Decode, 142, 2, 0, // Opcode: ANDr
/* 508 */     MCD::OPC_FilterValue, 8, 11, 0, 0, // Skip to: 524
/* 513 */     MCD::OPC_CheckField, 13, 3, 0, 226, 4, 0, // Skip to: 1770
/* 520 */     MCD::OPC_Decode, 147, 2, 0, // Opcode: BICr
/* 524 */     MCD::OPC_FilterValue, 20, 11, 0, 0, // Skip to: 540
/* 529 */     MCD::OPC_CheckField, 13, 3, 0, 210, 4, 0, // Skip to: 1770
/* 536 */     MCD::OPC_Decode, 166, 2, 0, // Opcode: CMOVLBSr
/* 540 */     MCD::OPC_FilterValue, 22, 11, 0, 0, // Skip to: 556
/* 545 */     MCD::OPC_CheckField, 13, 3, 0, 194, 4, 0, // Skip to: 1770
/* 552 */     MCD::OPC_Decode, 164, 2, 0, // Opcode: CMOVLBCr
/* 556 */     MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 572
/* 561 */     MCD::OPC_CheckField, 13, 3, 0, 178, 4, 0, // Skip to: 1770
/* 568 */     MCD::OPC_Decode, 149, 2, 0, // Opcode: BISr
/* 572 */     MCD::OPC_FilterValue, 36, 11, 0, 0, // Skip to: 588
/* 577 */     MCD::OPC_CheckField, 13, 3, 0, 162, 4, 0, // Skip to: 1770
/* 584 */     MCD::OPC_Decode, 158, 2, 0, // Opcode: CMOVEQr
/* 588 */     MCD::OPC_FilterValue, 38, 11, 0, 0, // Skip to: 604
/* 593 */     MCD::OPC_CheckField, 13, 3, 0, 146, 4, 0, // Skip to: 1770
/* 600 */     MCD::OPC_Decode, 172, 2, 0, // Opcode: CMOVNEr
/* 604 */     MCD::OPC_FilterValue, 40, 11, 0, 0, // Skip to: 620
/* 609 */     MCD::OPC_CheckField, 13, 3, 0, 130, 4, 0, // Skip to: 1770
/* 616 */     MCD::OPC_Decode, 144, 3, 0, // Opcode: ORNOTr
/* 620 */     MCD::OPC_FilterValue, 64, 11, 0, 0, // Skip to: 636
/* 625 */     MCD::OPC_CheckField, 13, 3, 0, 114, 4, 0, // Skip to: 1770
/* 632 */     MCD::OPC_Decode, 198, 3, 0, // Opcode: XORr
/* 636 */     MCD::OPC_FilterValue, 68, 11, 0, 0, // Skip to: 652
/* 641 */     MCD::OPC_CheckField, 13, 3, 0, 98, 4, 0, // Skip to: 1770
/* 648 */     MCD::OPC_Decode, 170, 2, 0, // Opcode: CMOVLTr
/* 652 */     MCD::OPC_FilterValue, 70, 11, 0, 0, // Skip to: 668
/* 657 */     MCD::OPC_CheckField, 13, 3, 0, 82, 4, 0, // Skip to: 1770
/* 664 */     MCD::OPC_Decode, 160, 2, 0, // Opcode: CMOVGEr
/* 668 */     MCD::OPC_FilterValue, 72, 11, 0, 0, // Skip to: 684
/* 673 */     MCD::OPC_CheckField, 13, 3, 0, 66, 4, 0, // Skip to: 1770
/* 680 */     MCD::OPC_Decode, 213, 2, 0, // Opcode: EQVr
/* 684 */     MCD::OPC_FilterValue, 100, 11, 0, 0, // Skip to: 700
/* 689 */     MCD::OPC_CheckField, 13, 3, 0, 50, 4, 0, // Skip to: 1770
/* 696 */     MCD::OPC_Decode, 168, 2, 0, // Opcode: CMOVLEr
/* 700 */     MCD::OPC_FilterValue, 102, 11, 0, 0, // Skip to: 716
/* 705 */     MCD::OPC_CheckField, 13, 3, 0, 34, 4, 0, // Skip to: 1770
/* 712 */     MCD::OPC_Decode, 162, 2, 0, // Opcode: CMOVGTr
/* 716 */     MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 726
/* 722 */     MCD::OPC_Decode, 141, 2, 0, // Opcode: ANDi
/* 726 */     MCD::OPC_FilterValue, 136, 1, 4, 0, 0, // Skip to: 736
/* 732 */     MCD::OPC_Decode, 146, 2, 0, // Opcode: BICi
/* 736 */     MCD::OPC_FilterValue, 148, 1, 4, 0, 0, // Skip to: 746
/* 742 */     MCD::OPC_Decode, 165, 2, 0, // Opcode: CMOVLBSi
/* 746 */     MCD::OPC_FilterValue, 150, 1, 4, 0, 0, // Skip to: 756
/* 752 */     MCD::OPC_Decode, 163, 2, 0, // Opcode: CMOVLBCi
/* 756 */     MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 766
/* 762 */     MCD::OPC_Decode, 148, 2, 0, // Opcode: BISi
/* 766 */     MCD::OPC_FilterValue, 164, 1, 4, 0, 0, // Skip to: 776
/* 772 */     MCD::OPC_Decode, 157, 2, 0, // Opcode: CMOVEQi
/* 776 */     MCD::OPC_FilterValue, 166, 1, 4, 0, 0, // Skip to: 786
/* 782 */     MCD::OPC_Decode, 171, 2, 0, // Opcode: CMOVNEi
/* 786 */     MCD::OPC_FilterValue, 168, 1, 4, 0, 0, // Skip to: 796
/* 792 */     MCD::OPC_Decode, 143, 3, 0, // Opcode: ORNOTi
/* 796 */     MCD::OPC_FilterValue, 192, 1, 4, 0, 0, // Skip to: 806
/* 802 */     MCD::OPC_Decode, 197, 3, 0, // Opcode: XORi
/* 806 */     MCD::OPC_FilterValue, 196, 1, 4, 0, 0, // Skip to: 816
/* 812 */     MCD::OPC_Decode, 169, 2, 0, // Opcode: CMOVLTi
/* 816 */     MCD::OPC_FilterValue, 198, 1, 4, 0, 0, // Skip to: 826
/* 822 */     MCD::OPC_Decode, 159, 2, 0, // Opcode: CMOVGEi
/* 826 */     MCD::OPC_FilterValue, 200, 1, 4, 0, 0, // Skip to: 836
/* 832 */     MCD::OPC_Decode, 212, 2, 0, // Opcode: EQVi
/* 836 */     MCD::OPC_FilterValue, 228, 1, 4, 0, 0, // Skip to: 846
/* 842 */     MCD::OPC_Decode, 167, 2, 0, // Opcode: CMOVLEi
/* 846 */     MCD::OPC_FilterValue, 230, 1, 150, 3, 0, // Skip to: 1770
/* 852 */     MCD::OPC_Decode, 161, 2, 0, // Opcode: CMOVGTi
/* 856 */     MCD::OPC_FilterValue, 18, 139, 0, 0, // Skip to: 1000
/* 861 */     MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 864 */     MCD::OPC_FilterValue, 6, 11, 0, 0, // Skip to: 880
/* 869 */     MCD::OPC_CheckField, 13, 3, 0, 126, 3, 0, // Skip to: 1770
/* 876 */     MCD::OPC_Decode, 214, 2, 0, // Opcode: EXTBL
/* 880 */     MCD::OPC_FilterValue, 22, 11, 0, 0, // Skip to: 896
/* 885 */     MCD::OPC_CheckField, 13, 3, 0, 110, 3, 0, // Skip to: 1770
/* 892 */     MCD::OPC_Decode, 216, 2, 0, // Opcode: EXTWL
/* 896 */     MCD::OPC_FilterValue, 38, 11, 0, 0, // Skip to: 912
/* 901 */     MCD::OPC_CheckField, 13, 3, 0, 94, 3, 0, // Skip to: 1770
/* 908 */     MCD::OPC_Decode, 215, 2, 0, // Opcode: EXTLL
/* 912 */     MCD::OPC_FilterValue, 52, 11, 0, 0, // Skip to: 928
/* 917 */     MCD::OPC_CheckField, 13, 3, 0, 78, 3, 0, // Skip to: 1770
/* 924 */     MCD::OPC_Decode, 173, 3, 0, // Opcode: SRLr
/* 928 */     MCD::OPC_FilterValue, 57, 11, 0, 0, // Skip to: 944
/* 933 */     MCD::OPC_CheckField, 13, 3, 0, 62, 3, 0, // Skip to: 1770
/* 940 */     MCD::OPC_Decode, 167, 3, 0, // Opcode: SLr
/* 944 */     MCD::OPC_FilterValue, 60, 11, 0, 0, // Skip to: 960
/* 949 */     MCD::OPC_CheckField, 13, 3, 0, 46, 3, 0, // Skip to: 1770
/* 956 */     MCD::OPC_Decode, 171, 3, 0, // Opcode: SRAr
/* 960 */     MCD::OPC_FilterValue, 177, 1, 4, 0, 0, // Skip to: 970
/* 966 */     MCD::OPC_Decode, 199, 3, 0, // Opcode: ZAPNOTi
/* 970 */     MCD::OPC_FilterValue, 180, 1, 4, 0, 0, // Skip to: 980
/* 976 */     MCD::OPC_Decode, 172, 3, 0, // Opcode: SRLi
/* 980 */     MCD::OPC_FilterValue, 185, 1, 4, 0, 0, // Skip to: 990
/* 986 */     MCD::OPC_Decode, 166, 3, 0, // Opcode: SLi
/* 990 */     MCD::OPC_FilterValue, 188, 1, 6, 3, 0, // Skip to: 1770
/* 996 */     MCD::OPC_Decode, 170, 3, 0, // Opcode: SRAi
/* 1000 */    MCD::OPC_FilterValue, 19, 81, 0, 0, // Skip to: 1086
/* 1005 */    MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 1008 */    MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 1024
/* 1013 */    MCD::OPC_CheckField, 13, 3, 0, 238, 2, 0, // Skip to: 1770
/* 1020 */    MCD::OPC_Decode, 138, 3, 0, // Opcode: MULLr
/* 1024 */    MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 1040
/* 1029 */    MCD::OPC_CheckField, 13, 3, 0, 222, 2, 0, // Skip to: 1770
/* 1036 */    MCD::OPC_Decode, 140, 3, 0, // Opcode: MULQr
/* 1040 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1056
/* 1045 */    MCD::OPC_CheckField, 13, 3, 0, 206, 2, 0, // Skip to: 1770
/* 1052 */    MCD::OPC_Decode, 195, 3, 0, // Opcode: UMULHr
/* 1056 */    MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 1066
/* 1062 */    MCD::OPC_Decode, 137, 3, 0, // Opcode: MULLi
/* 1066 */    MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 1076
/* 1072 */    MCD::OPC_Decode, 139, 3, 0, // Opcode: MULQi
/* 1076 */    MCD::OPC_FilterValue, 176, 1, 176, 2, 0, // Skip to: 1770
/* 1082 */    MCD::OPC_Decode, 194, 3, 0, // Opcode: UMULHi
/* 1086 */    MCD::OPC_FilterValue, 20, 69, 0, 0, // Skip to: 1160
/* 1091 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1094 */    MCD::OPC_FilterValue, 4, 11, 0, 0, // Skip to: 1110
/* 1099 */    MCD::OPC_CheckField, 16, 5, 31, 152, 2, 0, // Skip to: 1770
/* 1106 */    MCD::OPC_Decode, 237, 2, 0, // Opcode: ITOFS
/* 1110 */    MCD::OPC_FilterValue, 36, 11, 0, 0, // Skip to: 1126
/* 1115 */    MCD::OPC_CheckField, 16, 5, 31, 136, 2, 0, // Skip to: 1770
/* 1122 */    MCD::OPC_Decode, 238, 2, 0, // Opcode: ITOFT
/* 1126 */    MCD::OPC_FilterValue, 139, 11, 11, 0, 0, // Skip to: 1143
/* 1132 */    MCD::OPC_CheckField, 21, 5, 31, 119, 2, 0, // Skip to: 1770
/* 1139 */    MCD::OPC_Decode, 168, 3, 0, // Opcode: SQRTS
/* 1143 */    MCD::OPC_FilterValue, 171, 11, 109, 2, 0, // Skip to: 1770
/* 1149 */    MCD::OPC_CheckField, 21, 5, 31, 102, 2, 0, // Skip to: 1770
/* 1156 */    MCD::OPC_Decode, 169, 3, 0, // Opcode: SQRTT
/* 1160 */    MCD::OPC_FilterValue, 22, 208, 0, 0, // Skip to: 1373
/* 1165 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1168 */    MCD::OPC_FilterValue, 175, 10, 11, 0, 0, // Skip to: 1185
/* 1174 */    MCD::OPC_CheckField, 21, 5, 31, 77, 2, 0, // Skip to: 1770
/* 1181 */    MCD::OPC_Decode, 208, 2, 0, // Opcode: CVTTQ
/* 1185 */    MCD::OPC_FilterValue, 128, 11, 4, 0, 0, // Skip to: 1195
/* 1191 */    MCD::OPC_Decode, 139, 2, 0, // Opcode: ADDS
/* 1195 */    MCD::OPC_FilterValue, 129, 11, 4, 0, 0, // Skip to: 1205
/* 1201 */    MCD::OPC_Decode, 192, 3, 0, // Opcode: SUBS
/* 1205 */    MCD::OPC_FilterValue, 130, 11, 4, 0, 0, // Skip to: 1215
/* 1211 */    MCD::OPC_Decode, 141, 3, 0, // Opcode: MULS
/* 1215 */    MCD::OPC_FilterValue, 131, 11, 4, 0, 0, // Skip to: 1225
/* 1221 */    MCD::OPC_Decode, 210, 2, 0, // Opcode: DIVS
/* 1225 */    MCD::OPC_FilterValue, 160, 11, 4, 0, 0, // Skip to: 1235
/* 1231 */    MCD::OPC_Decode, 140, 2, 0, // Opcode: ADDT
/* 1235 */    MCD::OPC_FilterValue, 161, 11, 4, 0, 0, // Skip to: 1245
/* 1241 */    MCD::OPC_Decode, 193, 3, 0, // Opcode: SUBT
/* 1245 */    MCD::OPC_FilterValue, 162, 11, 4, 0, 0, // Skip to: 1255
/* 1251 */    MCD::OPC_Decode, 142, 3, 0, // Opcode: MULT
/* 1255 */    MCD::OPC_FilterValue, 163, 11, 4, 0, 0, // Skip to: 1265
/* 1261 */    MCD::OPC_Decode, 211, 2, 0, // Opcode: DIVT
/* 1265 */    MCD::OPC_FilterValue, 164, 11, 4, 0, 0, // Skip to: 1275
/* 1271 */    MCD::OPC_Decode, 184, 2, 0, // Opcode: CMPTUN
/* 1275 */    MCD::OPC_FilterValue, 165, 11, 4, 0, 0, // Skip to: 1285
/* 1281 */    MCD::OPC_Decode, 181, 2, 0, // Opcode: CMPTEQ
/* 1285 */    MCD::OPC_FilterValue, 166, 11, 4, 0, 0, // Skip to: 1295
/* 1291 */    MCD::OPC_Decode, 183, 2, 0, // Opcode: CMPTLT
/* 1295 */    MCD::OPC_FilterValue, 167, 11, 4, 0, 0, // Skip to: 1305
/* 1301 */    MCD::OPC_Decode, 182, 2, 0, // Opcode: CMPTLE
/* 1305 */    MCD::OPC_FilterValue, 172, 13, 11, 0, 0, // Skip to: 1322
/* 1311 */    MCD::OPC_CheckField, 21, 5, 31, 196, 1, 0, // Skip to: 1770
/* 1318 */    MCD::OPC_Decode, 207, 2, 0, // Opcode: CVTST
/* 1322 */    MCD::OPC_FilterValue, 172, 15, 11, 0, 0, // Skip to: 1339
/* 1328 */    MCD::OPC_CheckField, 21, 5, 31, 179, 1, 0, // Skip to: 1770
/* 1335 */    MCD::OPC_Decode, 209, 2, 0, // Opcode: CVTTS
/* 1339 */    MCD::OPC_FilterValue, 188, 15, 11, 0, 0, // Skip to: 1356
/* 1345 */    MCD::OPC_CheckField, 21, 5, 31, 162, 1, 0, // Skip to: 1770
/* 1352 */    MCD::OPC_Decode, 205, 2, 0, // Opcode: CVTQS
/* 1356 */    MCD::OPC_FilterValue, 190, 15, 152, 1, 0, // Skip to: 1770
/* 1362 */    MCD::OPC_CheckField, 21, 5, 31, 145, 1, 0, // Skip to: 1770
/* 1369 */    MCD::OPC_Decode, 206, 2, 0, // Opcode: CVTQT
/* 1373 */    MCD::OPC_FilterValue, 23, 48, 0, 0, // Skip to: 1426
/* 1378 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1381 */    MCD::OPC_FilterValue, 32, 0, 0, 0, // Skip to: 1386
/* 1386 */    MCD::OPC_FilterValue, 33, 0, 0, 0, // Skip to: 1391
/* 1391 */    MCD::OPC_FilterValue, 34, 0, 0, 0, // Skip to: 1396
/* 1396 */    MCD::OPC_FilterValue, 42, 0, 0, 0, // Skip to: 1401
/* 1401 */    MCD::OPC_FilterValue, 43, 0, 0, 0, // Skip to: 1406
/* 1406 */    MCD::OPC_FilterValue, 44, 0, 0, 0, // Skip to: 1411
/* 1411 */    MCD::OPC_FilterValue, 45, 0, 0, 0, // Skip to: 1416
/* 1416 */    MCD::OPC_FilterValue, 46, 0, 0, 0, // Skip to: 1421
/* 1421 */    MCD::OPC_FilterValue, 47, 88, 1, 0, // Skip to: 1770
/* 1426 */    MCD::OPC_FilterValue, 24, 50, 0, 0, // Skip to: 1481
/* 1431 */    MCD::OPC_ExtractField, 0, 21,  // Inst{20-0} ...
/* 1434 */    MCD::OPC_FilterValue, 128, 128, 1, 11, 0, 0, // Skip to: 1452
/* 1441 */    MCD::OPC_CheckField, 21, 5, 0, 66, 1, 0, // Skip to: 1770
/* 1448 */    MCD::OPC_Decode, 136, 3, 0, // Opcode: MB
/* 1452 */    MCD::OPC_FilterValue, 128, 136, 1, 11, 0, 0, // Skip to: 1470
/* 1459 */    MCD::OPC_CheckField, 21, 5, 0, 48, 1, 0, // Skip to: 1770
/* 1466 */    MCD::OPC_Decode, 196, 3, 0, // Opcode: WMB
/* 1470 */    MCD::OPC_FilterValue, 128, 128, 3, 37, 1, 0, // Skip to: 1770
/* 1477 */    MCD::OPC_Decode, 147, 3, 0, // Opcode: RPCC
/* 1481 */    MCD::OPC_FilterValue, 26, 35, 0, 0, // Skip to: 1521
/* 1486 */    MCD::OPC_ExtractField, 0, 26,  // Inst{25-0} ...
/* 1489 */    MCD::OPC_FilterValue, 128, 128, 237, 23, 4, 0, 0, // Skip to: 1501
/* 1497 */    MCD::OPC_Decode, 242, 2, 0, // Opcode: JSRs
/* 1501 */    MCD::OPC_FilterValue, 128, 128, 237, 26, 4, 0, 0, // Skip to: 1513
/* 1509 */    MCD::OPC_Decode, 240, 2, 0, // Opcode: JSR
/* 1513 */    MCD::OPC_FilterValue, 129, 128, 234, 31, 249, 0, 0, // Skip to: 1770
/* 1521 */    MCD::OPC_FilterValue, 28, 115, 0, 0, // Skip to: 1641
/* 1526 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1529 */    MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 1545
/* 1534 */    MCD::OPC_CheckField, 21, 5, 31, 229, 0, 0, // Skip to: 1770
/* 1541 */    MCD::OPC_Decode, 164, 3, 0, // Opcode: SEXTB
/* 1545 */    MCD::OPC_FilterValue, 1, 11, 0, 0, // Skip to: 1561
/* 1550 */    MCD::OPC_CheckField, 21, 5, 31, 213, 0, 0, // Skip to: 1770
/* 1557 */    MCD::OPC_Decode, 165, 3, 0, // Opcode: SEXTW
/* 1561 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1577
/* 1566 */    MCD::OPC_CheckField, 21, 5, 31, 197, 0, 0, // Skip to: 1770
/* 1573 */    MCD::OPC_Decode, 203, 2, 0, // Opcode: CTPOP
/* 1577 */    MCD::OPC_FilterValue, 50, 11, 0, 0, // Skip to: 1593
/* 1582 */    MCD::OPC_CheckField, 21, 5, 31, 181, 0, 0, // Skip to: 1770
/* 1589 */    MCD::OPC_Decode, 202, 2, 0, // Opcode: CTLZ
/* 1593 */    MCD::OPC_FilterValue, 51, 11, 0, 0, // Skip to: 1609
/* 1598 */    MCD::OPC_CheckField, 21, 5, 31, 165, 0, 0, // Skip to: 1770
/* 1605 */    MCD::OPC_Decode, 204, 2, 0, // Opcode: CTTZ
/* 1609 */    MCD::OPC_FilterValue, 112, 11, 0, 0, // Skip to: 1625
/* 1614 */    MCD::OPC_CheckField, 16, 5, 31, 149, 0, 0, // Skip to: 1770
/* 1621 */    MCD::OPC_Decode, 236, 2, 0, // Opcode: FTOIT
/* 1625 */    MCD::OPC_FilterValue, 120, 140, 0, 0, // Skip to: 1770
/* 1630 */    MCD::OPC_CheckField, 16, 5, 31, 133, 0, 0, // Skip to: 1770
/* 1637 */    MCD::OPC_Decode, 235, 2, 0, // Opcode: FTOIS
/* 1641 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1657
/* 1646 */    MCD::OPC_CheckField, 21, 5, 31, 117, 0, 0, // Skip to: 1770
/* 1653 */    MCD::OPC_Decode, 155, 2, 0, // Opcode: BR
/* 1657 */    MCD::OPC_FilterValue, 49, 4, 0, 0, // Skip to: 1666
/* 1662 */    MCD::OPC_Decode, 217, 2, 0, // Opcode: FBEQ
/* 1666 */    MCD::OPC_FilterValue, 50, 4, 0, 0, // Skip to: 1675
/* 1671 */    MCD::OPC_Decode, 221, 2, 0, // Opcode: FBLT
/* 1675 */    MCD::OPC_FilterValue, 51, 4, 0, 0, // Skip to: 1684
/* 1680 */    MCD::OPC_Decode, 220, 2, 0, // Opcode: FBLE
/* 1684 */    MCD::OPC_FilterValue, 54, 0, 0, 0, // Skip to: 1689
/* 1689 */    MCD::OPC_FilterValue, 55, 4, 0, 0, // Skip to: 1698
/* 1694 */    MCD::OPC_Decode, 219, 2, 0, // Opcode: FBGT
/* 1698 */    MCD::OPC_FilterValue, 56, 4, 0, 0, // Skip to: 1707
/* 1703 */    MCD::OPC_Decode, 150, 2, 0, // Opcode: BLBC
/* 1707 */    MCD::OPC_FilterValue, 57, 4, 0, 0, // Skip to: 1716
/* 1712 */    MCD::OPC_Decode, 143, 2, 0, // Opcode: BEQ
/* 1716 */    MCD::OPC_FilterValue, 58, 4, 0, 0, // Skip to: 1725
/* 1721 */    MCD::OPC_Decode, 153, 2, 0, // Opcode: BLT
/* 1725 */    MCD::OPC_FilterValue, 59, 4, 0, 0, // Skip to: 1734
/* 1730 */    MCD::OPC_Decode, 152, 2, 0, // Opcode: BLE
/* 1734 */    MCD::OPC_FilterValue, 60, 4, 0, 0, // Skip to: 1743
/* 1739 */    MCD::OPC_Decode, 151, 2, 0, // Opcode: BLBS
/* 1743 */    MCD::OPC_FilterValue, 61, 4, 0, 0, // Skip to: 1752
/* 1748 */    MCD::OPC_Decode, 154, 2, 0, // Opcode: BNE
/* 1752 */    MCD::OPC_FilterValue, 62, 4, 0, 0, // Skip to: 1761
/* 1757 */    MCD::OPC_Decode, 144, 2, 0, // Opcode: BGE
/* 1761 */    MCD::OPC_FilterValue, 63, 4, 0, 0, // Skip to: 1770
/* 1766 */    MCD::OPC_Decode, 145, 2, 0, // Opcode: BGT
/* 1770 */    MCD::OPC_Fail,
  0
};

static bool checkDecoderPredicate(unsigned Idx, const FeatureBitset &Bits) {
  llvm_unreachable("Invalid index!");
}

template <typename InsnType>
static DecodeStatus decodeToMCInst(DecodeStatus S, unsigned Idx, InsnType insn, MCInst &MI,
                                   uint64_t Address, const MCDisassembler *Decoder, bool &DecodeComplete) {
  DecodeComplete = true;
  using TmpType = std::conditional_t<std::is_integral<InsnType>::value, InsnType, uint64_t>;
  TmpType tmp;
  switch (Idx) {
  default: llvm_unreachable("Invalid index!");
  case 0:
    return S;
  }
}

template <typename InsnType>
static DecodeStatus decodeInstruction(const uint8_t DecodeTable[], MCInst &MI,
                                      InsnType insn, uint64_t Address,
                                      const MCDisassembler *DisAsm,
                                      const MCSubtargetInfo &STI) {
  const FeatureBitset &Bits = STI.getFeatureBits();

  const uint8_t *Ptr = DecodeTable;
  uint64_t CurFieldValue = 0;
  DecodeStatus S = MCDisassembler::Success;
  while (true) {
    ptrdiff_t Loc = Ptr - DecodeTable;
    switch (*Ptr) {
    default:
      errs() << Loc << ": Unexpected decode table opcode!\n";
      return MCDisassembler::Fail;
    case MCD::OPC_ExtractField: {
      unsigned Start = *++Ptr;
      unsigned Len = *++Ptr;
      ++Ptr;
      CurFieldValue = fieldFromInstruction(insn, Start, Len);
      LLVM_DEBUG(dbgs() << Loc << ": OPC_ExtractField(" << Start << ", "
                   << Len << "): " << CurFieldValue << "\n");
      break;
    }
    case MCD::OPC_FilterValue: {
      // Decode the field value.
      unsigned Len;
      uint64_t Val = decodeULEB128(++Ptr, &Len);
      Ptr += Len;
      // NumToSkip is a plain 24-bit integer.
      unsigned NumToSkip = *Ptr++;
      NumToSkip |= (*Ptr++) << 8;
      NumToSkip |= (*Ptr++) << 16;

      // Perform the filter operation.
      if (Val != CurFieldValue)
        Ptr += NumToSkip;
      LLVM_DEBUG(dbgs() << Loc << ": OPC_FilterValue(" << Val << ", " << NumToSkip
                   << "): " << ((Val != CurFieldValue) ? "FAIL:" : "PASS:")
                   << " continuing at " << (Ptr - DecodeTable) << "\n");

      break;
    }
    case MCD::OPC_CheckField: {
      unsigned Start = *++Ptr;
      unsigned Len = *++Ptr;
      uint64_t FieldValue = fieldFromInstruction(insn, Start, Len);
      // Decode the field value.
      unsigned PtrLen = 0;
      uint64_t ExpectedValue = decodeULEB128(++Ptr, &PtrLen);
      Ptr += PtrLen;
      // NumToSkip is a plain 24-bit integer.
      unsigned NumToSkip = *Ptr++;
      NumToSkip |= (*Ptr++) << 8;
      NumToSkip |= (*Ptr++) << 16;

      // If the actual and expected values don't match, skip.
      if (ExpectedValue != FieldValue)
        Ptr += NumToSkip;
      LLVM_DEBUG(dbgs() << Loc << ": OPC_CheckField(" << Start << ", "
                   << Len << ", " << ExpectedValue << ", " << NumToSkip
                   << "): FieldValue = " << FieldValue << ", ExpectedValue = "
                   << ExpectedValue << ": "
                   << ((ExpectedValue == FieldValue) ? "PASS\n" : "FAIL\n"));
      break;
    }
    case MCD::OPC_CheckPredicate: {
      unsigned Len;
      // Decode the Predicate Index value.
      unsigned PIdx = decodeULEB128(++Ptr, &Len);
      Ptr += Len;
      // NumToSkip is a plain 24-bit integer.
      unsigned NumToSkip = *Ptr++;
      NumToSkip |= (*Ptr++) << 8;
      NumToSkip |= (*Ptr++) << 16;
      // Check the predicate.
      bool Pred;
      if (!(Pred = checkDecoderPredicate(PIdx, Bits)))
        Ptr += NumToSkip;
      (void)Pred;
      LLVM_DEBUG(dbgs() << Loc << ": OPC_CheckPredicate(" << PIdx << "): "
            << (Pred ? "PASS\n" : "FAIL\n"));

      break;
    }
    case MCD::OPC_Decode: {
      unsigned Len;
      // Decode the Opcode value.
      unsigned Opc = decodeULEB128(++Ptr, &Len);
      Ptr += Len;
      unsigned DecodeIdx = decodeULEB128(Ptr, &Len);
      Ptr += Len;

      MI.clear();
      MI.setOpcode(Opc);
      bool DecodeComplete;
      S = decodeToMCInst(S, DecodeIdx, insn, MI, Address, DisAsm, DecodeComplete);
      assert(DecodeComplete);

      LLVM_DEBUG(dbgs() << Loc << ": OPC_Decode: opcode " << Opc
                   << ", using decoder " << DecodeIdx << ": "
                   << (S != MCDisassembler::Fail ? "PASS" : "FAIL") << "\n");
      return S;
    }
    case MCD::OPC_TryDecode: {
      unsigned Len;
      // Decode the Opcode value.
      unsigned Opc = decodeULEB128(++Ptr, &Len);
      Ptr += Len;
      unsigned DecodeIdx = decodeULEB128(Ptr, &Len);
      Ptr += Len;
      // NumToSkip is a plain 24-bit integer.
      unsigned NumToSkip = *Ptr++;
      NumToSkip |= (*Ptr++) << 8;
      NumToSkip |= (*Ptr++) << 16;

      // Perform the decode operation.
      MCInst TmpMI;
      TmpMI.setOpcode(Opc);
      bool DecodeComplete;
      S = decodeToMCInst(S, DecodeIdx, insn, TmpMI, Address, DisAsm, DecodeComplete);
      LLVM_DEBUG(dbgs() << Loc << ": OPC_TryDecode: opcode " << Opc
                   << ", using decoder " << DecodeIdx << ": ");

      if (DecodeComplete) {
        // Decoding complete.
        LLVM_DEBUG(dbgs() << (S != MCDisassembler::Fail ? "PASS" : "FAIL") << "\n");
        MI = TmpMI;
        return S;
      } else {
        assert(S == MCDisassembler::Fail);
        // If the decoding was incomplete, skip.
        Ptr += NumToSkip;
        LLVM_DEBUG(dbgs() << "FAIL: continuing at " << (Ptr - DecodeTable) << "\n");
        // Reset decode status. This also drops a SoftFail status that could be
        // set before the decode attempt.
        S = MCDisassembler::Success;
      }
      break;
    }
    case MCD::OPC_SoftFail: {
      // Decode the mask values.
      unsigned Len;
      uint64_t PositiveMask = decodeULEB128(++Ptr, &Len);
      Ptr += Len;
      uint64_t NegativeMask = decodeULEB128(Ptr, &Len);
      Ptr += Len;
      bool Fail = (insn & PositiveMask) != 0 || (~insn & NegativeMask) != 0;
      if (Fail)
        S = MCDisassembler::SoftFail;
      LLVM_DEBUG(dbgs() << Loc << ": OPC_SoftFail: " << (Fail ? "FAIL\n" : "PASS\n"));
      break;
    }
    case MCD::OPC_Fail: {
      LLVM_DEBUG(dbgs() << Loc << ": OPC_Fail\n");
      return MCDisassembler::Fail;
    }
    }
  }
  llvm_unreachable("bogosity detected in disassembler state machine!");
}

} // end namespace llvm

