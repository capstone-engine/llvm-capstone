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
/* 3 */       MCD::OPC_FilterValue, 0, 4, 0, 0, // Skip to: 12
/* 8 */       MCD::OPC_Decode, 190, 2, 0, // Opcode: COND_BRANCH_I
/* 12 */      MCD::OPC_FilterValue, 8, 4, 0, 0, // Skip to: 21
/* 17 */      MCD::OPC_Decode, 243, 2, 1, // Opcode: LDA
/* 21 */      MCD::OPC_FilterValue, 9, 4, 0, 0, // Skip to: 30
/* 26 */      MCD::OPC_Decode, 244, 2, 1, // Opcode: LDAH
/* 30 */      MCD::OPC_FilterValue, 10, 4, 0, 0, // Skip to: 39
/* 35 */      MCD::OPC_Decode, 249, 2, 1, // Opcode: LDBU
/* 39 */      MCD::OPC_FilterValue, 12, 4, 0, 0, // Skip to: 48
/* 44 */      MCD::OPC_Decode, 134, 3, 1, // Opcode: LDWU
/* 48 */      MCD::OPC_FilterValue, 13, 4, 0, 0, // Skip to: 57
/* 53 */      MCD::OPC_Decode, 186, 3, 1, // Opcode: STW
/* 57 */      MCD::OPC_FilterValue, 14, 4, 0, 0, // Skip to: 66
/* 62 */      MCD::OPC_Decode, 174, 3, 1, // Opcode: STB
/* 66 */      MCD::OPC_FilterValue, 16, 215, 1, 0, // Skip to: 542
/* 71 */      MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 74 */      MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 90
/* 79 */      MCD::OPC_CheckField, 13, 3, 0, 131, 7, 0, // Skip to: 2009
/* 86 */      MCD::OPC_Decode, 136, 2, 2, // Opcode: ADDLr
/* 90 */      MCD::OPC_FilterValue, 2, 11, 0, 0, // Skip to: 106
/* 95 */      MCD::OPC_CheckField, 13, 3, 0, 115, 7, 0, // Skip to: 2009
/* 102 */     MCD::OPC_Decode, 149, 3, 2, // Opcode: S4ADDLr
/* 106 */     MCD::OPC_FilterValue, 9, 11, 0, 0, // Skip to: 122
/* 111 */     MCD::OPC_CheckField, 13, 3, 0, 99, 7, 0, // Skip to: 2009
/* 118 */     MCD::OPC_Decode, 189, 3, 2, // Opcode: SUBLr
/* 122 */     MCD::OPC_FilterValue, 11, 11, 0, 0, // Skip to: 138
/* 127 */     MCD::OPC_CheckField, 13, 3, 0, 83, 7, 0, // Skip to: 2009
/* 134 */     MCD::OPC_Decode, 153, 3, 2, // Opcode: S4SUBLr
/* 138 */     MCD::OPC_FilterValue, 15, 11, 0, 0, // Skip to: 154
/* 143 */     MCD::OPC_CheckField, 13, 3, 0, 67, 7, 0, // Skip to: 2009
/* 150 */     MCD::OPC_Decode, 173, 2, 2, // Opcode: CMPBGE
/* 154 */     MCD::OPC_FilterValue, 18, 11, 0, 0, // Skip to: 170
/* 159 */     MCD::OPC_CheckField, 13, 3, 0, 51, 7, 0, // Skip to: 2009
/* 166 */     MCD::OPC_Decode, 157, 3, 2, // Opcode: S8ADDLr
/* 170 */     MCD::OPC_FilterValue, 27, 11, 0, 0, // Skip to: 186
/* 175 */     MCD::OPC_CheckField, 13, 3, 0, 35, 7, 0, // Skip to: 2009
/* 182 */     MCD::OPC_Decode, 161, 3, 2, // Opcode: S8SUBLr
/* 186 */     MCD::OPC_FilterValue, 29, 11, 0, 0, // Skip to: 202
/* 191 */     MCD::OPC_CheckField, 13, 3, 0, 19, 7, 0, // Skip to: 2009
/* 198 */     MCD::OPC_Decode, 187, 2, 2, // Opcode: CMPULT
/* 202 */     MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 218
/* 207 */     MCD::OPC_CheckField, 13, 3, 0, 3, 7, 0, // Skip to: 2009
/* 214 */     MCD::OPC_Decode, 138, 2, 2, // Opcode: ADDQr
/* 218 */     MCD::OPC_FilterValue, 34, 11, 0, 0, // Skip to: 234
/* 223 */     MCD::OPC_CheckField, 13, 3, 0, 243, 6, 0, // Skip to: 2009
/* 230 */     MCD::OPC_Decode, 151, 3, 2, // Opcode: S4ADDQr
/* 234 */     MCD::OPC_FilterValue, 41, 11, 0, 0, // Skip to: 250
/* 239 */     MCD::OPC_CheckField, 13, 3, 0, 227, 6, 0, // Skip to: 2009
/* 246 */     MCD::OPC_Decode, 191, 3, 2, // Opcode: SUBQr
/* 250 */     MCD::OPC_FilterValue, 43, 11, 0, 0, // Skip to: 266
/* 255 */     MCD::OPC_CheckField, 13, 3, 0, 211, 6, 0, // Skip to: 2009
/* 262 */     MCD::OPC_Decode, 155, 3, 2, // Opcode: S4SUBQr
/* 266 */     MCD::OPC_FilterValue, 45, 11, 0, 0, // Skip to: 282
/* 271 */     MCD::OPC_CheckField, 13, 3, 0, 195, 6, 0, // Skip to: 2009
/* 278 */     MCD::OPC_Decode, 175, 2, 2, // Opcode: CMPEQ
/* 282 */     MCD::OPC_FilterValue, 50, 11, 0, 0, // Skip to: 298
/* 287 */     MCD::OPC_CheckField, 13, 3, 0, 179, 6, 0, // Skip to: 2009
/* 294 */     MCD::OPC_Decode, 159, 3, 2, // Opcode: S8ADDQr
/* 298 */     MCD::OPC_FilterValue, 59, 11, 0, 0, // Skip to: 314
/* 303 */     MCD::OPC_CheckField, 13, 3, 0, 163, 6, 0, // Skip to: 2009
/* 310 */     MCD::OPC_Decode, 163, 3, 2, // Opcode: S8SUBQr
/* 314 */     MCD::OPC_FilterValue, 61, 11, 0, 0, // Skip to: 330
/* 319 */     MCD::OPC_CheckField, 13, 3, 0, 147, 6, 0, // Skip to: 2009
/* 326 */     MCD::OPC_Decode, 185, 2, 2, // Opcode: CMPULE
/* 330 */     MCD::OPC_FilterValue, 77, 11, 0, 0, // Skip to: 346
/* 335 */     MCD::OPC_CheckField, 13, 3, 0, 131, 6, 0, // Skip to: 2009
/* 342 */     MCD::OPC_Decode, 179, 2, 2, // Opcode: CMPLT
/* 346 */     MCD::OPC_FilterValue, 109, 11, 0, 0, // Skip to: 362
/* 351 */     MCD::OPC_CheckField, 13, 3, 0, 115, 6, 0, // Skip to: 2009
/* 358 */     MCD::OPC_Decode, 177, 2, 2, // Opcode: CMPLE
/* 362 */     MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 372
/* 368 */     MCD::OPC_Decode, 135, 2, 3, // Opcode: ADDLi
/* 372 */     MCD::OPC_FilterValue, 130, 1, 4, 0, 0, // Skip to: 382
/* 378 */     MCD::OPC_Decode, 148, 3, 3, // Opcode: S4ADDLi
/* 382 */     MCD::OPC_FilterValue, 137, 1, 4, 0, 0, // Skip to: 392
/* 388 */     MCD::OPC_Decode, 188, 3, 3, // Opcode: SUBLi
/* 392 */     MCD::OPC_FilterValue, 139, 1, 4, 0, 0, // Skip to: 402
/* 398 */     MCD::OPC_Decode, 152, 3, 3, // Opcode: S4SUBLi
/* 402 */     MCD::OPC_FilterValue, 143, 1, 4, 0, 0, // Skip to: 412
/* 408 */     MCD::OPC_Decode, 174, 2, 3, // Opcode: CMPBGEi
/* 412 */     MCD::OPC_FilterValue, 146, 1, 4, 0, 0, // Skip to: 422
/* 418 */     MCD::OPC_Decode, 156, 3, 3, // Opcode: S8ADDLi
/* 422 */     MCD::OPC_FilterValue, 155, 1, 4, 0, 0, // Skip to: 432
/* 428 */     MCD::OPC_Decode, 160, 3, 3, // Opcode: S8SUBLi
/* 432 */     MCD::OPC_FilterValue, 157, 1, 4, 0, 0, // Skip to: 442
/* 438 */     MCD::OPC_Decode, 188, 2, 3, // Opcode: CMPULTi
/* 442 */     MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 452
/* 448 */     MCD::OPC_Decode, 137, 2, 3, // Opcode: ADDQi
/* 452 */     MCD::OPC_FilterValue, 162, 1, 4, 0, 0, // Skip to: 462
/* 458 */     MCD::OPC_Decode, 150, 3, 3, // Opcode: S4ADDQi
/* 462 */     MCD::OPC_FilterValue, 169, 1, 4, 0, 0, // Skip to: 472
/* 468 */     MCD::OPC_Decode, 190, 3, 3, // Opcode: SUBQi
/* 472 */     MCD::OPC_FilterValue, 171, 1, 4, 0, 0, // Skip to: 482
/* 478 */     MCD::OPC_Decode, 154, 3, 3, // Opcode: S4SUBQi
/* 482 */     MCD::OPC_FilterValue, 173, 1, 4, 0, 0, // Skip to: 492
/* 488 */     MCD::OPC_Decode, 176, 2, 3, // Opcode: CMPEQi
/* 492 */     MCD::OPC_FilterValue, 178, 1, 4, 0, 0, // Skip to: 502
/* 498 */     MCD::OPC_Decode, 158, 3, 3, // Opcode: S8ADDQi
/* 502 */     MCD::OPC_FilterValue, 187, 1, 4, 0, 0, // Skip to: 512
/* 508 */     MCD::OPC_Decode, 162, 3, 3, // Opcode: S8SUBQi
/* 512 */     MCD::OPC_FilterValue, 189, 1, 4, 0, 0, // Skip to: 522
/* 518 */     MCD::OPC_Decode, 186, 2, 3, // Opcode: CMPULEi
/* 522 */     MCD::OPC_FilterValue, 205, 1, 4, 0, 0, // Skip to: 532
/* 528 */     MCD::OPC_Decode, 180, 2, 3, // Opcode: CMPLTi
/* 532 */     MCD::OPC_FilterValue, 237, 1, 191, 5, 0, // Skip to: 2009
/* 538 */     MCD::OPC_Decode, 178, 2, 3, // Opcode: CMPLEi
/* 542 */     MCD::OPC_FilterValue, 17, 111, 1, 0, // Skip to: 914
/* 547 */     MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 550 */     MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 566
/* 555 */     MCD::OPC_CheckField, 13, 3, 0, 167, 5, 0, // Skip to: 2009
/* 562 */     MCD::OPC_Decode, 142, 2, 2, // Opcode: ANDr
/* 566 */     MCD::OPC_FilterValue, 8, 11, 0, 0, // Skip to: 582
/* 571 */     MCD::OPC_CheckField, 13, 3, 0, 151, 5, 0, // Skip to: 2009
/* 578 */     MCD::OPC_Decode, 147, 2, 2, // Opcode: BICr
/* 582 */     MCD::OPC_FilterValue, 20, 11, 0, 0, // Skip to: 598
/* 587 */     MCD::OPC_CheckField, 13, 3, 0, 135, 5, 0, // Skip to: 2009
/* 594 */     MCD::OPC_Decode, 166, 2, 4, // Opcode: CMOVLBSr
/* 598 */     MCD::OPC_FilterValue, 22, 11, 0, 0, // Skip to: 614
/* 603 */     MCD::OPC_CheckField, 13, 3, 0, 119, 5, 0, // Skip to: 2009
/* 610 */     MCD::OPC_Decode, 164, 2, 4, // Opcode: CMOVLBCr
/* 614 */     MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 630
/* 619 */     MCD::OPC_CheckField, 13, 3, 0, 103, 5, 0, // Skip to: 2009
/* 626 */     MCD::OPC_Decode, 149, 2, 2, // Opcode: BISr
/* 630 */     MCD::OPC_FilterValue, 36, 11, 0, 0, // Skip to: 646
/* 635 */     MCD::OPC_CheckField, 13, 3, 0, 87, 5, 0, // Skip to: 2009
/* 642 */     MCD::OPC_Decode, 158, 2, 4, // Opcode: CMOVEQr
/* 646 */     MCD::OPC_FilterValue, 38, 11, 0, 0, // Skip to: 662
/* 651 */     MCD::OPC_CheckField, 13, 3, 0, 71, 5, 0, // Skip to: 2009
/* 658 */     MCD::OPC_Decode, 172, 2, 4, // Opcode: CMOVNEr
/* 662 */     MCD::OPC_FilterValue, 40, 11, 0, 0, // Skip to: 678
/* 667 */     MCD::OPC_CheckField, 13, 3, 0, 55, 5, 0, // Skip to: 2009
/* 674 */     MCD::OPC_Decode, 144, 3, 2, // Opcode: ORNOTr
/* 678 */     MCD::OPC_FilterValue, 64, 11, 0, 0, // Skip to: 694
/* 683 */     MCD::OPC_CheckField, 13, 3, 0, 39, 5, 0, // Skip to: 2009
/* 690 */     MCD::OPC_Decode, 198, 3, 2, // Opcode: XORr
/* 694 */     MCD::OPC_FilterValue, 68, 11, 0, 0, // Skip to: 710
/* 699 */     MCD::OPC_CheckField, 13, 3, 0, 23, 5, 0, // Skip to: 2009
/* 706 */     MCD::OPC_Decode, 170, 2, 4, // Opcode: CMOVLTr
/* 710 */     MCD::OPC_FilterValue, 70, 11, 0, 0, // Skip to: 726
/* 715 */     MCD::OPC_CheckField, 13, 3, 0, 7, 5, 0, // Skip to: 2009
/* 722 */     MCD::OPC_Decode, 160, 2, 4, // Opcode: CMOVGEr
/* 726 */     MCD::OPC_FilterValue, 72, 11, 0, 0, // Skip to: 742
/* 731 */     MCD::OPC_CheckField, 13, 3, 0, 247, 4, 0, // Skip to: 2009
/* 738 */     MCD::OPC_Decode, 213, 2, 2, // Opcode: EQVr
/* 742 */     MCD::OPC_FilterValue, 100, 11, 0, 0, // Skip to: 758
/* 747 */     MCD::OPC_CheckField, 13, 3, 0, 231, 4, 0, // Skip to: 2009
/* 754 */     MCD::OPC_Decode, 168, 2, 4, // Opcode: CMOVLEr
/* 758 */     MCD::OPC_FilterValue, 102, 11, 0, 0, // Skip to: 774
/* 763 */     MCD::OPC_CheckField, 13, 3, 0, 215, 4, 0, // Skip to: 2009
/* 770 */     MCD::OPC_Decode, 162, 2, 4, // Opcode: CMOVGTr
/* 774 */     MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 784
/* 780 */     MCD::OPC_Decode, 141, 2, 3, // Opcode: ANDi
/* 784 */     MCD::OPC_FilterValue, 136, 1, 4, 0, 0, // Skip to: 794
/* 790 */     MCD::OPC_Decode, 146, 2, 3, // Opcode: BICi
/* 794 */     MCD::OPC_FilterValue, 148, 1, 4, 0, 0, // Skip to: 804
/* 800 */     MCD::OPC_Decode, 165, 2, 5, // Opcode: CMOVLBSi
/* 804 */     MCD::OPC_FilterValue, 150, 1, 4, 0, 0, // Skip to: 814
/* 810 */     MCD::OPC_Decode, 163, 2, 5, // Opcode: CMOVLBCi
/* 814 */     MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 824
/* 820 */     MCD::OPC_Decode, 148, 2, 3, // Opcode: BISi
/* 824 */     MCD::OPC_FilterValue, 164, 1, 4, 0, 0, // Skip to: 834
/* 830 */     MCD::OPC_Decode, 157, 2, 5, // Opcode: CMOVEQi
/* 834 */     MCD::OPC_FilterValue, 166, 1, 4, 0, 0, // Skip to: 844
/* 840 */     MCD::OPC_Decode, 171, 2, 5, // Opcode: CMOVNEi
/* 844 */     MCD::OPC_FilterValue, 168, 1, 4, 0, 0, // Skip to: 854
/* 850 */     MCD::OPC_Decode, 143, 3, 3, // Opcode: ORNOTi
/* 854 */     MCD::OPC_FilterValue, 192, 1, 4, 0, 0, // Skip to: 864
/* 860 */     MCD::OPC_Decode, 197, 3, 3, // Opcode: XORi
/* 864 */     MCD::OPC_FilterValue, 196, 1, 4, 0, 0, // Skip to: 874
/* 870 */     MCD::OPC_Decode, 169, 2, 5, // Opcode: CMOVLTi
/* 874 */     MCD::OPC_FilterValue, 198, 1, 4, 0, 0, // Skip to: 884
/* 880 */     MCD::OPC_Decode, 159, 2, 5, // Opcode: CMOVGEi
/* 884 */     MCD::OPC_FilterValue, 200, 1, 4, 0, 0, // Skip to: 894
/* 890 */     MCD::OPC_Decode, 212, 2, 3, // Opcode: EQVi
/* 894 */     MCD::OPC_FilterValue, 228, 1, 4, 0, 0, // Skip to: 904
/* 900 */     MCD::OPC_Decode, 167, 2, 5, // Opcode: CMOVLEi
/* 904 */     MCD::OPC_FilterValue, 230, 1, 75, 4, 0, // Skip to: 2009
/* 910 */     MCD::OPC_Decode, 161, 2, 5, // Opcode: CMOVGTi
/* 914 */     MCD::OPC_FilterValue, 18, 139, 0, 0, // Skip to: 1058
/* 919 */     MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 922 */     MCD::OPC_FilterValue, 6, 11, 0, 0, // Skip to: 938
/* 927 */     MCD::OPC_CheckField, 13, 3, 0, 51, 4, 0, // Skip to: 2009
/* 934 */     MCD::OPC_Decode, 214, 2, 2, // Opcode: EXTBL
/* 938 */     MCD::OPC_FilterValue, 22, 11, 0, 0, // Skip to: 954
/* 943 */     MCD::OPC_CheckField, 13, 3, 0, 35, 4, 0, // Skip to: 2009
/* 950 */     MCD::OPC_Decode, 216, 2, 2, // Opcode: EXTWL
/* 954 */     MCD::OPC_FilterValue, 38, 11, 0, 0, // Skip to: 970
/* 959 */     MCD::OPC_CheckField, 13, 3, 0, 19, 4, 0, // Skip to: 2009
/* 966 */     MCD::OPC_Decode, 215, 2, 2, // Opcode: EXTLL
/* 970 */     MCD::OPC_FilterValue, 52, 11, 0, 0, // Skip to: 986
/* 975 */     MCD::OPC_CheckField, 13, 3, 0, 3, 4, 0, // Skip to: 2009
/* 982 */     MCD::OPC_Decode, 173, 3, 2, // Opcode: SRLr
/* 986 */     MCD::OPC_FilterValue, 57, 11, 0, 0, // Skip to: 1002
/* 991 */     MCD::OPC_CheckField, 13, 3, 0, 243, 3, 0, // Skip to: 2009
/* 998 */     MCD::OPC_Decode, 167, 3, 2, // Opcode: SLr
/* 1002 */    MCD::OPC_FilterValue, 60, 11, 0, 0, // Skip to: 1018
/* 1007 */    MCD::OPC_CheckField, 13, 3, 0, 227, 3, 0, // Skip to: 2009
/* 1014 */    MCD::OPC_Decode, 171, 3, 2, // Opcode: SRAr
/* 1018 */    MCD::OPC_FilterValue, 177, 1, 4, 0, 0, // Skip to: 1028
/* 1024 */    MCD::OPC_Decode, 199, 3, 3, // Opcode: ZAPNOTi
/* 1028 */    MCD::OPC_FilterValue, 180, 1, 4, 0, 0, // Skip to: 1038
/* 1034 */    MCD::OPC_Decode, 172, 3, 3, // Opcode: SRLi
/* 1038 */    MCD::OPC_FilterValue, 185, 1, 4, 0, 0, // Skip to: 1048
/* 1044 */    MCD::OPC_Decode, 166, 3, 3, // Opcode: SLi
/* 1048 */    MCD::OPC_FilterValue, 188, 1, 187, 3, 0, // Skip to: 2009
/* 1054 */    MCD::OPC_Decode, 170, 3, 3, // Opcode: SRAi
/* 1058 */    MCD::OPC_FilterValue, 19, 81, 0, 0, // Skip to: 1144
/* 1063 */    MCD::OPC_ExtractField, 5, 8,  // Inst{12-5} ...
/* 1066 */    MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 1082
/* 1071 */    MCD::OPC_CheckField, 13, 3, 0, 163, 3, 0, // Skip to: 2009
/* 1078 */    MCD::OPC_Decode, 138, 3, 2, // Opcode: MULLr
/* 1082 */    MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 1098
/* 1087 */    MCD::OPC_CheckField, 13, 3, 0, 147, 3, 0, // Skip to: 2009
/* 1094 */    MCD::OPC_Decode, 140, 3, 2, // Opcode: MULQr
/* 1098 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1114
/* 1103 */    MCD::OPC_CheckField, 13, 3, 0, 131, 3, 0, // Skip to: 2009
/* 1110 */    MCD::OPC_Decode, 195, 3, 2, // Opcode: UMULHr
/* 1114 */    MCD::OPC_FilterValue, 128, 1, 4, 0, 0, // Skip to: 1124
/* 1120 */    MCD::OPC_Decode, 137, 3, 3, // Opcode: MULLi
/* 1124 */    MCD::OPC_FilterValue, 160, 1, 4, 0, 0, // Skip to: 1134
/* 1130 */    MCD::OPC_Decode, 139, 3, 3, // Opcode: MULQi
/* 1134 */    MCD::OPC_FilterValue, 176, 1, 101, 3, 0, // Skip to: 2009
/* 1140 */    MCD::OPC_Decode, 194, 3, 3, // Opcode: UMULHi
/* 1144 */    MCD::OPC_FilterValue, 20, 69, 0, 0, // Skip to: 1218
/* 1149 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1152 */    MCD::OPC_FilterValue, 4, 11, 0, 0, // Skip to: 1168
/* 1157 */    MCD::OPC_CheckField, 16, 5, 31, 77, 3, 0, // Skip to: 2009
/* 1164 */    MCD::OPC_Decode, 237, 2, 6, // Opcode: ITOFS
/* 1168 */    MCD::OPC_FilterValue, 36, 11, 0, 0, // Skip to: 1184
/* 1173 */    MCD::OPC_CheckField, 16, 5, 31, 61, 3, 0, // Skip to: 2009
/* 1180 */    MCD::OPC_Decode, 238, 2, 7, // Opcode: ITOFT
/* 1184 */    MCD::OPC_FilterValue, 139, 11, 11, 0, 0, // Skip to: 1201
/* 1190 */    MCD::OPC_CheckField, 21, 5, 31, 44, 3, 0, // Skip to: 2009
/* 1197 */    MCD::OPC_Decode, 168, 3, 8, // Opcode: SQRTS
/* 1201 */    MCD::OPC_FilterValue, 171, 11, 34, 3, 0, // Skip to: 2009
/* 1207 */    MCD::OPC_CheckField, 21, 5, 31, 27, 3, 0, // Skip to: 2009
/* 1214 */    MCD::OPC_Decode, 169, 3, 9, // Opcode: SQRTT
/* 1218 */    MCD::OPC_FilterValue, 22, 208, 0, 0, // Skip to: 1431
/* 1223 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1226 */    MCD::OPC_FilterValue, 175, 10, 11, 0, 0, // Skip to: 1243
/* 1232 */    MCD::OPC_CheckField, 21, 5, 31, 2, 3, 0, // Skip to: 2009
/* 1239 */    MCD::OPC_Decode, 208, 2, 9, // Opcode: CVTTQ
/* 1243 */    MCD::OPC_FilterValue, 128, 11, 4, 0, 0, // Skip to: 1253
/* 1249 */    MCD::OPC_Decode, 139, 2, 10, // Opcode: ADDS
/* 1253 */    MCD::OPC_FilterValue, 129, 11, 4, 0, 0, // Skip to: 1263
/* 1259 */    MCD::OPC_Decode, 192, 3, 10, // Opcode: SUBS
/* 1263 */    MCD::OPC_FilterValue, 130, 11, 4, 0, 0, // Skip to: 1273
/* 1269 */    MCD::OPC_Decode, 141, 3, 10, // Opcode: MULS
/* 1273 */    MCD::OPC_FilterValue, 131, 11, 4, 0, 0, // Skip to: 1283
/* 1279 */    MCD::OPC_Decode, 210, 2, 10, // Opcode: DIVS
/* 1283 */    MCD::OPC_FilterValue, 160, 11, 4, 0, 0, // Skip to: 1293
/* 1289 */    MCD::OPC_Decode, 140, 2, 11, // Opcode: ADDT
/* 1293 */    MCD::OPC_FilterValue, 161, 11, 4, 0, 0, // Skip to: 1303
/* 1299 */    MCD::OPC_Decode, 193, 3, 11, // Opcode: SUBT
/* 1303 */    MCD::OPC_FilterValue, 162, 11, 4, 0, 0, // Skip to: 1313
/* 1309 */    MCD::OPC_Decode, 142, 3, 11, // Opcode: MULT
/* 1313 */    MCD::OPC_FilterValue, 163, 11, 4, 0, 0, // Skip to: 1323
/* 1319 */    MCD::OPC_Decode, 211, 2, 11, // Opcode: DIVT
/* 1323 */    MCD::OPC_FilterValue, 164, 11, 4, 0, 0, // Skip to: 1333
/* 1329 */    MCD::OPC_Decode, 184, 2, 11, // Opcode: CMPTUN
/* 1333 */    MCD::OPC_FilterValue, 165, 11, 4, 0, 0, // Skip to: 1343
/* 1339 */    MCD::OPC_Decode, 181, 2, 11, // Opcode: CMPTEQ
/* 1343 */    MCD::OPC_FilterValue, 166, 11, 4, 0, 0, // Skip to: 1353
/* 1349 */    MCD::OPC_Decode, 183, 2, 11, // Opcode: CMPTLT
/* 1353 */    MCD::OPC_FilterValue, 167, 11, 4, 0, 0, // Skip to: 1363
/* 1359 */    MCD::OPC_Decode, 182, 2, 11, // Opcode: CMPTLE
/* 1363 */    MCD::OPC_FilterValue, 172, 13, 11, 0, 0, // Skip to: 1380
/* 1369 */    MCD::OPC_CheckField, 21, 5, 31, 121, 2, 0, // Skip to: 2009
/* 1376 */    MCD::OPC_Decode, 207, 2, 12, // Opcode: CVTST
/* 1380 */    MCD::OPC_FilterValue, 172, 15, 11, 0, 0, // Skip to: 1397
/* 1386 */    MCD::OPC_CheckField, 21, 5, 31, 104, 2, 0, // Skip to: 2009
/* 1393 */    MCD::OPC_Decode, 209, 2, 13, // Opcode: CVTTS
/* 1397 */    MCD::OPC_FilterValue, 188, 15, 11, 0, 0, // Skip to: 1414
/* 1403 */    MCD::OPC_CheckField, 21, 5, 31, 87, 2, 0, // Skip to: 2009
/* 1410 */    MCD::OPC_Decode, 205, 2, 13, // Opcode: CVTQS
/* 1414 */    MCD::OPC_FilterValue, 190, 15, 77, 2, 0, // Skip to: 2009
/* 1420 */    MCD::OPC_CheckField, 21, 5, 31, 70, 2, 0, // Skip to: 2009
/* 1427 */    MCD::OPC_Decode, 206, 2, 9, // Opcode: CVTQT
/* 1431 */    MCD::OPC_FilterValue, 23, 84, 0, 0, // Skip to: 1520
/* 1436 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1439 */    MCD::OPC_FilterValue, 32, 4, 0, 0, // Skip to: 1448
/* 1444 */    MCD::OPC_Decode, 198, 2, 10, // Opcode: CPYSS
/* 1448 */    MCD::OPC_FilterValue, 33, 4, 0, 0, // Skip to: 1457
/* 1453 */    MCD::OPC_Decode, 196, 2, 11, // Opcode: CPYSNT
/* 1457 */    MCD::OPC_FilterValue, 34, 4, 0, 0, // Skip to: 1466
/* 1462 */    MCD::OPC_Decode, 191, 2, 10, // Opcode: CPYSES
/* 1466 */    MCD::OPC_FilterValue, 42, 4, 0, 0, // Skip to: 1475
/* 1471 */    MCD::OPC_Decode, 223, 2, 10, // Opcode: FCMOVEQS
/* 1475 */    MCD::OPC_FilterValue, 43, 4, 0, 0, // Skip to: 1484
/* 1480 */    MCD::OPC_Decode, 234, 2, 11, // Opcode: FCMOVNET
/* 1484 */    MCD::OPC_FilterValue, 44, 4, 0, 0, // Skip to: 1493
/* 1489 */    MCD::OPC_Decode, 231, 2, 10, // Opcode: FCMOVLTS
/* 1493 */    MCD::OPC_FilterValue, 45, 4, 0, 0, // Skip to: 1502
/* 1498 */    MCD::OPC_Decode, 225, 2, 10, // Opcode: FCMOVGES
/* 1502 */    MCD::OPC_FilterValue, 46, 4, 0, 0, // Skip to: 1511
/* 1507 */    MCD::OPC_Decode, 229, 2, 10, // Opcode: FCMOVLES
/* 1511 */    MCD::OPC_FilterValue, 47, 237, 1, 0, // Skip to: 2009
/* 1516 */    MCD::OPC_Decode, 227, 2, 10, // Opcode: FCMOVGTS
/* 1520 */    MCD::OPC_FilterValue, 24, 50, 0, 0, // Skip to: 1575
/* 1525 */    MCD::OPC_ExtractField, 0, 21,  // Inst{20-0} ...
/* 1528 */    MCD::OPC_FilterValue, 128, 128, 1, 11, 0, 0, // Skip to: 1546
/* 1535 */    MCD::OPC_CheckField, 21, 5, 0, 211, 1, 0, // Skip to: 2009
/* 1542 */    MCD::OPC_Decode, 136, 3, 14, // Opcode: MB
/* 1546 */    MCD::OPC_FilterValue, 128, 136, 1, 11, 0, 0, // Skip to: 1564
/* 1553 */    MCD::OPC_CheckField, 21, 5, 0, 193, 1, 0, // Skip to: 2009
/* 1560 */    MCD::OPC_Decode, 196, 3, 14, // Opcode: WMB
/* 1564 */    MCD::OPC_FilterValue, 128, 128, 3, 182, 1, 0, // Skip to: 2009
/* 1571 */    MCD::OPC_Decode, 147, 3, 15, // Opcode: RPCC
/* 1575 */    MCD::OPC_FilterValue, 26, 68, 0, 0, // Skip to: 1648
/* 1580 */    MCD::OPC_ExtractField, 0, 16,  // Inst{15-0} ...
/* 1583 */    MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 1599
/* 1588 */    MCD::OPC_CheckField, 21, 5, 31, 158, 1, 0, // Skip to: 2009
/* 1595 */    MCD::OPC_Decode, 239, 2, 16, // Opcode: JMP
/* 1599 */    MCD::OPC_FilterValue, 128, 128, 1, 23, 0, 0, // Skip to: 1629
/* 1606 */    MCD::OPC_ExtractField, 16, 10,  // Inst{25-16} ...
/* 1609 */    MCD::OPC_FilterValue, 251, 5, 4, 0, 0, // Skip to: 1619
/* 1615 */    MCD::OPC_Decode, 242, 2, 14, // Opcode: JSRs
/* 1619 */    MCD::OPC_FilterValue, 219, 6, 128, 1, 0, // Skip to: 2009
/* 1625 */    MCD::OPC_Decode, 240, 2, 14, // Opcode: JSR
/* 1629 */    MCD::OPC_FilterValue, 129, 128, 2, 117, 1, 0, // Skip to: 2009
/* 1636 */    MCD::OPC_CheckField, 16, 10, 250, 7, 109, 1, 0, // Skip to: 2009
/* 1644 */    MCD::OPC_Decode, 145, 3, 14, // Opcode: RETDAG
/* 1648 */    MCD::OPC_FilterValue, 28, 115, 0, 0, // Skip to: 1768
/* 1653 */    MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 1656 */    MCD::OPC_FilterValue, 0, 11, 0, 0, // Skip to: 1672
/* 1661 */    MCD::OPC_CheckField, 21, 5, 31, 85, 1, 0, // Skip to: 2009
/* 1668 */    MCD::OPC_Decode, 164, 3, 17, // Opcode: SEXTB
/* 1672 */    MCD::OPC_FilterValue, 1, 11, 0, 0, // Skip to: 1688
/* 1677 */    MCD::OPC_CheckField, 21, 5, 31, 69, 1, 0, // Skip to: 2009
/* 1684 */    MCD::OPC_Decode, 165, 3, 17, // Opcode: SEXTW
/* 1688 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1704
/* 1693 */    MCD::OPC_CheckField, 21, 5, 31, 53, 1, 0, // Skip to: 2009
/* 1700 */    MCD::OPC_Decode, 203, 2, 17, // Opcode: CTPOP
/* 1704 */    MCD::OPC_FilterValue, 50, 11, 0, 0, // Skip to: 1720
/* 1709 */    MCD::OPC_CheckField, 21, 5, 31, 37, 1, 0, // Skip to: 2009
/* 1716 */    MCD::OPC_Decode, 202, 2, 17, // Opcode: CTLZ
/* 1720 */    MCD::OPC_FilterValue, 51, 11, 0, 0, // Skip to: 1736
/* 1725 */    MCD::OPC_CheckField, 21, 5, 31, 21, 1, 0, // Skip to: 2009
/* 1732 */    MCD::OPC_Decode, 204, 2, 17, // Opcode: CTTZ
/* 1736 */    MCD::OPC_FilterValue, 112, 11, 0, 0, // Skip to: 1752
/* 1741 */    MCD::OPC_CheckField, 16, 5, 31, 5, 1, 0, // Skip to: 2009
/* 1748 */    MCD::OPC_Decode, 236, 2, 18, // Opcode: FTOIT
/* 1752 */    MCD::OPC_FilterValue, 120, 252, 0, 0, // Skip to: 2009
/* 1757 */    MCD::OPC_CheckField, 16, 5, 31, 245, 0, 0, // Skip to: 2009
/* 1764 */    MCD::OPC_Decode, 235, 2, 19, // Opcode: FTOIS
/* 1768 */    MCD::OPC_FilterValue, 34, 4, 0, 0, // Skip to: 1777
/* 1773 */    MCD::OPC_Decode, 130, 3, 20, // Opcode: LDS
/* 1777 */    MCD::OPC_FilterValue, 35, 4, 0, 0, // Skip to: 1786
/* 1782 */    MCD::OPC_Decode, 132, 3, 21, // Opcode: LDT
/* 1786 */    MCD::OPC_FilterValue, 38, 4, 0, 0, // Skip to: 1795
/* 1791 */    MCD::OPC_Decode, 182, 3, 20, // Opcode: STS
/* 1795 */    MCD::OPC_FilterValue, 39, 4, 0, 0, // Skip to: 1804
/* 1800 */    MCD::OPC_Decode, 184, 3, 21, // Opcode: STT
/* 1804 */    MCD::OPC_FilterValue, 40, 4, 0, 0, // Skip to: 1813
/* 1809 */    MCD::OPC_Decode, 251, 2, 1, // Opcode: LDL
/* 1813 */    MCD::OPC_FilterValue, 41, 4, 0, 0, // Skip to: 1822
/* 1818 */    MCD::OPC_Decode, 254, 2, 1, // Opcode: LDQ
/* 1822 */    MCD::OPC_FilterValue, 42, 4, 0, 0, // Skip to: 1831
/* 1827 */    MCD::OPC_Decode, 252, 2, 1, // Opcode: LDL_L
/* 1831 */    MCD::OPC_FilterValue, 43, 4, 0, 0, // Skip to: 1840
/* 1836 */    MCD::OPC_Decode, 255, 2, 1, // Opcode: LDQ_L
/* 1840 */    MCD::OPC_FilterValue, 44, 4, 0, 0, // Skip to: 1849
/* 1845 */    MCD::OPC_Decode, 176, 3, 1, // Opcode: STL
/* 1849 */    MCD::OPC_FilterValue, 45, 4, 0, 0, // Skip to: 1858
/* 1854 */    MCD::OPC_Decode, 179, 3, 1, // Opcode: STQ
/* 1858 */    MCD::OPC_FilterValue, 46, 4, 0, 0, // Skip to: 1867
/* 1863 */    MCD::OPC_Decode, 177, 3, 22, // Opcode: STL_C
/* 1867 */    MCD::OPC_FilterValue, 47, 4, 0, 0, // Skip to: 1876
/* 1872 */    MCD::OPC_Decode, 180, 3, 22, // Opcode: STQ_C
/* 1876 */    MCD::OPC_FilterValue, 48, 11, 0, 0, // Skip to: 1892
/* 1881 */    MCD::OPC_CheckField, 21, 5, 31, 121, 0, 0, // Skip to: 2009
/* 1888 */    MCD::OPC_Decode, 155, 2, 23, // Opcode: BR
/* 1892 */    MCD::OPC_FilterValue, 49, 4, 0, 0, // Skip to: 1901
/* 1897 */    MCD::OPC_Decode, 217, 2, 24, // Opcode: FBEQ
/* 1901 */    MCD::OPC_FilterValue, 50, 4, 0, 0, // Skip to: 1910
/* 1906 */    MCD::OPC_Decode, 221, 2, 24, // Opcode: FBLT
/* 1910 */    MCD::OPC_FilterValue, 51, 4, 0, 0, // Skip to: 1919
/* 1915 */    MCD::OPC_Decode, 220, 2, 24, // Opcode: FBLE
/* 1919 */    MCD::OPC_FilterValue, 54, 4, 0, 0, // Skip to: 1928
/* 1924 */    MCD::OPC_Decode, 218, 2, 24, // Opcode: FBGE
/* 1928 */    MCD::OPC_FilterValue, 55, 4, 0, 0, // Skip to: 1937
/* 1933 */    MCD::OPC_Decode, 219, 2, 24, // Opcode: FBGT
/* 1937 */    MCD::OPC_FilterValue, 56, 4, 0, 0, // Skip to: 1946
/* 1942 */    MCD::OPC_Decode, 150, 2, 25, // Opcode: BLBC
/* 1946 */    MCD::OPC_FilterValue, 57, 4, 0, 0, // Skip to: 1955
/* 1951 */    MCD::OPC_Decode, 143, 2, 25, // Opcode: BEQ
/* 1955 */    MCD::OPC_FilterValue, 58, 4, 0, 0, // Skip to: 1964
/* 1960 */    MCD::OPC_Decode, 153, 2, 25, // Opcode: BLT
/* 1964 */    MCD::OPC_FilterValue, 59, 4, 0, 0, // Skip to: 1973
/* 1969 */    MCD::OPC_Decode, 152, 2, 25, // Opcode: BLE
/* 1973 */    MCD::OPC_FilterValue, 60, 4, 0, 0, // Skip to: 1982
/* 1978 */    MCD::OPC_Decode, 151, 2, 25, // Opcode: BLBS
/* 1982 */    MCD::OPC_FilterValue, 61, 4, 0, 0, // Skip to: 1991
/* 1987 */    MCD::OPC_Decode, 154, 2, 25, // Opcode: BNE
/* 1991 */    MCD::OPC_FilterValue, 62, 4, 0, 0, // Skip to: 2000
/* 1996 */    MCD::OPC_Decode, 144, 2, 25, // Opcode: BGE
/* 2000 */    MCD::OPC_FilterValue, 63, 4, 0, 0, // Skip to: 2009
/* 2005 */    MCD::OPC_Decode, 145, 2, 25, // Opcode: BGT
/* 2009 */    MCD::OPC_Fail,
  0
};

static const uint8_t DecoderTableOther32[] = {
/* 0 */       MCD::OPC_ExtractField, 26, 6,  // Inst{31-26} ...
/* 3 */       MCD::OPC_FilterValue, 0, 4, 0, 0, // Skip to: 12
/* 8 */       MCD::OPC_Decode, 189, 2, 26, // Opcode: COND_BRANCH_F
/* 12 */      MCD::OPC_FilterValue, 8, 4, 0, 0, // Skip to: 21
/* 17 */      MCD::OPC_Decode, 248, 2, 1, // Opcode: LDAr
/* 21 */      MCD::OPC_FilterValue, 9, 4, 0, 0, // Skip to: 30
/* 26 */      MCD::OPC_Decode, 246, 2, 1, // Opcode: LDAHr
/* 30 */      MCD::OPC_FilterValue, 10, 4, 0, 0, // Skip to: 39
/* 35 */      MCD::OPC_Decode, 250, 2, 1, // Opcode: LDBUr
/* 39 */      MCD::OPC_FilterValue, 12, 4, 0, 0, // Skip to: 48
/* 44 */      MCD::OPC_Decode, 135, 3, 1, // Opcode: LDWUr
/* 48 */      MCD::OPC_FilterValue, 13, 4, 0, 0, // Skip to: 57
/* 53 */      MCD::OPC_Decode, 187, 3, 1, // Opcode: STWr
/* 57 */      MCD::OPC_FilterValue, 14, 4, 0, 0, // Skip to: 66
/* 62 */      MCD::OPC_Decode, 175, 3, 1, // Opcode: STBr
/* 66 */      MCD::OPC_FilterValue, 23, 84, 0, 0, // Skip to: 155
/* 71 */      MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 74 */      MCD::OPC_FilterValue, 32, 4, 0, 0, // Skip to: 83
/* 79 */      MCD::OPC_Decode, 200, 2, 11, // Opcode: CPYST
/* 83 */      MCD::OPC_FilterValue, 33, 4, 0, 0, // Skip to: 92
/* 88 */      MCD::OPC_Decode, 194, 2, 10, // Opcode: CPYSNS
/* 92 */      MCD::OPC_FilterValue, 34, 4, 0, 0, // Skip to: 101
/* 97 */      MCD::OPC_Decode, 193, 2, 11, // Opcode: CPYSET
/* 101 */     MCD::OPC_FilterValue, 42, 4, 0, 0, // Skip to: 110
/* 106 */     MCD::OPC_Decode, 224, 2, 11, // Opcode: FCMOVEQT
/* 110 */     MCD::OPC_FilterValue, 43, 4, 0, 0, // Skip to: 119
/* 115 */     MCD::OPC_Decode, 233, 2, 10, // Opcode: FCMOVNES
/* 119 */     MCD::OPC_FilterValue, 44, 4, 0, 0, // Skip to: 128
/* 124 */     MCD::OPC_Decode, 232, 2, 11, // Opcode: FCMOVLTT
/* 128 */     MCD::OPC_FilterValue, 45, 4, 0, 0, // Skip to: 137
/* 133 */     MCD::OPC_Decode, 226, 2, 11, // Opcode: FCMOVGET
/* 137 */     MCD::OPC_FilterValue, 46, 4, 0, 0, // Skip to: 146
/* 142 */     MCD::OPC_Decode, 230, 2, 11, // Opcode: FCMOVLET
/* 146 */     MCD::OPC_FilterValue, 47, 104, 0, 0, // Skip to: 255
/* 151 */     MCD::OPC_Decode, 228, 2, 11, // Opcode: FCMOVGTT
/* 155 */     MCD::OPC_FilterValue, 26, 14, 0, 0, // Skip to: 174
/* 160 */     MCD::OPC_CheckField, 0, 26, 129, 128, 234, 31, 85, 0, 0, // Skip to: 255
/* 170 */     MCD::OPC_Decode, 146, 3, 14, // Opcode: RETDAGp
/* 174 */     MCD::OPC_FilterValue, 34, 4, 0, 0, // Skip to: 183
/* 179 */     MCD::OPC_Decode, 131, 3, 20, // Opcode: LDSr
/* 183 */     MCD::OPC_FilterValue, 35, 4, 0, 0, // Skip to: 192
/* 188 */     MCD::OPC_Decode, 133, 3, 21, // Opcode: LDTr
/* 192 */     MCD::OPC_FilterValue, 38, 4, 0, 0, // Skip to: 201
/* 197 */     MCD::OPC_Decode, 183, 3, 20, // Opcode: STSr
/* 201 */     MCD::OPC_FilterValue, 39, 4, 0, 0, // Skip to: 210
/* 206 */     MCD::OPC_Decode, 185, 3, 21, // Opcode: STTr
/* 210 */     MCD::OPC_FilterValue, 40, 4, 0, 0, // Skip to: 219
/* 215 */     MCD::OPC_Decode, 253, 2, 1, // Opcode: LDLr
/* 219 */     MCD::OPC_FilterValue, 41, 4, 0, 0, // Skip to: 228
/* 224 */     MCD::OPC_Decode, 129, 3, 1, // Opcode: LDQr
/* 228 */     MCD::OPC_FilterValue, 44, 4, 0, 0, // Skip to: 237
/* 233 */     MCD::OPC_Decode, 178, 3, 1, // Opcode: STLr
/* 237 */     MCD::OPC_FilterValue, 45, 4, 0, 0, // Skip to: 246
/* 242 */     MCD::OPC_Decode, 181, 3, 1, // Opcode: STQr
/* 246 */     MCD::OPC_FilterValue, 54, 4, 0, 0, // Skip to: 255
/* 251 */     MCD::OPC_Decode, 222, 2, 24, // Opcode: FBNE
/* 255 */     MCD::OPC_Fail,
  0
};

static const uint8_t DecoderTablelInst32[] = {
/* 0 */       MCD::OPC_CheckField, 26, 6, 41, 4, 0, 0, // Skip to: 11
/* 7 */       MCD::OPC_Decode, 128, 3, 1, // Opcode: LDQl
/* 11 */      MCD::OPC_Fail,
  0
};

static const uint8_t DecoderTablesInst32[] = {
/* 0 */       MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 3 */       MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 19
/* 8 */       MCD::OPC_CheckField, 26, 6, 23, 20, 0, 0, // Skip to: 35
/* 15 */      MCD::OPC_Decode, 201, 2, 27, // Opcode: CPYSTs
/* 19 */      MCD::OPC_FilterValue, 33, 11, 0, 0, // Skip to: 35
/* 24 */      MCD::OPC_CheckField, 26, 6, 23, 4, 0, 0, // Skip to: 35
/* 31 */      MCD::OPC_Decode, 197, 2, 27, // Opcode: CPYSNTs
/* 35 */      MCD::OPC_Fail,
  0
};

static const uint8_t DecoderTabletInst32[] = {
/* 0 */       MCD::OPC_ExtractField, 5, 11,  // Inst{15-5} ...
/* 3 */       MCD::OPC_FilterValue, 32, 11, 0, 0, // Skip to: 19
/* 8 */       MCD::OPC_CheckField, 26, 6, 23, 36, 0, 0, // Skip to: 51
/* 15 */      MCD::OPC_Decode, 199, 2, 28, // Opcode: CPYSSt
/* 19 */      MCD::OPC_FilterValue, 33, 11, 0, 0, // Skip to: 35
/* 24 */      MCD::OPC_CheckField, 26, 6, 23, 20, 0, 0, // Skip to: 51
/* 31 */      MCD::OPC_Decode, 195, 2, 28, // Opcode: CPYSNSt
/* 35 */      MCD::OPC_FilterValue, 34, 11, 0, 0, // Skip to: 51
/* 40 */      MCD::OPC_CheckField, 26, 6, 23, 4, 0, 0, // Skip to: 51
/* 47 */      MCD::OPC_Decode, 192, 2, 28, // Opcode: CPYSESt
/* 51 */      MCD::OPC_Fail,
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
    tmp = fieldFromInstruction(insn, 21, 5);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 0, 21);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 1:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 16);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 2:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 3:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 13, 8);
    MI.addOperand(MCOperand::createImm(tmp));
    return S;
  case 4:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 5:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 13, 8);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 6:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 7:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 8:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 9:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 10:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 11:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 12:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 13:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 14:
    return S;
  case 15:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 16:
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 17:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 18:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 19:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 20:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 16);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 21:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 16);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 22:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 16);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 23:
    tmp = fieldFromInstruction(insn, 0, 21);
    MI.addOperand(MCOperand::createImm(tmp));
    return S;
  case 24:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 21);
    MI.addOperand(MCOperand::createImm(tmp));
    return S;
  case 25:
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeGPRCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 0, 21);
    MI.addOperand(MCOperand::createImm(tmp));
    return S;
  case 26:
    tmp = fieldFromInstruction(insn, 21, 5);
    MI.addOperand(MCOperand::createImm(tmp));
    tmp = fieldFromInstruction(insn, 0, 21);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 27:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    return S;
  case 28:
    tmp = fieldFromInstruction(insn, 0, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 21, 5);
    if (DecodeF8RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
    tmp = fieldFromInstruction(insn, 16, 5);
    if (DecodeF4RCRegisterClass(MI, tmp, Address, Decoder) == MCDisassembler::Fail) { return MCDisassembler::Fail; }
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

