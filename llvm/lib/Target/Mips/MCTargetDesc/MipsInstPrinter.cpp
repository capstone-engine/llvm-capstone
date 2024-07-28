//===-- MipsInstPrinter.cpp - Convert Mips MCInst to assembly syntax ------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an Mips MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "MipsInstPrinter.h"
#include "Mips.h"
#include "MipsMCExpr.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#define PRINT_ALIAS_INSTR
#include "MipsGenAsmWriter.inc"

template<unsigned R>
static bool isReg(const MCInst &MI, unsigned OpNo) {
  assert(MI.getOperand(OpNo).isReg() && "Register operand expected.");
  return MI.getOperand(OpNo).getReg() == R;
}

const char* Mips::MipsFCCToString(Mips::CondCode CC) {
  switch (CC) {
  case FCOND_F:
  case FCOND_T:   return "f";
  case FCOND_UN:
  case FCOND_OR:  return "un";
  case FCOND_OEQ:
  case FCOND_UNE: return "eq";
  case FCOND_UEQ:
  case FCOND_ONE: return "ueq";
  case FCOND_OLT:
  case FCOND_UGE: return "olt";
  case FCOND_ULT:
  case FCOND_OGE: return "ult";
  case FCOND_OLE:
  case FCOND_UGT: return "ole";
  case FCOND_ULE:
  case FCOND_OGT: return "ule";
  case FCOND_SF:
  case FCOND_ST:  return "sf";
  case FCOND_NGLE:
  case FCOND_GLE: return "ngle";
  case FCOND_SEQ:
  case FCOND_SNE: return "seq";
  case FCOND_NGL:
  case FCOND_GL:  return "ngl";
  case FCOND_LT:
  case FCOND_NLT: return "lt";
  case FCOND_NGE:
  case FCOND_GE:  return "nge";
  case FCOND_LE:
  case FCOND_NLE: return "le";
  case FCOND_NGT:
  case FCOND_GT:  return "ngt";
  }
  llvm_unreachable("Impossible condition code!");
}

void MipsInstPrinter::printRegName(raw_ostream &OS, MCRegister Reg) const {
  markup(OS, Markup::Register)
      << '$' << StringRef(getRegisterName(Reg)).lower();
}

void MipsInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                StringRef Annot, const MCSubtargetInfo &STI,
                                raw_ostream &O) {
  switch (MI->getOpcode()) {
  default:
    break;
  case Mips::RDHWR:
  case Mips::RDHWR64:
    O << "\t.set\tpush\n";
    O << "\t.set\tmips32r2\n";
    break;
  case Mips::Save16:
    O << "\tsave\t";
    printSaveRestore(MI, STI, O);
    O << " # 16 bit inst\n";
    return;
  case Mips::SaveX16:
    O << "\tsave\t";
    printSaveRestore(MI, STI, O);
    O << "\n";
    return;
  case Mips::Restore16:
    O << "\trestore\t";
    printSaveRestore(MI, STI, O);
    O << " # 16 bit inst\n";
    return;
  case Mips::RestoreX16:
    O << "\trestore\t";
    printSaveRestore(MI, STI, O);
    O << "\n";
    return;
  case Mips::RESTORE_NM:
    O << "\trestore\t";
    printSaveRestore(MI, STI,O);
    return;
  case Mips::SAVE16_NM:
  case Mips::SAVE_NM:
    O << "\tsave\t";
    printSaveRestore(MI ,STI,O);
    return;
  case Mips::RESTOREJRC16_NM:
  case Mips::RESTOREJRC_NM:
    O << "\trestore.jrc\t";
    printSaveRestore(MI, STI, O);
    return;
  }

  // Try to print any aliases first.
  if (!printAliasInstr(MI, Address, STI, O) &&
      !printAlias(*MI, Address, STI, O))
    printInstruction(MI, Address, STI, O);
  printAnnotation(O, Annot);

  switch (MI->getOpcode()) {
  default:
    break;
  case Mips::RDHWR:
  case Mips::RDHWR64:
    O << "\n\t.set\tpop";
  }
}

void MipsInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                   const MCSubtargetInfo &STI, raw_ostream &O) {
  switch (MI->getOpcode()) {
  default:
    break;
  case Mips::AND16_NM:
  case Mips::XOR16_NM:
  case Mips::OR16_NM:
    if (MI->getNumOperands() == 2 && OpNo == 2)
      OpNo = 0; // rt, rs -> rt, rs, rt
    break;
  case Mips::ADDu4x4_NM:
  case Mips::MUL4x4_NM:
    if (MI->getNumOperands() == 2 && OpNo > 0)
      OpNo = OpNo - 1; // rt, rs -> rt, rt, rs
    break;
  }

  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    printRegName(O, Op.getReg());
    return;
  }

  if (Op.isImm()) {
    switch (MI->getOpcode()) {
    case Mips::LI48_NM:
    case Mips::ANDI16_NM:
    case Mips::ANDI_NM:
    case Mips::ORI_NM:
    case Mips::XORI_NM:
    case Mips::TEQ_NM:
    case Mips::TNE_NM:
    case Mips::SIGRIE_NM:
    case Mips::SDBBP_NM:
    case Mips::SDBBP16_NM:
    case Mips::BREAK_NM:
    case Mips::BREAK16_NM:
    case Mips::SYSCALL_NM:
    case Mips::SYSCALL16_NM:
    case Mips::WAIT_NM:
      printUImm<32,0,16>(MI, OpNo, STI, O); break;
    default:
    O << markup("<imm:") << formatImm(Op.getImm()) << markup(">"); break;
    }
    return;
  }

  assert(Op.isExpr() && "unknown operand kind in printOperand");
  Op.getExpr()->print(O, &MAI, true);
}

void MipsInstPrinter::printJumpOperand(const MCInst *MI, unsigned OpNo,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (!Op.isImm())
    return printOperand(MI, OpNo, STI, O);

  if (PrintBranchImmAsAddress)
    markup(O, Markup::Immediate) << formatHex(Op.getImm());
  else
    markup(O, Markup::Immediate) << formatImm(Op.getImm());
}

void MipsInstPrinter::printBranchOperand(const MCInst *MI, uint64_t Address,
                                         unsigned OpNo,
                                         const MCSubtargetInfo &STI,
                                         raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (!Op.isImm())
    return printOperand(MI, OpNo, STI, O);

  if (PrintBranchImmAsAddress) {
    uint64_t Target = Address + Op.getImm();
    if (STI.hasFeature(Mips::FeatureMips32))
      Target &= 0xffffffff;
    else if (STI.hasFeature(Mips::FeatureMips16))
      Target &= 0xffff;
    markup(O, Markup::Immediate) << formatHex(Target);
  } else {
    markup(O, Markup::Immediate) << formatImm(Op.getImm());
  }
}

template <unsigned Bits, unsigned Offset>
void MipsInstPrinter::printUImm(const MCInst *MI, int opNum,
                                const MCSubtargetInfo &STI, raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);
  if (MO.isImm()) {
    uint64_t Imm = MO.getImm();
    Imm -= Offset;
    Imm &= (1 << Bits) - 1;
    Imm += Offset;
    markup(O, Markup::Immediate) << formatImm(Imm);
    return;
  }

  printOperand(MI, opNum, STI, O);
}

void MipsInstPrinter::printMemOperand(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo &STI,
                                      raw_ostream &O) {
  // Load/Store memory operands -- imm($reg)
  // If PIC target the target is loaded as the
  // pattern lw $25,%call16($28)

  // opNum can be invalid if instruction had reglist as operand.
  // MemOperand is always last operand of instruction (base + offset).
  switch (MI->getOpcode()) {
  default:
    break;
  case Mips::SWM32_MM:
  case Mips::LWM32_MM:
  case Mips::SWM16_MM:
  case Mips::SWM16_MMR6:
  case Mips::LWM16_MM:
  case Mips::LWM16_MMR6:
    opNum = MI->getNumOperands() - 2;
    break;
  }

  O << markup("<mem:");
  // Index register is encoded as immediate value
  // in case of nanoMIPS indexed instructions

  switch (MI->getOpcode()) {
    // No offset needed for paired LL/SC
    case Mips::LLWP_NM:
    case Mips::SCWP_NM:
      break;
    case Mips::LWX_NM:
    case Mips::LWXS_NM:
    case Mips::LWXS16_NM:
    case Mips::LBX_NM:
    case Mips::LBUX_NM:
    case Mips::LHX_NM:
    case Mips::LHUX_NM:
    case Mips::LHXS_NM:
    case Mips::LHUXS_NM:
    case Mips::SWX_NM:
    case Mips::SWXS_NM:
    case Mips::SBX_NM:
    case Mips::SHX_NM:
    case Mips::SHXS_NM:
      if (!MI->getOperand(opNum+1).isReg()) {
  printRegName(O, MI->getOperand(opNum+1).getImm());
  break;
      }
      // Fall through
    default:
      printOperand(MI, opNum+1,STI ,O);
      break;
  }
  O << "(";
  printOperand(MI, opNum, STI, O);
  O << ")";
  O << markup(">");
}

void MipsInstPrinter::printMemOperandEA(const MCInst *MI, int opNum,
                                        const MCSubtargetInfo &STI,
                                        raw_ostream &O) {
  // when using stack locations for not load/store instructions
  // print the same way as all normal 3 operand instructions.
  printOperand(MI, opNum, STI, O);
  O << ", ";
  printOperand(MI, opNum + 1, STI, O);
}

void MipsInstPrinter::
printMemOperandGPRel(const MCInst *MI, int opNum, const MCSubtargetInfo &STI, raw_ostream &O) {
  O << "%gprel(";
  printOperand(MI, opNum+1, STI, O);
  O << ")";
  O << "(";
  printOperand(MI, opNum, STI, O);
  O << ")";
}

void MipsInstPrinter::printFCCOperand(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo & /* STI */,
                                      raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);
  O << MipsFCCToString((Mips::CondCode)MO.getImm());
}

void MipsInstPrinter::
printSHFMask(const MCInst *MI, int opNum, raw_ostream &O) {
  llvm_unreachable("TODO");
}

bool MipsInstPrinter::printAlias(const char *Str, const MCInst &MI,
                                 uint64_t Address, unsigned OpNo,
                                 const MCSubtargetInfo &STI, raw_ostream &OS,
                                 bool IsBranch) {
  OS << "\t" << Str << "\t";
  if (IsBranch)
    printBranchOperand(&MI, Address, OpNo, STI, OS);
  else
    printOperand(&MI, OpNo, STI, OS);
  return true;
}

bool MipsInstPrinter::printAlias(const char *Str, const MCInst &MI,
                                 uint64_t Address, unsigned OpNo0,
                                 unsigned OpNo1, const MCSubtargetInfo &STI,
                                 raw_ostream &OS, bool IsBranch) {
  printAlias(Str, MI, Address, OpNo0, STI, OS, IsBranch);
  OS << ", ";
  if (IsBranch)
    printBranchOperand(&MI, Address, OpNo1, STI, OS);
  else
    printOperand(&MI, OpNo1, STI, OS);
  return true;
}

bool MipsInstPrinter::printAliasHex(const char *Str, const MCInst &MI,uint64_t Address,
                                 unsigned OpNo0, unsigned OpNo1,
                                 const MCSubtargetInfo &STI ,raw_ostream &OS) {

  printAlias(Str, MI, Address, OpNo0, STI, OS);
  OS << ", ";
  printUImm<32, 0, 16>(&MI, OpNo1, STI, OS);
  return true;
}

bool MipsInstPrinter::printAlias(const char *Str, const MCInst &MI,uint64_t Address,
                                 unsigned OpNo0, unsigned OpNo1,
                                 unsigned OpNo2,const MCSubtargetInfo &STI ,raw_ostream &OS) {
  printAlias(Str, MI, Address, OpNo0,STI ,OS);
  OS << ", ";
  printOperand(&MI, OpNo1,STI ,OS);
  OS << ", ";
  printOperand(&MI, OpNo2,STI ,OS);
  return true;
}

bool MipsInstPrinter::printAlias(const MCInst &MI, uint64_t Address,
                                 const MCSubtargetInfo &STI, raw_ostream &OS) {
  switch (MI.getOpcode()) {
  case Mips::BEQ:
  case Mips::BEQ_MM:
    // beq $zero, $zero, $L2 => b $L2
    // beq $r0, $zero, $L2 => beqz $r0, $L2
    return (isReg<Mips::ZERO>(MI, 0) && isReg<Mips::ZERO>(MI, 1) &&
            printAlias("b", MI, Address, 2, STI, OS, true)) ||
           (isReg<Mips::ZERO>(MI, 1) &&
            printAlias("beqz", MI, Address, 0, 2, STI, OS, true));
  case Mips::BEQ64:
    // beq $r0, $zero, $L2 => beqz $r0, $L2
    return isReg<Mips::ZERO_64>(MI, 1) &&
           printAlias("beqz", MI, Address, 0, 2, STI, OS, true);
  case Mips::BNE:
  case Mips::BNE_MM:
    // bne $r0, $zero, $L2 => bnez $r0, $L2
    return isReg<Mips::ZERO>(MI, 1) &&
           printAlias("bnez", MI, Address, 0, 2, STI, OS, true);
  case Mips::BNE64:
    // bne $r0, $zero, $L2 => bnez $r0, $L2
    return isReg<Mips::ZERO_64>(MI, 1) &&
           printAlias("bnez", MI, Address, 0, 2, STI, OS, true);
  case Mips::BGEZAL:
    // bgezal $zero, $L1 => bal $L1
    return isReg<Mips::ZERO>(MI, 0) &&
           printAlias("bal", MI, Address, 1, STI, OS, true);
  case Mips::BC1T:
    // bc1t $fcc0, $L1 => bc1t $L1
    return isReg<Mips::FCC0>(MI, 0) &&
           printAlias("bc1t", MI, Address, 1, STI, OS, true);
  case Mips::BC1F:
    // bc1f $fcc0, $L1 => bc1f $L1
    return isReg<Mips::FCC0>(MI, 0) &&
           printAlias("bc1f", MI, Address, 1, STI, OS, true);
  case Mips::JALR:
    // jalr $zero, $r1 => jr $r1
    // jalr $ra, $r1 => jalr $r1
    return (isReg<Mips::ZERO>(MI, 0) &&
            printAlias("jr", MI, Address, 1, STI, OS)) ||
           (isReg<Mips::RA>(MI, 0) &&
            printAlias("jalr", MI, Address, 1, STI, OS));
  case Mips::JALR64:
    // jalr $zero, $r1 => jr $r1
    // jalr $ra, $r1 => jalr $r1
    return (isReg<Mips::ZERO_64>(MI, 0) &&
            printAlias("jr", MI, Address, 1, STI, OS)) ||
           (isReg<Mips::RA_64>(MI, 0) &&
            printAlias("jalr", MI, Address, 1, STI, OS));
  case Mips::NOR:
  case Mips::NOR_MM:
  case Mips::NOR_MMR6:
    // nor $r0, $r1, $zero => not $r0, $r1
    return isReg<Mips::ZERO>(MI, 2) &&
           printAlias("not", MI, Address, 0, 1, STI, OS);
  case Mips::NOR64:
    // nor $r0, $r1, $zero => not $r0, $r1
    return isReg<Mips::ZERO_64>(MI, 2) &&
           printAlias("not", MI, Address, 0, 1, STI, OS);
  case Mips::OR:
  case Mips::ADDu:
    // or $r0, $r1, $zero => move $r0, $r1
    // addu $r0, $r1, $zero => move $r0, $r1
    return isReg<Mips::ZERO>(MI, 2) &&
           printAlias("move", MI, Address, 0, 1, STI, OS);
  case Mips::LI48_NM:
  case Mips::LI16_NM:
    // li[16/48] $r0, imm => li $r0, imm
    return printAlias("li", MI, Address, 0, 1, STI, OS);
  case Mips::ADDIU_NM:
  case Mips::ADDIUNEG_NM:
    if (isReg<Mips::ZERO_NM>(MI, 1))
      return printAlias("li", MI, Address, 0, 2, STI, OS);
    else
      return printAlias("addiu", MI, Address, 0, 1, 2, STI, OS);
  case Mips::ADDIU48_NM:
  case Mips::ADDIURS5_NM:
  case Mips::ADDIUR1SP_NM:
  case Mips::ADDIUR2_NM:
  case Mips::ADDIUGPB_NM:
  case Mips::ADDIUGPW_NM:
    return printAlias("addiu", MI, Address, 0, 1, 2, STI, OS);
  case Mips::ANDI16_NM:
  case Mips::ANDI_NM:
    // andi[16/32] $r0, $r1, imm => andi $r0, $r1, imm
    return printAlias("andi", MI, Address, 0, 1, 2, STI, OS);
  default:
    return false;
  }
}

void MipsInstPrinter::printSaveRestore(const MCInst *MI,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &O) {
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    if (i != 0) O << ", ";
    if (MI->getOperand(i).isReg())
      printRegName(O, MI->getOperand(i).getReg());
    else
      printUImm<16>(MI, i, STI, O);
  }
}

void MipsInstPrinter::printRegisterList(const MCInst *MI, int opNum,
                                        const MCSubtargetInfo & /* STI */,
                                        raw_ostream &O) {
  // - 2 because register List is always first operand of instruction and it is
  // always followed by memory operand (base + offset).
  for (int i = opNum, e = MI->getNumOperands() - 2; i != e; ++i) {
    if (i != opNum)
      O << ", ";
    printRegName(O, MI->getOperand(i).getReg());
  }
}

void MipsInstPrinter::printNanoMipsRegisterList(const MCInst *MI, int OpNum,
                                                const MCSubtargetInfo &STI,
                                                raw_ostream &O) {
  for (size_t I = OpNum; I < MI->getNumOperands(); I++) {
    O << ", ";
    printRegName(O, MI->getOperand(I).getReg());
  }
}

void MipsInstPrinter::printHi20(const MCInst *MI, int OpNum,
                                const MCSubtargetInfo &STI, raw_ostream &O) {
  const MCOperand& MO = MI->getOperand(OpNum);
  if (MO.isImm())
    O << "%hi(" <<  formatHex(MO.getImm()) << ")";
  else
    printOperand(MI, OpNum, STI, O);
}

void MipsInstPrinter::printHi20PCRel(const MCInst *MI, uint64_t Address,
                                    int OpNum, const MCSubtargetInfo &STI,
                                    raw_ostream &O) {
  const MCOperand& MO = MI->getOperand(OpNum);
  if (MO.isImm())
    O << "%pcrel_hi(" << formatHex(MO.getImm() + Address) << ")";
  else
    printOperand(MI, OpNum, STI, O);
}


void MipsInstPrinter::printPCRel(const MCInst *MI, uint64_t Address,
                                    int OpNum, const MCSubtargetInfo &STI,
                                    raw_ostream &O) {
  const MCOperand& MO = MI->getOperand(OpNum);
  if (MO.isImm())
    O << formatHex(Make_64(0, Lo_32((MO.getImm() + Address))));
  else
    printOperand(MI, OpNum, STI, O);
}
