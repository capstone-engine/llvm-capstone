//===- TriCoreOperands.td - TriCore instruction operands -------*- tblgen-*--===//
//
//                     The LLVM Compiler Infrastructure
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//Operands
def s4imm      : Operand<i32> { let PrintMethod = "printSExtImm<4>";  }
def s6imm      : Operand<i32> { let PrintMethod = "printSExtImm<6>";  }
def s8imm      : Operand<i32> { let PrintMethod = "printSExtImm<8>";  }
def s9imm      : Operand<i32> { let PrintMethod = "printSExtImm<9>";  }
def s10imm     : Operand<i32> { let PrintMethod = "printSExtImm<10>";  }
def s16imm     : Operand<i32> { let PrintMethod = "printSExtImm<16>"; }
def s24imm     : Operand<i32> { let PrintMethod = "printSExtImm<24>"; }
def u8imm      : Operand<i32> { let PrintMethod = "printZExtImm<8>";  }
def u4imm      : Operand<i32> { let PrintMethod = "printZExtImm<4>";  }
def u2imm      : Operand<i32> { let PrintMethod = "printZExtImm<2>";  }
def u9imm      : Operand<i32> { let PrintMethod = "printZExtImm<9>";  }
def u16imm     : Operand<i32> { let PrintMethod = "printZExtImm<16>";  }

def oext4imm: Operand<i32> { let PrintMethod = "printOExtImm<4>";  }

def off18imm   : Operand<i32> { let PrintMethod = "printOff18Imm";  }

def disp24imm  : Operand<i32> { let PrintMethod = "printDisp24Imm";  }
def disp15imm   : Operand<i32> { let PrintMethod = "printDisp15Imm";  }
def disp8imm   : Operand<i32> { let PrintMethod = "printDisp8Imm";  }
def disp4imm   : Operand<i32> { let PrintMethod = "printDisp4Imm";  }

def jmptarget : Operand<OtherVT> {
  let PrintMethod = "printPCRelImmOperand";
}

// Operand for printing out a condition code.
def cc : Operand<i32> {
  let PrintMethod = "printCCOperand";
}

// The target of a 24-bit call instruction.
def call_target : Operand<i32> {
  let EncoderMethod = "encodeCallTarget";
}
