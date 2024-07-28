//===-- MipsMTInstrFormats.td - Mips Instruction Formats ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Describe the MIPS MT instructions format
//
//  opcode - operation code.
//  rt     - destination register
//
//===----------------------------------------------------------------------===//

class MipsMTInst : MipsInst<(outs), (ins), "", [], NoItinerary, FrmOther> {
  let DecoderNamespace = "Mips";
  let EncodingPredicates = [HasStdEnc];
}

class OPCODE1<bits<1> Val> {
  bits<1> Value = Val;
}

def OPCODE_SC_D : OPCODE1<0b0>;
def OPCODE_SC_E : OPCODE1<0b1>;

class FIELD5<bits<5> Val> {
  bits<5> Value = Val;
}

def FIELD5_1_DMT_EMT  : FIELD5<0b00001>;
def FIELD5_2_DMT_EMT  : FIELD5<0b01111>;
def FIELD5_1_2_DVPE_EVPE : FIELD5<0b00000>;
def FIELD5_MFTR : FIELD5<0b01000>;
def FIELD5_MTTR : FIELD5<0b01100>;

class COP0_MFMC0_MT<FIELD5 Op1, FIELD5 Op2, OPCODE1 sc> : MipsMTInst {
  bits<32> Inst;

  bits<5> rt;
  let Inst{31-26} = 0b010000; // COP0
  let Inst{25-21} = 0b01011;  // MFMC0
  let Inst{20-16} = rt;
  let Inst{15-11} = Op1.Value;
  let Inst{10-6}  = Op2.Value;
  let Inst{5}     = sc.Value;
  let Inst{4-3}   = 0b00;
  let Inst{2-0}   = 0b001;
}

class COP0_MFTTR_MT<FIELD5 Op> : MipsMTInst {
  bits<32> Inst;

  bits<5> rt;
  bits<5> rd;
  bits<1> u;
  bits<1> h;
  bits<3> sel;
  let Inst{31-26} = 0b010000; // COP0
  let Inst{25-21} = Op.Value; // MFMC0
  let Inst{20-16} = rt;
  let Inst{15-11} = rd;
  let Inst{10-6}  = 0b00000;  // rx - currently unsupported.
  let Inst{5}     = u;
  let Inst{4}     = h;
  let Inst{3}     = 0b0;
  let Inst{2-0}   = sel;
}

class SPECIAL3_MT_FORK : MipsMTInst {
  bits<32> Inst;

  bits<5> rs;
  bits<5> rt;
  bits<5> rd;
  let Inst{31-26} = 0b011111; // SPECIAL3
  let Inst{25-21} = rs;
  let Inst{20-16} = rt;
  let Inst{15-11} = rd;
  let Inst{10-6}  = 0b00000;
  let Inst{5-0}   = 0b001000; // FORK
}

class SPECIAL3_MT_YIELD : MipsMTInst {
  bits<32> Inst;

  bits<5> rs;
  bits<5> rd;
  let Inst{31-26} = 0b011111; // SPECIAL3
  let Inst{25-21} = rs;
  let Inst{20-16} = 0b00000;
  let Inst{15-11} = rd;
  let Inst{10-6}  = 0b00000;
  let Inst{5-0}   = 0b001001; // FORK
}

class _Pool_P0_NM_MT<bits<6> isel> : NanoMipsMTInst {
  field bits<32> Inst;
  let Inst{31...26} = 0b001000;
  let Inst{9...6} = isel{5...2};
  let Inst{5} = 1;
  let Inst{4...3} = isel{1...0};
  let Inst{2...0} = 0b000;
}

class COP0_MFMC0_NM_MT_ENC<bits<5> isel, bits<1> sc> : _Pool_P0_NM_MT<0b101010> {
  bits<5> rt;
  let Inst{25-21} = rt;
  let Inst{20-16} = isel;
  let Inst{15...11} = 0b00001;
  let Inst{10} = sc;
}

class FORK_NM_MT_ENC : _Pool_P0_NM_MT<0b100001> {
  bits<5> rs;
  bits<5> rt;
  bits<5> rd;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
}

class YIELD_NM_MT_ENC : _Pool_P0_NM_MT<0b100101> {
  bits<5> rs;
  bits<5> rt;
  bits<5> rd;
  let Inst{25...21} = rd;
  let Inst{20...16} = rs;
  let Inst{15...10} = 0b000000;
}

class _Pool_P1_NM_MT<bits<6> isel> : NanoMipsMTInst {
  field bits<32> Inst;
  let Inst{31...26} = 0b001000;
  let Inst{9...4} = isel;
  let Inst{2...0} = 0b000;
}

class COP0_MFTR_NM_MT<bits<6> isel> : _Pool_P1_NM_MT<isel> {
  bits<5> rt;
  bits<5> rd;
  bits<1> u;
  bits<1> h;
  bits<5> sel;
  let Inst{25...21} = rd;
  let Inst{20...16} = rt;
  let Inst{15...11} = sel;
  let Inst{10} = u;
  let Inst{3} = h;
}

class COP0_MTTR_NM_MT<bits<6> isel> : _Pool_P1_NM_MT<isel> {
  bits<5> rt;
  bits<5> rd;
  bits<1> u;
  bits<1> h;
  bits<5> sel;
  let Inst{25...21} = rt;
  let Inst{20...16} = rd;
  let Inst{15...11} = sel;
  let Inst{10} = u;
  let Inst{3} = h;
}
