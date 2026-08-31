//===- AlphaInstrFormats.td - Alpha Instruction Formats ----*- tablegen -*-===//
// 
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
// 
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//3.3:
//Memory
//Branch
//Operate
//Floating-point
//PALcode

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//
// Alpha instruction baseline
class I<bits<6> op, string asmstr, InstrItinClass itin> : Instruction {
  field bits<32> Inst;
  let Namespace = "Alpha";
  let AsmString = asmstr;
  let Inst{31-26} = op;
  let Itinerary = itin;
  let Size =  4;
}


//3.3.1
class MForm<bits<6> opcode, bit load, string asmstr, list<dag> pattern, InstrItinClass itin> 
        : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let canFoldAsLoad = load;

  bits<5> RA;
  bits<16> DISP;
  bits<5> RB;

  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-0} = DISP;
}
class MfcForm<bits<6> opcode, bits<16> fc, string asmstr, InstrItinClass itin> 
        : I<opcode, asmstr, itin> {    
  bits<5> RA;

  let OutOperandList = (outs GPRC:$RA);
  let InOperandList = (ins);
  let Inst{25-21} = RA;
  let Inst{20-16} = 0;
  let Inst{15-0} = fc;
}
class MfcForm2<bits<6> opcode, bits<16> fc, string asmstr, InstrItinClass itin> 
        : I<opcode, asmstr, itin> {    
  bits<5> RA;
  bits<5> RB;

  let OutOperandList = (outs GPRC:$RA);
  let InOperandList = (ins GPRC:$RB);
  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-0} = fc;
}
class MfcPForm<bits<6> opcode, bits<16> fc, string asmstr, InstrItinClass itin> 
        : I<opcode, asmstr, itin> {    
  let OutOperandList = (outs);
  let InOperandList = (ins);
  let Inst{25-21} = 0;
  let Inst{20-16} = 0;
  let Inst{15-0} = fc;
}

class MbrForm<bits<6> opcode, bits<2> TB, dag OL, string asmstr, InstrItinClass itin>
    : I<opcode, asmstr, itin> {
  bits<5> RA;
  bits<5> RB;
  bits<14> DISP;

  let OutOperandList = (outs);
  let InOperandList = OL;

  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-14} = TB;
  let Inst{13-0} = DISP;
}
class MbrpForm<bits<6> opcode, bits<2> TB, dag OL, string asmstr, list<dag> pattern, InstrItinClass itin>
    : I<opcode, asmstr, itin> {
  let Pattern=pattern;
  bits<5> RA;
  bits<5> RB;
  bits<14> DISP;

  let OutOperandList = (outs);
  let InOperandList = OL;

  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-14} = TB;
  let Inst{13-0} = DISP;
}


let isBranch = 1, isTerminator = 1, hasCtrlDep = 1 in {
class BFormN<bits<6> opcode, dag OL, string asmstr, InstrItinClass itin>
   : I<opcode, asmstr, itin> {
  let OutOperandList = (outs);
  let InOperandList = OL;
  bits<64> Opc; //dummy
  bits<5> RA;
  bits<21> dst;

  let Inst{25-21} = RA;
  let Inst{20-0} = dst;
}
}

let isBranch = 1, isTerminator = 1 in
class BFormD<bits<6> opcode, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs);
  let InOperandList = (ins target:$DISP);
  bits<5> RA;
  bits<21> DISP;

  let Inst{25-21} = RA;
  let Inst{20-0} = DISP;
}

//3.3.3
class OForm<bits<6> opcode, bits<7> fun, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs GPRC:$RC);
  let InOperandList = (ins GPRC:$RA, GPRC:$RB);

  bits<5> RC;
  bits<5> RA;
  bits<5> RB;
  bits<7> Function = fun;

  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-13} = 0;
  let Inst{12} = 0;
  let Inst{11-5} = Function;
  let Inst{4-0} = RC;
}

class OForm2<bits<6> opcode, bits<7> fun, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs GPRC:$RC);
  let InOperandList = (ins GPRC:$RB);

  bits<5> RC;
  bits<5> RB;
  bits<7> Function = fun;

  let Inst{25-21} = 31;
  let Inst{20-16} = RB;
  let Inst{15-13} = 0;
  let Inst{12} = 0;
  let Inst{11-5} = Function;
  let Inst{4-0} = RC;
}

class OForm4<bits<6> opcode, bits<7> fun, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs GPRC:$RDEST);
  let InOperandList = (ins GPRC:$RCOND, GPRC:$RTRUE, GPRC:$RFALSE);
  let Constraints = "$RFALSE = $RDEST";
  let DisableEncoding = "$RFALSE";

  bits<5> RC;
  bits<5> RA;
  bits<5> RB;
  bits<7> Function = fun;

  let Inst{25-21} = RA;
  let Inst{20-16} = RB;
  let Inst{15-13} = 0;
  let Inst{12} = 0;
  let Inst{11-5} = Function;
  let Inst{4-0} = RC;
}


class OFormL<bits<6> opcode, bits<7> fun, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs GPRC:$RC);
  let InOperandList = (ins GPRC:$RA, u8imm:$L);

  bits<5> RC;
  bits<5> RA;
  bits<8> L;
  bits<7> Function = fun;

  let Inst{25-21} = RA;
  let Inst{20-13} = L;
  let Inst{12} = 1;
  let Inst{11-5} = Function;
  let Inst{4-0} = RC;
}

class OForm4L<bits<6> opcode, bits<7> fun, string asmstr, list<dag> pattern, InstrItinClass itin> 
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;
  let OutOperandList = (outs GPRC:$RDEST);
  let InOperandList = (ins GPRC:$RCOND, s64imm:$RTRUE, GPRC:$RFALSE);
  let Constraints = "$RFALSE = $RDEST";
  let DisableEncoding = "$RFALSE";

  bits<5> RC;
  bits<5> RA;
  bits<8> L;
  bits<7> Function = fun;

  let Inst{25-21} = RA;
  let Inst{20-13} = L;
  let Inst{12} = 1;
  let Inst{11-5} = Function;
  let Inst{4-0} = RC;
}

//3.3.4
class FPForm<bits<6> opcode, bits<11> fun, string asmstr, list<dag> pattern, InstrItinClass itin>
    : I<opcode, asmstr, itin> {
  let Pattern = pattern;

  bits<5> Fc;
  bits<5> Fa;
  bits<5> Fb;
  bits<11> Function = fun;

  let Inst{25-21} = Fa;
  let Inst{20-16} = Fb;
  let Inst{15-5} = Function;
  let Inst{4-0} = Fc;
}

//3.3.5
class PALForm<bits<6> opcode, dag OL, string asmstr, InstrItinClass itin>
    : I<opcode, asmstr, itin> {
  let OutOperandList = (outs);
  let InOperandList = OL;
  bits<26> Function;

  let Inst{25-0} = Function;
}


// Pseudo instructions.
class PseudoInstAlpha<dag OOL, dag IOL, string nm, list<dag> pattern, InstrItinClass itin>
    : I<0, nm, itin>  {
  let OutOperandList = OOL;
  let InOperandList = IOL;
  let Pattern = pattern;
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}
