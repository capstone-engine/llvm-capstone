//=- NanoMipsInstrInfo.td - NanoMips Instruction Information -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes NanoMips instructions.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Mips profiles and nodes
//===----------------------------------------------------------------------===//

def MipsFullAddr : SDNode<"MipsISD::FullAddr", SDTIntUnaryOp>;
def MipsFullAddrAdd : SDNode<"MipsISD::FullAddrAdd", SDTIntBinOp>;
def NMUnalignedLW : SDNode<"MipsISD::UALW", SDTMipsLoadLR, [SDNPHasChain, SDNPMayLoad, SDNPMemOperand]>;
def NMUnalignedLH : SDNode<"MipsISD::UALH", SDTMipsLoadLR, [SDNPHasChain, SDNPMayLoad, SDNPMemOperand]>;
def NMUnalignedSW : SDNode<"MipsISD::UASW", SDTStore, [SDNPHasChain, SDNPMayStore, SDNPMemOperand]>;
def NMUnalignedSH : SDNode<"MipsISD::UASH", SDTStore, [SDNPHasChain, SDNPMayStore, SDNPMemOperand]>;

def SDT_NMbr_jt : SDTypeProfile<0, 2, []>;

def NMbr_jt : SDNode<"MipsISD::BR_JT", SDT_NMbr_jt, [SDNPHasChain]>;

//===----------------------------------------------------------------------===//
// nanoMIPS Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

def imm32_NM : IntImmLeaf<i32, [{
    return (uint32_t)Imm.getZExtValue() == (uint64_t)Imm.getZExtValue();
}]>;
def immFitsAddiu32 : IntImmLeaf<i32, [{
  int64_t N = Imm.getSExtValue();
  return (N > 4095 && N <= 65535);
}]>;
def immFitsAddiuNeg : IntImmLeaf<i32, [{
  int64_t N = Imm.getSExtValue();
  return (N < -256 && N >= -4095);
}]>;
def immFitsAddiu48 : IntImmLeaf<i32, [{
  int64_t N = Imm.getSExtValue();
  return (N < -4095 || N  > 65535);
}]>;
def imm32ZExt12 : IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()); }]>;
def imm32ZExt12ANDI :
  IntImmLeaf<i32, [{ return isUInt<12>(Imm.getZExtValue()) ||
                            Imm.getZExtValue() == 65535; }]>;
def imm32ZExt5  : IntImmLeaf<i32, [{ return isUInt<5>(Imm.getZExtValue()); }]>;
def imm32ZExt3  : IntImmLeaf<i32, [{ return Imm.getZExtValue() > 0 &&
				  Imm.getZExtValue() <= 8;}]>;
def imm32SExt12  : IntImmLeaf<i32, [{ return isInt<12>(Imm.getSExtValue()); }]>;
def imm32Neg12  : IntImmLeaf<i32, [{ return isInt<13>(Imm.getSExtValue()) &&
				    Imm.getSExtValue() < 0 &&
				    Imm.getSExtValue() >= -4095; }]>;
// True if (N + 1) fits in 12-bit field.
def immZExt12Plus1 : PatLeaf<(imm), [{
  return isUInt<13>(N->getZExtValue()) && isUInt<12>(N->getZExtValue() + 1);
}]>;
def immZExt7Plus1 : PatLeaf<(imm), [{
  return isUInt<8>(N->getZExtValue()) && isUInt<7>(N->getZExtValue() + 1);
}]>;
// Immediate range for signed 20-bit upper part
def imm32SExt20s12 : IntImmLeaf<i32, [{ return isInt<32>(Imm.getSExtValue())
                                       && (Imm.getZExtValue() % 4096 == 0); }]>;

// Immediate range for 16-bit load immediate
def imm7M1To126 : IntImmLeaf<i32, [{
    return (Imm.getSExtValue() >= -1 &&  Imm.getSExtValue() < 127);
}]>;

// Immediate range for unsigned 18-bit
def imm32ZExt18 : IntImmLeaf<i32, [{ return isInt<18>(Imm.getZExtValue());}]>;

// Immediate range for signed 4-bit
def imm32ZExt4 : IntImmLeaf<i32, [{ return isInt<4>(Imm.getSExtValue());}]>;

// Immediate range for unsigned 21-bit word-aligned
def imm32ZExt21s2 : IntImmLeaf<i32, [{ return isInt<21>(Imm.getZExtValue()) &&
					      (Imm.getZExtValue() % 4 == 0); }]>;

// Immediate range for unsigned 21-bit word-aligned
def imm32ZExt8s2 : IntImmLeaf<i32, [{ return isInt<8>(Imm.getZExtValue()) &&
					     (Imm.getZExtValue() % 4 == 0); }]>;

// Immediate range for 16-bit AND immediate
def imm4Mask : IntImmLeaf<i32, [{
    return ((Imm.getZExtValue() < 11) ||
	   (Imm.getZExtValue() == 0xff) || (Imm.getZExtValue() == 0xffff) ||
	   (Imm.getZExtValue() == 0xe) ||  (Imm.getZExtValue() == 0xf));
}]>;

class ConstantSImmRangeAsmOperandClass<int Bottom, int Top,
                                       list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "ConstantNegImmRange" # Bottom # "_" # Top;
  let RenderMethod = "addSImmOperands<" # 12 # ">";
  let PredicateMethod = "isConstantNegImmRange<" # Bottom # ", " # Top # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "SImmRange" # Bottom # "_" # Top;
}

class NegImmAsmOperandClass<int Bits, list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "NegImm" # Bits;
  let RenderMethod = "addSImmOperands<32>";
  let PredicateMethod = "isNegImm<" # Bits # ">";
  let SuperClasses = Supers;
}

class Imm7AsmOperandClass : AsmOperandClass {
  let Name = "UImm7N1";
  let RenderMethod = "addSImmOperands<8>";
  let PredicateMethod = "isConstantSImmRange<-1,126>";
}

class UImm4AsmOperandClass : AsmOperandClass {
  let Name = "UImm4Mask";
  let RenderMethod = "addUImmOperands<16>";
  let PredicateMethod = "isConstantUImmMask";
}

class UImm3ShiftAsmOperandClass : AsmOperandClass {
  let Name = "UImm3Shift";
  let RenderMethod = "addUImmOperands<4>";
  let PredicateMethod = "isConstantUImmRange<1,8>";
}

def UImm12AsmOperandClass : UImmAsmOperandClass<12, []>;
def UImm5AsmOperandClass : UImmAsmOperandClass<5, [UImm12AsmOperandClass]>;
def Imm7AsmOperand : Imm7AsmOperandClass;
def UImm4AsmOperand : UImm4AsmOperandClass;
def UImm3ShiftAsmOperand : UImm3ShiftAsmOperandClass;

def SImm12AsmOperandClass : SImmAsmOperandClass<12, []>;
def NImm12AsmOperandClass : NegImmAsmOperandClass<12, []>;
def NaImm12AsmOperandClass : ConstantSImmRangeAsmOperandClass<4095, 1, []>;

class SImmScaledAsmOperandClass<int Bits, int Align, list<AsmOperandClass> Supers = []>
    : AsmOperandClass{
  let Name = "SImm" # Bits # "s" # Align;
  let RenderMethod = "addSImmOperands<32>";
  let PredicateMethod = "isSImm<" # Bits # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "SImm" # Bits # "s" # Align;
}

class NMMemUimmAsmOperand<int Width, int Shift = 0,
      string RegClass = "Mips::GPRNM32RegClassID",
      list<AsmOperandClass> Supers = [MipsMemAsmOperand]> : AsmOperandClass {
  let Name = "NMMemOffsetUimm" # Width # "_" # Shift;
  let SuperClasses = Supers;
  let RenderMethod = "addNMMemOperands";
  let ParserMethod = "parseMemOperand";
  let PredicateMethod = "isMemWithBaseUimmOffset<" # Width # ", " # Shift # ", " # RegClass # ">";
}

class NMMemSimmAsmOperand<int Width, int Shift = 0,
      string RegClass = "Mips::GPRNM32RegClassID",
      list<AsmOperandClass> Supers = [MipsMemAsmOperand]> : AsmOperandClass {
  let Name = "NMMemOffsetSimm" # Width # "_" # Shift;
  let SuperClasses = Supers;
  let RenderMethod = "addNMMemOperands";
  let ParserMethod = "parseMemOperand";
  let PredicateMethod = "isMemWithBaseSimmOffset<" # Width # ", " # Shift # ", " # RegClass # ">";
}

class NMMemZeroAsmOperand<string RegClass = "Mips::GPRNM32RegClassID",
      list<AsmOperandClass> Supers = [MipsMemAsmOperand]> : AsmOperandClass {
  let Name = "NMMemOffsetZero";
  let SuperClasses = Supers;
  let RenderMethod = "addNMMemOperands";
  let ParserMethod = "parseMemOperand";
  let PredicateMethod = "isMemWithBaseNoOffset<" # RegClass # ">";
}

class NMJumpTargetAsmOperandClass<int Bits = 0,
				  list<AsmOperandClass> Supers = []> : AsmOperandClass {
  let Name = "JumpTargetNM" # Bits;
  let ParserMethod = "parseJumpTarget";
  let PredicateMethod = "isImm";
  let RenderMethod = "addImmOperands";
  let SuperClasses = Supers;
}

def NMJumpTargetAsmOperand : NMJumpTargetAsmOperandClass;

class NMUImmAsmOperandClass<int Bits,
			    list<AsmOperandClass> Supers = [],
			    int Offset = 0> :
			    AsmOperandClass {
  let Name = "NMUImm" # Bits # "_" # Offset;
  let RenderMethod = "addConstantUImmOperands<" # Bits # "," # Offset # ">";
  let PredicateMethod = "isConstantUImm<" # Bits # "," # Offset # ">";
  let SuperClasses = Supers;
}

class NMUImm16AsmOperandClass<list<AsmOperandClass> Supers = []> :
      AsmOperandClass {
  let Name = "NMUImm16";
  let RenderMethod = "addUImmOperands<16>";
  let PredicateMethod = "isLo12OrUImm16Offset";
  let SuperClasses = Supers;
}

class NMSImmAsmOperandClass<int Bits,
			    list<AsmOperandClass> Supers = [],
			    int Offset = 0> :
			    AsmOperandClass {
  let Name = "NMSImm" # Bits # "_" # Offset;
  let RenderMethod = "addSImmOperands<" # Bits # ">";
  let PredicateMethod = "isConstantSImm<" # Bits # "," # Offset # ">";
  let SuperClasses = Supers;
}

class NMUImmScaledAsmOperandClass<int Bits,
			          int Shift = 0,
				  list<AsmOperandClass> Supers = []> :
    AsmOperandClass {
  let Name = "NMUImm" # Bits # "_s" # Shift;
  let RenderMethod = "addConstantUImmOperands<32>";
  let PredicateMethod = "isScaledUImm<" # Bits # "," # Shift # ">";
  let SuperClasses = Supers;
}

def Hi20AsmOperand : AsmOperandClass {
  let Name = "Hi20Offset";
  let RenderMethod = "addHi20Operands";
  let ParserMethod = "parseHi20Op";
}

def Hi20PCRelAsmOperand : AsmOperandClass {
  let Name = "Hi20OffsetPCRel";
  let RenderMethod = "addUImmOperands<20>";
}

class SymAsmOperandClass<string Type = "", string Suffix = "",
			list<AsmOperandClass> Supers = []>
    : AsmOperandClass{
  let Name = "Sym" # Type # Suffix;
  let DiagnosticType = "Sym" # Type;
  let PredicateMethod = "isSym" # Type;
  let SuperClasses = Supers;
  let RenderMethod = "addSym" # Type # "Operands";
}

def SymAsmOperand : SymAsmOperandClass;

def SymAsmOperandGPRel18 : SymAsmOperandClass<"GPRel", "18"> {
  let PredicateMethod = "isSymGPRel<18>";
  let RenderMethod = "addSymGPRelOperands<18>";
}
def SymAsmOperandGPRel19s2 : SymAsmOperandClass<"GPRel", "19s2"> {
  let PredicateMethod = "isSymGPRel<19, 2>";
  let RenderMethod = "addSymGPRelOperands<21,0>";
}
def SymAsmOperandGPRel : SymAsmOperandClass<"GPRel">;

def UImm19AsmOperand : NMUImmAsmOperandClass<19>;
def UImm18AsmOperand : NMUImmAsmOperandClass<18>;
def UImm3AsmOperand : NMUImmAsmOperandClass<3, [UImm19AsmOperand]>;
def UImm2AsmOperand : NMUImmAsmOperandClass<2, [UImm18AsmOperand]>;

def uimm19_nm : Operand<i32> {
  let ParserMatchClass = UImm19AsmOperand;
}

def uimm18_nm : Operand<i32> {
  let ParserMatchClass = UImm18AsmOperand;
}

def uimm3_nm : Operand<i32> {
  let ParserMatchClass = UImm3AsmOperand;
}

def uimm2_nm : Operand<i32> {
  let ParserMatchClass = UImm2AsmOperand;
}

def uimm12_nm : Operand<i32> {
  let ParserMatchClass = NMUImmAsmOperandClass<12>;
}

def uimm12s3_nm : Operand<i32> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<9, 3, []>;
}
def uimm8s4_nm : Operand<i32> {
  let PrintMethod = "printUImm<8>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<4, 4, [uimm12s3_nm.ParserMatchClass]>;
}
def uimm3plus1_nm : Operand<i32> {
  let ParserMatchClass = NMUImmAsmOperandClass<3, [], 1>;
  let DecoderMethod = "DecodeUImm3Shift";
}
def nimm12_nm : Operand<i32> {
  let ParserMatchClass = !cast<AsmOperandClass>("NImm12AsmOperandClass");
  let EncoderMethod = "getNegImm12Encoding";
  let DecoderMethod = "DecodeNegImm12";
}
def uimm16_nm : Operand<i32> {
  let PrintMethod = "printUImm<16>";
  let ParserMatchClass = NMUImm16AsmOperandClass<>;
}
def uimm5s1_nm : Operand<i32> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<4, 1>;
}
def uimm5s2_nm : Operand<i32> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<3, 2, [uimm16_nm.ParserMatchClass]>;
}
def uimm8s2_nm : Operand<i32> {
  let PrintMethod = "printUImm<8>";
  let ParserMatchClass = NMUImmScaledAsmOperandClass<6, 2, [uimm16_nm.ParserMatchClass]>;
  let DecoderMethod = "DecodeUImmWithReg<8, 0, 1, Mips::SP_NM>";
}
def nimm7_nm : Operand<i32> {
  let ParserMatchClass = !cast<AsmOperandClass>("Imm7AsmOperand");
  let DecoderMethod = "DecodeImmM1To126";
  let PrintMethod = "printUImm<7>";
}
def uimm4mask_nm : Operand<i32> {
  let EncoderMethod = "getUImm4MaskEncoding";
  let PrintMethod = "printUImm<16>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm4AsmOperand");
  let DecoderMethod = "DecodeUImm4Mask";
}
def uimm3shift_nm : Operand<i32> {
  let EncoderMethod = "getUImm3ShiftEncoding";
  let PrintMethod = "printUImm<4>";
  let ParserMatchClass = !cast<AsmOperandClass>("UImm3ShiftAsmOperand");
  let DecoderMethod = "DecodeUImm3Shift";
}

def MemNMS9Parser : NMMemSimmAsmOperand<9,0,
    "Mips::GPRNM32RegClassID">;

def mem_nms9 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<9,0>";
  let DecoderMethod = "DecodeMemNM<9,true,Mips::GPRNM32RegClassID>";
  let ParserMatchClass = MemNMS9Parser;
}

def MemNMU12Parser : NMMemUimmAsmOperand<12,0,
    "Mips::GPRNM32RegClassID",[MemNMS9Parser]>;

def mem_nmu12 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<12,0>";
  let DecoderMethod = "DecodeMemNM<12,false,Mips::GPRNM32RegClassID>";
  let ParserMatchClass = MemNMU12Parser;
}

def MemNMGP19S2Parser : NMMemUimmAsmOperand<19, 2,
    "Mips::GPRNMGPRegClassID", [MemNMU12Parser]>;
def MemNMGP18Parser : NMMemUimmAsmOperand<18, 0,
    "Mips::GPRNMGPRegClassID", [MemNMU12Parser]>;
def MemNMGP17S1Parser : NMMemUimmAsmOperand<17, 1,
    "Mips::GPRNMGPRegClassID", [MemNMU12Parser]>;

def mem_nm_gp19s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMGP";
  let OperandType = "OPERAND_NM_GPREL21";
  let OperandNamespace = "NanoMips";
  let DecoderMethod = "DecodeMemNM<21,false,Mips::GPRNMGPRegClassID>";
  let ParserMatchClass = MemNMGP19S2Parser;
}

def mem_nm_gp18 : mem_generic {
  let EncoderMethod = "getMemEncodingNMGP";
  let DecoderMethod = "DecodeMemNM<18,false,Mips::GPRNMGPRegClassID>";
  let OperandType = "OPERAND_NM_GPREL18";
  let OperandNamespace = "NanoMips";
  let ParserMatchClass = MemNMGP18Parser;
}

def mem_nm_gp17s1 : mem_generic {
  let EncoderMethod = "getMemEncodingNMGP";
  let DecoderMethod = "DecodeMemNM<18,false,Mips::GPRNMGPRegClassID>";
  let OperandType = "OPERAND_NM_GPREL18";
  let OperandNamespace = "NanoMips";
  let ParserMatchClass = MemNMGP17S1Parser;
}

def MemNMGP7S2Parser : NMMemUimmAsmOperand<7, 2, "Mips::GPRNMGPRegClassID",
						[MemNMU12Parser]>;

def mem_nm_gp7s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMGP";
  let DecoderMethod = "DecodeMemNM<9,false,Mips::GPRNMGPRegClassID>";
  let OperandType = "OPERAND_NM_GPREL9";
  let OperandNamespace = "NanoMips";
  let ParserMatchClass = MemNMGP7S2Parser;
}

def MemNM7S2Parser : NMMemSimmAsmOperand<9,2,
    "Mips::GPRNM32RegClassID">;

def mem_nm7s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<9,2>";
  let DecoderMethod = "DecodeMemNM<9,true,Mips::GPRNM32RegClassID>";
  let ParserMatchClass = MemNM7S2Parser;
}

def MemNMSP5S2Parser :
    NMMemUimmAsmOperand<5,2,"Mips::GPRNMSPRegClassID",[MemNMGP7S2Parser]>;

def mem_nm_sp5s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMSP";
  let DecoderMethod = "DecodeMemNM<7,false,Mips::GPRNMSPRegClassID>";
  let OperandType = "OPERAND_NM_SPREL7";
  let OperandNamespace = "NanoMips";
  let ParserMatchClass = MemNMSP5S2Parser;
}

def MemNM6S2Parser :
    NMMemUimmAsmOperand<4,2,"Mips::GPRNM3RegClassID",[MemNMSP5S2Parser]>;
def MemNM3S1Parser :
    NMMemUimmAsmOperand<2,1,"Mips::GPRNM3RegClassID",[MemNMSP5S2Parser]>;
def MemNM2Parser :
    NMMemUimmAsmOperand<2,0,"Mips::GPRNM3RegClassID",[MemNMSP5S2Parser]>;
def MemNM4S2Parser :
    NMMemUimmAsmOperand<2,2,"Mips::GPRNM4RegClassID",[MemNMSP5S2Parser]>;

def mem_nm6s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<6,2>";
  let DecoderMethod = "DecodeMemNM<6,false,Mips::GPRNM3RegClassID>";
  let ParserMatchClass = MemNM6S2Parser;
}

def mem_nm3s1 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<3,1>";
  let DecoderMethod = "DecodeMemNM<3,false,Mips::GPRNM3RegClassID>";
  let ParserMatchClass = MemNM3S1Parser;
}

def mem_nm2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<2>";
  let DecoderMethod = "DecodeMemNM<2,false,Mips::GPRNM3RegClassID>";
  let ParserMatchClass = MemNM2Parser;
}

def mem_nm4s2 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<4,2>";
  let ParserMatchClass = MemNM4S2Parser;
  let DecoderMethod = "DecodeMemNM4x4";
}

class NMMemRXAsmOperandClass<int Suffix = 32> : AsmOperandClass {
  let Name = "NMMemRX" # Suffix;
  let SuperClasses = [MipsMemAsmOperand];
  let RenderMethod = "addNMMemOperands";
  let ParserMethod = "parseMemNMRX";
  let PredicateMethod = "isMemNMRX" # !if(!eq(Suffix, 16), "<Mips::GPRNM3RegClassID>", "<>");
}

def mem_nmrx : mem_generic {
  let EncoderMethod = "getMemEncodingNMRX";
  let DecoderMethod = "DecodeMemNMRX<Mips::GPRNM32RegClassID>";
  let ParserMatchClass = NMMemRXAsmOperandClass<32>;
}

def mem_nmrx16 : mem_generic {
  let EncoderMethod = "getMemEncodingNMRX";
  let DecoderMethod = "DecodeMemNMRX<Mips::GPRNM3RegClassID>";
  let ParserMatchClass = NMMemRXAsmOperandClass<16>;
}

def MemNM0Parser : NMMemZeroAsmOperand<"Mips::GPRNM32RegClassID">;

def mem_nm0 : mem_generic {
  let EncoderMethod = "getMemEncodingNMImm<0,0>";
  let DecoderMethod = "DecodeMemZeroNM";
  let ParserMatchClass = MemNM0Parser;
}

let OperandType = "OPERAND_PCREL", PrintMethod = "printPCRel" in {

foreach I = {7, 10, 11, 14, 25} in
  def brtarget # I # _nm : Operand<OtherVT> {
    let EncoderMethod = "getBranchTargetOpValueNM<" # I # ">";
    let PrintMethod = "printBranchOperand";
    let DecoderMethod = "DecodeBranchTargetNM<" # I # ">";
    let ParserMatchClass = NMJumpTargetAsmOperand;
  }

def brtarget4s1_nm : Operand<OtherVT> {
  let EncoderMethod = "getBranchTargetOpValueNM<4>";
  let DecoderMethod = "DecodeBranchTargetNM<4>";
  // BEQC[16]/BNEC[16] has match priority over BEQC[32]/BNEC[32]
  let ParserMatchClass = NMJumpTargetAsmOperandClass<4, [NMJumpTargetAsmOperand]>;
  let PrintMethod = "printBranchOperand";
}

def calltarget21_nm : Operand<iPTR> {
  let EncoderMethod = "getBranchTargetOpValueNM<21>";
  let ParserMatchClass = NMJumpTargetAsmOperand;
  let DecoderMethod = "DecodeBranchTargetNM<21>";
}

def calltarget10_nm : Operand<iPTR> {
  let EncoderMethod = "getBranchTargetOpValueNM<10>";
  let DecoderMethod = "DecodeBranchTargetNM<10>";
  let ParserMatchClass = NMJumpTargetAsmOperandClass<10, [NMJumpTargetAsmOperand]>;
}

def calltarget25_nm : Operand<iPTR> {
  let EncoderMethod = "getBranchTargetOpValueNM<25>";
  let DecoderMethod = "DecodeBranchTargetNM<25>";
  let ParserMatchClass = NMJumpTargetAsmOperandClass<25, [calltarget10_nm.ParserMatchClass]>;
}

def jmptarget_nm   : Operand<OtherVT> {
  let EncoderMethod = "getBranchTargetOpValueNM<25>";
  let ParserMatchClass = NMJumpTargetAsmOperand;
}
} // let OperandType = "OPERAND_PCREL", ...

def simm32_nm : Operand<i32> {
  let ParserMatchClass = NMSImmAsmOperandClass<32>;
  let DecoderMethod = "DecodeSimm32";
  let EncoderMethod = "getSImm32Encoding";
}

def uimm32_nm : Operand<i32> {
  let ParserMatchClass = UImmAsmOperandClass<32>;
}

def simm32s12_nm : Operand<i32> {
  let ParserMatchClass = Hi20AsmOperand;
  let PrintMethod = "printHi20";
  let DecoderMethod = "DecodeSImm32s12";
}

def simm32s12_pcrel_nm : Operand<i32> {
  let ParserMatchClass = Hi20PCRelAsmOperand;
  let PrintMethod = "printHi20PCRel";
  let DecoderMethod = "DecodeSImm32s12";
  let OperandType   = "OPERAND_PCREL";
}

def sym32_pc_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymPCRel";
  let ParserMatchClass = SymAsmOperand;
  let OperandType   = "OPERAND_PCREL";
  let DecoderMethod = "DecodeAddressPCRelNM<32>";
  let PrintMethod = "printPCRel";
}

def sym32_pc22_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymPCRel";
  let ParserMatchClass = SymAsmOperand;
  let OperandType   = "OPERAND_PCREL";
  let DecoderMethod = "DecodeAddressPCRelNM<22>";
  let PrintMethod = "printPCRel";
}

def sym32_gp_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel;
  let DecoderMethod = "DecodeSImmWithReg<32, 0, 1, Mips::GP_NM>";
}

def sym32_gp18_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel18;
  let DecoderMethod = "DecodeUImmWithReg<18, 0, 1, Mips::GP_NM>";
}

def sym32_gp19s2_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymGPRel";
  let ParserMatchClass = SymAsmOperandGPRel19s2;
  let DecoderMethod = "DecodeUImmWithReg<21, 0, 1, Mips::GP_NM>";
}

def sym32_nm : Operand<iPTRAny> {
  let EncoderMethod = "getSymAbs";
  let ParserMatchClass = SymAsmOperand;
  let PrintMethod = "printUImm<32>";
}

def simm32power2 : IntImmLeaf<i32, [{ return isPowerOf2_32(Imm.getSExtValue()); }]>;

def addrsimm9 : ComplexPattern<iPTR, 2, "selectIntAddrSImm9", [frameindex]>;
def addruimm12 : ComplexPattern<iPTR, 2, "selectIntAddrUImm12", [frameindex]>;
def addrindexed : ComplexPattern<iPTR, 2, "selectIntAddrIndexed", [frameindex]>;
def addrindexedlsl1 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl1", [frameindex]>;
def addrindexedlsl2 : ComplexPattern<iPTR, 2, "selectIntAddrIndexedLsl2", []>;
def addrgp19s2 : ComplexPattern<iPTR, 2, "selectIntAddrUImm19s2", [frameindex]>;
def addrgp18 : ComplexPattern<iPTR, 2, "selectIntAddrUImm18", [frameindex]>;
def addrgp17s1 : ComplexPattern<iPTR, 2, "selectIntAddrUImm17s1", [frameindex]>;
def addrsym : ComplexPattern<iPTR, 1, "selectAddrSym", [frameindex]>;
def addrgprel : ComplexPattern<iPTR, 1, "selectAddrSymGPRel", [frameindex]>;
def offsetgp19s2 : ComplexPattern<iPTR, 1, "selectOffsetGP19s2", [frameindex]>;
def offsetgp18 : ComplexPattern<iPTR, 1, "selectOffsetGP18", [frameindex]>;

def Log2XForm : SDNodeXForm<imm, [{ return CurDAG->getTargetConstant(Log2_32(N->getZExtValue()),
                                                                   SDLoc(N), MVT::i32);
                                  }]>;

class RegConstraint<string C> {
  string Constraints = C;
}
class NoEncode<string E> {
  string DisableEncoding = E;
}

//===----------------------------------------------------------------------===//
//
// Instruction Descriptions
//
//===----------------------------------------------------------------------===//


class _Pool_P32<bits<5> psel> : InstSize32 {
  let Inst{31-26} = {psel{4...2}, 0b0, psel{1...0}};
}

class _Pool_P_ADDIU<bits<5> psel> : _Pool_P32<0b00000> {
  let Inst{25...21} = psel;
}

class _Pool_P32A<bits<3> psel> : _Pool_P32<0b00100> {
  let Inst{2...0} = psel;
}

class _Pool_P_GP_W<bits<2> psel> : _Pool_P32<0b01000> {
  let Inst{1...0} = psel;
}

class _Pool_P_U12<bits<4> psel> : _Pool_P32<0b10000> {
  let Inst{15-12} = psel;
}

class _Pool_P_LUI<bits<1> psel> : _Pool_P32<0b11100> {
  let Inst{1} = psel;
}

class _Pool_P48I<bits<5> psel> : InstSize48 {
  let Inst{47...42} = 0b011000;
  let Inst{36...32} = psel;
}

class _Pool_P_GP_BH<bits<3> psel> : _Pool_P32<0b01001> {
  let Inst{20...18} = psel;
}

class _Pool_P_LS_U12<bits<4> psel> : _Pool_P32<0b10001> {
  let Inst{15...12} = psel;
}

class _Pool_P_LS_S9<bits<3> psel> : _Pool_P32<0b10101> {
  let Inst{10...8} = psel;
}

// TODO MOVE.BALC

class _Pool_P_BAL<bits<1> psel> : _Pool_P32<0b00110> {
  let Inst{25} = psel;
}

class _Pool_P_J<bits<4> psel> : _Pool_P32<0b01010> {
  let Inst{15...12} = psel;
}

class _Pool_P_BR1<bits<2> psel> : _Pool_P32<0b10010> {
  let Inst{15...14} = psel;
}

class _Pool_P_BR2<bits<2> psel> : _Pool_P32<0b10110> {
  let Inst{15...14} = psel;
}

class _Pool_P_BR12<bits<3> psel> : _Pool_P32<0b10010> {
  let Inst{29} = psel{2};
  let Inst{15...14} = psel{1...0};
}

class _Pool_P_BRI<bits<3> psel> : _Pool_P32<0b11010> {
  let Inst{20...18} = psel;
}

class _Pool_P_BALRSC<bits<5> psel> : _Pool_P_J<0b1000> {
  let Inst{25...21} = psel;
}

class _Pool_P_SHIFT<bits<4> psel> : _Pool_P_U12<0b1100> {
  let Inst{8...5} = psel;
  bits<5> shift;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = shift;
}

class _Pool_P_SLL<bits<5> rt, bits<5> shift> : _Pool_P_SHIFT<0b0000> {
  let Inst{25...21} = rt;
  let Inst{4...0} = shift;
}

class _Pool_P_ROTX<bits<2> psel> : _Pool_P_U12<0b1101> {
  let Inst{11} = psel{1};
  let Inst{5} = psel{0};
}

class _Pool_P_EXT_INS<bits<4> psel> : _Pool_P_U12<psel> {
  let Inst{11} = 0;
  let Inst{5} = 0;
}

class _Pool_P_PREF_U12<bits<5> psel> : _Pool_P_LS_U12<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_LS_S0<bits<4> psel>: _Pool_P_LS_S9<0b000> {
  let Inst{14...11} = psel;
}

class _Pool_P_PREF_S9<bits<5> psel>: _Pool_P_LS_S0<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_LS_S1<bits<4> psel>: _Pool_P_LS_S9<0b001> {
  let Inst{14...11} = psel;
}

class _Pool_P_LL<bits<2> psel>: _Pool_P_LS_S1<0b1010> {
  let Inst{1...0} = psel;
}

class _Pool_P_SC<bits<2> psel>: _Pool_P_LS_S1<0b1011> {
  let Inst{1...0} = psel;
}

class _Pool_P_LS_E0<bits<4> psel>: _Pool_P_LS_S9<0b010> {
  let Inst{14...11} = psel;
}

class _Pool_P_LLE<bits<2> psel>: _Pool_P_LS_E0<0b1010> {
  let Inst{1...0} = psel;
}

class _Pool_P_PREFE<bits<5> psel>: _Pool_P_LS_E0<0b0011> {
  let Inst{25...21} = psel;
}

class _Pool_P_SCE<bits<2> psel>: _Pool_P_LS_E0<0b1011> {
  let Inst{1...0} = psel;
}

class _Pool_P_LS_WM<bits<1> psel>: _Pool_P_LS_S9<0b100> {
  let Inst{11} = psel;
}

class _Pool_P_LS_UAWM<bits<1> psel>: _Pool_P_LS_S9<0b101> {
  let Inst{11} = psel;
}

class _Pool_P16<bits<5> psel> : InstSize16 {
  let Inst{15-10} = {psel{4...2}, 0b1, psel{1...0}};
}

class _Pool_P16_MV : _Pool_P16<0b00000>;

class _Pool_P16_SR<bits<1> psel> : _Pool_P16<0b00011> {
  let Inst{8} = psel;
}

class _Pool_P16_SHIFT<bits<1> isel> : _Pool_P16<0b00100> {
  let Inst{3} = isel;
}

class _Pool_P16_4X4<bits<2> psel> : _Pool_P16<0b00111> {
  let Inst{8} = psel{1};
  let Inst{3} = psel{0};
}

class _Pool_P16C<bits<1> psel> : _Pool_P16<0b01000> {
  let Inst{0} = psel;
}

class _Pool_POOL16C_00<bits<2> psel> : _Pool_P16<0b01000> {
  let Inst{3...2} = psel;
  let Inst{1...0} = 0b00;
}

class _Pool_P16_LB<bits<2> psel> : _Pool_P16<0b01011> {
  let Inst{3...2} = psel;
}

class _Pool_P16_A1<bits<1> psel> : _Pool_P16<0b01100> {
  let Inst{6} = psel;
}

class _Pool_P16_LH<bits<2> psel> : _Pool_P16<0b01111> {
  let Inst{3} = psel{1};
  let Inst{0} = psel{0};
}

class _Pool_P16_A2<bits<1> psel> : _Pool_P16<0b10000> {
  let Inst{3} = psel;
}

class _Pool_P16_ADDU<bits<1> psel> : _Pool_P16<0b10100> {
  let Inst{0} = psel;
}

class _Pool_P16_BR<bits<4> psel> : _Pool_P16<0b11010> {
  let Inst{3...0} = psel;
}

class _Pool_P16_RI<bits<2> psel> : _Pool_P16_MV {
  let Inst{4...3} = psel;
  let Inst{9...5} = 0b00000;
}

class _Pool_P16_SYSCALL<bits<1> psel> : _Pool_P16_RI<0b01> {
  let Inst{2} = psel;
}

class _Pool_P_RI<bits<2> psel> : _Pool_P_ADDIU<0b00000> {
  let Inst{20...19} = psel;
}

class _Pool_POOL32A0<bits<1> psel> : _Pool_P32A<0b000> {
  let Inst{5} = psel;
}

class _Pool_POOL32A0_0<bits<6> psel> : _Pool_POOL32A0<0b0> {
  let Inst{9...6} = psel{5...2};
  let Inst{4...3} = psel{1...0};
}

class _Pool_P_TRAP<bits<1> isel> : _Pool_POOL32A0_0<0b000000>
{
  let Inst{10} = isel;
}

class _Pool_P_CMOVE<bits<1> isel> : _Pool_POOL32A0_0<0b100010>
{
  let Inst{10} = isel;
}

class _Pool_POOL32A7<bits<3> psel> : _Pool_P32A<0b111> {
  let Inst{5...3} = psel;
}

class _Pool_POOL32Axf_4<bits<7> isel> : _Pool_POOL32A7<0b111> {
  let Inst{15...9} = isel;
  let Inst{8...6} = 0b100;
}

class _Pool_POOL32Axf_5<bits<2> psel> : _Pool_POOL32A7<0b111> {
  let Inst{8...6} = 0b101;
  let Inst{15...14} = psel;
}

class _Pool_POOL32Axf_5_group0<bits<5> isel> : _Pool_POOL32Axf_5<0b00> {
  let Inst{13...9} = isel;
}

class _Pool_POOL32Axf_5_group1<bits<5> isel> : _Pool_POOL32Axf_5<0b01> {
  let Inst{13...9} = isel;
}

class _Pool_POOL32Axf_5_group3<bits<5> isel> : _Pool_POOL32Axf_5<0b11> {
  let Inst{13...9} = isel;
}

class _Pool_PP_LSX<bit scaled, bits<4> isel> : _Pool_POOL32A7<0b000> {
  let Inst{6} = scaled;
  let Inst{10...7} = isel;
}

class _Pool_POOL32A0_1<bits<6> psel> : _Pool_POOL32A0<0b1> {
  let Inst{9...6} = psel{5...2};
  let Inst{4...3} = psel{1...0};
}

// 16-bit arithmetic and logical instructions with 4x4 register operands, P16_4X4
class Arith4x4_Desc<string opstr, RegisterOperand RO,
			SDPatternOperator OpNode = null_frag>:
    InstAsmNM<(outs RO:$dst), (ins RO:$rt, RO:$rs),
	!strconcat(opstr, "\t$dst, $rt, $rs"),
	   [(set RO:$dst, (OpNode RO:$rt, RO:$rs))]> {
  Format Form = FrmR;
}

class Arith4x4_Enc<bits<2> isel>:  _Pool_P16_4X4<isel> {
      bits<5> dst;
      bits<5> rs;
      bits<5> rt;

      let Inst{9} = dst{4};
      let Inst{7...5} = dst{2...0};
      let Inst{4} = rs{4};
      let Inst{2...0} = rs{2...0};
}

// 32-bit arithmetic and logical instructions with 3 register operands.
class ArithLogicR32_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
   InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt),
	 !strconcat(opstr, "\t$rd, $rs, $rt"),
	 [(set RO:$rd, (OpNode RO:$rs, RO:$rt))]>;

class ArithLogicR32_Enc<bits<6> psel>: _Pool_POOL32A0_0<psel> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
}

// 16-bit arithmetic and logical instructions with 2 register operands, POOL16C_0
class ArithLogicR16_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
    InstAsmNM<(outs RO:$dst), (ins RO:$rs, RO:$rt),
	      !strconcat(opstr, "\t$dst, $rs, $rt"),
	      [(set RO:$dst, (OpNode RO:$rs, RO:$rt))]>;

class ArithLogicR16U_Desc<string opstr, RegisterOperand RO,
				 SDPatternOperator OpNode = null_frag>:
    InstNM<(outs RO:$rt), (ins RO:$rs),
	      !strconcat(opstr, "\t$rt, $rs"),
	      [(set RO:$rt, (OpNode RO:$rs))]> {
}

class ArithLogicR16_Enc<bits<2> isel>: _Pool_POOL16C_00<isel> {
  bits<3> rs;
  bits<3> rt;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
}

class CLZO_Enc<bits<7> isel>: _Pool_POOL32Axf_4<isel> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

class LogicU32_Enc<bits<6> isel>: _Pool_POOL32A0_0<isel> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

// 16-bit arithmetic instructions with 2 register operands, POOL16C_0
class AddSubR16_Desc<string opstr, RegisterOperand RO,
		    SDPatternOperator OpNode = null_frag>:
    InstAsmNM<(outs RO:$rd), (ins RO:$rs, RO:$rt), !strconcat(opstr, "\t$rd, $rs, $rt"),
	      [(set RO:$rd, (OpNode RO:$rs, RO:$rt))]>;

class AddSubR16_Enc<bits<1> isel>: _Pool_P16_ADDU<isel> {
  bits<3> rd;
  bits<3> rs;
  bits<3> rt;
  let Inst{15-10} = 0b101100;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{3...1} = rd;
}

class ArithLogicINM2Reg<string opstr, Operand Od, RegisterOperand DRO,
		        RegisterOperand SRO,
		        SDPatternOperator imm_type = null_frag,
		        SDPatternOperator OpNode = null_frag> :
  InstNM<(outs DRO:$rt), (ins SRO:$rs, Od:$imm),
	 !strconcat(opstr, "\t$rt, $rs, $imm"),
	 [(set DRO:$rt, (OpNode SRO:$rs, imm_type:$imm))]>;

class ArithLogicAsmNM<string opstr, Operand Od, RegisterOperand DRO,
		      RegisterOperand SRO,
		      SDPatternOperator imm_type = null_frag,
		      SDPatternOperator OpNode = null_frag> :
  InstAsmNM<(outs DRO:$rt), (ins SRO:$rs, Od:$imm),
	    !strconcat(opstr, "\t$rt, $rs, $imm"),
	    [(set DRO:$rt, (OpNode SRO:$rs, imm_type:$imm))]>;

// Arithmetic and logical instructions with 2 register operands and immediate.
class ArithLogicINM<string opstr, Operand Od, RegisterOperand RO,
		    SDPatternOperator imm_type = null_frag,
		    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM2Reg<opstr, Od, RO, RO, imm_type, OpNode>;

class ArithLogicI32<string opstr, Operand Od, RegisterOperand RO,
		    SDPatternOperator imm_type = null_frag,
		    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class AddImmediate<string opstr, Operand Od, RegisterOperand DRO,
		   RegisterOperand SRO,
		   SDPatternOperator imm_type = null_frag,
		   SDPatternOperator OpNode = null_frag> :
  ArithLogicINM2Reg<opstr, Od, DRO, SRO, imm_type, OpNode>;

class LoadUpperI32<string opstr, Operand Od, RegisterOperand RO,
		   SDPatternOperator imm_type = null_frag,
		   SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<5> rs;
  bits<16> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...0} = imm;
}

class DivMod<string opstr, SDPatternOperator OpNode = null_frag> :
    ArithLogicR32_Desc<opstr, GPRNM32Opnd, OpNode> {
  // Need to insert TEQ, because DIV(U)/MOD(U) don't trap on division by zero.
  bit usesCustomInserter = 1;
}

class CondLogicI32_Desc<string opstr, Operand Od, RegisterOperand RO,
                          SDPatternOperator imm_type = null_frag,
			  SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class CondLogicI32_Enc<bits<4> psel> : _Pool_P_U12<psel> {
  bits<5> rt;
  bits<5> rs;
  bits<12> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{11...0} = imm;
}

class ShiftI_Desc<string opstr, Operand Od, RegisterOperand RO,
                    SDPatternOperator imm_type = null_frag,
                    SDPatternOperator OpNode = null_frag> :
  ArithLogicINM<opstr, Od, RO, imm_type, OpNode>;

class ShiftI32_Enc<bits<4> psel> : _Pool_P_SHIFT<psel> {
  bits<5> rt;
  bits<5> rs;
  bits<5> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = imm;
}

class ShiftI16_Enc<bits<1> isel> : _Pool_P16_SHIFT<isel> {
  bits<3> rt;
  bits<3> rs;
  bits<3> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{2...0} = imm;
}

class ExtBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt), (ins RO:$rs, uimm5:$pos, uimm5_plus1:$size),
         "ext\t$rt, $rs, $pos, $size",
         [(set RO:$rt, (MipsExt RO:$rs, immZExt5:$pos, immZExt5Plus1:$size))],
         II_EXT>;

class InsBaseNM<RegisterOperand RO = GPRNM32Opnd> :
  InstNM<(outs RO:$rt),
         (ins RO:$rs, uimm5:$pos, uimm5_inssize_plus1:$size, RO:$src),
         "ins \t$rt, $rs, $pos, $size",
         [(set RO:$rt,
               (MipsIns RO:$rs, immZExt5:$pos, immZExt5Plus1:$size, RO:$src))],
         II_INS>, RegConstraint<"$src = $rt">;

class ExtInsBase_Enc<bits<4> isel> : _Pool_P_EXT_INS<isel> {
  bits<5> rt;
  bits<5> rs;
  bits<5> pos;
  bits<5> size;

  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{4...0} = pos;
  let Inst{10...6} = size;
}

class Trap_Desc<string opstr, RegisterOperand RO, Operand ImmOp> :
  InstNM<(outs), (ins RO:$rs, RO:$rt, ImmOp:$imm),
	 !strconcat(opstr, "\t$rs, $rt, $imm"), []> {
  let hasSideEffects = 1;
  let isCTI = 1;
}

class Trap_Enc<bits<1> isel> : _Pool_P_TRAP<isel> {
  bits<5> rt;
  bits<5> rs;
  bits<5> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = imm;
}

// Instructions with 1 register (dest) and 1 immediate operand.
class RegImmNM<string opstr, Operand Od, RegisterOperand RO,
               SDPatternOperator imm_type = null_frag,
               SDPatternOperator OpNode = null_frag> :
  InstNM<(outs RO:$rt), (ins Od:$imm), !strconcat(opstr, "\t$rt, $imm"),
         [(set RO:$rt, (OpNode imm_type:$imm))]>;

class LSMem_S9_Enc<bits<3> psel, bits<4> isel> : _Pool_P_LS_S9<psel>
{
  bits<5> rt;
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  let Inst{25...21} = rt;
  let Inst{20...16} = base;
  let Inst{14...11} = isel;
  let Inst{15} = offset{8};
  let Inst{7...0} = offset{7...0};
}

class LSMem_S9_Mult_Enc<bits<4> isel> : _Pool_P_LS_S9<isel{2...0}>
{
  bits<5> rt;
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  bits<3> rcount;
  let Inst{25...21} = rt;
  let Inst{20...16} = base;
  let Inst{11} = isel{3};
  let Inst{15} = offset{8};
  let Inst{7...0} = offset{7...0};
  let Inst{14...12} = rcount;
}

class LSMem_U12_Enc<bits<4> isel> : _Pool_P_LS_U12<isel>
{
  bits<5> rt;
  bits<17> addr;
  bits<5> base = addr{16...12};
  bits<12> offset = addr{11...0};
  let Inst{25...21} = rt;
  let Inst{20...16} = base;
  let Inst{11...0} = offset;
}

class LSMemX_Enc<bit scaled, bits<4> isel> : _Pool_PP_LSX<scaled, isel>
{
  bits<5> rt;
  bits<10> addr;
  let Inst{25...21} = addr{9...5};
  let Inst{20...16} = addr{4...0};
  let Inst{15...11} = rt;
}

class LSMem16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<9> addr;
  bits<3> base = addr{8...6};
  bits<4> offset = addr{5...2};
  let Inst{9...7} = rt;
  let Inst{6...4} = base;
  let Inst{3...0} = offset;
}

class LSGPRegMem16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<9> addr;
  bits<7> offset = addr{8...2};
  let Inst{9...7} = rt;
  let Inst{6...0} = offset;
}

class LSSPRegMem16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<5> rt;
  bits<7> addr;
  bits<5> offset = addr{6...2};
  let Inst{9...5} = rt;
  let Inst{4...0} = offset;
}

class LHMem16_Enc<bits<2> isel> : _Pool_P16_LH<isel>
{
  bits<3> rt;
  bits<6> addr;
  bits<3> base = addr{5...3};
  bits<3> offset = addr{2...0};
  let Inst{9...7} = rt;
  let Inst{6...4} = base;
  let Inst{2...1} = offset{2...1};
}

class LBMem16_Enc<bits<2> isel> : _Pool_P16_LB<isel>
{
  bits<3> rt;
  bits<5> addr;
  bits<3> base = addr{4...2};
  bits<2> offset = addr{1...0};
  let Inst{9...7} = rt;
  let Inst{6...4} = base;
  let Inst{1...0} = offset{1...0};
}

class LSMemGP19S2_Enc<bits<2> isel> : _Pool_P_GP_W<isel>
{
  bits<5> rt;
  bits<21> addr;
  let Inst{25...21} = rt;
  let Inst{20...2} = addr{20...2};
}

class LSMemGP18_Enc<bits<3> isel> : _Pool_P_GP_BH<isel>
{
  bits<5> rt;
  bits<18> addr;
  let Inst{25...21} = rt;
  let Inst{17...0} = addr;
}

class LSMemGP17S1_Enc<bits<4> isel> : _Pool_P_GP_BH<isel{3...1}>
{
  bits<5> rt;
  bits<18> addr;
  let Inst{25...21} = rt;
  let Inst{17...1} = addr{17...1};
  let Inst{0} = isel{0};
}

class LSMem4x4_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<5> rt;
  bits<9> addr;
  bits<5> rs = addr{8...4};
  bits<4> offset = addr{3...0};
  let Inst{9} = rt{4};
  let Inst{7...5} = rt{2...0};
  let Inst{4} = rs{4};
  let Inst{2...0} = rs{2...0};
  let Inst{3} = offset{3};
  let Inst{8} = offset{2};
}

class LoadMemoryNM<string opstr, RegisterOperand RO, DAGOperand MO,
                    SDPatternOperator OpNode = null_frag,
                    ComplexPattern Addr = addr,
                    InstrItinClass itin = NoItinerary> :
    InstNM<(outs RO:$rt), (ins MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
           [(set RO:$rt, (OpNode Addr:$addr))],
           itin> {
  let canFoldAsLoad = 1;
  let mayLoad = 1;
  string BaseOpcode = opstr;
}

class StoreMemoryNM<string opstr, RegisterOperand RO, DAGOperand MO,
                      SDPatternOperator OpNode = null_frag,
                      ComplexPattern Addr = addr,
                      InstrItinClass itin> :
    InstNM<(outs), (ins RO:$rt, MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
           [(OpNode RO:$rt, Addr:$addr)],
           itin> {
  let mayStore = 1;
  string BaseOpcode = opstr;
}

class LoadAsmNM<string opstr, RegisterOperand RO, DAGOperand MO,
		SDPatternOperator OpNode, InstrItinClass itin = NoItinerary> :
    InstAsmNM<(outs RO:$rt), (ins MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
              [(set RO:$rt, (OpNode addrDefault:$addr))], itin>;

class StoreAsmNM<string opstr, RegisterOperand RO, DAGOperand MO,
		 SDPatternOperator OpNode, InstrItinClass itin = NoItinerary> :
    InstAsmNM<(outs), (ins RO:$rt, MO:$addr), !strconcat(opstr, "\t$rt, $addr"),
              [(OpNode RO:$rt, addrDefault:$addr)], itin>;

class Op48_Addr_Enc<bits<5> isel> : _Pool_P48I<isel> {
  bits<5> rt;
  bits<32> addr;
  let Inst{41...37} = rt;
  let Inst{31...16} = addr{15...0};
  let Inst{15...0} = addr{31...16};
}

class LoadPCBase<DAGOperand RO, DAGOperand MO, ComplexPattern Addr> :
    InstNM<(outs RO:$rt),
           (ins MO:$addr), "lwpc\t$rt, $addr",
           [(set RO:$rt, (load Addr:$addr))],
           II_LW> {
  let mayLoad = 1;
}

class StorePCBase<DAGOperand RO, DAGOperand MO, ComplexPattern Addr> :
    InstNM<(outs),
           (ins RO:$rt, MO:$addr), "swpc\t$rt, $addr",
           [(store RO:$rt, Addr:$addr)],
	   II_SW> {
  let mayStore = 1;
}

class CallNM<string opstr,
             SDPatternOperator OpNode = null_frag,
             DAGOperand calltarget> :
  InstNM<(outs), (ins calltarget:$addr), !strconcat(opstr, "\t$addr"),
         [(OpNode tglobaladdr:$addr)]> {
  let isCall = 1;
  let isCTI = 1;
  let Defs = [RA_NM];
}

class IndirectCallNM<string opstr, RegisterOperand RA,
		     RegisterOperand RO> :
  InstNM<(outs RA:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         []> {
  let isCall = 1;
  let isCTI = 1;
}

class MoveBalcBase : InstNM<(outs GPRNM32Opnd:$rd),
                            (ins GPRNM32Opnd:$rt, calltarget21_nm:$addr),
                            "move.balc\t$rd, $rt, $addr", []>, InstSize32 {
  let isCall = 1;
  let isCTI = 1;
  let Defs = [RA_NM];
}

// Indirect branch instruction.
class IndirectBranchNM<string opstr, RegisterOperand RO,
                       SDPatternOperator operator = null_frag>:
    InstNM<(outs), (ins RO:$rs), !strconcat(opstr, "\t$rs"),
           [(operator RO:$rs)]> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isCTI = 1;
  let isBranch = 1;
  let isIndirectBranch = 1;
}

class AdjustStack<SDPatternOperator operator> :
    PseudoInstNM<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                 [(operator timm:$amt1, timm:$amt2)]> {
  let Defs = [SP_NM];
  let Uses = [SP_NM];
  let hasSideEffects = 1;
  let hasNoSchedulingInfo = 1;
}

class NMRegListAsmOperandClass<list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "NMRegList32";
  let RenderMethod = "addRegListOperands";
  let PredicateMethod = "isRegList";
  let SuperClasses = Supers;
  let ParserMethod = "parseNMRegisterList";
}

class NMRegList16AsmOperandClass<list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "NMRegList16";
  let PredicateMethod = "isNMRegList16";
  let RenderMethod = "addRegListOperands";
  let SuperClasses = Supers;
  let ParserMethod = "parseNMRegisterList";
}

def nmreglist16 : Operand<OtherVT> {
  let PrintMethod = "printNanoMipsRegisterList";
  let ParserMatchClass = NMRegList16AsmOperandClass<>;
  let DecoderMethod = "DecodeNMRegList16Operand";
  let EncoderMethod = "getNMRegList16Encoding";
  let OperandType = "OPERAND_NM_SAVE_REGLIST";
  let OperandNamespace = "NanoMips";
}

def nmreglist : Operand<OtherVT> {
  let PrintMethod = "printNanoMipsRegisterList";
  let ParserMatchClass = NMRegListAsmOperandClass<>;
  let DecoderMethod = "DecodeNMRegListOperand";
  let EncoderMethod = "getNMRegListEncoding";
  let OperandType = "OPERAND_NM_SAVE_REGLIST";
  let OperandNamespace = "NanoMips";
}

class SaveRestore<string opstr, Operand Od, Operand RL> :
  InstNM<(outs),
         (ins Od:$adj, RL:$regs, variable_ops),
         !strconcat(opstr, "\t$adj$regs"), []> {
  let Defs = [SP_NM];
  let Uses = [SP_NM];
  let hasSideEffects = 1;
}

class _Pool_PP_SR<bits<2> isel> : _Pool_P_U12<0b0011> {
  let Inst{20} = 0;
  let Inst{1...0} = isel;
}

class SaveRestore32_Enc<bits<2> isel> : _Pool_PP_SR<isel> {
  bits<12> adj;
  bits<10> regs;
  let Inst{11...3} = adj{11...3};
  let Inst{25...21} = regs{9...5};
  let Inst{19...16} = regs{4...1};
  let Inst{2} = regs{0};
}


class SaveRestore16_Enc<bits<1> isel> : _Pool_P16_SR<isel> {
  bits<8> adj;
  bits<5> regs;
  let Inst{7...4} = adj{7...4};
  let Inst{9} = regs{4};
  let Inst{3...0} = regs{3...0};
}

def SAVE16_NM : SaveRestore<"save", uimm8s4_nm, nmreglist16>, SaveRestore16_Enc<0b0>;
def RESTOREJRC16_NM : SaveRestore<"restore.jrc", uimm8s4_nm, nmreglist16>, SaveRestore16_Enc<0b1>;
def SAVE16_NOREG_NM : MipsInstAlias<"save $adj",
				    (SAVE16_NM uimm8s4_nm:$adj, 0)>;
def RESTOREJRC16_NOREG_NM : MipsInstAlias<"restore.jrc $adj",
					  (RESTOREJRC16_NM uimm8s4_nm:$adj, 0)>;

def SAVE_NM : SaveRestore<"save", uimm12s3_nm, nmreglist>, SaveRestore32_Enc<0b00>;
def RESTORE_NM : SaveRestore<"restore", uimm12s3_nm, nmreglist>, SaveRestore32_Enc<0b10>;
def RESTOREJRC_NM : SaveRestore<"restore.jrc", uimm12s3_nm, nmreglist>, SaveRestore32_Enc<0b11>;

def SAVE_NOREG_NM : MipsInstAlias<"save $adj",
				  (SAVE_NM uimm12s3_nm:$adj, 0)>;
def RESTORE_NOREG_NM : MipsInstAlias<"restore $adj",
				     (RESTORE_NM uimm12s3_nm:$adj, 0)>;
def RESTOREJRC_NOREG_NM : MipsInstAlias<"restore.jrc $adj",
					(RESTOREJRC_NM uimm12s3_nm:$adj, 0)>;

class LoadMultipleNM<string opstr> :
    InstNM<(outs GPRNM32Opnd:$rt),
	   (ins mem_nms9:$addr, uimm3plus1_nm:$rcount),
           !strconcat(opstr, "\t$rt, $addr, $rcount"), []> {
  let mayLoad = 1;
}
class StoreMultipleNM<string opstr> :
    InstNM<(outs), (ins GPRNM32Opnd:$rt, mem_nms9:$addr, uimm3plus1_nm:$rcount),
           !strconcat(opstr, "\t$rt, $addr, $rcount"), []> {
  let mayStore = 1;
}

class EffectiveAddressNM<string opstr, RegisterOperand RO> :
  InstNM<(outs RO:$rt), (ins mem_ea:$addr), !strconcat(opstr, "\t$rt, $addr"),
         [(set RO:$rt, addr:$addr)]> {
  let isCodeGenOnly = 1;
  let hasNoSchedulingInfo = 1;
  let DecoderMethod = "DecodeMem";
  string BaseOpcode = !strconcat(opstr, "_lea");
}

class UnalignedLoad<string opstr, SDNode OpNode,
                    RegisterOperand RO = GPRNM32Opnd> :
    InstNM<(outs RO:$rt), (ins mem_nms9:$addr, RO:$src),
           !strconcat(opstr, "\t$rt, $addr"),
           [(set RO:$rt, (OpNode addrsimm9:$addr, RO:$src))]>,
	   RegConstraint<"$src = $rt"> {
  let DecoderMethod = "DecodeMem";
  let mayLoad = 1;
  let canFoldAsLoad = 1;
}

class UnalignedStore<string opstr, SDNode OpNode,
                     RegisterOperand RO = GPRNM32Opnd> :
    InstNM<(outs), (ins RO:$rt, mem_nms9:$addr),
           !strconcat(opstr, "\t$rt, $addr"),
           [(OpNode RO:$rt, addrsimm9:$addr)]> {
  let DecoderMethod = "DecodeMem";
  let mayStore = 1;
}

class ReadHardwareNM<RegisterOperand CPURegOperand, RegisterOperand RO> :
  InstNM<(outs CPURegOperand:$rt), (ins RO:$hs, uimm5:$sel),
  "rdhwr\t$rt, $hs, $sel", []>;

class SignExtInRegNM<string opstr, ValueType vt, RegisterOperand RO> :
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (sext_inreg RO:$rs, vt))]>;

class LoadScaledAddressNM<string opstr, RegisterOperand RO> :
  InstNM<(outs RO:$rd), (ins RO:$rs, RO:$rt, uimm2:$shift),
         !strconcat(opstr, "\t$rd, $rs, $rt, $shift"),
         [(set RO:$rd, (add (shl RO:$rs, immZExt2:$shift), RO:$rt))]>;

class LoadScaledAddress_Enc: _Pool_POOL32A7<0b001> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  bits<2> shift;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{10...9} = shift;
}

class CountLeading0NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (ctlz RO:$rs))]>;

class CountLeading1NM<string opstr, RegisterOperand RO>:
  InstNM<(outs RO:$rt), (ins RO:$rs), !strconcat(opstr, "\t$rt, $rs"),
         [(set RO:$rt, (ctlz (not RO:$rs)))]>;

class ReverseNM<string opstr, RegisterOperand RO, SDNode OpNode>:
  InstNM<(outs RO:$rd), (ins RO:$rs), !strconcat(opstr, "\t$rd, $rs"),
         [(set RO:$rd, (OpNode RO:$rs))]>;

class LoadImmAsmNM<string opstr, RegisterOperand RO, Operand Od,
			SDPatternOperator imm_type = null_frag> :
  InstAsmNM<(outs RO:$rt), (ins Od:$eu),
            !strconcat(opstr, "\t$rt, $eu"),
            [(set RO:$rt, imm_type:$eu)]>;

class LoadAddressPCRelNM<string opstr, RegisterOperand RO,
			 DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins MO:$addr),
         !strconcat(opstr, "\t$rt, $addr"),
         [(set RO:$rt, Addr:$addr)]>;

class LoadAddressGPRelNM<string opstr, RegisterOperand RO, DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins RO:$rs, MO:$addr),
         !strconcat(opstr, "\t$rt, $rs, $addr"),
         [(set RO:$rt, (add RO:$rs, Addr:$addr))]>;

class LoadAddressAbsNM<string opstr, RegisterOperand RO, DAGOperand MO, ComplexPattern Addr> :
  InstNM<(outs RO:$rt), (ins MO:$imm),
         !strconcat(opstr, "\t$rt, $imm"),
         [(set RO:$rt, Addr:$imm)]>;

//===----------------------------------------------------------------------===//
//
// Instruction Definitions
//
//===----------------------------------------------------------------------===//


// 16-bit register operations
def NOT16_NM : ArithLogicR16U_Desc<"not", GPRNM16R3Opnd, not>,
	       ArithLogicR16_Enc<0b00>;
let isCommutable=1, TwoOperandAliasConstraint = "$rt = $dst" in {
    def XOR16_NM : ArithLogicR16_Desc<"xor", GPRNM16R3Opnd, xor>,
	           ArithLogicR16_Enc<0b01>;
    def AND16_NM : ArithLogicR16_Desc<"and", GPRNM16R3Opnd, and>,
	           ArithLogicR16_Enc<0b10>;
    def OR16_NM  : ArithLogicR16_Desc<"or", GPRNM16R3Opnd, or>,
	           ArithLogicR16_Enc<0b11>;
}

class CommuteBinOpAlias<string op, InstNM OpCode, RegisterOperand RO> :
	MipsInstAlias<!strconcat(op, "\t$rd, $rt, $rs"),
		(OpCode RO:$rd, RO:$rs, RO:$rt), 0>,
		ISA_NANOMIPS;

def : CommuteBinOpAlias<"and", AND16_NM, GPRNM16R3Opnd>;
def : CommuteBinOpAlias<"or", OR16_NM, GPRNM16R3Opnd>;
def : CommuteBinOpAlias<"xor", XOR16_NM, GPRNM16R3Opnd>;

class ANDI16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<3> rs;
  bits<4> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{3...0} = imm;
}

let TwoOperandAliasConstraint = "$rs = $rt" in {
  def ANDI16_NM : ArithLogicAsmNM<"andi[16]", uimm4mask_nm, GPRNM16R3Opnd,
				 GPRNM16R3Opnd, imm4Mask>,
				 ANDI16_Enc<0b11100>;
  def PseudoANDI_NM : NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rt),
					    (ins GPRNM32Opnd:$rs, uimm16_nm:$mask),
					    "andi\t$rt, $rs, $mask",
					    [(set GPRNM32Opnd:$rt, (and GPRNM32Opnd:$rs,
								    imm32ZExt12ANDI:$mask))]>;
}

// _POOL32A0_0 pool of instructions.
let TwoOperandAliasConstraint = "$rs = $rd" in {
def SLLV_NM  : ArithLogicR32_Desc<"sllv", GPRNM32Opnd, shl>,
	       ArithLogicR32_Enc<0b000010>;
def SRLV_NM  : ArithLogicR32_Desc<"srlv", GPRNM32Opnd, srl>,
	       ArithLogicR32_Enc<0b000110>;
def SRAV_NM  : ArithLogicR32_Desc<"srav", GPRNM32Opnd, sra>,
	       ArithLogicR32_Enc<0b001010>;
def ROTRV_NM : ArithLogicR32_Desc<"rotrv", GPRNM32Opnd, rotr>,
	       ArithLogicR32_Enc<0b001110>;
def ADD_NM   : ArithLogicR32_Desc<"add", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b010010>;
def ADDu_NM  : ArithLogicR32_Desc<"addu", GPRNM32Opnd, add>,
	       ArithLogicR32_Enc<0b010110>;
def SUB_NM   : ArithLogicR32_Desc<"sub", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b011010>;
def SUBu_NM  : ArithLogicR32_Desc<"subu", GPRNM32Opnd, sub>,
	       ArithLogicR32_Enc<0b011110>;
def AND_NM   : ArithLogicR32_Desc<"and", GPRNM32Opnd, and>,
	       ArithLogicR32_Enc<0b100110>;
def OR_NM    : ArithLogicR32_Desc<"or", GPRNM32Opnd, or>,
	       ArithLogicR32_Enc<0b101010>;
def NOR_NM   : ArithLogicR32_Desc<"nor", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b101110>;
def XOR_NM   : ArithLogicR32_Desc<"xor", GPRNM32Opnd, xor>,
	       ArithLogicR32_Enc<0b110010>;
def SLT_NM   : ArithLogicR32_Desc<"slt", GPRNM32Opnd, setlt>,
	       ArithLogicR32_Enc<0b110110>;
def SLTU_NM  : ArithLogicR32_Desc<"sltu", GPRNM32Opnd, setult>,
	       ArithLogicR32_Enc<0b111010>;
def MUL_NM   : ArithLogicR32_Desc<"mul", GPRNM32Opnd, mul>,
	       ArithLogicR32_Enc<0b000011>;
def MUH_NM   : ArithLogicR32_Desc<"muh", GPRNM32Opnd, mulhs>,
	       ArithLogicR32_Enc<0b000111>;
def MULU_NM  : ArithLogicR32_Desc<"mulu", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b001011>;
def MUHU_NM  : ArithLogicR32_Desc<"muhu", GPRNM32Opnd, mulhu>,
	       ArithLogicR32_Enc<0b001111>;
def DIV_NM   : DivMod<"div", sdiv>,
	       ArithLogicR32_Enc<0b010011>;
def MOD_NM   : DivMod<"mod", srem>,
	       ArithLogicR32_Enc<0b010111>;
def DIVU_NM  : DivMod<"divu", udiv>,
	       ArithLogicR32_Enc<0b011011>;
def MODU_NM  : DivMod<"modu", urem>,
	       ArithLogicR32_Enc<0b011111>;
def SOV_NM   : ArithLogicR32_Desc<"sov", GPRNM32Opnd>,
	       ArithLogicR32_Enc<0b111110>;
}
def NOT_NM   : MipsInstAlias<"not $rt, $rs",
			    (NOR_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, ZERO_NM)>,
			    ISA_NANOMIPS;

def : NMPat<(not GPRNM32Opnd:$rt), (NOR_NM GPRNM32Opnd:$rt, ZERO_NM)>;

// 16-bit register operations
let isCommutable=1 in {
def SUBu16_NM  : AddSubR16_Desc<"subu", GPRNM16R3Opnd, sub>,
		 AddSubR16_Enc<0b1>;
def ADDu16_NM : AddSubR16_Desc<"addu", GPRNM16R3Opnd, add>,
		AddSubR16_Enc<0b0>;
def ADDu4x4_NM : Arith4x4_Desc<"addu", GPRNM4Opnd, add>,
		 Arith4x4_Enc<0b00>, NoEncode<"$rt">;
def MUL4x4_NM  : Arith4x4_Desc<"mul", GPRNM4Opnd, mul>,
		  Arith4x4_Enc<0b01>, NoEncode<"$rt">;
}

def : CommuteBinOpAlias<"addu", ADDu4x4_NM, GPRNM4Opnd>;
def : CommuteBinOpAlias<"mul", MUL4x4_NM, GPRNM4Opnd>;


// P.U12 pool of instructions
let TwoOperandAliasConstraint = "$rs = $rt" in {
def ORI_NM   : CondLogicI32_Desc<"ori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, or>,
	       CondLogicI32_Enc<0b0000>;
}
def : NanoMipsInstAlias<"or $rt, $imm",
			(ORI_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rt, uimm12_nm:$imm), 0>;

def XORI_NM  : CondLogicI32_Desc<"xori", uimm12_nm, GPRNM32Opnd, imm32ZExt12, xor>,
	       CondLogicI32_Enc<0b0001>;
def ANDI_NM  : CondLogicI32_Desc<"andi[32]", uimm12_nm, GPRNM32Opnd, imm32ZExt12ANDI, and>,
	       CondLogicI32_Enc<0b0010>;
def SLTI_NM  : CondLogicI32_Desc<"slti", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setlt>,
	       CondLogicI32_Enc<0b0100>;
def SLTIU_NM : CondLogicI32_Desc<"sltiu", uimm12_nm, GPRNM32Opnd, imm32ZExt12, setult>,
	       CondLogicI32_Enc<0b0101>;
def SEQI_NM  : CondLogicI32_Desc<"seqi", uimm12_nm, GPRNM32Opnd, imm32ZExt12, seteq>,
	       CondLogicI32_Enc<0b0110>;

// P.SHIFT pool of instructions
def SLL16_NM : ArithLogicAsmNM<"sll", uimm3shift_nm, GPRNM16R3Opnd, GPRNM16R3Opnd,
	       imm32ZExt3, shl>,
	       ShiftI16_Enc<0b0>;
def SLL_NM  : ShiftI_Desc<"sll", uimm5, GPRNM32Opnd, imm32ZExt5, shl>,
	      ShiftI32_Enc<0b0000>;
def SRL16_NM : ArithLogicAsmNM<"srl", uimm3shift_nm, GPRNM16R3Opnd, GPRNM16R3Opnd, 
	       imm32ZExt3, srl>,
	       ShiftI16_Enc<0b1>;
def SRL_NM  : ShiftI_Desc<"srl", uimm5, GPRNM32Opnd, imm32ZExt5, srl>,
	      ShiftI32_Enc<0b0010>;
def SRA_NM  : ShiftI_Desc<"sra", uimm5, GPRNM32Opnd, imm32ZExt5, sra>,
	      ShiftI32_Enc<0b0100>;
def ROTR_NM : ShiftI_Desc<"rotr", uimm5, GPRNM32Opnd, imm32ZExt5, rotr>,
	      ShiftI32_Enc<0b0110>;

def EXT_NM : ExtBaseNM, ExtInsBase_Enc<0b1111>;
def INS_NM : InsBaseNM, ExtInsBase_Enc<0b1110>;

let TwoOperandAliasConstraint = "$rs = $rt" in {
def ADDIU_NM : AddImmediate<"addiu[32]", uimm16_nm, GPRNM32Opnd, GPRNM32Opnd,
                            imm32ZExt16, add>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<5> rs;
  bits<16> imm;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...0} = imm;
}

def PseudoADDIU_NM: NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rt),
					  (ins GPRNM32Opnd:$rs, sym32_nm:$imm),
					  "addiu\t$rt, $rs, $imm",
					  []>;
}

def : NanoMipsInstAlias<"addu $rt, $rs, $imm",
		        (ADDIU_NM GPRNM32NZOpnd:$rt, GPRNM32Opnd:$rs, uimm16_nm:$imm), 0>;

def ADDIUNEG_NM : AddImmediate<"addiu[neg]", nimm12_nm, GPRNM32Opnd, GPRNM32Opnd,
                               imm32Neg12, add>, CondLogicI32_Enc<0b1000>;

def PseudoSUBU_NM: NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rt),
					 (ins GPRNM32Opnd:$rs, uimm32_nm:$imm),
					 "subu\t$rt, $rs, $imm", []>;

def ADDIUR2_NM : ArithLogicAsmNM<"addiu[r2]", uimm5s2_nm, GPRNM16R3Opnd, GPRNM16R3Opnd,
                                 imm32ZExt5, add>, _Pool_P16_A2<0b0> {
  bits<3> rt;
  bits<3> rs;
  bits<5> imm;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{2...0} = imm{4...2};
}

def ADDIURS5_NM : AddImmediate<"addiu[rs5]", simm4, GPRNM16R5NZOpnd, GPRNM16R5NZOpnd,
			       imm32ZExt4, add>, _Pool_P16_A2<0b1>,
			       RegConstraint<"$rs = $rt">, NoEncode<"$rs">  {
  bits<5> rt;
  bits<4> imm;
  let Inst{9...5} = rt;
  let Inst{4} = imm{3};
  let Inst{2...0} = imm{2...0};
}

def ADDIUR1SP_NM : ArithLogicAsmNM<"addiu[r1.sp]", uimm8s2_nm, GPRNM16R3Opnd,
				   GPRNMSPOpnd, imm32ZExt8s2, add>,
				   _Pool_P16<0b01100>, NoEncode<"$rs"> {
  bits<3> rt;
  bits<8> imm;
  let Inst{9...7} = rt;
  let Inst{6} = 0b1;
  let Inst{5...0} = imm{7...2};
}

class Op48_Imm_Enc<bits<5> isel> : _Pool_P48I<isel> {
  bits<5> rt;
  bits<32> imm;
  let Inst{41...37} = rt;
  let Inst{31...16} = imm{15...0};
  let Inst{15...0} = imm{31...16};
}

def ADDIU48_NM : ArithLogicINM<"addiu[48]", sym32_nm, GPRNM48Opnd,
			imm32_NM, add>,
			Op48_Imm_Enc<0b00001>,
			RegConstraint<"$rs = $rt">;

class AddImmediateGP<string opstr, Operand Od, RegisterOperand RO,
		     ComplexPattern offset_type,
		     SDPatternOperator OpNode = null_frag> :
  InstNM<(outs RO:$rt), (ins GPRNMGPOpnd:$rs, Od:$offset),
	 !strconcat(opstr, "\t$rt, $rs, $offset"),
	 [(set RO:$rt, (add GPRNMGPOpnd:$rs, offset_type:$offset))]>;

def ADDIUGPB_NM : AddImmediateGP<"addiu[gp.b]", sym32_gp18_nm, GPRNM32Opnd,
				offsetgp18, add>,
				_Pool_P_GP_BH<0b011>, NoEncode<"$rs"> {
  bits<5> rt;
  bits<18> offset;
  let Inst{25...21} = rt;
  let Inst{17...0} = offset;
 }

def ADDIUGPW_NM : AddImmediateGP<"addiu[gp.w]", sym32_gp19s2_nm, GPRNM32Opnd,
				offsetgp19s2>,
				_Pool_P_GP_W<0b00>, NoEncode<"$rs"> {
  bits<5> rt;
  bits<21> offset;
  let Inst{25...21} = rt;
  let Inst{20...2} = offset{20...2};
}

class Op32_Addr_Enc<bits<5> isel> : _Pool_P32<isel> {
  bits<5> rt;
  bits<22> addr;
  let Inst{25...21} = rt;
  let Inst{20...1} = addr{20...1};
  let Inst{0} = addr{21};
}

def : MipsInstAlias<"addiu.b $rt, $rs, $offset",
			(ADDIUGPB_NM GPRNM32Opnd:$rt, GPRNMGPOpnd:$rs, sym32_gp_nm:$offset)>,
			ISA_NANOMIPS;
def : MipsInstAlias<"addiu.w $rt, $rs, $offset",
			(ADDIUGPW_NM GPRNM32Opnd:$rt, GPRNMGPOpnd:$rs, sym32_gp_nm:$offset)>,
			ISA_NANOMIPS;

let isReMaterializable = 1 in {
  def LAPC32_NM : LoadAddressPCRelNM<"lapc.h", GPRNM32Opnd, sym32_pc22_nm,
				     addrsym>, Op32_Addr_Enc<0b00001>;
  def LAPC48_NM : LoadAddressPCRelNM<"lapc.b", GPRNM48Opnd, sym32_pc_nm,
				     addrsym>, Op48_Addr_Enc<0b00011>;
}

def ADDIUPC48_NM : MipsInstAlias<"addiupc $rt, $addr",
			    (LAPC48_NM GPRNM48Opnd: $rt, sym32_pc_nm:$addr), 0>,
			    ISA_NANOMIPS;
def : MnemonicAlias<"lapc", "lapc.h">, Requires<[HasNanoMips]>;

def LSA_NM : LoadScaledAddressNM<"lsa", GPRNM32Opnd>, LoadScaledAddress_Enc;

def SEB_NM : SignExtInRegNM<"seb", i8, GPRNM32Opnd>, LogicU32_Enc<0b000001>;
def SEH_NM : SignExtInRegNM<"seh", i16, GPRNM32Opnd>, LogicU32_Enc<0b000101>;

def CLZ_NM : CountLeading0NM<"clz", GPRNM32Opnd>, CLZO_Enc<0b0101101>;
def CLO_NM : CountLeading1NM<"clo", GPRNM32Opnd>, CLZO_Enc<0b0100101>;

// P.TRAP pool of instructions.
def TEQ_NM : Trap_Desc<"teq", GPRNM32Opnd, uimm5>, Trap_Enc<0b0>;
def TNE_NM : Trap_Desc<"tne", GPRNM32Opnd, uimm5>, Trap_Enc<0b1>;

def : MipsInstAlias<"teq $rt, $rs",
		    (TEQ_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 0), 0>, ISA_NANOMIPS;

def MOVEBALC_NM : InstNM<(outs GPRNM1R1Opnd:$rd),
			    (ins GPRNM4ZOpnd:$rt, calltarget21_nm:$addr),
			    "move.balc\t$rd, $rt, $addr", []>,
			    _Pool_P32<0b00010> {
  let isCall = 1;
  let isCTI = 1;
  let Defs = [RA_NM];
  bits<5> rt;
  bits<1> rd;
  bits<22> addr;
  let Inst{25} = rt{4};
  let Inst{24} = rd;
  let Inst{23...21} = rt{2...0};
  let Inst{20...1} = addr{20...1};
  let Inst{0} = addr{21};
}

class LI16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rt;
  bits<7> eu;
  let Inst{9...7} = rt;
  let Inst{6...0} = eu;
}

def LI16_NM : LoadImmAsmNM<"li", GPRNM16R3Opnd, nimm7_nm, imm7M1To126>,
	      LI16_Enc<0b11000>;

let isReMaterializable = 1 in {
  def LI48_NM : LoadAddressAbsNM<"li[48]", GPRNM48Opnd, sym32_nm,
			         addrsym>, Op48_Imm_Enc<0b00000>;

  def PseudoLI_NM : NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rt),
					  (ins sym32_nm:$imm),
					  "li\t$rt, $imm",
					  [(set GPRNM32Opnd:$rt, imm:$imm)]>;

  def PseudoLA_NM : NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rt), (ins sym32_nm:$addr),
					  "la\t$rt, $addr",
					  [(set GPRNM32Opnd:$rt, addrsym:$addr)]>;
}

def ADDIUGP48_NM : LoadAddressGPRelNM<"addiu[gp48]", GPRNM48Opnd, sym32_gp_nm,
				      addrgprel>, Op48_Addr_Enc<0b00010>,
				      NoEncode<"$rs">;

def ADDIUB32_NM : MipsInstAlias<"addiu.b32 $rt, $rs, $addr",
				(ADDIUGP48_NM GPRNM48Opnd:$rt,
				GPRNMGPOpnd:$rs, sym32_gp_nm:$addr)>,
				ISA_NANOMIPS;

class LUI_Enc<bits<1> isel> : _Pool_P_LUI<isel> {
  bits<5> rt;
  bits<20> imm;
  let Inst{25...21} = rt;
  let Inst{20...12} = imm{8...0};
  let Inst{11...2} = imm{18...9};
  let Inst{0} = imm{19};
}

let isReMaterializable = 1 in {
  def LUI_NM : RegImmNM<"lui", simm32s12_nm, GPRNM32Opnd, imm32SExt20s12>,
	       LUI_Enc<0b0>;
}

def ALUIPC_NM : RegImmNM<"aluipc", simm32s12_pcrel_nm, GPRNM32Opnd, imm32SExt20s12>, LUI_Enc<0b1>;

def LEA_ADDIU_NM : EffectiveAddressNM<"addiu", GPRNM32Opnd>, _Pool_P32<0b00000> {
  bits<5> rt;
  bits<16> addr;
  let Inst{25...21} = rt;
  let Inst{20...16} = 0b00000;
  let Inst{15...0} = addr;
}

def RDHWR_NM : ReadHardwareNM<GPRNM32Opnd, COP0Opnd>,
	       _Pool_POOL32A0_0<0b011100> {
  bits<5> rt;
  bits<5> hs;
  bits<5> sel;
  let Inst{25...21} = rt;
  let Inst{20...16} = hs;
  let Inst{15...11} = sel;
}

def : MipsInstAlias<"rdhwr $rt, $hs",
			(RDHWR_NM GPRNM32Opnd:$rt, COP0Opnd:$hs, 0), 0>, ISA_NANOMIPS;

def ADJCALLSTACKDOWN_NM : AdjustStack<callseq_start>;
def ADJCALLSTACKUP_NM   : AdjustStack<callseq_end>;

// Return instruction is matched as RetRA, then expanded into PseudoReturnNM
// after register allocation. Finally, MipsAsmPrinter expands this into JRC_NM.
def PseudoReturnNM : PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs), []> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
  bit isCTI = 1;
}

let isCodeGenOnly = 1, hasNoSchedulingInfo = 1, hasDelaySlot = 0 in
def LoadJumpTableOffset : PseudoInstNM<(outs GPRNM32Opnd:$rd), (ins GPRNM32Opnd:$table, GPRNM32Opnd:$entry, i32imm:$jti), []>;

class IndirectBranch32_Enc : _Pool_P_J<0b1000>
{
  bits<5> rs;
  let Inst{25...21} = 0b00000;
  let Inst{20...16} = rs;
}

class IndirectBranch16_Enc<bits<1> isel> : _Pool_P16_BR<0b0000>
{
  bits<5> rs;
  let Inst{9...5} = rs;
  let Inst{4} = isel;
}

class IndirectCall32_Enc<bits<4> isel> : _Pool_P_J<isel>
{
  bits<5> rt;
  bits<5> rs;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

class IndirectCall16_Enc<bits<1> isel> : _Pool_P16_BR<0b0000>
{
  bits<5> rt;
  bits<5> rs;
  let Inst{9...5} = rs;
  let Inst{4} = rt{0}; // always 1, forced for disassembly of JALRC[16]
}

def JRC_NM : IndirectBranchNM<"jrc", GPRNM32Opnd>, IndirectBranch16_Enc<0>;
def : MnemonicAlias<"jr", "jrc">, Requires<[HasNanoMips]>;

// Indirect branch is matched as PseudoIndirectBranchNM and expanded to JRC_NM.
def PseudoIndirectBranchNM :
    PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs), [(brind GPRNM32Opnd:$rs)]>,
    PseudoInstExpansion<(JRC_NM GPRNM32Opnd:$rs)> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isBranch = 1;
  let isIndirectBranch = 1;
  bit isCTI = 1;
}

let hasPostISelHook = 1, isCall = 1, isCTI = 1, Defs = [RA_NM] in {
  def JALRC16_NM : IndirectCallNM<"jalrc", GPRNMRAOpnd, GPRNM32Opnd>,
			 IndirectCall16_Enc<1>;
  def JALRC_NM : IndirectCallNM<"jalrc", GPRNM32Opnd, GPRNM32Opnd>,
	         IndirectCall32_Enc<0b0000>;
  def JALRCHB_NM : IndirectCallNM<"jalrc.hb", GPRNM32Opnd, GPRNM32Opnd>,
	           IndirectCall32_Enc<0b0001>;
  def JALRCPseudo : PseudoInstNM<(outs), (ins GPRNM32Opnd:$rs),
                                 [(MipsJmpLink GPRNM32Opnd:$rs)]>,
    PseudoInstExpansion<(JALRC16_NM RA_NM, GPRNM32Opnd:$rs)>;
}
def : MipsInstAlias<"jalrc $rt", (JALRC16_NM RA_NM, GPRNM32Opnd:$rt), 0>, ISA_NANOMIPS;
def : MipsInstAlias<"jalrc.hb $rt", (JALRCHB_NM RA_NM, GPRNM32Opnd:$rt), 0>, ISA_NANOMIPS;
def : MipsInstAlias<"jrc.hb $rs", (JALRCHB_NM ZERO_NM, GPRNM32Opnd:$rs), 1>, ISA_NANOMIPS;
def : MnemonicAlias<"jalr", "jalrc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"jalr.hb", "jalrc.hb">, Requires<[HasNanoMips]>;

def : NMPat<(MipsFullAddr tglobaladdr:$in), (PseudoLA_NM tglobaladdr:$in)>;
def : NMPat<(MipsFullAddr tglobaltlsaddr:$in), (PseudoLA_NM tglobaltlsaddr:$in)>;
def : NMPat<(MipsFullAddr tblockaddress:$in), (PseudoLA_NM tblockaddress:$in)>;
def : NMPat<(MipsFullAddr tjumptable:$in), (LAPC48_NM tjumptable:$in)>;
def : NMPat<(MipsFullAddrAdd GPRNM32:$rs, tglobaladdr:$in), (ADDIU48_NM GPRNM32:$rs, tglobaladdr:$in)>;
 
def : NMPat<(not (or GPRNM32:$rs, GPRNM32:$rt)),
            (NOR_NM GPRNM32:$rs, GPRNM32:$rt)>;

// SetCC patterns.
def : NMPat<(setne GPRNM32:$lhs, 0), (SLTU_NM ZERO_NM, GPRNM32:$lhs)>;
def : NMPat<(seteq GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTIU_NM (XOR_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;
def : NMPat<(setne GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTU_NM ZERO_NM, (XOR_NM GPRNM32:$lhs, GPRNM32:$rhs))>;
def : NMPat<(setle GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLT_NM GPRNM32:$rhs, GPRNM32:$lhs), 1)>;
def : NMPat<(setule GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLTU_NM GPRNM32:$rhs, GPRNM32:$lhs), 1)>;
def : NMPat<(setgt GPRNM32:$lhs, GPRNM32:$rhs),
            (SLT_NM GPRNM32:$rhs, GPRNM32:$lhs)>;
def : NMPat<(setugt GPRNM32:$lhs, GPRNM32:$rhs),
            (SLTU_NM GPRNM32:$rhs, GPRNM32:$lhs)>;
def : NMPat<(setge GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLT_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;
def : NMPat<(setuge GPRNM32:$lhs, GPRNM32:$rhs),
            (XORI_NM (SLTU_NM GPRNM32:$lhs, GPRNM32:$rhs), 1)>;

//===----------------------------------------------------------------------===//
//
// Jump and Branch Instructions.
//
//===----------------------------------------------------------------------===//

// NanoMips has only 'compact' jumps and branches, with no delay slots.
// Described using IsntSE class for the moment, should use specific
// NanoMips instruction formats later to get correct sizes.  Some
// branch instructions exist in multiple different sizes, and should
// likely be distinct instructions later.

// This class is missing instruction size, because these instructions can be
// encoded in formats of different size (eg. BEQC16 and BEQC32).
class CBranchNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                RegisterOperand RO>
    : InstNM<(outs), (ins RO:$rs, RO:$rt, opnd:$offset),
             !strconcat(opstr, "\t$rs, $rt, $offset"),
             [(brcond (i32 (cond_op RO:$rs, RO:$rt)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class CBranchAsmNM<string opstr, DAGOperand opnd, PatFrag cond_op,
		   RegisterOperand RS, RegisterOperand RT>
    : InstAsmNM<(outs), (ins RS:$rs, RT:$rt, opnd:$offset),
                !strconcat(opstr, "\t$rs, $rt, $offset"),
                [(brcond (i32 (cond_op RS:$rs, RT:$rt)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class CBranchImmNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                   RegisterOperand RO,
                   Operand imm_type, SDPatternOperator ImmOp>
    : InstNM<(outs), (ins RO:$rt, imm_type:$u, opnd:$offset),
             !strconcat(opstr, "\t$rt, $u, $offset"),
             [(brcond (i32 (cond_op RO:$rt, ImmOp:$u)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class CBranchZeroNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                    RegisterOperand RO>
    : InstNM<(outs), (ins RO:$rs, opnd:$offset),
             !strconcat(opstr, "\t$rs, $offset"),
             [(brcond (i32 (cond_op RO:$rs, 0)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  bit isCTI = 1;
}

class CBranchZeroAsmNM<string opstr, DAGOperand opnd, PatFrag cond_op,
		       RegisterOperand RO>
    : InstAsmNM<(outs), (ins RO:$rs, opnd:$offset),
                !strconcat(opstr, "\t$rs, $offset"),
                [(brcond (i32 (cond_op RO:$rs, 0)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  bit isCTI = 1;
}

class CBranchBitNM<string opstr, DAGOperand opnd, PatFrag cond_op,
                   RegisterOperand RO,
                   Operand imm_type, SDPatternOperator ImmOp>
    : InstNM<(outs), (ins RO:$rt, imm_type:$u, opnd:$offset),
             !strconcat(opstr, "\t$rt, $u, $offset"),
        [(brcond (i32 (cond_op (and RO:$rt, (shl 1, ImmOp:$u)), 0)), bb:$offset)]> {
  let isBranch = 1;
  let isTerminator = 1;
  let hasDelaySlot = 0;
  bit isCTI = 1;
}

class BCBase<DAGOperand brtarget>
      : InstNM<(outs), (ins brtarget:$addr), "bc\t$addr",
	               [(br bb:$addr)], II_J> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isBranch = 1;
  let isCTI = 1;
}

class BC16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<11> addr;
  let Inst{9...1} = addr{9...1};
  let Inst{0} = addr{10};
}

class BC32_Enc<bits<1> isel> : _Pool_P_BAL<isel>
{
  bits<26> addr;
  let Inst{24...1} = addr{24...1};
  let Inst{0} = addr{25};
}
 
class B14_Enc<bits<3> isel> : _Pool_P_BR12<isel>
{
  bits<5> rt;
  bits<5> rs;
  bits<15> offset;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{13...1} = offset{13...1};
  let Inst{0} =  offset{14};
}

class B14Zero_Enc<bits<3> isel> : _Pool_P_BR12<isel>
{
  bits<5> rs;
  bits<15> offset;
  let Inst{25...21} = rs;
  let Inst{20...16} = 0b00000;
  let Inst{13...1} = offset{13...1};
  let Inst{0} =  offset{14};
}

class BZ16_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<3> rs;
  bits<8> offset;
  let Inst{9...7} = rs;
  let Inst{6...1} = offset{6...1};
  let Inst{0} =  offset{7};
}

class BRI_Enc<bits<3> isel> : _Pool_P_BRI<isel>
{
  bits<5> rt;
  bits<7> u;
  bits<12> offset;
  let Inst{25...21} = rt;
  let Inst{17...11} = u;
  let Inst{10...1} = offset{10...1};
  let Inst{0} =  offset{11};
}

class BBit_Enc<bits<3> isel> : _Pool_P_BRI<isel>
{
  bits<5> rt;
  bits<6> u;
  bits<12> offset;
  let Inst{25...21} = rt;
  let Inst{16...11} = u;
  let Inst{10...1} = offset{10...1};
  let Inst{0} =  offset{11};
}

def BC_NM : BCBase<brtarget25_nm>, BC32_Enc<0b0>;
def BALC_NM : CallNM<"balc", MipsJmpLink, calltarget25_nm>, BC32_Enc<0b1>;
def BC16_NM : BCBase<brtarget10_nm>, BC16_Enc<0b00010>;
def BALC16_NM : CallNM<"balc[16]", MipsJmpLink, calltarget10_nm>, BC16_Enc<0b00110>;

def : NMPat<(MipsJmpLink (i32 texternalsym:$dst)), (BALC_NM texternalsym:$dst)>;

def : MnemonicAlias<"b", "bc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bal", "balc">, Requires<[HasNanoMips]>;
def : NanoMipsInstAlias<"j $target", (BC_NM brtarget25_nm:$target), 0>;

class B4_Enc : _Pool_P16<0b11010> {
  bits<3> rs;
  bits<3> rt;
  bits<4> offset;
  let Inst{9...7} = rt;
  let Inst{6...4} = rs;
  let Inst{3...0} = offset;
}

def BEQC_NM : CBranchNM<"beqc", brtarget14_nm, seteq, GPRNM32Opnd>,
	      B14_Enc<0b000>;
def BEQC16_NM : CBranchAsmNM<"beqc", brtarget4s1_nm, seteq, GPRNM16R3Opnd, GPRNM16R3Opnd>,
	        B4_Enc, DecodeDisambiguates<"BranchConflictNM">;
def BGEC_NM : CBranchNM<"bgec", brtarget14_nm, setge, GPRNM32Opnd>,
	      B14_Enc<0b010>;
def BGEUC_NM : CBranchNM<"bgeuc", brtarget14_nm, setuge, GPRNM32Opnd>,
	       B14_Enc<0b011>;

def BNEC_NM : CBranchNM<"bnec", brtarget14_nm, setne, GPRNM32Opnd>,
	      B14_Enc<0b100>;
def BNEC16_NM : CBranchAsmNM<"bnec", brtarget4s1_nm, setne, GPRNM16R3Opnd, GPRNM16R3Opnd>,
	        B4_Enc, DecodeDisambiguates<"BranchConflictNM"> {
  let DecoderNamespace = "NanoMips_Conflict_Space";
}
def BLTC_NM : CBranchNM<"bltc", brtarget14_nm, setlt, GPRNM32Opnd>,
	      B14_Enc<0b110>;
def BLTUC_NM : CBranchNM<"bltuc", brtarget14_nm, setult, GPRNM32Opnd>,
	       B14_Enc<0b111>;

def : MipsInstAlias<"beqc $rt, $rs, $offset",
      (BEQC16_NM GPRNM16R3Opnd:$rs, GPRNM16R3Opnd:$rt, brtarget4s1_nm:$offset), 1>,
      ISA_NANOMIPS;
def : MipsInstAlias<"bnec $rt, $rs, $offset",
      (BNEC16_NM GPRNM16R3Opnd:$rs, GPRNM16R3Opnd:$rt, brtarget4s1_nm:$offset), 1>,
      ISA_NANOMIPS;

def BEQZC16_NM : CBranchZeroAsmNM<"beqzc", brtarget7_nm, seteq, GPRNM16R3Opnd>,
	     BZ16_Enc<0b10010>;
def BNEZC16_NM : CBranchZeroAsmNM<"bnezc", brtarget7_nm, setne, GPRNM16R3Opnd>,
	       BZ16_Enc<0b10110>;

// Encode b[eq|ne]c with $zero operand as b[eq|ne]c16
let isAsmParserOnly = 1 in {
  def BEQCzero_NM : CBranchAsmNM<"beqc", brtarget7_nm, seteq,
				 GPRNM16R3Opnd, GPR32ZeroOpnd>,
				BZ16_Enc<0b10010>, NoEncode<"$rt">;
  def BNECzero_NM : CBranchAsmNM<"bnec", brtarget7_nm, setne, GPRNM16R3Opnd,
				 GPR32ZeroOpnd>,
				BZ16_Enc<0b10110>, NoEncode<"$rt">;
}

// Above pattern with operands commuted
def : NanoMipsInstAlias<"beqc $rt, $rs, $offset",
			(BEQCzero_NM GPRNM16R3Opnd:$rs, GPR32ZeroOpnd:$rt,
			brtarget7_nm:$offset), 0>;
def : NanoMipsInstAlias<"bnec $rt, $rs, $offset",
			(BNECzero_NM  GPRNM16R3Opnd:$rs, GPR32ZeroOpnd:$rt,
			brtarget7_nm:$offset), 0>;

// Encode BEQZC/BNEZC using BEQC/BNEC
let isAsmParserOnly = 1 in {
def BEQZC_NM : CBranchZeroNM<"beqzc", brtarget14_nm, seteq, GPRNM32Opnd>,
	       B14Zero_Enc<0b000>;
def BNEZC_NM : CBranchZeroNM<"bnezc", brtarget14_nm, setne, GPRNM32Opnd>,
	       B14Zero_Enc<0b100>;
}

// Aliases required to disassemble above pattern as BEQZC/BNEZC
def : NanoMipsInstAlias<"beqzc $rt, $offset",
			(BEQC_NM GPRNM32Opnd:$rt, ZERO_NM,
			brtarget14_nm:$offset), 1>;
def : NanoMipsInstAlias<"bnezc $rt, $offset",
			(BNEC_NM  GPRNM32Opnd:$rt, ZERO_NM,
			brtarget14_nm:$offset), 1>;
def : NanoMipsInstAlias<"beqzc $rt, $offset",
			(BEQC_NM ZERO_NM, GPRNM32Opnd:$rt,
			brtarget14_nm:$offset), 1>;
def : NanoMipsInstAlias<"bnezc $rt, $offset",
			(BNEC_NM ZERO_NM, GPRNM32Opnd:$rt,
			brtarget14_nm:$offset), 1>;

def : MipsInstAlias<"bgtzc $rt, $offset",
		(BLTC_NM ZERO_NM, GPRNM32Opnd:$rt, brtarget14_nm:$offset), 0>, ISA_NANOMIPS;

def BEQIC_NM : CBranchImmNM<"beqic", brtarget11_nm, seteq, GPRNM32Opnd, uimm7, immZExt7>,
	       BRI_Enc<0b000>;
def BGEIC_NM : CBranchImmNM<"bgeic", brtarget11_nm, setge, GPRNM32Opnd, uimm7, immZExt7>,
	       BRI_Enc<0b010>;
def BLTIC_NM : CBranchImmNM<"bltic", brtarget11_nm, setlt, GPRNM32Opnd, uimm7, immZExt7>,
	       BRI_Enc<0b110>;
def BNEIC_NM : CBranchImmNM<"bneic", brtarget11_nm, setne, GPRNM32Opnd, uimm7, immZExt7>,
	       BRI_Enc<0b100>;
def BGEIUC_NM : CBranchImmNM<"bgeiuc", brtarget11_nm, setuge, GPRNM32Opnd, uimm7, immZExt7>,
	        BRI_Enc<0b011>;
def BLTIUC_NM : CBranchImmNM<"bltiuc", brtarget11_nm, setult, GPRNM32Opnd, uimm7, immZExt7>,
	        BRI_Enc<0b111>;

def : MnemonicAlias<"beq", "beqc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bne", "bnec">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"blt", "bltc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bgtz", "bgtzc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bge", "b">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bltu", "bltuc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bgeu", "bgeuc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"beqz", "beqzc">, Requires<[HasNanoMips]>;
def : MnemonicAlias<"bnez", "bnezc">, Requires<[HasNanoMips]>;

let hasNoSchedulingInfo = 1,
    hasDelaySlot = 0, isBranch = 1,
    isBarrier = 1,
   isTerminator = 1,
   isCTI = 1 in
def BRSC_NM : InstNM<(outs), (ins GPRNM32Opnd:$rs, i32imm:$jti), "brsc\t$rs", [(NMbr_jt GPRNM32Opnd:$rs, tjumptable:$jti)]>, IndirectBranch32_Enc;
def BALRSC_NM : IndirectCallNM<"balrsc", GPRNM32NZOpnd, GPRNM32Opnd>, IndirectCall32_Enc<0b1000>;

// Bit-test branches
let hasNoSchedulingInfo = 1,
    hasDelaySlot = 0 in {
def BBNEZC_NM : CBranchBitNM<"bbnezc", brtarget11_nm, setne, GPRNM32Opnd, simm32, simm32power2>,
		BBit_Enc<0b101>;
def BBEQZC_NM : CBranchBitNM<"bbeqzc", brtarget11_nm, seteq, GPRNM32Opnd, simm32, simm32power2>,
		BBit_Enc<0b001>;
}

class TailCallBase : PseudoInstNM<(outs), (ins calltarget25_nm:$target), []>,
                     PseudoInstExpansion<(BC_NM brtarget25_nm:$target)> {
  let isCall = 1;
  let isTerminator = 1;
  let isReturn = 1;
  let isBarrier = 1;
  let isCTI = 1;
}
def TAILCALL_NM : TailCallBase;
def MUSTTAILCALL_NM : TailCallBase;

class TailCallRegBase<list<dag> pattern>
    : PseudoInstNM<(outs), (ins RegisterOperand<GPRNM32_TAIL>:$rs), pattern>,
      PseudoInstExpansion<(JRC_NM GPRNM32Opnd:$rs)> {
  let isCall = 1;
  let isTerminator = 1;
  let isReturn = 1;
  let isBarrier = 1;
  let isCTI = 1;
  let hasPostISelHook = 1;
}
def TAILCALLREG_NM
    : TailCallRegBase<[(MipsTailCall RegisterOperand<GPRNM32_TAIL>:$rs)]>;
def MUSTTAILCALLREG_NM
    : TailCallRegBase<[(MipsMustTailCall RegisterOperand<GPRNM32_TAIL>:$rs)]>;

def : NMPat<(MipsTailCall (i32 tglobaladdr:$dst)),
            (TAILCALL_NM tglobaladdr:$dst)>;
def : NMPat<(MipsTailCall (i32 texternalsym:$dst)),
            (TAILCALL_NM texternalsym:$dst)>;
def : NMPat<(MipsMustTailCall (i32 tglobaladdr:$dst)),
            (MUSTTAILCALL_NM tglobaladdr:$dst)>;
def : NMPat<(MipsMustTailCall (i32 texternalsym:$dst)),
            (MUSTTAILCALL_NM texternalsym:$dst)>;

// '>-1' can be represented as '>=0'.
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, -1)), bb:$dst),
            (BGEC_NM GPRNM32:$lhs, ZERO_NM, bb:$dst)>;

// Reversed branch conditions:
def : NMPat<(brcond (i32 (setugt GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BLTUC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BLTC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setule GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BGEUC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;
def : NMPat<(brcond (i32 (setle GPRNM32:$lhs, GPRNM32:$rhs)), bb:$dst),
            (BGEC_NM GPRNM32:$rhs, GPRNM32:$lhs, bb:$dst)>;

// Immediate comparison branches with offset
def : NMPat<(brcond (i32 (setgt GPRNM32:$lhs, immZExt7Plus1:$imm)), bb:$dst),
            (BGEIC_NM GPRNM32:$lhs, (Plus1 $imm), bb:$dst)>;
def : NMPat<(brcond (i32 (setugt GPRNM32:$lhs, immZExt7Plus1:$imm)), bb:$dst),
            (BGEIUC_NM GPRNM32:$lhs, (Plus1 $imm), bb:$dst)>;

// Branch on integer value
def : NMPat<(brcond (i32 GPRNM32:$lhs), bb:$dst),
            (BNEC_NM GPRNM32:$lhs, ZERO_NM, bb:$dst)>;

// Bit-test branch
def : NMPat<(brcond (i32 (setne (and GPRNM32:$rt, simm32power2:$imm), 0)), bb:$offset),
          (BBNEZC_NM GPRNM32:$rt, (Log2XForm $imm), bb:$offset)>;
def : NMPat<(brcond (i32 (seteq (and GPRNM32:$rt, simm32power2:$imm), 0)), bb:$offset),
          (BBEQZC_NM GPRNM32:$rt, (Log2XForm $imm), bb:$offset)>;


//===----------------------------------------------------------------------===//
//
// Load / store instructions
//
//===----------------------------------------------------------------------===//
def LWSP16_NM : LoadAsmNM<"lw", GPRNM32Opnd, mem_nm_sp5s2, load, II_LW>,
	        LSSPRegMem16_Enc<0b00101>;
def SWSP16_NM : StoreAsmNM<"sw", GPRNM32Opnd, mem_nm_sp5s2, store, II_SW>,
	        LSSPRegMem16_Enc<0b10101>;
def LWGP16_NM : LoadAsmNM<"lw", GPRNM16R3Opnd, mem_nm_gp7s2, load, II_LW>,
	        LSGPRegMem16_Enc<0b01001>;
def SWGP16_NM : StoreAsmNM<"sw", GPRNM16R3ZOpnd, mem_nm_gp7s2, store, II_SW>,
	        LSGPRegMem16_Enc<0b11001>;

def LW16_NM : LoadAsmNM<"lw", GPRNM16R3Opnd, mem_nm6s2, load, II_LW>,
    	      LSMem16_Enc<0b00001>;
def SW16_NM : StoreAsmNM<"sw", GPRNM16R3ZOpnd, mem_nm6s2, store, II_SW>,
    	      LSMem16_Enc<0b10001>;
def LH16_NM : LoadAsmNM<"lh", GPRNM16R3Opnd, mem_nm3s1, sextloadi16, II_LH>,
    	      LHMem16_Enc<0b00>;
def LHU16_NM : LoadAsmNM<"lhu", GPRNM16R3Opnd, mem_nm3s1, zextloadi16, II_LH>,
    	      LHMem16_Enc<0b10>;
def SH16_NM : StoreAsmNM<"sh", GPRNM16R3ZOpnd, mem_nm3s1, truncstorei16, II_SH>,
    	      LHMem16_Enc<0b01>;

def LB16_NM : LoadAsmNM<"lb", GPRNM16R3Opnd, mem_nm2, sextloadi8, II_LB>,
    	      LBMem16_Enc<0b00>;
def LBU16_NM : LoadAsmNM<"lbu", GPRNM16R3Opnd, mem_nm2, zextloadi8, II_LB>,
    	      LBMem16_Enc<0b10>;
def SB16_NM : StoreAsmNM<"sb", GPRNM16R3ZOpnd, mem_nm2, truncstorei8, II_SB>,
	      LBMem16_Enc<0b01>;

def LWGP_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem_nm_gp19s2, load, addrgp19s2, II_LW>,
	      LSMemGP19S2_Enc<0b10>;
def SWGP_NM : StoreMemoryNM<"sw", GPRNM32Opnd, mem_nm_gp19s2, store, addrgp19s2, II_SW>,
	      LSMemGP19S2_Enc<0b11>;

def LBGP_NM : LoadMemoryNM<"lb", GPRNM32Opnd, mem_nm_gp18, sextloadi8, addrgp18, II_LB>,
	      LSMemGP18_Enc<0b000>;
def SBGP_NM : StoreMemoryNM<"sb", GPRNM32Opnd, mem_nm_gp18, truncstorei8, addrgp18, II_SB>,
	      LSMemGP18_Enc<0b001>;
def LBUGP_NM : LoadMemoryNM<"lbu", GPRNM32Opnd, mem_nm_gp18, zextloadi8, addrgp18, II_LB>,
	      LSMemGP18_Enc<0b010>;

def LHGP_NM : LoadMemoryNM<"lh", GPRNM32Opnd, mem_nm_gp17s1, sextloadi16, addrgp17s1, II_LH>,
	      LSMemGP17S1_Enc<0b1000>;
def SHGP_NM : StoreMemoryNM<"sh", GPRNM32Opnd, mem_nm_gp17s1, truncstorei16, addrgp17s1, II_SH>,
	      LSMemGP17S1_Enc<0b1010>;
def LHUGP_NM : LoadMemoryNM<"lhu", GPRNM32Opnd, mem_nm_gp17s1, zextloadi16, addrgp17s1, II_LH>,
	      LSMemGP17S1_Enc<0b1001>;

// TODO: addruimm12 and addrsimm9 need to be looked into again. Currently
//       addruimm12 needs to come before addrsimm9, because addrsimm9 handles
//       loads/store that do not fit the offset. If we put the same handling in
//       addruimm12, this will cause addrsimm9 to never get selected.

def LB_NM : LoadMemoryNM<"lb", GPRNM32Opnd, mem_nmu12, sextloadi8, addruimm12, II_LB>,
		LSMem_U12_Enc<0b0000>;
def SB_NM : StoreMemoryNM<"sb", GPRNM32Opnd, mem_nmu12, truncstorei8, addruimm12, II_SB>,
		LSMem_U12_Enc<0b0001>;
def LH_NM : LoadMemoryNM<"lh", GPRNM32Opnd, mem_nmu12, sextloadi16, addruimm12, II_LH>,
		LSMem_U12_Enc<0b0100>;
def SH_NM : StoreMemoryNM<"sh", GPRNM32Opnd, mem_nmu12, truncstorei16, addruimm12, II_SH>,
		LSMem_U12_Enc<0b0101>;
def LW_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem_nmu12, load, addruimm12, II_LW>,
	      LSMem_U12_Enc<0b1000>;
def SW_NM : StoreMemoryNM<"sw", GPRNM32Opnd, mem_nmu12, store, addruimm12, II_SW>,
	      LSMem_U12_Enc<0b1001>;
def LBU_NM : LoadMemoryNM<"lbu", GPRNM32Opnd, mem_nmu12, zextloadi8, addruimm12, II_LBU>,
		LSMem_U12_Enc<0b0010>;
def LHU_NM : LoadMemoryNM<"lhu", GPRNM32Opnd, mem_nmu12, zextloadi16, addruimm12, II_LHU>,
		LSMem_U12_Enc<0b0110>;

def LBs9_NM : LoadMemoryNM<"lb", GPRNM32Opnd, mem_nms9, sextloadi8, addrsimm9, II_LB>,
		LSMem_S9_Enc<0b000, 0b0000>;
def SBs9_NM : StoreMemoryNM<"sb", GPRNM32Opnd, mem_nms9, truncstorei8, addrsimm9, II_SB>,
		LSMem_S9_Enc<0b000, 0b0001>;
def LHs9_NM : LoadMemoryNM<"lh", GPRNM32Opnd, mem_nms9, sextloadi16, addrsimm9, II_LH>,
		LSMem_S9_Enc<0b000, 0b0100>;
def SHs9_NM : StoreMemoryNM<"sh", GPRNM32Opnd, mem_nms9, truncstorei16, addrsimm9, II_SH>,
		LSMem_S9_Enc<0b000, 0b0101>;
def LWs9_NM : LoadMemoryNM<"lw", GPRNM32Opnd, mem_nms9, load, addrsimm9, II_LW>,
	      LSMem_S9_Enc<0b000, 0b1000>;
def SWs9_NM : StoreMemoryNM<"sw", GPRNM32Opnd, mem_nms9, store, addrsimm9, II_SW>,
	      LSMem_S9_Enc<0b000, 0b1001>;
def LBUs9_NM : LoadMemoryNM<"lbu", GPRNM32Opnd, mem_nms9, zextloadi8, addrsimm9, II_LBU>,
		LSMem_S9_Enc<0b000, 0b0010>;
def LHUs9_NM : LoadMemoryNM<"lhu", GPRNM32Opnd, mem_nms9, zextloadi16, addrsimm9, II_LHU>,
		LSMem_S9_Enc<0b000, 0b0110>;

def LBX_NM : LoadMemoryNM<"lbx", GPRNM32Opnd, mem_nmrx, sextloadi8, addrindexed, II_LB>,
		LSMemX_Enc<0, 0b0000>;
def SBX_NM : StoreMemoryNM<"sbx", GPRNM32Opnd, mem_nmrx, truncstorei8, addrindexed, II_SB>,
		LSMemX_Enc<0, 0b0001>;
def LHX_NM : LoadMemoryNM<"lhx", GPRNM32Opnd, mem_nmrx, sextloadi16, addrindexed, II_LH>,
		LSMemX_Enc<0, 0b0100>;
def SHX_NM : StoreMemoryNM<"shx", GPRNM32Opnd, mem_nmrx, truncstorei16, addrindexed, II_SH>,
		LSMemX_Enc<0, 0b0101>;
def LWX_NM : LoadMemoryNM<"lwx", GPRNM32Opnd, mem_nmrx, load, addrindexed, II_LW>,
		LSMemX_Enc<0, 0b1000>;
def SWX_NM : StoreMemoryNM<"swx", GPRNM32Opnd, mem_nmrx, store, addrindexed, II_SW>,
		LSMemX_Enc<0, 0b1001>;
def LBUX_NM : LoadMemoryNM<"lbux", GPRNM32Opnd, mem_nmrx, zextloadi8, addrindexed, II_LBU>,
		LSMemX_Enc<0, 0b0010>;
def LHUX_NM : LoadMemoryNM<"lhux", GPRNM32Opnd, mem_nmrx, zextloadi16, addrindexed, II_LHU>,
		LSMemX_Enc<0, 0b0110>;

def LHXS_NM : LoadMemoryNM<"lhxs", GPRNM32Opnd, mem_nmrx, sextloadi16, addrindexedlsl1, II_LH>,
		LSMemX_Enc<0b1, 0b0100>;
def SHXS_NM : StoreMemoryNM<"shxs", GPRNM32Opnd, mem_nmrx, truncstorei16, addrindexedlsl1, II_SH>,
		LSMemX_Enc<0b1, 0b0101>;
def LHUXS_NM : LoadMemoryNM<"lhuxs", GPRNM32Opnd, mem_nmrx, zextloadi16, addrindexedlsl1, II_LHU>,
		LSMemX_Enc<0b1, 0b0110>;
def LWXS16_NM : LoadAsmNM<"lwxs", GPRNM16R3Opnd, mem_nmrx16, load, II_LW>,
		_Pool_P16C<0b1> {
  bits<3> rt;
  // Uses same encoding method as LWXS to get 5+5 register bits
  // but only needs the lower 3 bits of each register.
  bits<10> addr;
  let Inst{9...7} = addr{7...5}; // Discard {9-8}
  let Inst{6...4} = addr{2...0}; // Discard {4-3}
  let Inst{3...1} = rt;
}
def LWXS_NM : LoadMemoryNM<"lwxs", GPRNM32Opnd, mem_nmrx, load, addrindexedlsl2, II_LW>,
		LSMemX_Enc<0b1, 0b1000>;
def SWXS_NM : StoreMemoryNM<"swxs", GPRNM32Opnd, mem_nmrx, store, addrindexedlsl2, II_SW>,
		LSMemX_Enc<0b1, 0b1001>;

def UALH_NM : UnalignedLoad<"ualh", NMUnalignedLH>, LSMem_S9_Enc<0b001, 0b0100>;
def UASH_NM : UnalignedStore<"uash", NMUnalignedSH>, LSMem_S9_Enc<0b001, 0b0101>;
def LWM_NM : LoadMultipleNM<"lwm">, LSMem_S9_Mult_Enc<0b0100>;
def UALWM_NM : LoadMultipleNM<"ualwm">, LSMem_S9_Mult_Enc<0b0101>;
def SWM_NM : StoreMultipleNM<"swm">, LSMem_S9_Mult_Enc<0b1100>;
def UASWM_NM : StoreMultipleNM<"uaswm">, LSMem_S9_Mult_Enc<0b1101>;

def UALW_NM :  UnalignedLoad<"ualw", NMUnalignedLW>, LSMem_S9_Enc<0b101, 0b0010>;
def UASW_NM : UnalignedStore<"uasw", NMUnalignedSW>, LSMem_S9_Enc<0b101, 0b0011>;

def SWPC_NM : StorePCBase<GPRNM32Opnd, sym32_pc_nm, addrsym>, Op48_Addr_Enc<0b01111>;
def LWPC_NM : LoadPCBase<GPRNM32Opnd, sym32_pc_nm, addrsym>, Op48_Addr_Enc<0b01011>;

def LW4x4_NM : LoadAsmNM<"lw", GPRNM4Opnd, mem_nm4s2, load, II_LW>,
	       LSMem4x4_Enc<0b01101>;
def SW4x4_NM : StoreAsmNM<"sw", GPRNM4ZOpnd, mem_nm4s2, store, II_SW>,
	       LSMem4x4_Enc<0b11101>;

class MOVE_Enc : _Pool_P16_MV
{
  bits<5> rt;
  bits<5> rs;
  let Inst{9...5} = rt;
  let Inst{4...0} = rs;
}

class MOVEP_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<2> dst1;
  bits<5> src1;
  bits<5> src2;

  let Inst{9} = src2{4};
  let Inst{7...5} = src2{2...0};
  let Inst{4} = src1{4};
  let Inst{2...0} = src1{2...0};
  let Inst{8} = dst1{0};
  let Inst{3} = dst1{1};
}

class MOVEPREV_Enc<bits<5> isel> : _Pool_P16<isel>
{
  bits<2> src1;
  bits<5> dst1;
  bits<5> dst2;

  let Inst{9} = dst2{4};
  let Inst{7...5} = dst2{2...0};
  let Inst{4} = dst1{4};
  let Inst{2...0} = dst1{2...0};
  let Inst{8} = src1{0};
  let Inst{3} = src1{1};
}

let isMoveReg = 1 in {
def MOVE_NM : InstNM<(outs GPRNM32NZOpnd:$rt), (ins GPRNM32Opnd:$rs),
                      "move\t$rt, $rs", []>, MOVE_Enc;

def MOVEP_NM : InstNM<(outs GPRNM2R1Opnd:$dst1, GPRNM2R2Opnd:$dst2),
		      (ins GPRNM4ZOpnd:$src1, GPRNM4ZOpnd:$src2),
		      "movep\t$dst1, $dst2, $src1, $src2", []>,
		      MOVEP_Enc<0b10111>, NoEncode<"$dst2">;

def MOVEPREV_NM : InstNM<(outs GPRNM4Opnd:$dst1, GPRNM4Opnd:$dst2),
			 (ins GPRNM2R1Opnd:$src1, GPRNM2R2Opnd:$src2),
			 "movep\t$dst1, $dst2, $src1, $src2", []>,
			 MOVEPREV_Enc<0b11111>, NoEncode<"$src2">;
}

class LoadExtend <SDPatternOperator OpNode, InstNM InstLoad,
		       SDPatternOperator ImmType, InstNM InstAdd> :
	NMPat<(i32 (OpNode (add GPRNM32:$rs, ImmType:$imm))),
	     (InstLoad (InstAdd GPRNM32:$rs, ImmType:$imm), 0)>;

class StoreExtend <SDPatternOperator OpNode, RegisterOperand RO,
		  InstNM InstStore, SDPatternOperator ImmType,
		  InstNM InstAdd> :
	NMPat<(OpNode RO:$rt, (add GPRNM32:$rs, ImmType:$imm)),
	     (InstStore RO:$rt, (InstAdd GPRNM32:$rs, imm:$imm), 0)>;

multiclass LoadStoreExtend_M<SDPatternOperator ImmType, InstNM InstAdd> {
  def : LoadExtend<extloadi8, LBU_NM, ImmType, InstAdd>;
  def : LoadExtend<zextloadi8, LBU_NM, ImmType, InstAdd>;
  def : LoadExtend<sextloadi8, LB_NM, ImmType, InstAdd>;
  def : LoadExtend<extloadi16, LHU_NM, ImmType, InstAdd>;
  def : LoadExtend<zextloadi16, LHU_NM, ImmType, InstAdd>;
  def : LoadExtend<sextloadi16, LH_NM, ImmType, InstAdd>;
  def : LoadExtend<load, LW_NM, ImmType, InstAdd>;
  def : StoreExtend<truncstorei8, GPRNM32Opnd, SB_NM, ImmType, InstAdd>;
  def : StoreExtend<truncstorei16, GPRNM32Opnd, SH_NM, ImmType, InstAdd>;
  def : StoreExtend<store, GPRNM32Opnd, SW_NM, ImmType, InstAdd>;
}

// These patterns help generate better loads/stores when the immediate doesn't
// fit and ends up being and indexed load/store. For example:
//
//  li $a0, -300        ->  addiu $a0, $a2, -300
//  lbux $a1, $a0($a2)  ->  lbu $a1, 0($a0)
//
// On the left is what would be generated without these patterns. This results
// in two 32-bit instructions. While on the right, we have one 32-bit and one
// 16-bit instruction.
//

let AddedComplexity = 3 in {
  defm : LoadStoreExtend_M<immFitsAddiu32, ADDIU_NM>;
  defm : LoadStoreExtend_M<immFitsAddiuNeg, ADDIUNEG_NM>;
}

// Catching out-of-range immediate loads/stores.
def : NMPat<(i32 (extloadi8 GPRNM32:$rs)), (LBU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (zextloadi8 GPRNM32:$rs)), (LBU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (sextloadi8 GPRNM32:$rs)), (LB_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (extloadi16 GPRNM32:$rs)), (LHU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (zextloadi16 GPRNM32:$rs)), (LHU_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (sextloadi16 GPRNM32:$rs)), (LH_NM GPRNM32:$rs, 0)>;
def : NMPat<(i32 (load GPRNM32:$rs)), (LW_NM GPRNM32:$rs, 0)>;
def : NMPat<(truncstorei8 GPRNM32:$rt, GPRNM32:$rs),
            (SB_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(truncstorei16 GPRNM32:$rt, GPRNM32:$rs),
            (SH_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(store GPRNM32:$rt, GPRNM32:$rs),
            (SW_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;

// Any-extending loads
def : NMPat<(i32 (extloadi8 addruimm12:$addr)), (LBU_NM addruimm12:$addr)>;
def : NMPat<(i32 (extloadi8 addrsimm9:$addr)), (LBUs9_NM addrsimm9:$addr)>;
def : NMPat<(i32 (extloadi8 addrindexed:$addr)), (LBUX_NM addrindexed:$addr)>;
def : NMPat<(i32 (extloadi16 addruimm12:$addr)), (LHU_NM addruimm12:$addr)>;
def : NMPat<(i32 (extloadi16 addrsimm9:$addr)), (LHUs9_NM addrsimm9:$addr)>;
def : NMPat<(i32 (extloadi16 addrindexed:$addr)), (LHUX_NM addrindexed:$addr)>;

// Load/store scaled patterns
let AddedComplexity = 1 in {
  def : NMPat<(i32 (zextloadi16 addrindexedlsl1:$addr)),
            (LHUXS_NM addrindexedlsl1:$addr)>;
  def : NMPat<(i32 (extloadi16 addrindexedlsl1:$addr)),
            (LHUXS_NM addrindexedlsl1:$addr)>;
  def : NMPat<(sextloadi16 addrindexedlsl1:$addr),
             (LHXS_NM addrindexedlsl1:$addr)>;
  def : NMPat<(load addrindexedlsl2:$addr), (LWXS_NM addrindexedlsl2:$addr)>;
  def : NMPat<(truncstorei16 GPRNM32:$rt, addrindexedlsl1:$addr),
            (SHXS_NM GPRNM32:$rt, addrindexedlsl1:$addr)>;
  def : NMPat<(store GPRNM32:$rt, addrindexedlsl2:$addr),
            (SWXS_NM GPRNM32:$rt, addrindexedlsl2:$addr)>;
}

// Catching out-of-range immediate unaligned loads/stores.
def : NMPat<(i32 (NMUnalignedLW GPRNM32:$rs, GPRNM32:$src)),
            (UALW_NM GPRNM32:$rs, 0, GPRNM32:$src)>;
def : NMPat<(NMUnalignedSW GPRNM32:$rt, GPRNM32:$rs),
            (UASW_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;
def : NMPat<(i32 (NMUnalignedLH GPRNM32:$rs, GPRNM32:$src)),
            (UALH_NM GPRNM32:$rs, 0, GPRNM32:$src)>;
def : NMPat<(NMUnalignedSH GPRNM32:$rt, GPRNM32:$rs),
            (UASH_NM GPRNM32:$rt, GPRNM32:$rs, 0)>;

// GP-relative load.
def : NMPat<(i32 (load (add GP_NM, (MipsGPRel tglobaladdr:$in)))),
            (LWGP_NM GP_NM, tglobaladdr:$in)>;
def : NMPat<(add GP_NM, (MipsGPRel tglobaladdr:$in)),
            (ADDIU_NM GP_NM, tglobaladdr:$in)>;

// Atomic load patterns.
def : NMPat<(atomic_load_8 addr:$a), (LB_NM addr:$a)>;
def : NMPat<(atomic_load_16 addr:$a), (LH_NM addr:$a)>;
def : NMPat<(atomic_load_32 addr:$a), (LW_NM addr:$a)>;

// Atomic store patterns.
def : NMPat<(atomic_store_8 addr:$a, GPR32:$v), (SB_NM GPR32:$v, addr:$a)>;
def : NMPat<(atomic_store_16 addr:$a, GPR32:$v), (SH_NM GPR32:$v, addr:$a)>;
def : NMPat<(atomic_store_32 addr:$a, GPR32:$v), (SW_NM GPR32:$v, addr:$a)>;

class LLSC_Enc<bits<4> psel, bits<2> isel> : _Pool_P_LS_S1<psel> {
  bits<5> rt;
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  let Inst{25...21} = rt;
  let Inst{20...16} = base;
  let Inst{15} = offset{8};
  let Inst{7...2} = offset{7...2};
  let Inst{1...0} = isel;
}

class LLSCWP_Enc<bits<4> psel, bits<2> isel> : _Pool_P_LS_S1<psel> {
  bits<5> rt;
  bits<5> ru;
  bits<5> addr;
  let Inst{25...21} = rt;
  let Inst{20...16} = addr;
  let Inst{7...3} = ru;
  let Inst{1...0} = isel;
}

def LL_NM : LoadMemoryNM<"ll", GPRNM32Opnd, mem_nm7s2, load, addrsimm9, II_LL>,
		LLSC_Enc<0b1010, 0b00> {
  let hasNoSchedulingInfo = 1;
}

def SC_NM : InstNM<(outs GPRNM32Opnd:$dst), (ins GPRNM32Opnd:$rt, mem_nm7s2:$addr),
		"sc\t$rt, $addr", []>, RegConstraint<"$rt = $dst">,
		LLSC_Enc<0b1011, 0b00> {
  let mayStore = 1;
  let hasNoSchedulingInfo = 1;
}

def LLWP_NM : InstNM<(outs GPRNM32Opnd:$rt, GPRNM32Opnd:$ru), (ins mem_nm0:$addr),
		     "llwp $rt, $ru, $addr", [], II_LL>,
	      LLSCWP_Enc<0b1010, 0b01> {
  let canFoldAsLoad = 1;
  let mayLoad = 1;
  string BaseOpcode = "llwp";
  let hasNoSchedulingInfo = 1;
}

def SCWP_NM : InstNM<(outs GPRNM32Opnd:$dst), (ins GPRNM32Opnd:$rt, GPRNM32Opnd:$ru, mem_nm0:$addr),
		     "scwp $rt, $ru, $addr", [], II_SC>,
	      RegConstraint<"$dst = $rt">,
	      LLSCWP_Enc<0b1011, 0b01> {
  let mayStore = 1;
  string BaseOpcode = "scwp";
  let hasNoSchedulingInfo = 1;
}

// Conditional Moves
class CondMove_Enc<bits<1> isel> : _Pool_P_CMOVE<isel>
{
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
}

def MOVZ_NM : InstNM<(outs GPRNM32Opnd:$rd),
                     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, GPRNM32Opnd:$F),
                     "movz\t$rd, $rs, $rt",
                     []>, CondMove_Enc<0b0>, RegConstraint<"$F = $rd">;

def MOVN_NM : InstNM<(outs GPRNM32Opnd:$rd),
                     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, GPRNM32Opnd:$F),
                     "movn\t$rd, $rs, $rt",
                     []>, CondMove_Enc<0b1>, RegConstraint<"$F = $rd">;

multiclass MovzPats2NM<RegisterClass CRC, RegisterClass DRC,
                     Instruction MOVZInst, Instruction XORiOp> {
  def : NMPat<(select (i32 (seteq CRC:$lhs, imm32ZExt12:$uimm12_nm)), DRC:$T,
                      DRC:$F),
              (MOVZInst DRC:$T, (XORiOp CRC:$lhs, imm32ZExt12:$uimm12_nm),
                        DRC:$F)>;
}

multiclass MovzPats0NM<RegisterClass CRC, RegisterClass DRC,
                       Instruction MOVZInst, Instruction SLTOp,
                       Instruction SLTuOp, Instruction SLTiOp,
                       Instruction SLTiuOp> {
  def : NMPat<(select (i32 (setge CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTOp CRC:$lhs, CRC:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setuge CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTuOp CRC:$lhs, CRC:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setge CRC:$lhs, imm32ZExt12:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiOp CRC:$lhs, imm32ZExt12:$rhs), DRC:$F)>;
  def : NMPat<(select (i32 (setuge CRC:$lh, imm32ZExt12:$rh)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiuOp CRC:$lh, imm32ZExt12:$rh), DRC:$F)>;
  def : NMPat<(select (i32 (setle CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTOp CRC:$rhs, CRC:$lhs), DRC:$F)>;
  def : NMPat<(select (i32 (setule CRC:$lhs, CRC:$rhs)), DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTuOp CRC:$rhs, CRC:$lhs), DRC:$F)>;
  def : NMPat<(select (i32 (setgt CRC:$lhs, immZExt12Plus1:$rhs)),
                      DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiOp CRC:$lhs, (Plus1 imm:$rhs)), DRC:$F)>;
  def : NMPat<(select (i32 (setugt CRC:$lhs, immZExt12Plus1:$rhs)),
                      DRC:$T, DRC:$F),
              (MOVZInst DRC:$T, (SLTiuOp CRC:$lhs, (Plus1 imm:$rhs)), DRC:$F)>;
}

// Use MIPS conditional instruction definitions
defm : MovzPats0NM<GPRNM32, GPRNM32, MOVZ_NM, SLT_NM, SLTU_NM, SLTI_NM,
                   SLTIU_NM>;


defm : MovzPats1<GPRNM32, GPRNM32, MOVZ_NM, XOR_NM>, ISA_NANOMIPS;
defm : MovzPats2NM<GPRNM32, GPRNM32, MOVZ_NM, XORI_NM>;
defm : MovnPats<GPRNM32, GPRNM32, MOVN_NM, XOR_NM>, ISA_NANOMIPS;

class ToggleInterrupts_Enc<bits<5> isel> : _Pool_POOL32Axf_5_group1<isel> {
  bits<5> rt;
  let Inst{25...21} = rt;
}

def DI_NM : InstNM<(outs GPRNM32Opnd:$rt), (ins),
                   "di\t$rt", []>, ToggleInterrupts_Enc<0b00011>;

def EI_NM : InstNM<(outs GPRNM32Opnd:$rt), (ins),
                   "ei\t$rt", []>, ToggleInterrupts_Enc<0b01011>;

def : MipsInstAlias<"ei", (EI_NM ZERO_NM)>, ISA_NANOMIPS;
def : MipsInstAlias<"di", (DI_NM ZERO_NM)>, ISA_NANOMIPS;

// Instruction with no operands
class MnemonicOnly<string opstr>:
    InstNM<(outs), (ins), opstr, []>;

def ERET_NM : MnemonicOnly<"eret">, _Pool_POOL32Axf_5_group3<0b11001> {
  let Inst{16} = 0b0;
}

def ERETNC_NM : MnemonicOnly<"eretnc">, _Pool_POOL32Axf_5_group3<0b11001> {
  let Inst{16} = 0b1;
}

def DERET_NM : MnemonicOnly<"deret">, _Pool_POOL32Axf_5_group3<0b10001>;

class SLL_VAR_Enc<bits<5> sh> : _Pool_P_SHIFT<0b0000> {
  let Inst{25...21} = 0b00000;
  let Inst{20...16} = 0b00000;
  let Inst{4...0} = sh;
}

def NOP32_NM : MnemonicOnly<"nop32">, SLL_VAR_Enc<0>;

def EHB_NM : MnemonicOnly<"ehb">, SLL_VAR_Enc<3>;

def PAUSE_NM : MnemonicOnly<"pause">, SLL_VAR_Enc<5>;

def NOP_NM : MnemonicOnly<"nop">, _Pool_P16_A2<0b1> {
  let Inst{9...5} = 0b00000;
  let Inst{4} = 0b0;
  let Inst{2...0} = 0b000;
}

class IntExcpFlow_Enc<bits<2> isel> : _Pool_P_RI<isel>
{
  bits<19> imm;
  let Inst{18...0} = imm;
}

class IntExcpFlow16_Enc<bits<2> isel> : _Pool_P16_RI<isel>
{
  bits<3> imm;
  let Inst{2...0} = imm;
}

def SIGRIE_NM : InstNM<(outs), (ins uimm19_nm:$imm),
                       "sigrie\t$imm", []>, IntExcpFlow_Enc<0b00>;

def BREAK_NM : InstNM<(outs), (ins uimm19_nm:$imm),
                      "break\t$imm", []>, IntExcpFlow_Enc<0b10>;

def SDBBP_NM : InstNM<(outs), (ins uimm19_nm:$imm),
                      "sdbbp\t$imm", []>, IntExcpFlow_Enc<0b11>;


def SYSCALL_NM : InstNM<(outs), (ins uimm18_nm:$imm),
                       "syscall\t$imm", []>, IntExcpFlow_Enc<0b01> {
  let Inst{18} = 0b0;
}

def BREAK16_NM : InstNM<(outs), (ins uimm3_nm:$imm),
                      "break\t$imm", []>, IntExcpFlow16_Enc<0b10>;
def : NanoMipsInstAlias<"break", (BREAK16_NM 0), 0>;

def SDBBP16_NM : InstNM<(outs), (ins uimm3_nm:$imm),
                      "sdbbp\t$imm", []>, IntExcpFlow16_Enc<0b11>;
def : NanoMipsInstAlias<"sdbbp", (SDBBP16_NM 0), 0>;

def SYSCALL16_NM : InstNM<(outs), (ins uimm2_nm:$imm),
                       "syscall\t$imm", []>, IntExcpFlow16_Enc<0b01> {
  let Inst{2} = 0b0;
}
def : NanoMipsInstAlias<"syscall", (SYSCALL16_NM 0), 0>;


def : NMPat<(trap), (BREAK_NM 0)>;

class ShadowRW_Enc<bits<5> isel>: _Pool_POOL32Axf_5_group3<isel> {
  bits<5> rs;
  bits<5> rt;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
}

def WRPGPR_NM : InstNM<(outs GPRNM32Opnd:$rt), (ins GPRNM32Opnd:$rs),
                       "wrpgpr\t$rt, $rs", []>, ShadowRW_Enc<0b11000>;

def RDPGPR_NM : InstNM<(outs GPRNM32Opnd:$rt), (ins GPRNM32Opnd:$rs),
                       "rdpgpr\t$rt, $rs", []>, ShadowRW_Enc<0b10000>;

def EXTW_NM : InstNM<(outs GPRNM32Opnd:$rd),
		     (ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, uimm5:$shift),
		     "extw\t$rd, $rs, $rt, $shift", []>,
		     _Pool_POOL32A7<0b011> {
  bits<5> rd;
  bits<5> rs;
  bits<5> rt;
  bits<5> shift;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{15...11} = rd;
  let Inst{10...6} = shift;
}

def ALIGN_NM : NanoMipsAsmPseudoInst<(outs GPRNM32Opnd:$rd),
			(ins GPRNM32Opnd:$rs, GPRNM32Opnd:$rt, uimm2_nm:$bp),
			"align\t$rd, $rs, $rt, $bp">;

class ROTX_Enc : _Pool_P_U12<0b1101> {
  bits<5> rt;
  bits<5> rs;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{11} = 0;
  let Inst{5} = 0;
}

class RotxAlias<string op, SDPatternOperator OpNode, bits<5> shift,
			bits<5> shiftx, bit stripe> :
      InstNM<(outs GPRNM32Opnd:$rt), (ins GPRNM32Opnd:$rs),
		     !strconcat(op, "\t$rt, $rs"),
		     [(set GPRNM32Opnd:$rt, (OpNode GPRNM32Opnd:$rs))]>,
		     ROTX_Enc {
  let Inst{10...7} = shiftx{4...1};
  let Inst{4...0} = shift;
  let Inst{6} = stripe;
}

def BITREVW_NM : RotxAlias<"bitrevw", bitreverse, 31, 0, 0>;
def BYTEREVW_NM : RotxAlias<"byterevw", bswap, 24, 8, 0>;

def ROTX_NM : InstNM<(outs GPRNM32Opnd:$rt),
		     (ins GPRNM32Opnd:$rs, uimm5:$shift,
		     uimm5s1_nm:$shiftx, uimm1:$stripe),
		     "rotx\t$rt, $rs, $shift, $shiftx, $stripe", []>,
		     ROTX_Enc {
  bits<5> shift;
  bits<5> shiftx;
  bits<1> stripe;
  let Inst{10...7} = shiftx{4...1};
  let Inst{4...0} = shift;
  let Inst{6} = stripe;
}

def BITREVB_NM : MipsInstAlias<"bitrevb $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 7, 8, 1)>,
				ISA_NANOMIPS;
def BITREVH_NM : MipsInstAlias<"bitrevh $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 15, 16, 0)>,
				ISA_NANOMIPS;
def BYTEREVH_NM : MipsInstAlias<"byterevh $rt, $rs",
				(ROTX_NM GPRNM32Opnd:$rt, GPRNM32Opnd:$rs, 8, 24, 0)>,
				ISA_NANOMIPS;

class CacheOpNM<string opstr, Operand Od, DAGOperand MO,
		    ComplexPattern Addr = addr,
		    InstrItinClass itin = NoItinerary> :
    InstNM<(outs), (ins Od:$op, MO:$addr), !strconcat(opstr, "\t$op, $addr"),
	   [], itin>;
def CACHE_NM : CacheOpNM<"cache", uimm5, mem_nms9, addrsimm9, II_CACHE>,
	       _Pool_P_LS_S1<0b0111> {
  bits<5> op;
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  let Inst{25...21} = op;
  let Inst{20...16} = base;
  let Inst{15} = offset{8};
  let Inst{7...0} = offset{7...0};
}

def PREFs9_NM : CacheOpNM<"pref", uimm5, mem_nms9, addrsimm9, II_PREF>,
	       _Pool_P_LS_S0<0b0011> {
  bits<5> op;
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  let Inst{25...21} = op;
  let Inst{20...16} = base;
  let Inst{15} = offset{8};
  let Inst{7...0} = offset{7...0};
}

def PREF_NM : CacheOpNM<"pref", uimm5, mem_nmu12, addruimm12, II_PREF>,
	       _Pool_P_LS_U12<0b0011> {
  bits<5> op;
  bits<17> addr;
  bits<5> base = addr{16...12};
 bits<12> offset = addr{11...0};
  let Inst{25...21} = op;
  let Inst{20...16} = base;
  let Inst{11...0} = offset{11...0};
}

class SyncOpNM<string opstr, DAGOperand MO,
		ComplexPattern Addr = addr,
		InstrItinClass itin = NoItinerary> :
    InstNM<(outs), (ins MO:$addr), !strconcat(opstr, "\t$addr"),
	   [], itin>;

def SYNCIs9_NM : SyncOpNM<"synci", mem_nms9, addrsimm9, II_SYNCI>,
	       _Pool_P_LS_S0<0b0011> {
  bits<14> addr;
  bits<5> base = addr{13...9};
  bits<9> offset = addr{8...0};
  let Inst{25...21} = 0b11111;
  let Inst{20...16} = base;
  let Inst{15} = offset{8};
  let Inst{7...0} = offset{7...0};
}

def SYNCI_NM : SyncOpNM<"synci", mem_nmu12, addruimm12, II_SYNCI>,
	       _Pool_P_LS_U12<0b0011> {
  bits<17> addr;
  bits<5> base = addr{16...12};
  bits<12> offset = addr{11...0};
  let Inst{25...21} = 0b11111;
  let Inst{20...16} = base;
  let Inst{11...0} = offset;
}

def WAIT_NM : InstNM<(outs), (ins uimm10:$cd),
		       "wait\t$cd", [], II_WAIT>,
		       _Pool_POOL32Axf_5_group3<0b00001> {
  bits<10> cd;
  let Inst{25...16} = cd;
}
def : MipsInstAlias<"wait", (WAIT_NM 0)>, ISA_NANOMIPS;

def SYNC_NM : InstNM<(outs), (ins uimm5:$stype), "sync\t$stype",
	   [], II_SYNC>, _Pool_P_SHIFT<0b0000> {
  bits<5> stype;
  let Inst{25...21} = 0b00000;
  let Inst{20...16} = stype;
  let Inst{4...0} = 0b00110;
}

def : MipsInstAlias<"sync", (SYNC_NM 0)>, ISA_NANOMIPS;
def : MipsInstAlias<"sync_wmb", (SYNC_NM 0x4)>, ISA_NANOMIPS;
def : MipsInstAlias<"sync_mb", (SYNC_NM 0x10)>, ISA_NANOMIPS;
def : MipsInstAlias<"sync_acquire", (SYNC_NM 0x11)>, ISA_NANOMIPS;
def : MipsInstAlias<"sync_release", (SYNC_NM 0x12)>, ISA_NANOMIPS;
def : MipsInstAlias<"sync_rmb", (SYNC_NM 0x13)>, ISA_NANOMIPS;

class WriteToCP0<string opstr, RegisterOperand RO, RegisterOperand CO> :
  InstNM<(outs), (ins RO:$rt, CO:$c0s, uimm5:$sel),
  !strconcat(opstr, "\t$rt, $c0s, $sel"), []>;

class WriteToCP0Sel<string opstr, RegisterOperand RO, RegisterOperand CO> :
  InstNM<(outs), (ins RO:$rt, CO:$c0s),
  !strconcat(opstr, "\t$rt, $c0s"), []>;

class ReadFromCP0<string opstr, RegisterOperand RO, RegisterOperand CO> :
  InstNM<(outs RO:$rt), (ins CO:$c0s, uimm5:$sel),
  !strconcat(opstr, "\t$rt, $c0s, $sel"), []>;

class ReadFromCP0Sel<string opstr, RegisterOperand RO, RegisterOperand CO> :
  InstNM<(outs RO:$rt), (ins CO:$c0s),
  !strconcat(opstr, "\t$rt, $c0s"), []>;

class ReadWriteCP0_Enc<bits<6> isel> : _Pool_POOL32A0_1<isel> {
  bits<5> rt;
  bits<5> c0s;
  bits<5> sel;
  let Inst{25...21} = rt;
  let Inst{20...16} = c0s;
  let Inst{15...11} = sel;
}

class ReadWriteCP0Sel_Enc<bits<6> isel> : _Pool_POOL32A0_1<isel> {
  bits<5> rt;
  bits<10> c0s;
  let Inst{25...21} = rt;
  let Inst{20...11} = c0s;
}

def MFC0Sel_NM  : ReadFromCP0Sel<"mfc0", GPRNM32Opnd, COP0SelOpnd>, ReadWriteCP0Sel_Enc<0b000010>;
def MFHC0Sel_NM : ReadFromCP0Sel<"mfhc0", GPRNM32Opnd, COP0SelOpnd>, ReadWriteCP0Sel_Enc<0b000011>;
def MTC0Sel_NM  : WriteToCP0Sel<"mtc0", GPRNM32Opnd, COP0SelOpnd>, ReadWriteCP0Sel_Enc<0b000110>;
def MTHC0Sel_NM : WriteToCP0Sel<"mthc0", GPRNM32Opnd, COP0SelOpnd>, ReadWriteCP0Sel_Enc<0b000111>;

let isAsmParserOnly = 1 in {
  def MFC0_NM  : ReadFromCP0<"mfc0", GPRNM32Opnd, COP0Opnd>, ReadWriteCP0_Enc<0b000010>;
  def MFHC0_NM : ReadFromCP0<"mfhc0", GPRNM32Opnd, COP0Opnd>, ReadWriteCP0_Enc<0b000011>;
  def MTC0_NM  : WriteToCP0<"mtc0", GPRNM32Opnd, COP0Opnd>, ReadWriteCP0_Enc<0b000110>;
  def MTHC0_NM : WriteToCP0<"mthc0", GPRNM32Opnd, COP0Opnd>, ReadWriteCP0_Enc<0b000111>;
}

def : MipsInstAlias<"mfc0 $rt, $c0s", (MFC0_NM GPRNM32Opnd:$rt, COP0Opnd:$c0s, 0)>, ISA_NANOMIPS;
def : MipsInstAlias<"mfhc0 $rt, $c0s", (MFHC0_NM GPRNM32Opnd:$rt, COP0Opnd:$c0s, 0)>, ISA_NANOMIPS;
def : MipsInstAlias<"mtc0 $rt, $c0s", (MTC0_NM GPRNM32Opnd:$rt, COP0Opnd:$c0s, 0)>, ISA_NANOMIPS;
def : MipsInstAlias<"mthc0 $rt, $c0s", (MTHC0_NM GPRNM32Opnd:$rt, COP0Opnd:$c0s, 0)>, ISA_NANOMIPS;

def GINVI_NM : InstNM<(outs), (ins GPRNM32Opnd:$rs), "ginvi\t$rs", []>,
		_Pool_POOL32Axf_5_group0<0b01111>, ASE_GINV {
  bits<5> rs;
  let Inst{20...16} = rs;
}

def GINVT_NM : InstNM<(outs), (ins GPRNM32Opnd:$rs, uimm2:$type),
		"ginvt\t$rs, $type", []>,
		_Pool_POOL32Axf_5_group0<0b00111>, ASE_GINV {
  bits<5> rs;
  bits<2> type;
  let Inst{22...21} = type;
  let Inst{20...16} = rs;
}

class CRC_Enc<bits<3> isel> : _Pool_POOL32A0_1<0b111101> {
  bits<5> rt;
  bits<5> rs;
  let Inst{25...21} = rt;
  let Inst{20...16} = rs;
  let Inst{12...10} = isel;
}

class CRC_Op<string opstr> : InstNM<(outs GPRNM32Opnd:$rt),
			     (ins GPRNM32Opnd:$val, GPRNM32Opnd:$rs),
      !strconcat(opstr, "\t$rt, $rs"), []> {
  let Constraints = "$val = $rt";
}

def CRC32B_NM : CRC_Op<"crc32b">, CRC_Enc<0b000>, ASE_CRC;
def CRC32H_NM : CRC_Op<"crc32h">, CRC_Enc<0b001>, ASE_CRC;
def CRC32W_NM : CRC_Op<"crc32w">, CRC_Enc<0b010>, ASE_CRC;
def CRC32CB_NM : CRC_Op<"crc32cb">, CRC_Enc<0b100>, ASE_CRC;
def CRC32CH_NM : CRC_Op<"crc32ch">, CRC_Enc<0b101>, ASE_CRC;
def CRC32CW_NM : CRC_Op<"crc32cw">, CRC_Enc<0b110>, ASE_CRC;

def TLBINV_NM : MnemonicOnly<"tlbinv">, _Pool_POOL32Axf_5_group0<0b00011>, ASE_TLB;
def TLBINVF_NM : MnemonicOnly<"tlbinvf">, _Pool_POOL32Axf_5_group0<0b01011>, ASE_TLB;
def TLBP_NM : MnemonicOnly<"tlbp">, _Pool_POOL32Axf_5_group0<0b00001>, ASE_TLB;
def TLBR_NM : MnemonicOnly<"tlbr">, _Pool_POOL32Axf_5_group0<0b01001>, ASE_TLB;
def TLBWI_NM : MnemonicOnly<"tlbwi">, _Pool_POOL32Axf_5_group0<0b10001>, ASE_TLB;
def TLBWR_NM : MnemonicOnly<"tlbwr">, _Pool_POOL32Axf_5_group0<0b11001>, ASE_TLB;
