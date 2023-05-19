// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// Definitions for the PowerPC Paired Single instructions.
//
// Originally these were created by DarkKirb in https://reviews.llvm.org/D85137.
// terorie extended these defintions in https://github.com/capstone-engine/capstone/pull/1898


#ifndef INCLUDED_CAPSTONE_PS_FORMATS
#ifdef CAPSTONE_PS_FORMATS
#define INCLUDED_CAPSTONE_PS_FORMATS

def PPCDispRID12Operand : AsmOperandClass {
  let Name = "DispRID12"; let PredicateMethod = "isS12Imm";
  let RenderMethod = "addImmOperands";
}

def dispRID12 : Operand<iPTR> {
  let ParserMatchClass = PPCDispRID12Operand;
  let OperandType = "OPERAND_IMMEDIATE";
}

def memrid12 : Operand<iPTR> {  // Paired Single displacement where imm is 12 bits.
  let PrintMethod = "printMemRegImm";
  let MIOperandInfo = (ops dispRID12:$imm, ptr_rc_nor0:$reg);
  let OperandType = "OPERAND_MEMORY";
}

// PSForm_qd - Undocumented paired-singles quantized load/store form direct.
class PSForm_qd<bits<6> op, dag OOL, dag IOL, string asmstr,
                InstrItinClass itin>
    : I<op, OOL, IOL, asmstr, itin> {
  bits<5> FRT;
  bit W;
  bits<3> I;
  bits<12> d;
  bits<5> A;

  let Inst{6-10}  = FRT;
  let Inst{11-15} = A;
  let Inst{16}    = W;
  let Inst{17-19} = I;
  let Inst{20-31} = d;
}

// PSForm_qi - Undocumented paired-singles quantized load/store form indexed.
class PSForm_qi<bits<6> psqop, dag OOL, dag IOL, string asmstr,
                InstrItinClass itin>
    : I<4, OOL, IOL, asmstr, itin> {
  bits<5> FRT;
  bits<5> A;
  bits<5> B;
  bit W;
  bits<3> I;

  let Inst{6-10}  = FRT;
  let Inst{11-15} = A;
  let Inst{16-20} = B;
  let Inst{21}    = W;
  let Inst{22-24} = I;
  let Inst{25-30} = psqop;
  let Inst{31}    = 0;
}

// PSForm_x - Undocumented paired-singles operation base form, short opcode.
class PSForm_x<bits<5> psxop, dag OOL, dag IOL, string asmstr,
               InstrItinClass itin>
    : I<4, OOL, IOL, asmstr, itin> {
  bits<5> FRT;
  bits<5> FRA;
  bits<5> FRB;
  bits<5> FRC;
  bit RC = 0; // set by isRecordForm

  let Inst{6-10}  = FRT;
  let Inst{11-15} = FRA;
  let Inst{16-20} = FRB;
  let Inst{21-25} = FRC;
  let Inst{26-30} = psxop;
  let Inst{31}    = RC;
}

// PSForm_y - Undocumented paired-singles operation base form, long opcode.
class PSForm_y<bits<10> psyop, dag OOL, dag IOL, string asmstr,
               InstrItinClass itin>
    : I<4, OOL, IOL, asmstr, itin> {
  bits<5> FRT;
  bits<5> FRA;
  bits<5> FRB;
  bit RC = 0; // set by isRecordForm

  let Inst{6-10}  = FRT;
  let Inst{11-15} = FRA;
  let Inst{16-20} = FRB;
  let Inst{21-30} = psyop;
  let Inst{31}    = RC;
}

// PSForm_c - Undocumented paired-singles compare form.
class PSForm_c<bits<10> pszop, dag OOL, dag IOL, string asmstr,
               InstrItinClass itin>
    : I<4, OOL, IOL, asmstr, itin> {
  bits<3> BF;
  bits<5> FRA;
  bits<5> FRB;

  let Inst{6-8}   = BF;
  let Inst{9-10}  = 0;
  let Inst{11-15} = FRA;
  let Inst{16-20} = FRB;
  let Inst{21-30} = pszop;
  let Inst{31}    = 0;
}

// Undocumented dcbz_l instruction.
class DCBZL_Form<bits<10> xop, dag OOL, dag IOL, string asmstr,
                 InstrItinClass itin>
    : I<4, OOL, IOL, asmstr, itin> {
  bits<5> A;
  bits<5> B;

  let Inst{6-10}  = 0;
  let Inst{11-15} = A;
  let Inst{16-20} = B;
  let Inst{21-30} = xop;
  let Inst{31}    = 0;
}

#endif // INCLUDED_CAPSTONE_PS_FORMATS
#endif // CAPSTONE_PS_FORMATS

#ifndef INCLUDED_CAPSTONE_PS_INSTR
#ifdef CAPSTONE_PS_INSTR
#define INCLUDED_CAPSTONE_PS_INSTR

let Predicates = [HasPS] in {
let DecoderNamespace = "PS" in {

let mayLoad = 1 in {
def PSQ_L    : PSForm_qd<56,
               (outs f8rc:$FRT), (ins memrid12:$src, u1imm:$W, u3imm: $I),
               "psq_l $FRT, $src, $W, $I", IIC_FPGeneral>;
def PSQ_LU   : PSForm_qd<57,
               (outs f8rc:$FRT), (ins memrid12:$src, u1imm:$W, u3imm: $I),
               "psq_lu $FRT, $src, $W, $I", IIC_FPGeneral>;
def PSQ_LX   : PSForm_qi<6,
               (outs f8rc:$FRT), (ins gprc:$rA, gprc:$rB, u1imm:$W, u3imm: $I),
               "psq_lx $FRT, $rA, $rB, $W, $I", IIC_FPGeneral>;
def PSQ_LUX  : PSForm_qi<38,
               (outs f8rc:$FRT), (ins gprc:$rA, gprc:$rB, u1imm:$W, u3imm: $I),
               "psq_lux $FRT, $rA, $rB, $W, $I", IIC_FPGeneral>;
}

let mayStore = 1 in {
def PSQ_ST   : PSForm_qd<60,
               (outs), (ins f8rc:$FRT, memrid12:$dst, u1imm:$W, u3imm: $I),
               "psq_st $FRT, $dst, $W, $I", IIC_FPGeneral>;
def PSQ_STU  : PSForm_qd<61,
               (outs), (ins f8rc:$FRT, memrid12:$dst, u1imm:$W, u3imm: $I),
               "psq_stu $FRT, $dst, $W, $I", IIC_FPGeneral>;
def PSQ_STX  : PSForm_qi<7,
               (outs), (ins f8rc:$FRT,gprc:$rA, gprc:$rB, u1imm:$W, u3imm: $I),
               "psq_stx $FRT, $rA, $rB, $W, $I", IIC_FPGeneral>;
def PSQ_STUX : PSForm_qi<39,
               (outs), (ins f8rc:$FRT,gprc:$rA, gprc:$rB, u1imm:$W, u3imm: $I),
               "psq_stux $FRT, $rA, $rB, $W, $I", IIC_FPGeneral>;
}

// op. FRT, FRA, FRC, FRB
multiclass PSForm_xr<bits<5> psxop, dag OOL, dag IOL, string asmbase,
                     string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_x<psxop, OOL, IOL,
                        !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_x<psxop, OOL, IOL,
                        !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                        isRecordForm;
  }
}

// op FRT, FRA, FRB
class PSForm_x1<bits<5> psxop, dag OOL, dag IOL, string asmstr,
                 InstrItinClass itin>
    : PSForm_x<psxop, OOL, IOL, asmstr, itin> {
  let FRC = 0;
}

// op. FRT, FRA, FRB
multiclass PSForm_x1r<bits<5> psxop, dag OOL, dag IOL, string asmbase,
                      string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_x1<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_x1<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                         isRecordForm;
  }
}

// op FRT, FRB
class PSForm_x2<bits<5> psxop, dag OOL, dag IOL, string asmstr,
                 InstrItinClass itin>
    : PSForm_x<psxop, OOL, IOL, asmstr, itin> {
  let FRA = 0;
  let FRC = 0;
}

// op. FRT, FRB
multiclass PSForm_x2r<bits<5> psxop, dag OOL, dag IOL, string asmbase,
                      string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_x2<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_x2<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                         isRecordForm;
  }
}

// op FRT, FRA, FRC
class PSForm_x3<bits<5> psxop, dag OOL, dag IOL, string asmstr,
                 InstrItinClass itin>
    : PSForm_x<psxop, OOL, IOL, asmstr, itin> {
  let FRB = 0;
}

// op. FRT, FRA, FRC
multiclass PSForm_x3r<bits<5> psxop, dag OOL, dag IOL, string asmbase,
                      string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_x3<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_x3<psxop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                         isRecordForm;
  }
}

// op. FRT, FRA, FRB
multiclass PSForm_yr<bits<10> psyop, dag OOL, dag IOL, string asmbase,
                     string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_y<psyop, OOL, IOL,
                        !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_y<psyop, OOL, IOL,
                        !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                        isRecordForm;
  }
}

// op FRT, FRA, FRB
class PSForm_y2<bits<10> psyop, dag OOL, dag IOL, string asmstr,
                 InstrItinClass itin>
    : PSForm_y<psyop, OOL, IOL, asmstr, itin> {
  let FRA = 0;
}

// op. FRT, FRB
multiclass PSForm_y2r<bits<10> psyop, dag OOL, dag IOL, string asmbase,
                      string asmstr, InstrItinClass itin> {
  let BaseName = asmbase in {
    def NAME : PSForm_y2<psyop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(" ", asmstr)), itin>;
    let Defs = [CR1] in
    def o    : PSForm_y2<psyop, OOL, IOL,
                         !strconcat(asmbase, !strconcat(". ", asmstr)), itin>,
                         isRecordForm;
  }
}

defm PS_DIV : PSForm_x1r<18, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_div", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_SUB : PSForm_x1r<20, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_sub", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_ADD : PSForm_x1r<21, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_add", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_SEL : PSForm_xr<23,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_sel", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_RES : PSForm_x2r<24, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_res", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_MUL : PSForm_x3r<25, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC),
                      "ps_mul", "$FRT, $FRA, $FRC", IIC_FPGeneral>;
defm PS_RSQRTE : PSForm_x2r<26, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_rsqrte", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_MSUB : PSForm_xr<28,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_msub", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_MADD : PSForm_xr<29,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_madd", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_NMSUB : PSForm_xr<30,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_nmsub", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_NMADD : PSForm_xr<31,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_nmadd", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_NEG : PSForm_y2r<40, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_neg", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_MR : PSForm_y2r<72, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_mr", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_NABS : PSForm_y2r<136, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_nabs", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_ABS : PSForm_y2r<264, (outs f8rc:$FRT), (ins f8rc:$FRB),
                      "ps_abs", "$FRT, $FRB", IIC_FPGeneral>;
defm PS_SUM0 : PSForm_xr<10,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_sum0", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_SUM1 : PSForm_xr<11,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_sum1", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_MULS0 : PSForm_x3r<12, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC),
                      "ps_muls0", "$FRT, $FRA, $FRC", IIC_FPGeneral>;
defm PS_MULS1 : PSForm_x3r<13, (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC),
                      "ps_muls1", "$FRT, $FRA, $FRC", IIC_FPGeneral>;
defm PS_MADDS0 : PSForm_xr<14,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_madds0", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
defm PS_MADDS1 : PSForm_xr<15,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRC, f8rc:$FRB),
                      "ps_madds1", "$FRT, $FRA, $FRC, $FRB", IIC_FPGeneral>;
def PS_CMPU0 : PSForm_c<0,
                      (outs crrc:$crD), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_cmpu0 $crD, $FRA, $FRB", IIC_FPGeneral>;
def PS_CMPO0 : PSForm_c<32,
                      (outs crrc:$crD), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_cmpo0 $crD, $FRA, $FRB", IIC_FPGeneral>;
def PS_CMPU1 : PSForm_c<64,
                      (outs crrc:$crD), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_cmpu1 $crD, $FRA, $FRB", IIC_FPGeneral>;
def PS_CMPO1 : PSForm_c<96,
                      (outs crrc:$crD), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_cmpo1 $crD, $FRA, $FRB", IIC_FPGeneral>;
defm PS_MERGE00 : PSForm_yr<528,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_merge00", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_MERGE01 : PSForm_yr<560,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_merge01", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_MERGE10 : PSForm_yr<592,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_merge10", "$FRT, $FRA, $FRB", IIC_FPGeneral>;
defm PS_MERGE11 : PSForm_yr<624,
                      (outs f8rc:$FRT), (ins f8rc:$FRA, f8rc:$FRB),
                      "ps_merge11", "$FRT, $FRA, $FRB", IIC_FPGeneral>;

def PSC_DCBZL : DCBZL_Form<1014,
                           (outs), (ins gprc:$rA, gprc:$rB),
                           "dcbz_l $rA, $rB", IIC_FPGeneral>;

}
}
#endif // INCLUDED_CAPSTONE_PS_INSTR
#endif // CAPSTONE_PS_INSTR

#ifndef INCLUDED_CAPSTONE_PS_FEATURE
#ifdef CAPSTONE_PS_FEATURE
#define INCLUDED_CAPSTONE_PS_FEATURE

def FeaturePS        : SubtargetFeature<"ps","HasPS","false",
                              "Enable paired-singles instructions",
                              [FeatureFPU]>;

def HasPS : Predicate<"PPCSubTarget->hasPS()">,
  AssemblerPredicate<(all_of FeaturePS), "ps">;

#endif // INCLUDED_CAPSTONE_PS_FEATURE
#endif // CAPSTONE_PS_FEATURE

#ifndef INCLUDED_CAPSTONE_PS_PROCESSOR
#ifdef CAPSTONE_PS_PROCESSOR
#define INCLUDED_CAPSTONE_PS_PROCESSOR

def : Processor<"750cl", G4Itineraries, [Directive750,
                                       FeatureFRES, FeatureFRSQRTE,
                                       FeatureMFTB, FeaturePS]>;

#endif // INCLUDED_CAPSTONE_PS_PROCESSOR
#endif // CAPSTONE_PS_PROCESSOR
