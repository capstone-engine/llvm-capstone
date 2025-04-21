// SPDX-FileCopyrightText: 2025 Rot127 <unisono@quyllur.org>
// SPDX-License-Identifier: LGPL-3.0-only
//
// Definitions of Apple's proprietary instructions.


def FeatureAMX : SubtargetFeature<"amx", "HasAMX", "true",
  "Enable proprietary Apple AArch64 extension AMX.">;

def HasAMX : Predicate<"Subtarget->hasAMX()">,
                       AssemblerPredicate<(all_of FeatureAMX), "amx">;

class ReservedI<dag oops, dag iops, string asm, string operands,
                  list<dag> pattern = []>
    : I<oops, iops, asm, operands, "", pattern> {
  let Inst{31-29} = 0b000;
  let Inst{28-25} = 0b0000;
  let Inst{24-21} = 0b0001;
}

//
// ====== AMX ======
//
// Reference: https://github.com/corsix/amx

// For now not all sub registers are defined here.
// Because they cannot be addressed explicit or implicitly
// by the AMX instructions.
// The registers it uses are encoded in the GPR value
// and hence only known during runtime.
def X_LANE : AArch64Reg<0, "X">;
def Y_LANE : AArch64Reg<0, "Y">;
def Z_MATRIX : AArch64Reg<0, "Z">;

class AppleAMX<bits<5> amx_opcode, dag oops, dag iops, string asm, string operands,
                  list<dag> pattern = []>
    : ReservedI<oops, iops, asm, operands, pattern> {
  bits<5> src;
  let Inst{20-16} = 0b00000;
  let Inst{15-12} = 0b0001;
  let Inst{11-10} = 0b00;
  let Inst{9-5} = amx_opcode;
  let Inst{4-0} = src;

  let Predicates = [HasAMX];
}

let mayLoad = 1 in {
  let Defs = [X_LANE] in
  def LDX : AppleAMX<0, (outs), (ins GPR64:$src), "ldx", "\t$src">;

  let Defs = [Y_LANE] in
  def LDY : AppleAMX<1, (outs), (ins GPR64:$src), "ldy", "\t$src">;

  let Defs = [Z_MATRIX] in {
    def LDZ : AppleAMX<4, (outs), (ins GPR64:$src), "ldz", "\t$src">;
    def LDZI : AppleAMX<6, (outs), (ins GPR64:$src), "ldzi", "\t$src">;
  }
}

let mayStore = 1 in {
  let Uses = [X_LANE] in
  def STX : AppleAMX<2, (outs), (ins GPR64:$src), "stx", "\t$src">;

  let Uses = [Y_LANE] in
  def STY : AppleAMX<3, (outs), (ins GPR64:$src), "sty", "\t$src">;

  let Uses = [Z_MATRIX] in {
    def STZ : AppleAMX<5, (outs), (ins GPR64:$src), "stz", "\t$src">;
    def STZI : AppleAMX<7, (outs), (ins GPR64:$src), "stzi", "\t$src">;
  }
}

// The "set" and "clr" instruction techincally are defined by having an
// immediate value operand.
// I assume there is some kind of "amx state change" instruction.
// The immediate value then determines what specific instruction it is.
// Similar like the hint instructions which have alias depending on some
// bits.
// Since the underlying "state change instruction" is not documented
// I make it an instruction with hard-coded bits[4:0] and no
// immediate operand.
let Defs = [X_LANE, Y_LANE, Z_MATRIX] in
def SET : AppleAMX<17, (outs), (ins), "set", ""> {
  let Inst{4-0} = 0b00000;
}

def CLR : AppleAMX<17, (outs), (ins), "clr", ""> {
  let Inst{4-0} = 0b00001;
}

// These two instructions decode differently depending on the operand value.
// They also use and define different registers.
// All of it is onlye known during runtime, so implicit Defs and Uses
// are omitted here.
def EXTRX : AppleAMX<8, (outs), (ins GPR64:$src), "extrx", "\t$src">;
def EXTRY : AppleAMX<9, (outs), (ins GPR64:$src), "extry", "\t$src">;

let Uses = [X_LANE, Y_LANE, Z_MATRIX], Defs = [Z_MATRIX] in {
  def FMA64 : AppleAMX<10, (outs), (ins GPR64:$src), "fma64", "\t$src">;
  def FMS64 : AppleAMX<11, (outs), (ins GPR64:$src), "fms64", "\t$src">;
  def FMA32 : AppleAMX<12, (outs), (ins GPR64:$src), "fma32", "\t$src">;
  def FMS32 : AppleAMX<13, (outs), (ins GPR64:$src), "fms32", "\t$src">;
  def FMA16 : AppleAMX<15, (outs), (ins GPR64:$src), "fma16", "\t$src">;
  def FMS16 : AppleAMX<16, (outs), (ins GPR64:$src), "fms16", "\t$src">;
  def VECFP : AppleAMX<19, (outs), (ins GPR64:$src), "vecfp", "\t$src">;
  def MAC16 : AppleAMX<14, (outs), (ins GPR64:$src), "mac16", "\t$src">;
  def MATFP : AppleAMX<21, (outs), (ins GPR64:$src), "matfp", "\t$src">;
}

// These instructions might read Z as well (depends on bit 47 in the operand value).
// But the indeirect read is only known during runtime when the operand value is known.
// So here we don't add Z to the implicit read list.
let Uses = [X_LANE, Y_LANE], Defs = [Z_MATRIX] in {
  def VECINT : AppleAMX<18, (outs), (ins GPR64:$src), "vecint", "\t$src">;
  def MATINT : AppleAMX<20, (outs), (ins GPR64:$src), "matint", "\t$src">;
}

def GENLUT : AppleAMX<22, (outs), (ins GPR64:$src), "genlut", "\t$src">;
