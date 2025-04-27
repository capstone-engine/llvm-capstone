// SPDX-FileCopyrightText: 2025 Rot127 <unisono@quyllur.org>
// SPDX-License-Identifier: LGPL-3.0-only
//
// Definitions of Apple's proprietary instructions.


def FeatureAMX : SubtargetFeature<"amx", "HasAMX", "true",
  "Enable proprietary Apple AArch64 extension AMX.">;

def HasAMX : Predicate<"Subtarget->hasAMX()">,
                       AssemblerPredicate<(all_of FeatureAMX), "amx">;

class ReservedI<dag oops, dag iops, string asm, string operands,
                  list<dag> pattern = [], string constraints = "">
    : I<oops, iops, asm, operands, "", pattern> {
  let Inst{31-29} = 0b000;
  let Inst{28-25} = 0b0000;
  let Inst{24-21} = 0b0001;
  let Constraints = constraints;
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

//
// ====== MUL53 ======
//
// Reference: https://gist.github.com/TrungNguyen1909/5b323edda9a21550a1621af506e8ce5f

def FeatureMUL53 : SubtargetFeature<"mul53", "HasMUL53", "true",
  "Enable proprietary Apple AArch64 extension Mul53.">;

def HasMUL53 : Predicate<"Subtarget->hasMUL53()">,
                       AssemblerPredicate<(all_of FeatureMUL53), "mul53">;

// It is an assumption that the mul53 opcode field is 5 bits. It might as well
// is more norrow or wider.
class AppleMUL53<bits<5> mul53_opcode, dag oops, dag iops, string asm, string operands,
                  list<dag> pattern = [], string constraints = "">
    : ReservedI<oops, iops, asm, operands, pattern, constraints> {
  bits<5> Vn;
  bits<5> Vm;
  let Inst{20-15} = 0b000000;
  let Inst{14-10} = mul53_opcode;
  let Inst{9-5} = Vm;
  let Inst{4-0} = Vn;

  let Predicates = [HasMUL53];
}

def MUL53LO : AppleMUL53<0, (outs V128:$Vn_wb), (ins V128:$Vn, V128:$Vm), "mul53lo.2d", "\t$Vn, $Vm", [], "$Vn = $Vn_wb">;
def MUL53HI : AppleMUL53<1, (outs V128:$Vn_wb), (ins V128:$Vn, V128:$Vm), "mul53hi.2d", "\t$Vn, $Vm", [], "$Vn = $Vn_wb">;

//
// ====== System instructions ======
//
// Reference: https://github.com/AsahiLinux/docs/blob/main/docs/hw/cpu/apple-instructions.md


def FeatureAppleSys : SubtargetFeature<"apple-sys", "HasAppleSys", "true",
  "Enable proprietary Apple system instructions.">;

def HasAppleSys : Predicate<"Subtarget->hasAppleSys()">,
                       AssemblerPredicate<(all_of FeatureAppleSys), "apple-sys">;


//
// GL instructions
//
// See: https://github.com/AsahiLinux/docs/blob/main/docs/hw/cpu/sprr-gxf.md
//

def GEXIT : ReservedI<(outs), (ins), "gexit", "">, Sched<[WriteSys]> {
  let Inst{20-0} = 0b000000001010000000000;
  let Predicates = [HasAppleSys];
}

def GENTER : ReservedI<(outs), (ins simm5_32b:$perm), "genter", "\t$perm">, Sched<[WriteSys]> {
  bits<5> perm;
  let Inst{20-5} = 0b0000000010100001;
  // The permission of the pages. See document 
  let Inst{4-0} = perm;
  let Predicates = [HasAppleSys];
}

let mayLoad = 1, mayStore = 1 in {
def WKDMC : ReservedI<(outs GPR64:$src_wb), (ins GPR64:$dst, GPR64:$src), "wkdmc", "\t$dst, $src", [], "$src = $src_wb">, Sched<[WriteSys]> {
  bits<5> src;
  bits<5> dst;
  let Inst{20-10} = 0b00000000010;
  let Inst{9-5} = dst;
  let Inst{4-0} = src;
  let Predicates = [HasAppleSys];
}

def WKDMD : ReservedI<(outs GPR64:$src_wb), (ins GPR64:$dst, GPR64:$src), "wkdmd", "\t$dst, $src", [], "$src = $src_wb">, Sched<[WriteSys]> {
  bits<5> src;
  bits<5> dst;
  let Inst{20-10} = 0b00000000011;
  let Inst{9-5} = dst;
  let Inst{4-0} = src;
  let Predicates = [HasAppleSys];
}
}

def AT_AS1ELX : ReservedI<(outs GPR64:$Ra_wb), (ins GPR64:$Ra), "at_as1elx", "\t$Ra", [], "$Ra = $Ra_wb">, Sched<[WriteSys]> {
  bits<5> Ra;
  let Inst{20-5} = 0b0000000010100010;
  let Inst{4-0} = Ra;
  let Predicates = [HasAppleSys];
}

def apple_sys_barrier_op : Operand<i32> {
  let PrintMethod = "printAppleSysBarrierOption";
}

def SDSB : ReservedI<(outs), (ins apple_sys_barrier_op:$CRm), "sdsb", "\t$CRm">, Sched<[WriteBarrier]> {
  bits<4> CRm;
  let Inst{20-4} = 0b00000000101000110;
  let Inst{3-0} = CRm;
}

