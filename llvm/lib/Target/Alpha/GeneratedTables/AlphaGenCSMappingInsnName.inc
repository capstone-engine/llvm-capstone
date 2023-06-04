/* Capstone Disassembly Engine, http://www.capstone-engine.org */
/* By Nguyen Anh Quynh <aquynh@gmail.com>, 2013-2022, */
/*    Rot127 <unisono@quyllur.org> 2022-2023 */
/* Automatically generated file by Capstone's LLVM TableGen Disassembler Backend. */

/* LLVM-commit: <commit> */
/* LLVM-tag: <tag> */

/* Do not edit. */

/* Capstone's LLVM TableGen Backends: */
/* https://github.com/capstone-engine/llvm-capstone */

  "invalid", // ALPHA_INS_INVALID
  "addl", // ALPHA_INS_ADDL
  "addq", // ALPHA_INS_ADDQ
  "adds/su", // ALPHA_INS_ADDS/SU
  "addt/su", // ALPHA_INS_ADDT/SU
  ";", // ALPHA_INS_;
  "and", // ALPHA_INS_AND
  "beq", // ALPHA_INS_BEQ
  "bge", // ALPHA_INS_BGE
  "bgt", // ALPHA_INS_BGT
  "bic", // ALPHA_INS_BIC
  "bis", // ALPHA_INS_BIS
  "blbc", // ALPHA_INS_BLBC
  "blbs", // ALPHA_INS_BLBS
  "ble", // ALPHA_INS_BLE
  "blt", // ALPHA_INS_BLT
  "bne", // ALPHA_INS_BNE
  "br", // ALPHA_INS_BR
  "cas32", // ALPHA_INS_CAS32
  "cas64", // ALPHA_INS_CAS64
  "cmoveq", // ALPHA_INS_CMOVEQ
  "cmovge", // ALPHA_INS_CMOVGE
  "cmovgt", // ALPHA_INS_CMOVGT
  "cmovlbc", // ALPHA_INS_CMOVLBC
  "cmovlbs", // ALPHA_INS_CMOVLBS
  "cmovle", // ALPHA_INS_CMOVLE
  "cmovlt", // ALPHA_INS_CMOVLT
  "cmovne", // ALPHA_INS_CMOVNE
  "cmpbge", // ALPHA_INS_CMPBGE
  "cmpeq", // ALPHA_INS_CMPEQ
  "cmple", // ALPHA_INS_CMPLE
  "cmplt", // ALPHA_INS_CMPLT
  "cmpteq/su", // ALPHA_INS_CMPTEQ/SU
  "cmptle/su", // ALPHA_INS_CMPTLE/SU
  "cmptlt/su", // ALPHA_INS_CMPTLT/SU
  "cmptun/su", // ALPHA_INS_CMPTUN/SU
  "cmpule", // ALPHA_INS_CMPULE
  "cmpult", // ALPHA_INS_CMPULT
  "{:comment}", // ALPHA_INS_:COMMENT}
  "cpyse", // ALPHA_INS_CPYSE
  "cpysn", // ALPHA_INS_CPYSN
  "cpys", // ALPHA_INS_CPYS
  "CTLZ", // ALPHA_INS_CTLZ
  "CTPOP", // ALPHA_INS_CTPOP
  "CTTZ", // ALPHA_INS_CTTZ
  "cvtqs/sui", // ALPHA_INS_CVTQS/SUI
  "cvtqt/sui", // ALPHA_INS_CVTQT/SUI
  "cvtst/s", // ALPHA_INS_CVTST/S
  "cvttq/svc", // ALPHA_INS_CVTTQ/SVC
  "cvtts/sui", // ALPHA_INS_CVTTS/SUI
  "divs/su", // ALPHA_INS_DIVS/SU
  "divt/su", // ALPHA_INS_DIVT/SU
  "eqv", // ALPHA_INS_EQV
  "EXTBL", // ALPHA_INS_EXTBL
  "EXTLL", // ALPHA_INS_EXTLL
  "EXTWL", // ALPHA_INS_EXTWL
  "fbeq", // ALPHA_INS_FBEQ
  "fbge", // ALPHA_INS_FBGE
  "fbgt", // ALPHA_INS_FBGT
  "fble", // ALPHA_INS_FBLE
  "fblt", // ALPHA_INS_FBLT
  "fbne", // ALPHA_INS_FBNE
  "fcmoveq", // ALPHA_INS_FCMOVEQ
  "fcmovge", // ALPHA_INS_FCMOVGE
  "fcmovgt", // ALPHA_INS_FCMOVGT
  "fcmovle", // ALPHA_INS_FCMOVLE
  "fcmovlt", // ALPHA_INS_FCMOVLT
  "fcmovne", // ALPHA_INS_FCMOVNE
  "ftois", // ALPHA_INS_FTOIS
  "ftoit", // ALPHA_INS_FTOIT
  "itofs", // ALPHA_INS_ITOFS
  "itoft", // ALPHA_INS_ITOFT
  "jsr", // ALPHA_INS_JSR
  "las32", // ALPHA_INS_LAS32
  "las64", // ALPHA_INS_LAS64
  "mb", // ALPHA_INS_MB
  "mull", // ALPHA_INS_MULL
  "mulq", // ALPHA_INS_MULQ
  "muls/su", // ALPHA_INS_MULS/SU
  "mult/su", // ALPHA_INS_MULT/SU
  "ornot", // ALPHA_INS_ORNOT
  "ret", // ALPHA_INS_RET
  "rpcc", // ALPHA_INS_RPCC
  "s4addl", // ALPHA_INS_S4ADDL
  "s4addq", // ALPHA_INS_S4ADDQ
  "s4subl", // ALPHA_INS_S4SUBL
  "s4subq", // ALPHA_INS_S4SUBQ
  "s8addl", // ALPHA_INS_S8ADDL
  "s8addq", // ALPHA_INS_S8ADDQ
  "s8subl", // ALPHA_INS_S8SUBL
  "s8subq", // ALPHA_INS_S8SUBQ
  "sextb", // ALPHA_INS_SEXTB
  "sextw", // ALPHA_INS_SEXTW
  "sll", // ALPHA_INS_SLL
  "sqrts/su", // ALPHA_INS_SQRTS/SU
  "sqrtt/su", // ALPHA_INS_SQRTT/SU
  "sra", // ALPHA_INS_SRA
  "srl", // ALPHA_INS_SRL
  "subl", // ALPHA_INS_SUBL
  "subq", // ALPHA_INS_SUBQ
  "subs/su", // ALPHA_INS_SUBS/SU
  "subt/su", // ALPHA_INS_SUBT/SU
  "swap32", // ALPHA_INS_SWAP32
  "swap64", // ALPHA_INS_SWAP64
  "umulh", // ALPHA_INS_UMULH
  "wmb", // ALPHA_INS_WMB
  "#wtf", // ALPHA_INS_#WTF
  "xor", // ALPHA_INS_XOR
  "zapnot", // ALPHA_INS_ZAPNOT
