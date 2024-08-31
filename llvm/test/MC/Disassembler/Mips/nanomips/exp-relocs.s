# Explicit placeholder relocations
#
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:   | llvm-objdump --triple nanomips-elf -d -r - | FileCheck %s
	.text
	.globl foo
	.set noat
	.linkrelax
	.reloc	1f,R_NANOMIPS_JALR32,end # CHECK: R_NANOMIPS_JALR32	end
1:	jalr	$t9
	.reloc	1f,R_NANOMIPS_JALR16,end # CHECK: R_NANOMIPS_JALR16	end
1:	jalr	$t9
	.reloc	1f,R_NANOMIPS_INSN32,end # CHECK: R_NANOMIPS_INSN32	end
1:	balc	foo
	.reloc	1f,R_NANOMIPS_INSN16,foo # CHECK: R_NANOMIPS_INSN16	foo
1:	bc end
	.reloc	1f,R_NANOMIPS_FIXED,foo # CHECK: R_NANOMIPS_FIXED	foo
1:	beqc $a3, $a4, end
	.reloc	1f,R_NANOMIPS_NOTRAMP # CHECK: R_NANOMIPS_NOTRAMP	*ABS*
1:	balc	foo
	.reloc  1f, R_NANOMIPS_SAVERESTORE # CHECK: R_NANOMIPS_SAVERESTORE	*ABS*
1:	save	128,$fp,$ra,$s0-$s7

