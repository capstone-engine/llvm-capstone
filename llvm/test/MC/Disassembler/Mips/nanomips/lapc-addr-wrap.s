# LAPC32/48 - PC-relative calculation with address space wrap
#
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:   | llvm-objdump --triple nanomips-elf -d -r --adjust-vma=0xffffff00 - | FileCheck %s

	.text
	.globl main
main:
	# Instruction encodings to target 0x4ac from 0xffffff00 without
	# generating relocations
	.short 0x6083 		# CHECK: {{.*}}: 83 60 a6 05 00 00    lapc.b $a0, 0x4ac
	.short 0x05a6
	.short 0x0000
	.short 0x0480		# CHECK: {{.*}}: 80 04 a2 05    lapc.h $a0, 0x4ac
	.short 0x05a2
