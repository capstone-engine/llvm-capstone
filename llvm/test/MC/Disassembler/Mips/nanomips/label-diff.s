# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:   | llvm-readelf -r - | FileCheck %s
  .module	pcrel
  .module	softfloat
  .linkrelax
  .globl	foo
  .text
  .ent	foo
  .type	foo, @function
foo:
.L1:
  nop
.L2:
  nop
  .reloc	1f,R_NANOMIPS_JUMPTABLE_LOAD,foo # CHECK-NOT: R_NANOMIPS_JUMPTABLE_LOAD
1:	lbux	$a1,$a2($a3)
  .end foo

  .section        .rodata,"a",@progbits
  .jumptable 1,5,1
  .word (.L1 + 4 - .L2)		# CHECK: {{0+}}   00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 4
				# CHECK: {{0+}}   00000101 R_NANOMIPS_32	{{0+}}    .L1 + 0
  .2byte (.L1 - .L2)		# CHECK: {{0+}}4  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 0
				# CHECK: {{0+}}4  00000107 R_NANOMIPS_UNSIGNED_16 {{0+}}  .L1 + 0
  .2byte (.L1 - .L2) >> 1	# CHECK: {{0+}}6  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 0
				# CHECK: {{0+}}6  00000104 R_NANOMIPS_ASHIFTR_1	{{0+}}    .L1 + 0
				# CHECK: {{0+}}6  00000007 R_NANOMIPS_UNSIGNED_16 0
  .shword (.L1 - 4 - .L2)	# CHECK: {{0+}}8  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 - 4
				# CHECK: {{0+}}8  00000108 R_NANOMIPS_SIGNED_16	{{0+}}    .L1 + 0
  .shword (.L1 - .L2) >> 1	# CHECK: {{0+}}a  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 0
				# CHECK: {{0+}}a  00000104 R_NANOMIPS_ASHIFTR_1	{{0+}}    .L1 + 0
				# CHECK: {{0+}}a  00000008 R_NANOMIPS_SIGNED_16 0
  .byte (.L1 - .L2 + 4)		# CHECK: {{0+}}c  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 4
				# CHECK: {{0+}}c  00000105 R_NANOMIPS_UNSIGNED_8 {{0+}}   .L1 + 0
  .byte (.L1 - .L2) >> 1	# CHECK: {{0+}}d  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 0
				# CHECK: {{0+}}d  00000104 R_NANOMIPS_ASHIFTR_1	{{0+}}    .L1 + 0
				# CHECK: {{0+}}d  00000005 R_NANOMIPS_UNSIGNED_8 0
  .sbyte (.L1 - .L2 - 4)	# CHECK: {{0+}}e  00000203 R_NANOMIPS_NEG {{0+}}2         .L2 - 4
				# CHECK: {{0+}}e  00000106 R_NANOMIPS_SIGNED_8	{{0+}}    .L1 + 0
  .sbyte (.L1 - .L2) >> 1	# CHECK: {{0+}}f  00000203 R_NANOMIPS_NEG	{{0+}}2   .L2 + 0
				# CHECK: {{0+}}f  00000104 R_NANOMIPS_ASHIFTR_1	{{0+}}    .L1 + 0
				# CHECK: {{0+}}f  00000006 R_NANOMIPS_SIGNED_8	0

