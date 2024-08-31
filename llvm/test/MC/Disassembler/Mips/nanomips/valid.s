# Instructions that are valid
#
# Branches have some unusual encoding rules in nanoMIPS so we need to test:
#   rs == 0
#   rs != 0
#   rt == 0
#   rt != 0
#   rs < rt
#   rs == rt
#   rs > rt
# appropriately for each branch instruction
#
# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst 2> %t0 | FileCheck %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf %s -o - \
# RUN:   | llvm-objdump --no-print-imm-hex --triple nanomips-elf -dr - | FileCheck --check-prefixes DISAS %s
	.text
	# CHECK: .text
	.set noat
#	.linkrelax
	# reg3-reg3 arithmetic, 16-bit
	addu	$a1, $s2, $a3	# CHECK: addu	$a1, $s2, $a3	# encoding: [0xaa,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu16_NM
				# DISAS: {{.*}}: aa b3        addu $a1, $s2, $a3
	subu	$a0, $a2, $a3	# CHECK: subu	$a0, $a2, $a3	# encoding: [0xe9,0xb3]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu16_NM
				# DISAS: {{.*}}: e9 b3	subu $a0, $a2, $a3

	# reg4-reg4 arithmetic, 16-bit
	addu	$a1, $a1, $a7	# CHECK: addu $a1, $a1, $a7	# encoding: [0xa3,0x3c]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu4x4_NM
				# DISAS: {{.*}}: a3 3c	addu $a1, $a1, $a7
	mul	$s2, $s2, $a3	# CHECK: mul $s2, $s2, $a3	# encoding: [0x4f,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL4x4_NM
				# DISAS: {{.*}}: 4f 3e	mul $s2, $s2, $a3

	# 16-bit 4x4 with operands commuted
	addu	$a1, $a7, $a1	# CHECK: addu $a1, $a1, $a7	# encoding: [0xa3,0x3c]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu4x4_NM
				# DISAS: {{.*}}: a3 3c	addu $a1, $a1, $a7

	mul	$s2, $a3, $s2	# CHECK: mul $s2, $s2, $a3	# encoding: [0x4f,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL4x4_NM
				# DISAS: {{.*}}: 4f 3e	mul $s2, $s2, $a3

	# reg-reg arithmetic, 32-bit
	add	$a1, $a2, $a3	# CHECK: add $a1, $a2, $a3	# encoding: [0xe6,0x20,0x10,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} ADD_NM
				# DISAS: {{.*}}: e6 20 10 29	add $a1, $a2, $a3
	addu	$a1, $s2, $t3	# CHECK: addu $a1, $s2, $t3	# encoding: [0xf2,0x21,0x50,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDu_NM
				# DISAS: {{.*}}: f2 21 50 29  	addu	$a1, $s2, $t3
	sub	$a1, $s2, $a3	# CHECK: sub $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} SUB_NM
				# DISAS: {{.*}}: f2 20 90 29  	sub	$a1, $s2, $a3
	subu	$a0, $a2, $t3	# CHECK: subu $a0, $a2, $t3	# encoding: [0xe6,0x21,0xd0,0x21]
				# CHECK-NEXT: # <MCInst #{{.*}} SUBu_NM
				# DISAS: {{.*}}: e6 21 d0 21  	subu	$a0, $a2, $t
	mul	$t1, $s2, $a3	# CHECK: mul $t1, $s2, $a3	# encoding: [0xf2,0x20,0x18,0x68]
				# CHECK-NEXT: # <MCInst #{{.*}} MUL_NM
				# DISAS: {{.*}}: f2 20 18 68  	mul	$t1, $s2, $a3
	muh	$a1, $s2, $a3	# CHECK: muh $a1, $s2, $a3	# encoding: [0xf2,0x20,0x58,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} MUH_NM
				# DISAS: {{.*}}: f2 20 58 28  	muh	$a1, $s2, $a3
	mulu	$k1, $s2, $a3	# CHECK: mulu $k1, $s2, $a3	# encoding: [0xf2,0x20,0x98,0xd8]
				# CHECK-NEXT: # <MCInst #{{.*}} MULU_NM
				# DISAS: {{.*}} f2 20 98 d8  	mulu	$k1, $s2, $a3
	muhu	$a1, $s2, $a3	# CHECK: muhu $a1, $s2, $a3	# encoding: [0xf2,0x20,0xd8,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} MUHU_NM
				# DISAS: {{.*}} f2 20 d8 28  	muhu	$a1, $s2, $a3
	div	$a1, $s2, $t0	# CHECK: div $a1, $s2, $t0	# encoding: [0x92,0x21,0x18,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} DIV_NM
				# DISAS: {{.*}} 92 21 18 29  	div	$a1, $s2, $t0
	divu	$t4, $s2, $a3	# CHECK: divu $t4, $s2, $a3	# encoding: [0xf2,0x20,0x98,0x11]
				# CHECK-NEXT: # <MCInst #{{.*}} DIVU_NM
				# DISAS: {{.*}} f2 20 98 11  	divu	$t4, $s2, $a3

	mod	$a1, $t2, $a3	# CHECK: mod $a1, $t2, $a3	# encoding: [0xee,0x20,0x58,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} MOD_NM
				# DISAS: {{.*}} ee 20 58 29  	mod	$a1, $t2, $a3
 
	modu	$a1, $s2, $t5	# CHECK: modu $a1, $s2, $t5	# encoding: [0x72,0x20,0xd8,0x29]
				# CHECK-NEXT: # <MCInst #{{.*}} MODU_NM
				# DISAS: {{.*}} 72 20 d8 29  	modu	$a1, $s2, $t5
	
	# reg-reg logic, 16-bit
	and	$a1, $a3, $a1	# CHECK: and $a1, $a3, $a1	# encoding: [0xf8,0x52]
				# CHECK-NEXT: # <MCInst #{{.*}} AND16_NM
				# DISAS: {{.*}}  f8 52        	and	$a3, $a1, $a3
	or	$s2, $a3, $s2	# CHECK: or $s2, $a3, $s2	# encoding: [0x7c,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} OR16_NM
				# DISAS: {{.*}}  7c 51        	or	$a3, $s2, $a3
	xor	$a3, $s3, $a3	# CHECK: xor $a3, $s3, $a3	# encoding: [0xb4,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR16_NM
				# DISAS: {{.*}}  b4 53        	xor	$s3, $a3, $s3
	not	$a3, $s3	# CHECK: not $a3, $s3		# encoding: [0xb0,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} NOT16_NM
				# DISAS: {{.*}}  b0 53        	not	$a3, $s3
	
	# 16-bit with operands commuted
	and	$a1, $a1, $a3	# CHECK: and $a1, $a3, $a1	# encoding: [0xf8,0x52]
				# CHECK-NEXT: # <MCInst #{{.*}} AND16_NM
				# DISAS: {{.*}}  f8 52        	and	$a3, $a1, $a3
	or	$s2, $s2, $a3	# CHECK: or $s2, $a3, $s2	# encoding: [0x7c,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} OR16_NM
				# DISAS: {{.*}}  7c 51        	or	$a3, $s2, $a3
	xor	$a3, $a3, $s3	# CHECK: xor $a3, $s3, $a3	# encoding: [0xb4,0x53]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR16_NM
				# DISAS: {{.*}}  b4 53        	xor	$s3, $a3, $s3
	
	# reg-reg logic, 32-bit
	and	$a1, $s4, $a3	# CHECK: and $a1, $s4, $a3	# encoding: [0xf4,0x20,0x50,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} AND_NM
				# DISAS: {{.*}}  f4 20 50 2a  	and	$a1, $s4, $a3
	or	$a1, $t2, $a3	# CHECK: or $a1, $t2, $a3	# encoding: [0xee,0x20,0x90,0x2a]
				# CHECK-NEXT: # <MCInst #{{.*}} OR_NM
				# DISAS: {{.*}}  ee 20 90 2a  	or	$a1, $t2, $a3
	nor	$a3, $t3, $s3	# CHECK: nor $a3, $t3, $s3	# encoding: [0x6f,0x22,0xd0,0x3a]
				# CHECK-NEXT: # <MCInst #{{.*}} NOR_NM
				# DISAS: {{.*}}  6f 22 d0 3a  	nor	$a3, $t3, $s3
	xor	$t0, $s2, $a3	# CHECK: xor $t0, $s2, $a3	# encoding: [0xf2,0x20,0x10,0x63]
				# CHECK-NEXT: # <MCInst #{{.*}} XOR_NM
				# DISAS: {{.*}}  f2 20 10 63  	xor	$t0, $s2, $a3
	not	$a7, $s7	# CHECK: not $a7, $s7	# encoding: [0x17,0x20,0xd0,0x5a]
				# CHECK-NEXT: # <MCInst #{{.*}} NOR_NM
				# DISAS: {{.*}}  17 20 d0 5a  	not	$a7, $s7
	
	# reg-reg bitwise, 32-bit
	sllv	$a1, $a7, $a3	# CHECK: sllv $a1, $a7, $a3	# encoding: [0xeb,0x20,0x10,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SLLV_NM
				# DISAS: {{.*}}  eb 20 10 28  	sllv	$a1, $a7, $a3
	srlv	$a1, $s1, $a3	# CHECK: srlv $a1, $s1, $a3	# encoding: [0xf1,0x20,0x50,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SRLV_NM
				# DISAS: {{.*}}  f1 20 50 28  	srlv	$a1, $s1, $a3
	srav	$a1, $s2, $a3	# CHECK: srav $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} SRAV_NM
				# DISAS: {{.*}}  f2 20 90 28  	srav	$a1, $s2, $a3
	rotrv	$a1, $s2, $a3	# CHECK: rotrv $a1, $s2, $a3	# encoding: [0xf2,0x20,0xd0,0x28]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTRV_NM
				# DISAS: {{.*}}  f2 20 d0 28  	rotrv	$a1, $s2, $a3
	slt	$a1, $s2, $a3	# CHECK: slt $a1, $s2, $a3	# encoding: [0xf2,0x20,0x50,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SLT_NM
				# DISAS: {{.*}}  f2 20 50 2b  	slt	$a1, $s2, $a3
	sltu	$a1, $s2, $a3	# CHECK: sltu $a1, $s2, $a3	# encoding: [0xf2,0x20,0x90,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTU_NM
				# DISAS: {{.*}}  f2 20 90 2b  	sltu	$a1, $s2, $a3
	sov	$a1, $s3, $t2	# CHECK: sov $a1, $s3, $t2	# encoding: [0xd3,0x21,0xd0,0x2b]
				# CHECK-NEXT: # <MCInst #{{.*}} SOV_NM
				# DISAS: {{.*}}  d3 21 d0 2b  	sov	$a1, $s3, $t2
	
	# reg-imm logic, 16-bit
	sll	$a1, $s2, 1	# CHECK: sll $a1, $s2, 1	# encoding: [0xa1,0x32]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL16_NM
				# DISAS: {{.*}}  a1 32        	sll	$a1, $s2, 1
	srl	$s1, $a3, 8	# CHECK: srl $s1, $a3, 8	# encoding: [0xf8,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL16_NM
				# DISAS: {{.*}}  f8 30        	srl	$s1, $a3, 8
	andi	$a0, $a1, 0	# CHECK: andi $a0, $a1, 0x0	# encoding: [0x50,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  50 f2        	andi	$a0, $a1, 0x0
	andi	$a1, $a2, 1	# CHECK: andi $a1, $a2, 0x1	# encoding: [0xe1,0xf2]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  e1 f2        	andi	$a1, $a2, 0x1
	andi	$a2, $a3, 6	# CHECK: andi $a2, $a3, 0x6	# encoding: [0x76,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  76 f3        	andi	$a2, $a3, 0x6
	andi	$a3, $s0, 0xff	# CHECK: andi $a3, $s0, 0xff	# encoding: [0x8c,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  8c f3        	andi	$a3, $s0, 0xff
	andi	$s1, $s3, 0xffff	# CHECK: andi $s1, $s3, 0xffff	# encoding: [0xbd,0xf0]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  bd f0        	andi	$s1, $s3, 0xffff
	andi	$s3, $a3, 0xe	# CHECK: andi $s3, $a3, 0xe	# encoding: [0xfe,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  fe f1        	andi	$s3, $a3, 0xe
	andi[16] $s2, $a1, 0xf	# CHECK: andi $s2, $a1, 0xf	# encoding: [0x5f,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI16_NM
				# DISAS: {{.*}}  5f f1        	andi	$s2, $a1, 0xf
	
	# reg-imm logic, 32-bit
	andi	$a4, $a1, 0	# CHECK: andi $a4, $a1, 0x0	# encoding: [0x05,0x81,0x00,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  05 81 00 20  	andi	$a4, $a1, 0x0
	andi	$a1, $t2, 1	# CHECK: andi $a1, $t2, 0x1	# encoding: [0xae,0x80,0x01,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  ae 80 01 20  	andi	$a1, $t2, 0x1
	andi	$a2, $t3, 6	# CHECK: andi $a2, $t3, 0x6	# encoding: [0xcf,0x80,0x06,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  cf 80 06 20  	andi	$a2, $t3, 0x6
	andi	$a6, $s0, 0xff	# CHECK: andi $a6, $s0, 0xff	# encoding: [0x50,0x81,0xff,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  50 81 ff 20  	andi	$a6, $s0, 0xff
	andi[32] $s1, $s4, 0xfff	# CHECK: andi $s1, $s4, 0xfff	# encoding: [0x34,0x82,0xff,0x2f]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  34 82 ff 2f  	andi	$s1, $s4, 0xfff
	andi	$s3, $a4, 0xe	# CHECK: andi $s3, $a4, 0xe	# encoding: [0x68,0x82,0x0e,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  68 82 0e 20  	andi	$s3, $a4, 0xe
	andi	$s2, $t0, 0xf	# CHECK: andi $s2, $t0, 0xf	# encoding: [0x4c,0x82,0x0f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  4c 82 0f 20  	andi	$s2, $t0, 0xf
	andi	$a3, $s3, 12	# CHECK: andi $a3, $s3, 0xc	# encoding: [0xf3,0x80,0x0c,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  f3 80 0c 20  	andi	$a3, $s3, 0xc
	andi	$a1, $s2, 13	# CHECK: andi $a1, $s2, 0xd	# encoding: [0xb2,0x80,0x0d,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  b2 80 0d 20  	andi	$a1, $s2, 0xd
	andi	$a2, $s1, 16	# CHECK: andi $a2, $s1, 0x10	# encoding: [0xd1,0x80,0x10,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} ANDI_NM
				# DISAS: {{.*}}  d1 80 10 20  	andi	$a2, $s1, 0x10
	ori	$a1, $s5, 0x1	# CHECK: ori $a1, $s5, 0x1	# encoding: [0xb5,0x80,0x01,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} ORI_NM
				# DISAS: {{.*}}  b5 80 01 00  	ori	$a1, $s5, 0x1
	xori	$t1, $s2, 0xfff # CHECK: xori $t1, $s2, 0xfff	# encoding: [0xb2,0x81,0xff,0x1f]
				# CHECK-NEXT: # <MCInst #{{.*}} XORI_NM
				# DISAS: {{.*}}  b2 81 ff 1f  	xori	$t1, $s2, 0xfff
	
	# reg-imm bit-wise, 32-bit
	slti	$a1, $s2, 0xff	# CHECK: slti $a1, $s2, 255	# encoding: [0xb2,0x80,0xff,0x40]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTI_NM
				# DISAS: {{.*}}  b2 80 ff 40  	slti	$a1, $s2, 255
	sltiu	$a1, $s2, 0xfff	# CHECK: sltiu $a1, $s2, 4095	# encoding: [0xb2,0x80,0xff,0x5f]
				# CHECK-NEXT: # <MCInst #{{.*}} SLTIU_NM
				# DISAS: {{.*}}  b2 80 ff 5f  	sltiu	$a1, $s2, 4095
	seqi	$a1, $s2, 0xfff	# CHECK: seqi $a1, $s2, 4095	# encoding: [0xb2,0x80,0xff,0x6f]
				# CHECK-NEXT: # <MCInst #{{.*}} SEQI_NM
				# DISAS: {{.*}}  b2 80 ff 6f  	seqi	$a1, $s2, 4095
	sll	$a1, $s2, 9	# CHECK: sll $a1, $s2, 9	# encoding: [0xb2,0x80,0x09,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
				# DISAS: {{.*}}  b2 80 09 c0  	sll	$a1, $s2, 9
	sll	$a1, $t2, 1	# CHECK: sll $a1, $t2, 1	# encoding: [0xae,0x80,0x01,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SLL_NM
				# DISAS: {{.*}}  ae 80 01 c0  	sll	$a1, $t2, 1
	srl	$a1, $a2, 31	# CHECK: srl $a1, $a2, 31	# encoding: [0xa6,0x80,0x5f,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
				# DISAS: {{.*}}  a6 80 5f c0  	srl	$a1, $a2, 31
	srl	$a1, $a7, 8	# CHECK: srl $a1, $a7, 8	# encoding: [0xab,0x80,0x48,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRL_NM
				# DISAS: {{.*}}  ab 80 48 c0  	srl	$a1, $a7, 8
	sra	$a1, $a7, 15	# CHECK: sra $a1, $a7, 15	# encoding: [0xab,0x80,0x8f,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} SRA_NM
				# DISAS: {{.*}}  ab 80 8f c0  	sra	$a1, $a7, 15
	rotr	$a1, $s2, 31	# CHECK: rotr $a1, $s2, 31	# encoding: [0xb2,0x80,0xdf,0xc0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTR_NM
				# DISAS: {{.*}}  b2 80 df c0  	rotr	$a1, $s2, 31
	ext	$a1, $s2, 0, 1	# CHECK: ext $a1, $s2, 0, 1	# encoding: [0xb2,0x80,0x00,0xf0]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
				# DISAS: {{.*}}  b2 80 00 f0  	ext	$a1, $s2, 0, 1
	ext	$a1, $s2, 16, 15	# CHECK: ext $a1, $s2, 16, 15	# encoding: [0xb2,0x80,0x90,0xf3]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
				# DISAS: {{.*}}  b2 80 90 f3  	ext	$a1, $s2, 16, 15
	ext	$a1, $s2, 0, 32	# CHECK: ext $a1, $s2, 0, 32	# encoding: [0xb2,0x80,0xc0,0xf7]
				# CHECK-NEXT: # <MCInst #{{.*}} EXT_NM
				# DISAS: {{.*}}  b2 80 c0 f7  	ext	$a1, $s2, 0, 32
	ins	$a1, $s2, 31, 1	# CHECK: ins $a1, $s2, 31, 1	# encoding: [0xb2,0x80,0xdf,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM
				# DISAS: {{.*}}  b2 80 df e7  	ins 	$a1, $s2, 31, 1
	ins	$a1, $s2, 15, 16	# CHECK: ins $a1, $s2, 15, 16	# encoding: [0xb2,0x80,0x8f,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM
				# DISAS: {{.*}}  b2 80 8f e7  	ins 	$a1, $s2, 15, 16
	ins	$a1, $s2, 2, 30	# CHECK: ins $a1, $s2, 2, 30	# encoding: [0xb2,0x80,0xc2,0xe7]
				# CHECK-NEXT: # <MCInst #{{.*}} INS_NM
				# DISAS: {{.*}}  b2 80 c2 e7  	ins 	$a1, $s2, 2, 30
	
	extw	$a0, $a1, $a2, 2	# CHECK: extw $a0, $a1, $a2, 2 # encoding: [0xc5,0x20,0x9f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
				# DISAS: {{.*}}  c5 20 9f 20  	extw	$a0, $a1, $a2, 2
	extw	$a0, $s1, $t5, 0	# CHECK: extw $a0, $s1, $t5, 0	# encoding: [0x71,0x20,0x1f,0x20]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
				# DISAS: {{.*}}  71 20 1f 20  	extw	$a0, $s1, $t5, 0
	extw	$s1, $k1, $gp, 16	# CHECK: extw $s1, $k1, $gp, 16	# encoding: [0x9b,0x23,0x1f,0x8c]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
				# DISAS: {{.*}}  9b 23 1f 8c  	extw	$s1, $k1, $gp, 16
	extw	$a7, $s7, $t4, 31	# CHECK: extw $a7, $s7, $t4, 31	# encoding: [0x57,0x20,0xdf,0x5f]
				# CHECK-NEXT: # <MCInst #{{.*}} EXTW_NM
				# DISAS: {{.*}}  57 20 df 5f  	extw	$a7, $s7, $t4, 31
	rotx	$a1, $s3, 0, 0, 0	# CHECK: rotx $a1, $s3, 0, 0, 0	# encoding: [0xb3,0x80,0x00,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  b3 80 00 d0  	rotx	$a1, $s3, 0, 0, 0
	rotx	$a7, $s4, 1, 2, 1	# CHECK: rotx $a7, $s4, 1, 2, 1	# encoding: [0x74,0x81,0xc1,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  74 81 c1 d0  	rotx	$a7, $s4, 1, 2, 1
	rotx	$t8, $t0, 31, 6, 1	# CHECK: rotx $t8, $t0, 31, 6, 1 # encoding: [0x0c,0x83,0xdf,0xd1]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  0c 83 df d1  	rotx	$t8, $t0, 31, 6, 1
	rotx	$a1, $s3, 3, 16, 0	# CHECK: rotx $a1, $s3, 3, 16, 0 # encoding: [0xb3,0x80,0x03,0xd4]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  b3 80 03 d4  	rotx	$a1, $s3, 3, 16, 0
	rotx	$a7, $s4, 16, 8, 0	# CHECK: rotx $a7, $s4, 16, 8, 0 # encoding: [0x74,0x81,0x10,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  74 81 10 d2  	rotx	$a7, $s4, 16, 8, 0
	rotx	$t8, $t0, 25, 30, 1	# CHECK: rotx $t8, $t0, 25, 30, 1 # encoding: [0x0c,0x83,0xd9,0xd7]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  0c 83 d9 d7  	rotx	$t8, $t0, 25, 30, 1
	bitrevb $a0, $s0	# CHECK: bitrevb $a0, $s0 # encoding: [0x90,0x80,0x47,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  90 80 47 d2  	bitrevb	$a0, $s0
	bitrevh $a1, $s1	# CHECK: bitrevh $a1, $s1 # encoding: [0xb1,0x80,0x0f,0xd4]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  b1 80 0f d4  	bitrevh	$a1, $s1
	bitrevw $t0, $t3	# CHECK: bitrevw $t0, $t3 # encoding: [0x8f,0x81,0x1f,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} BITREVW_NM
				# DISAS: {{.*}}  8f 81 1f d0  	bitrevw	$t0, $t3
	byterevh $a3, $s3	# CHECK: byterevh $a3, $s3 # encoding: [0xf3,0x80,0x08,0xd6]
				# CHECK-NEXT: # <MCInst #{{.*}} ROTX_NM
				# DISAS: {{.*}}  f3 80 08 d6  	byterevh	$a3, $s3
	byterevw $t0, $k0	# CHECK: byterevw $t0, $k0 # encoding: [0x9a,0x81,0x18,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} BYTEREVW_NM
				# DISAS: {{.*}}  9a 81 18 d2  	byterevw	$t0, $k0
	
	# compare and trap, 32-bit
	teq	$s2, $a3, 0	# CHECK: teq $s2, $a3, 0	# encoding: [0xf2,0x20,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} TEQ_NM
				# DISAS: {{.*}}  f2 20 00 00  	teq	$s2, $a3, 0
	teq	$s2, $a3, 31	# CHECK: teq $s2, $a3, 31	# encoding: [0xf2,0x20,0x00,0xf8]
				# CHECK-NEXT: # <MCInst #{{.*}} TEQ_NM
				# DISAS: {{.*}}  f2 20 00 f8  	teq	$s2, $a3, 31
	tne	$s2, $a3, 15	# CHECK: tne $s2, $a3, 15	# encoding: [0xf2,0x20,0x00,0x7c]
				# CHECK-NEXT: # <MCInst #{{.*}} TNE_NM
				# DISAS: {{.*}}  f2 20 00 7c  	tne	$s2, $a3, 15
	tne	$s2, $a3, 1	# CHECK: tne $s2, $a3, 1	# encoding: [0xf2,0x20,0x00,0x0c]
				# CHECK-NEXT: # <MCInst #{{.*}} TNE_NM
				# DISAS: {{.*}}  f2 20 00 0c  	tne	$s2, $a3, 1
	
	# single and paired moves
	move	$a4, $a7	# CHECK: move $a4, $a7	# encoding: [0x0b,0x11]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
				# DISAS: {{.*}}  0b 11        	move	$a4, $a7
	move	$s0, $k0	# CHECK: move $s0, $k0	# encoding: [0x1a,0x12]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
				# DISAS: {{.*}}  1a 12        	move	$s0, $k0
	move	$t4, $gp	# CHECK: move $t4, $gp	# encoding: [0x5c,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVE_NM
				# DISAS: {{.*}}  5c 10        	move	$t4, $gp
	movep	$s0, $s1, $a0, $a1	# CHECK: movep $s0, $s1, $a0, $a1	# encoding: [0x30,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  30 fe        	movep	$s0, $s1, $a0, $a1
	movep	$a7, $s7, $a1, $a2	# CHECK: movep $a7, $s7, $a1, $a2	# encoding: [0xe3,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  e3 ff        	movep	$a7, $s7, $a1, $a2
	movep	$a1, $s3, $a2, $a3	# CHECK: movep $a1, $s3, $a2, $a3	# encoding: [0x6d,0xfe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  6d fe        	movep	$a1, $s3, $a2, $a3
	movep	$a6, $s0, $a3, $a4	# CHECK: movep $a6, $s0, $a3, $a4	# encoding: [0x0a,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEPREV_NM
				# DISAS: {{.*}}  0a ff        	movep	$a6, $s0, $a3, $a4
	movep	$a0, $a1, $s0, $s1	# CHECK: movep $a0, $a1, $s0, $s1	# encoding: [0x30,0xbe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
				# DISAS: {{.*}}  30 be        	movep	$a0, $a1, $s0, $s1
	movep	$a1, $a2, $zero, $s7	# CHECK: movep $a1, $a2, $zero, $s7	# encoding: [0xe3,0xbf]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
				# DISAS: {{.*}}  e3 bf        	movep	$a1, $a2, $zero, $s7
	movep	$a2, $a3, $a1, $s3	# CHECK: movep $a2, $a3, $a1, $s3	# encoding: [0x6d,0xbe]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
				# DISAS: {{.*}}  6d be        	movep	$a2, $a3, $a1, $s3
	movep	$a3, $a4, $a6, $s0	# CHECK: movep $a3, $a4, $a6, $s0	# encoding: [0x0a,0xbf]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVEP_NM
				# DISAS: {{.*}}  0a bf        	movep	$a3, $a4, $a6, $s0
	
	li	$a0, 1		# CHECK: li $a0, 1	# encoding: [0x01,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  01 d2        	li	$a0, 1
	li	$a1, 64		# CHECK: li $a1, 64	# encoding: [0xc0,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  c0 d2        	li	$a1, 64
	li	$a3, 0		# CHECK: li $a3, 0	# encoding: [0x80,0xd3]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  80 d3        	li	$a3, 0
	li	$s0, 126	# CHECK: li $s0, 126	# encoding: [0x7e,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  7e d0        	li	$s0, 126
	li	$s1, -1		# CHECK: li $s1, -1	# encoding: [0xff,0xd0]
				# CHECK-NEXT: # <MCInst #{{.*}} LI16_NM
				# DISAS: {{.*}}  ff d0        	li	$s1, -1
	li	$a2, 127	# CHECK: li $a2, 127	# encoding: [0xc0,0x00,0x7f,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  c0 00 7f 00  	li	$a2, 127
	li	$a5, 65535	# CHECK: li $a5, 65535	# encoding: [0x20,0x01,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  20 01 ff ff  	li	$a5, 65535
	li	$s2, -2		# CHECK: li $s2, -2	# encoding: [0x40,0x82,0x02,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  40 82 02 80  	li	$s2, -2
	li	$s7, -4095	# CHECK: li $s7, -4095	# encoding: [0xe0,0x82,0xff,0x8f]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  e0 82 ff 8f  	li	$s7, -4095
	li	$t0, 65537	# CHECK: li $t0, 0x10001	# encoding: [0x80,0x61,0x01,0x00,0x01,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  80 61 01 00 01 00    	li	$t0, 0x10001
	li	$t1, 2147483647	# CHECK: li $t1,  0x7fffffff	# encoding: [0xa0,0x61,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  a0 61 ff ff ff 7f    	li	$t1, 0x7fffffff
	li	$t4, -4097	# CHECK: li $t4, 0xffffefff	# encoding: [0x40,0x60,0xff,0xef,0xff,0xff]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  40 60 ff ef ff ff    	li	$t4, 0xffffefff
	li[48]	$t5, -2147483647 # CHECK: li $t5, 0x80000001	# encoding: [0x60,0x60,0x01,0x00,0x00,0x80]
				 # CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				 # DISAS: {{.*}}  60 60 01 00 00 80    	li	$t5, 0x80000001
	li	$a0, foo	# CHECK: li $a0, foo	# encoding: [0x80,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: foo, kind: fixup_NANOMIPS_I32
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
				# DISAS: {{.*}}  80 60 00 00 00 00    	li	$a0, 0
	
	movn	$a1, $s2, $t4	# CHECK: movn $a1, $s2, $t4	# encoding: [0x52,0x20,0x10,0x2e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
				# DISAS: {{.*}}  52 20 10 2e  	movn	$a1, $s2, $t4
	movn	$a3, $s7, $t8	# CHECK: movn $a3, $s7, $t8	# encoding: [0x17,0x23,0x10,0x3e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
				# DISAS: {{.*}}  17 23 10 3e  	movn	$a3, $s7, $t8
	movn	$at, $t9, $s7	# CHECK: movn $at, $t9, $s7	# encoding: [0xf9,0x22,0x10,0x0e]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVN_NM
				# DISAS: {{.*}}  f9 22 10 0e  	movn	$at, $t9, $s7
	movz	$s1, $a2, $gp	# CHECK: movz $s1, $a2, $gp	# encoding: [0x86,0x23,0x10,0x8a]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM
				# DISAS: {{.*}}  86 23 10 8a  	movz	$s1, $a2, $gp
	movz	$s3, $a7, $sp	# CHECK: movz $s3, $a7, $sp	# encoding: [0xab,0x23,0x10,0x9a]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM
				# DISAS: {{.*}}  ab 23 10 9a  	movz	$s3, $a7, $sp
	movz	$s7, $k0, $fp	# CHECK: movz $s7, $k0, $fp	# encoding: [0xda,0x23,0x10,0xba]
				# CHECK-NEXT: # <MCInst #{{.*}} MOVZ_NM
				# DISAS: {{.*}}  da 23 10 ba  	movz	$s7, $k0, $fp
	
	clo	$a0, $a1	# CHECK: clo $a0, $a1	# encoding: [0x85,0x20,0x3f,0x4b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLO_NM
				# DISAS: {{.*}}  85 20 3f 4b  	clo	$a0, $a1
	clo	$t0, $s1	# CHECK: clo $t0, $s1	# encoding: [0x91,0x21,0x3f,0x4b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLO_NM
				# DISAS: {{.*}}  91 21 3f 4b  	clo	$t0, $s1
	clz	$k0, $s7	# CHECK: clz $k0, $s7	# encoding: [0x57,0x23,0x3f,0x5b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLZ_NM
				# DISAS: {{.*}}  57 23 3f 5b  	clz	$k0, $s7
	clz	$s4, $a0	# CHECK: clz $s4, $a0	# encoding: [0x84,0x22,0x3f,0x5b]
				# CHECK-NEXT: # <MCInst #{{.*}} CLZ_NM
				# DISAS: {{.*}}  84 22 3f 5b  	clz	$s4, $a0
	
	seb	$a0, $a1	# CHECK: seb $a0, $a1	# encoding: [0x85,0x20,0x08,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEB_NM
				# DISAS: {{.*}}  85 20 08 00  	seb	$a0, $a1
	seb	$t0, $s1	# CHECK: seb $t0, $s1	# encoding: [0x91,0x21,0x08,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEB_NM
				# DISAS: {{.*}}  91 21 08 00  	seb	$t0, $s1
	seh	$k0, $s7	# CHECK: seh $k0, $s7	# encoding: [0x57,0x23,0x48,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEH_NM
				# DISAS: {{.*}}  57 23 48 00  	seh	$k0, $s7
	seh	$s4, $a0	# CHECK: seh $s4, $a0	# encoding: [0x84,0x22,0x48,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} SEH_NM
				# DISAS: {{.*}}  84 22 48 00  	seh	$s4, $a0
	
	lsa $a0, $a1, $a4, 1	# CHECK: lsa $a0, $a1, $a4, 1	# encoding: [0x05,0x21,0x0f,0x22]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
				# DISAS: {{.*}}  05 21 0f 22  	lsa	$a0, $a1, $a4, 1
	lsa $s0, $s1, $s4, 2	# CHECK: lsa $s0, $s1, $s4, 2	# encoding: [0x91,0x22,0x0f,0x84] 
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
				# DISAS: {{.*}}  91 22 0f 84  	lsa	$s0, $s1, $s4, 2
	lsa $s3, $a7, $a4, 3	# CHECK: lsa $s3, $a7, $a4, 3	# encoding: [0x0b,0x21,0x0f,0x9e]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
				# DISAS: {{.*}}  0b 21 0f 9e  	lsa	$s3, $a7, $a4, 3
	lsa $t4, $a3, $s4, 0	# CHECK: lsa $t4, $a3, $s4, 0	# encoding: [0x87,0x22,0x0f,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} LSA_NM
				# DISAS: {{.*}}  87 22 0f 10  	lsa	$t4, $a3, $s4, 0
	
	di		# CHECK: di # encoding: [0x00,0x20,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
			# DISAS: {{.*}}  00 20 7f 47  	di
	di $a0		# CHECK: di $a0	# encoding: [0x80,0x20,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
			# DISAS: {{.*}}  80 20 7f 47  	di	$a0
	di $s7		# CHECK: di $s7	# encoding: [0xe0,0x22,0x7f,0x47]
			# CHECK-NEXT: # <MCInst #{{.*}} DI_NM
			# DISAS: {{.*}}  e0 22 7f 47  	di	$s7
	ei		# CHECK: ei # encoding: [0x00,0x20,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
			# DISAS: {{.*}}  00 20 7f 57  	ei
	ei $ra		# CHECK: ei $ra	# encoding: [0xe0,0x23,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
			# DISAS: {{.*}}  e0 23 7f 57  	ei	$ra
	ei $t0		# CHECK: ei $t0	# encoding: [0x80,0x21,0x7f,0x57]
			# CHECK-NEXT: # <MCInst #{{.*}} EI_NM
			# DISAS: {{.*}}  80 21 7f 57  	ei	$t0
	eret		# CHECK: eret	# encoding: [0x00,0x20,0x7f,0xf3]
			# CHECK-NEXT: # <MCInst #{{.*}} ERET_NM
			# DISAS: {{.*}}  00 20 7f f3  	eret
	eretnc		# CHECK: eretnc	# encoding: [0x01,0x20,0x7f,0xf3]
			# CHECK-NEXT: # <MCInst #{{.*}} ERETNC_NM
			# DISAS: {{.*}}  01 20 7f f3  	eretnc
	deret		# CHECK: deret	# encoding: [0x00,0x20,0x7f,0xe3]
			# CHECK-NEXT: # <MCInst #{{.*}} DERET_NM
			# DISAS: {{.*}}  00 20 7f e3  	deret
	nop32		# CHECK: nop32	# encoding: [0x00,0x80,0x00,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} NOP32_NM
			# DISAS: {{.*}}  00 80 00 c0  	nop32
	pause		# CHECK: pause	# encoding: [0x00,0x80,0x05,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} PAUSE_NM
			# DISAS: {{.*}}  00 80 05 c0  	pause
	ehb		# CHECK: ehb	# encoding: [0x00,0x80,0x03,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} EHB_NM
			# DISAS: {{.*}}  00 80 03 c0  	ehb
	
	sigrie 0	# CHECK: sigrie	0x0 # encoding: [0x00,0x00,0x00,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
			# DISAS: {{.*}}  00 00 00 00  	sigrie	0x0
	sigrie 0xffff	# CHECK: sigrie	0xffff # encoding: [0x00,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
			# DISAS: {{.*}}  00 00 ff ff  	sigrie	0xffff
	sigrie 0x7ffff	# CHECK: sigrie	0x7ffff # encoding: [0x07,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SIGRIE_NM
			# DISAS: {{.*}}  07 00 ff ff  	sigrie	0x7ffff
	sdbbp		# CHECK: sdbbp	0x0 # encoding: [0x18,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
			# DISAS: {{.*}}  18 10        	sdbbp 0x0
	sdbbp 0		# CHECK: sdbbp	0x0 # encoding: [0x18,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
			# DISAS: {{.*}}  18 10        	sdbbp 0x0
	sdbbp 7		# CHECK: sdbbp	0x7 # encoding: [0x1f,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP16_NM
			# DISAS: {{.*}}  1f 10        	sdbbp	0x7
	sdbbp 8		# CHECK: sdbbp	0x8 # encoding: [0x18,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
			# DISAS: {{.*}}  18 00 08 00  	sdbbp	0x8
	sdbbp 0x7ffff	# CHECK: sdbbp	0x7ffff # encoding: [0x1f,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SDBBP_NM
			# DISAS: {{.*}}  1f 00 ff ff  	sdbbp	0x7ffff
	break		# CHECK: break	0x0 # encoding: [0x10,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
			# DISAS: {{.*}}  10 10        	break	0x0
	break 0		# CHECK: break	0x0 # encoding: [0x10,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
			# DISAS: {{.*}}  10 10        	break	0x0
	break 7		# CHECK: break	0x7 # encoding: [0x17,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK16_NM
			# DISAS: {{.*}}  17 10        	break	0x7
	break 8		# CHECK: break	0x8 # encoding: [0x10,0x00,0x08,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
			# DISAS: {{.*}}  10 00 08 00  	break	0x8
	break 0x7ffff	# CHECK: break	0x7ffff # encoding: [0x17,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} BREAK_NM
			# DISAS: {{.*}}  17 00 ff ff  	break	0x7ffff
	syscall 	# CHECK: syscall 0x0 # encoding: [0x08,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
			# DISAS: {{.*}}  08 10        	syscall	0x0
	syscall	0	# CHECK: syscall 0x0 # encoding: [0x08,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
			# DISAS: {{.*}}  08 10        	syscall	0x0
	syscall 3	# CHECK: syscall 0x3 # encoding: [0x0b,0x10]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL16_NM
			# DISAS: {{.*}}  0b 10        	syscall	0x3
	syscall 4	# CHECK: syscall 0x4 # encoding: [0x08,0x00,0x04,0x00]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
			# DISAS: {{.*}}  08 00 04 00  	syscall	0x4
	syscall 0x3ffff	# CHECK: syscall 0x3ffff # encoding: [0x0b,0x00,0xff,0xff]
			# CHECK-NEXT: # <MCInst #{{.*}} SYSCALL_NM
			# DISAS: {{.*}}  0b 00 ff ff  	syscall	0x3ffff
	
	rdpgpr $a3, $s7	# CHECK: rdpgpr $a3, $s7 # encoding: [0xf7,0x20,0x7f,0xe1]
			# CHECK-NEXT: # <MCInst #{{.*}} RDPGPR_NM
			# DISAS: {{.*}}  f7 20 7f e1  	rdpgpr	$a3, $s7
	rdpgpr $fp, $sp	# CHECK: rdpgpr $fp, $sp # encoding: [0xdd,0x23,0x7f,0xe1]
			# CHECK-NEXT: # <MCInst #{{.*}} RDPGPR_NM
			# DISAS: {{.*}}  dd 23 7f e1  	rdpgpr	$fp, $sp
	wrpgpr $t0, $k0	# CHECK: wrpgpr $t0, $k0 # encoding: [0x9a,0x21,0x7f,0xf1]
			# CHECK-NEXT: # <MCInst #{{.*}} WRPGPR_NM
			# DISAS: {{.*}}  9a 21 7f f1  	wrpgpr	$t0, $k0
	wrpgpr $a0, $ra	# CHECK: wrpgpr $a0, $ra # encoding: [0x9f,0x20,0x7f,0xf1]
			# CHECK-NEXT: # <MCInst #{{.*}} WRPGPR_NM
			# DISAS: {{.*}}  9f 20 7f f1  	wrpgpr	$a0, $ra
	
	rdhwr	$a0, $0, 31	# CHECK: rdhwr $a0, $0, 31 # encoding: [0x80,0x20,0xc0,0xf9]
				# CHECK-NEXT: # <MCInst #{{.*}} RDHWR_NM
				# DISAS: {{.*}}  80 20 c0 f9  	rdhwr	$a0, $0, 31
	rdhwr	$s0, $16, 16	# CHECK: rdhwr $s0, $16, 16 # encoding: [0x10,0x22,0xc0,0x81]
				# CHECK-NEXT: # <MCInst #{{.*}} RDHWR_NM
				# DISAS: {{.*}}  10 22 c0 81  	rdhwr	$s0, $16, 16
	rdhwr	$a4, $31, 0	# CHECK: rdhwr $a4, $31, 0 # encoding: [0x1f,0x21,0xc0,0x01]
				# CHECK-NEXT: # <MCInst #{{.*}} RDHWR_NM
				# DISAS: {{.*}}  1f 21 c0 01  	rdhwr	$a4, $31, 0
	
	lwm	$a1, 0($a3), 1	# CHECK: lwm $a1, 0($a3), 1 # encoding: [0xa7,0xa4,0x00,0x14]
				# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
				# DISAS: {{.*}}  a7 a4 00 14  	lwm	$a1, 0($a3), 1
	lwm	$a7, 252($s7), 8	# CHECK: lwm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x04]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
					# DISAS: {{.*}}  77 a5 fc 04  	lwm	$a7, 252($s7), 8
	lwm	$t0, -256($gp), 8	# CHECK: lwm $t0, -256($gp), 8 # encoding: [0x9c,0xa5,0x00,0x84]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
					# DISAS: {{.*}}  9c a5 00 84  	lwm	$t0, -256($gp), 8
	lwm	$a2, 128($a4), 4	# CHECK: lwm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x44]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
					# DISAS: {{.*}}  c8 a4 80 44  	lwm	$a2, 128($a4), 4
	lwm	$a6, -128($s6), 3	# CHECK: lwm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xb4]
					# CHECK-NEXT: # <MCInst #{{.*}} LWM_NM
					# DISAS: {{.*}}  56 a5 80 b4  	lwm	$a6, -128($s6), 3
	
	swm	$a7, 252($s7), 8	# CHECK: swm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x0c]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
					# DISAS: {{.*}}  77 a5 fc 0c  	swm	$a7, 252($s7), 8
	swm	$t0, -256($gp), 7	# CHECK: swm $t0, -256($gp), 7 # encoding: [0x9c,0xa5,0x00,0xfc]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
					# DISAS: {{.*}}  9c a5 00 fc  	swm	$t0, -256($gp), 7
	swm	$a2, 128($a4), 4	# CHECK: swm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x4c]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
					# DISAS: {{.*}}  c8 a4 80 4c  	swm	$a2, 128($a4), 4
	swm	$a6, -128($s6), 3	# CHECK: swm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xbc]
					# CHECK-NEXT: # <MCInst #{{.*}} SWM_NM
					# DISAS: {{.*}}  56 a5 80 bc  	swm	$a6, -128($s6), 3
	
	ualwm	$a1, 0($a3), 1		# CHECK: ualwm $a1, 0($a3), 1 # encoding: [0xa7,0xa4,0x00,0x15]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
					# DISAS: {{.*}}  a7 a4 00 15  	ualw	$7, 5376($5)
	ualwm	$a7, 252($s7), 8	# CHECK: ualwm $a7, 252($s7), 8 # encoding: [0x77,0xa5,0xfc,0x05]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
					# DISAS: {{.*}}  77 a5 fc 05  	ualwm	$a7, 252($s7), 8
	ualwm	$t0, -256($gp), 8	# CHECK: ualwm $t0, -256($gp), 8 # encoding: [0x9c,0xa5,0x00,0x85]
					# CHECK-NEXT: # <MCInst #{{.*}} UALWM_NM
					# DISAS: {{.*}}  9c a5 00 85  	ualwm	$t0, -256($gp), 8
	uaswm	$a2, 128($a4), 4	# CHECK: uaswm $a2, 128($a4), 4 # encoding: [0xc8,0xa4,0x80,0x4d]
					# CHECK-NEXT: # <MCInst #{{.*}} UASWM_NM
					# DISAS: {{.*}}  c8 a4 80 4d  	uaswm	$a2, 128($a4), 4
	uaswm	$a6, -128($s6), 3	# CHECK: uaswm $a6, -128($s6), 3 # encoding: [0x56,0xa5,0x80,0xbd]
					# CHECK-NEXT: # <MCInst #{{.*}} UASWM_NM
					# DISAS: {{.*}}  56 a5 80 bd  	uaswm	$a6, -128($s6), 3
	
	ualw	$a1,0($a3)	# CHECK: ualw $a1, 0($a3) # encoding: [0xa7,0xa4,0x00,0x15]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM
				# DISAS: {{.*}}  a7 a4 00 15  	ualw	$7, 5376($5)
	uasw	$a7,252($s7)	# CHECK: uasw $a7, 252($s7) # encoding: [0x77,0xa5,0xfc,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} UASW_NM
				# DISAS: {{.*}}  77 a5 fc 1d  	uasw	$23, 7676($11)
	ualw	$t0,-256($gp)	# CHECK: ualw $t0, -256($gp) # encoding: [0x9c,0xa5,0x00,0x95]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM
				# DISAS: {{.*}}  9c a5 00 95  	ualw	$gp, -27392($12)
	ualw	$a2,128($a4)	# CHECK: ualw $a2, 128($a4) # encoding: [0xc8,0xa4,0x80,0x15]
				# CHECK-NEXT: # <MCInst #{{.*}} UALW_NM
				# DISAS: {{.*}}  c8 a4 80 15  	ualw	$8, 5504($6)

	# 16-bit SAVE/RESTORE[.JRC]
	save	16		# CHECK: save 16, 0 # encoding: [0x10,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  10 1c        	save	16, $fp
	save	128		# CHECK: save 128, 0 # encoding: [0x80,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  80 1c        	save	128, $fp
	save	240		# CHECK: save 240, 0 # encoding: [0xf0,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  f0 1c        	save	240, $fp
	save	32, $ra		# CHECK: save 32, $ra # encoding: [0x21,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  21 1e        	save	32, $ra
	save	32, $fp, $ra	# CHECK: save 32, $fp, $ra # encoding: [0x22,0x1c]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  22 1c        	save	32, $fp, $ra
	save	32, $ra, $s0	# CHECK: save 32, $ra, $s0  # encoding: [0x22,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  22 1e        	save	32, $ra, $s0
	save	192, $ra, $s0-$gp	# CHECK: save 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xce,0x1e]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  ce 1e        	save	192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	save	192, $fp, $ra, $s0-$gp	# CHECK: save 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x1c]
					# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
				# DISAS: {{.*}}  cf 1c        	save	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	save	128, $fp, $ra, $s0-$k1, $gp	# CHECK: save 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0x8f,0x1c]
					# CHECK-NEXT: # <MCInst #{{.*}} SAVE16_NM
					# DISAS: {{.*}}  8f 1c        	save	128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	restore.jrc	16	# CHECK: restore.jrc 16, 0	# encoding: [0x10,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  10 1d        	restore.jrc	16, $fp
	restore.jrc	128	# CHECK: restore.jrc 128, 0	# encoding: [0x80,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  80 1d        	restore.jrc	128, $fp
	restore.jrc	240	# CHECK: restore.jrc 240, 0	# encoding: [0xf0,0x1d]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  f0 1d        	restore.jrc	240, $fp
	restore.jrc	32, $ra	# CHECK: restore.jrc 32, $ra	# encoding: [0x21,0x1f]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
				# DISAS: {{.*}}  21 1f        	restore.jrc	32, $ra
	restore.jrc	32, $fp, $ra	# CHECK: restore.jrc 32, $fp, $ra	# encoding: [0x22,0x1d]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
					# DISAS: {{.*}}  22 1d        	restore.jrc	32, $fp, $ra
	restore.jrc	32, $ra, $s0	# CHECK: restore.jrc 32, $ra, $s0	# encoding: [0x22,0x1f]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  22 1f        	restore.jrc	32, $ra, $s0
	restore.jrc	32, $ra, $s0-$s3	# CHECK: restore.jrc 32, $ra, $s0, $s1, $s2, $s3 # encoding: [0x25,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  25 1f        	restore.jrc	32, $ra, $s0, $s1, $s2, $s3
	restore.jrc	64, $fp, $ra, $s0-$s7	# CHECK: restore.jrc 64, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7 # encoding: [0x4a,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  4a 1d        	restore.jrc	64, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7
	restore.jrc	192, $ra, $s0-$gp	# CHECK: restore.jrc 192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xce,0x1f]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  ce 1f        	restore.jrc	192, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	restore.jrc	192, $fp, $ra, $s0-$gp	# CHECK: restore.jrc 192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  cf 1d        	restore.jrc	192, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp
	restore.jrc	128, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0x8f,0x1d]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC16_NM
						# DISAS: {{.*}}  8f 1d        	restore.jrc	128, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	# 32-bit SAVE/RESTORE[.JRC]
	save	40, $ra		# CHECK: save 40, $ra	# encoding: [0xe1,0x83,0x28,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e1 83 28 30  	save	40, $ra
	save	256, $fp, $ra	# CHECK: save 256, $fp, $ra # encoding: [0xc2,0x83,0x00,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  c2 83 00 31  	save	256, $fp, $ra
	save	1024, $ra, $s0	# CHECK: save 1024, $ra, $s0 # encoding: [0xe2,0x83,0x00,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e2 83 00 34  	save	1024, $ra, $s0
	save	160, $zero, $gp	# CHECK: save 160, $zero, $gp # encoding: [0x02,0x80,0xa4,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  02 80 a4 30  	save	160, $zero, $gp
	save	64, $s0-$s3, $gp # CHECK: save 64, $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  05 82 44 30  	save	64, $s0, $s1, $s2, $s3, $gp
	save	64, $a0-$s0, $gp # CHECK: save 64, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $gp # encoding: [0x8e,0x80,0x44,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  8e 80 44 30  	save	64, $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $gp
	save	128, $t3, $s0, $s1, $gp # CHECK: save 128, $t3, $s0, $s1, $gp # encoding: [0xe4,0x81,0x84,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  e4 81 84 30  	save	128, $t3, $s0, $s1, $gp
	save	128, $a4-$t3, $s0-$s3 # CHECK: save 128, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $s1, $s2, $s3 # encoding: [0x0c,0x81,0x80,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} SAVE_NM
				# DISAS: {{.*}}  0c 81 80 30  	save	128, $a4, $a5, $a6, $a7, $t0, $t1, $t2, $t3, $s0, $s1, $s2, $s3
	restore	48, $ra		# CHECK: restore 48, $ra	# encoding: [0xe1,0x83,0x32,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  e1 83 32 30  	restore	48, $ra
	restore	256, $fp, $ra	# CHECK: restore 256, $fp, $ra	# encoding: [0xc2,0x83,0x02,0x31]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  c2 83 02 31  	restore	256, $fp, $ra
	restore	1024, $ra, $s0	# CHECK: restore 1024, $ra, $s0	# encoding: [0xe2,0x83,0x02,0x34]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  e2 83 02 34  	restore	1024, $ra, $s0
	restore	160, $zero, $gp	# CHECK: restore 160, $zero, $gp # encoding: [0x02,0x80,0xa6,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
				# DISAS: {{.*}}  02 80 a6 30  	restore	160, $zero, $gp
	restore	64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore 64,  $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x46,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  05 82 46 30  	restore	64, $s0, $s1, $s2, $s3, $gp
	restore	64, $t4, $t5, $gp	# CHECK: restore 64, $t4, $t5, $gp # encoding: [0x43,0x80,0x46,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
					# DISAS: {{.*}}  43 80 46 30  	restore	64, $t4, $t5, $gp
	restore	128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore 128, $fp, $ra, $s0, $s1, $gp # encoding: [0xc5,0x83,0x86,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  c5 83 86 30  	restore	128, $fp, $ra, $s0, $s1, $gp
	restore	120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x83,0x7e,0x30]
						# CHECK-NEXT: # <MCInst #{{.*}} RESTORE_NM
						# DISAS: {{.*}}  cf 83 7e 30  	restore	120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	restore.jrc	56, $ra	# CHECK: restore.jrc 56, $ra	# encoding: [0xe1,0x83,0x3b,0x30]
				# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
				# DISAS: {{.*}}  e1 83 3b 30  	restore.jrc	56, $ra
	restore.jrc	256, $fp, $ra	# CHECK: restore.jrc 256, $fp, $ra	# encoding: [0xc2,0x83,0x03,0x31]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  c2 83 03 31  	restore.jrc	256, $fp, $ra
	restore.jrc	1024, $ra, $s0	# CHECK: restore.jrc 1024, $ra, $s0	# encoding: [0xe2,0x83,0x03,0x34]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  e2 83 03 34  	restore.jrc	1024, $ra, $s0
	restore.jrc	160, $zero, $gp	# CHECK: restore.jrc 160, $zero, $gp	# encoding: [0x02,0x80,0xa7,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  02 80 a7 30  	restore.jrc	160, $zero, $gp
	restore.jrc	64, $s0, $s1, $s2, $s3, $gp	# CHECK: restore.jrc 64, $s0, $s1, $s2, $s3, $gp # encoding: [0x05,0x82,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  05 82 47 30  	restore.jrc	64, $s0, $s1, $s2, $s3, $gp
	restore.jrc	64, $t4, $t5, $gp	# CHECK: restore.jrc 64, $t4, $t5, $gp # encoding: [0x43,0x80,0x47,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  43 80 47 30  	restore.jrc	64, $t4, $t5, $gp
	restore.jrc	128, $fp, $ra, $s0, $s1, $gp	# CHECK: restore.jrc 128, $fp, $ra, $s0, $s1, $gp # encoding: [0xc5,0x83,0x87,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  c5 83 87 30  	restore.jrc	128, $fp, $ra, $s0, $s1, $gp
	restore.jrc	120, $fp, $ra, $s0-$k1, $gp	# CHECK: restore.jrc 120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp # encoding: [0xcf,0x83,0x7f,0x30]
					# CHECK-NEXT: # <MCInst #{{.*}} RESTOREJRC_NM
					# DISAS: {{.*}}  cf 83 7f 30  	restore.jrc	120, $fp, $ra, $s0, $s1, $s2, $s3, $s4, $s5, $s6, $s7, $t8, $t9, $k0, $k1, $gp

	addiu	$a1,$a1,8	# CHECK: addiu	$a1, $a1, 8 # encoding: [0xd2,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  d2 92        	addiu	$a1, $a1, 8
	addiu	$a1,$a3,0	# CHECK: addiu	$a1, $a3, 0 # encoding: [0xf0,0x92]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  f0 92        	addiu	$a1, $a3, 0
	addiu	$s1,$a1,28	# CHECK: addiu	$s1, $a1, 28 # encoding: [0xd7,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR2_NM
				# DISAS: {{.*}}  d7 90        	addiu	$s1, $a1, 28
	addiu[32] $s1,$a1,32	# CHECK: addiu	$s1, $a1, 32 # encoding: [0x25,0x02,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  25 02 20 00  	addiu	$s1, $a1, 32
	addiu	$a1,$a1,65535	# CHECK: addiu	$a1, $a1, 65535 # encoding: [0xa5,0x00,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  a5 00 ff ff  	addiu	$a1, $a1, 65535
	addiu	$s2,$s2,65536	# CHECK: addiu	$s2, $s2, 65536 # encoding: [0x41,0x62,0x00,0x00,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  41 62 00 00 01 00    	addiu	$s2, $s2, 65536
	addiu	$t4,$t4,0x7fffffff	# CHECK: addiu	$t4, $t4, 2147483647 # encoding: [0x41,0x60,0xff,0xff,0xff,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  41 60 ff ff ff 7f    	addiu	$t4, $t4, 2147483647
	addiu	$a4,$a4,0	# CHECK: addiu	$a4, $a4, 0 # encoding: [0x08,0x91]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  08 91        	addiu	$a4, $a4, 0
	addiu	$t8,$t8,7	# CHECK: addiu	$t8, $t8, 7 # encoding: [0x0f,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  0f 93        	addiu	$t8, $t8, 7
	addiu	$sp,$sp,-8	# CHECK: addiu	$sp, $sp, -8 # encoding: [0xb8,0x93]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIURS5_NM
				# DISAS: {{.*}}  b8 93        	addiu	$sp, $sp, -8
	addiu	$k1,$k1,8	# CHECK: addiu	$k1, $k1, 8 # encoding: [0x7b,0x03,0x08,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  7b 03 08 00  	addiu	$k1, $k1, 8
	addiu	$k1,$k1,-9	# CHECK: addiu	$k1, $k1, -9 # encoding: [0x7b,0x83,0x09,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  7b 83 09 80  	addiu	$k1, $k1, -9
	addiu	$a1,$sp,0	# CHECK: addiu	$a1, $sp, 0 # encoding: [0xc0,0x72]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  c0 72        	addiu	$a1, $sp, 0
	addiu	$s0,$sp,128	# CHECK: addiu	$s0, $sp, 128 # encoding: [0x60,0x70]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  60 70        	addiu	$s0, $sp, 128
	addiu	$s3,$sp,252	# CHECK: addiu	$s3, $sp, 252 # encoding: [0xff,0x71]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUR1SP_NM
				# DISAS: {{.*}}  ff 71        	addiu	$s3, $sp, 252
	addiu	$s3,$sp,256	# CHECK: addiu	$s3, $sp, 256 # encoding: [0x7d,0x02,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  7d 02 00 01  	addiu	$s3, $sp, 256
	addiu	$a1,$a2,-1	# CHECK: addiu	$a1, $a2, -1 # encoding: [0xa6,0x80,0x01,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  a6 80 01 80  	addiu	$a1, $a2, -1
	addiu	$a1,$a2,-4095	# CHECK: addiu	$a1, $a2, -4095 # encoding: [0xa6,0x80,0xff,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUNEG_NM
				# DISAS: {{.*}}  a6 80 ff 8f  	addiu	$a1, $a2, -4095
	addiu	$s3,$s3,-4096	# CHECK: addiu	$s3, $s3, -4096 # encoding: [0x61,0x62,0x00,0xf0,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  61 62 00 f0 ff ff    	addiu	$s3, $s3, 4294963200
	addiu[48] $a3,$a3,-2147483648	# CHECK: addiu	$a3, $a3, -2147483648 # encoding: [0xe1,0x60,0x00,0x00,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIU48_NM
				# DISAS: {{.*}}  e1 60 00 00 00 80    	addiu	$a3, $a3, 2147483648
	addiu	$a1, $gp, 0	# CHECK: addiu.b $a1, $gp, 0 # encoding: [0xac,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  ac 44 00 00  	addiu.b	$a1, $gp, 0
	addiu	$s0,$gp,131701	# CHECK: addiu.b $s0, $gp, 131701 # encoding: [0x0e,0x46,0x75,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  0e 46 75 02  	addiu.b	$s0, $gp, 131701
	addiu	$s3,$gp,262143	# CHECK: addiu.b $s3, $gp, 262143 # encoding: [0x6f,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPB_NM
				# DISAS: {{.*}}  6f 46 ff ff  	addiu.b	$s3, $gp, 262143
	addiu	$k0,$gp,262144	# CHECK: addiu.w $k0, $gp, 262144 # encoding: [0x44,0x43,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
				# DISAS: {{.*}}  44 43 00 00  	addiu.w	$k0, $gp, 262144
	addiu	$s7,$gp,2097148	# CHECK: addiu.w $s7, $gp, 2097148 # encoding: [0xff,0x42,0xfc,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGPW_NM
				# DISAS: {{.*}}  ff 42 fc ff  	addiu.w	$s7, $gp, 2097148
	addiu	$a4,$gp,2097152	# CHECK: addiu.b32 $a4, $gp, 2097152 # encoding: [0x02,0x61,0x00,0x00,0x20,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM
				# DISAS: {{.*}}  02 61 00 00 20 00    	addiu.b32	$a4, $gp, 2097152
	addiu	$s4,$gp,262146	# CHECK: addiu.b32 $s4, $gp, 262146 # encoding: [0x82,0x62,0x02,0x00,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} ADDIUGP48_NM
				# DISAS: {{.*}}  82 62 02 00 04 00    	addiu.b32	$s4, $gp, 262146

	move.balc $a0, $a3, test	# CHECK: move.balc $a0, $a3, test # encoding: [0b111AAAAA,0x08,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
					# DISAS: {{.*}}  e0 08 00 00  	move.balc	$a0, $a3, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	move.balc $a1, $zero, test	# CHECK: move.balc $a1, $zero, test # encoding: [0b011AAAAA,0x09,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
					# DISAS: {{.*}}  60 09 00 00  	move.balc	$a1, $zero, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	move.balc $a0, $a6, test	# CHECK: move.balc $a0, $a6, test # encoding: [0b010AAAAA,0x08,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
					# DISAS: {{.*}}  40 08 00 00  	move.balc	$a0, $a6, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	move.balc $a1, $s0, test	# CHECK: move.balc $a1, $s0, test # encoding: [0b000AAAAA,0x0b,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
					# DISAS: {{.*}}  00 0b 00 00  	move.balc	$a1, $s0, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	move.balc $a0, $s7, test	# CHECK: move.balc $a0, $s7, test # encoding: [0b111AAAAA,0x0a,A,A]
					# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC21_S1
					# <MCInst #{{.*}} MOVEBALC_NM
					# DISAS: {{.*}}  e0 0a 00 00  	move.balc	$a0, $s7, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test

	lw	$a0, 0($s3)	# CHECK: lw $a0, 0($s3) # encoding: [0x30,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  30 16        	lw	$a0, 0($s3)
	lw	$a1, 4($s2)	# CHECK: lw $a1, 4($s2) # encoding: [0xa1,0x16]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  a1 16        	lw	$a1, 4($s2)
	lw	$a2, 32($s1)	# CHECK: lw $a2, 32($s1) # encoding: [0x18,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  18 17        	lw	$a2, 32($s1)
	lw	$a3, 60($s0)	# CHECK: lw $a3, 60($s0) # encoding: [0x8f,0x17]
				# CHECK-NEXT: <MCInst  #{{.*}} LW16_NM
				# DISAS: {{.*}}  8f 17        	lw	$a3, 60($s0)

	lw	$a7, 0($s7)	# CHECK: lw $a7, 0($s7) # encoding: [0x77,0x74]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  77 74        	lw	$a7, 0($s7)
	lw	$s3, 4($a5)	# CHECK: lw $s3, 4($a5) # encoding: [0x61,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  61 77        	lw	$s3, 4($at)
	lw	$s4, 8($a3)	# CHECK: lw $s4, 8($a3) # encoding: [0x8f,0x76]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  8f 76        	lw	$s4, 8($a3)
	lw	$s6, 12($a0)	# CHECK: lw $s6, 12($a0) # encoding: [0xcc,0x77]
				# CHECK-NEXT: <MCInst  #{{.*}} LW4x4_NM
				# DISAS: {{.*}}  cc 77        	lw	$s6, 12($a0)

	lw	$a2, -4($s4)	# CHECK: lw $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  d4 a4 fc c0  	lw	$a2, -4($s4)
	lw	$a3, -256($s0)	# CHECK: lw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc0]
				# CHECK-NEXT: <MCInst  #{{.*}} LWs9_NM
				# DISAS: {{.*}}  f0 a4 00 c0  	lw	$a3, -256($s0)
	lw	$a4, 64($s3)	# CHECK: lw $a4, 64($s3) # encoding: [0x13,0x85,0x40,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  13 85 40 80  	lw	$a4, 64($s3)
	lw	$a1, 252($s2)	# CHECK: lw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  b2 84 fc 80  	lw	$a1, 252($s2)
	lw	$s3, 4092($t5)	# CHECK: lw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x8f]
				# CHECK-NEXT: <MCInst  #{{.*}} LW_NM
				# DISAS: {{.*}}  63 86 fc 8f  	lw	$s3, 4092($t5)

	lw	$a0, 0($sp)	# CHECK: lw $a0, 0($sp) # encoding: [0x80,0x34]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  80 34        	lw	$a0, 0($sp)
	lw	$t0, 64($sp)	# CHECK: lw $t0, 64($sp) # encoding: [0x90,0x35]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  90 35        	lw	$t0, 64($sp)
	lw	$k0, 124($sp)	# CHECK: lw $k0, 124($sp) # encoding: [0x5f,0x37]
				# CHECK-NEXT: <MCInst  #{{.*}} LWSP16_NM
				# DISAS: {{.*}}  5f 37        	lw	$k0, 124($sp)

	lw	$a0, 0($gp)	# CHECK: lw $a0, 0($gp) # encoding: [0x00,0x56]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  00 56        	lw	$a0, 0($gp)
	lw	$a3, 256($gp)	# CHECK: lw $a3, 256($gp) # encoding: [0xc0,0x57]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  c0 57        	lw	$a3, 256($gp)
	lw	$s0, 508($gp)	# CHECK: lw $s0, 508($gp) # encoding: [0x7f,0x54]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP16_NM
				# DISAS: {{.*}}  7f 54        	lw	$s0, 508($gp)
	lw	$a3, 4096($gp)	# CHECK: lw $a3, 4096($gp) # encoding: [0xe0,0x40,0x02,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  e0 40 02 10  	lw	$a3, 4096($gp)
	lw	$s3, 65536($gp)	# CHECK: lw $s3, 65536($gp) # encoding: [0x61,0x42,0x02,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  61 42 02 00  	lw	$s3, 65536($gp)
	lw	$s0, 2097148($gp)	# CHECK: lw $s0, 2097148($gp) # encoding: [0x1f,0x42,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LWGP_NM
				# DISAS: {{.*}}  1f 42 fe ff  	lw	$s0, 2097148($gp)

	sw	$a0, 4($a1)	# CHECK: sw $a0, 4($a1) # encoding: [0x51,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  51 96        	sw	$a0, 4($a1)
	sw	$a1, 4($s2)	# CHECK: sw $a1, 4($s2) # encoding: [0xa1,0x96]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  a1 96        	sw	$a1, 4($s2)
	sw	$a2, 32($s1)	# CHECK: sw $a2, 32($s1) # encoding: [0x18,0x97]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  18 97        	sw	$a2, 32($s1)
	sw	$zero, 60($a3)	# CHECK: sw $zero, 60($a3) # encoding: [0x7f,0x94]
				# CHECK-NEXT: <MCInst  #{{.*}} SW16_NM
				# DISAS: {{.*}}  7f 94        	sw	$zero, 60($a3)

	sw	$zero, 0($s7)	# CHECK: sw $zero, 0($s7) # encoding: [0x77,0xf4]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  77 f4        	sw	$zero, 0($s7)
	sw	$s3, 4($a5)	# CHECK: sw $s3, 4($a5) # encoding: [0x61,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  61 f7        	sw	$s3, 4($at)
	sw	$s4, 8($a3)	# CHECK: sw $s4, 8($a3) # encoding: [0x8f,0xf6]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  8f f6        	sw	$s4, 8($a3)
	sw	$s6, 12($a0)	# CHECK: sw $s6, 12($a0) # encoding: [0xcc,0xf7]
				# CHECK-NEXT: <MCInst  #{{.*}} SW4x4_NM
				# DISAS: {{.*}}  cc f7        	sw	$s6, 12($a0)

	sw	$a2, -4($s6)	# CHECK: sw $a2, -4($s6) # encoding: [0xd6,0xa4,0xfc,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  d6 a4 fc c8  	sw	$a2, -4($s6)
	sw	$a3, -256($s0)	# CHECK: sw $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xc8]
				# CHECK-NEXT: <MCInst  #{{.*}} SWs9_NM
				# DISAS: {{.*}}  f0 a4 00 c8  	sw	$a3, -256($s0)
	sw	$a5, 64($s3)	# CHECK: sw $a5, 64($s3) # encoding: [0x33,0x85,0x40,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  33 85 40 90  	sw	$a5, 64($s3)
	sw	$a1, 252($s2)	# CHECK: sw $a1, 252($s2) # encoding: [0xb2,0x84,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  b2 84 fc 90  	sw	$a1, 252($s2)
	sw	$s3, 4092($t5)	# CHECK: sw $s3, 4092($t5) # encoding: [0x63,0x86,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  63 86 fc 9f  	sw	$s3, 4092($t5)

	sw	$a0, 0($sp)	# CHECK: sw $a0, 0($sp) # encoding: [0x80,0xb4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  80 b4        	sw	$a0, 0($sp)
	sw	$t0, 64($sp)	# CHECK: sw $t0, 64($sp) # encoding: [0x90,0xb5]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  90 b5        	sw	$t0, 64($sp)
	sw	$k0, 124($sp)	# CHECK: sw $k0, 124($sp) # encoding: [0x5f,0xb7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWSP16_NM
				# DISAS: {{.*}}  5f b7        	sw	$k0, 124($sp)
	sw	$a1, 4092($sp)	# CHECK: sw $a1, 4092($sp) # encoding: [0xbd,0x84,0xfc,0x9f]
				# CHECK-NEXT: <MCInst  #{{.*}} SW_NM
				# DISAS: {{.*}}  bd 84 fc 9f  	sw	$a1, 4092($sp)

	sw	$a0, 0($gp)	# CHECK: sw $a0, 0($gp) # encoding: [0x00,0xd6]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  00 d6        	sw	$a0, 0($gp)
	sw	$a3, 256($gp)	# CHECK: sw $a3, 256($gp) # encoding: [0xc0,0xd7]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  c0 d7        	sw	$a3, 256($gp)
	sw	$zero, 508($gp)	# CHECK: sw $zero, 508($gp) # encoding: [0x7f,0xd4]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP16_NM
				# DISAS: {{.*}}  7f d4        	sw	$zero, 508($gp)
	sw	$a2, 512($gp)	# CHECK: sw $a2, 512($gp) # encoding: [0xc0,0x40,0x03,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  c0 40 03 02  	sw	$a2, 512($gp)
	sw	$s2, 65536($gp)	# CHECK: sw $s2, 65536($gp) # encoding: [0x41,0x42,0x03,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  41 42 03 00  	sw	$s2, 65536($gp)
	sw	$zero, 2097148($gp)	# CHECK: sw $zero, 2097148($gp) # encoding: [0x1f,0x40,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} SWGP_NM
				# DISAS: {{.*}}  1f 40 ff ff  	sw	$zero, 2097148($gp)

	lb	$a0, 0($s3)	# CHECK: lb $a0, 0($s3) # encoding:  [0x30,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  30 5e        	lb	$a0, 0($s3)
	lb	$a1, 1($s2)	# CHECK: lb $a1, 1($s2) # encoding: [0xa1,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  a1 5e        	lb	$a1, 1($s2)
	lb	$a2, 2($s1)	# CHECK: lb $a2, 2($s1) # encoding: [0x12,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  12 5f        	lb	$a2, 2($s1)
	lb	$s0, 3($s0)	# CHECK: lb $s0, 3($s0) # encoding: [0x03,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LB16_NM
				# DISAS: {{.*}}  03 5c        	lb	$s0, 3($s0)

	lb	$a2, -4($s1)	# CHECK: lb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  d1 a4 fc 80  	lb	$a2, -4($s1)
	lb	$a3, -256($s5)	# CHECK: lb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x80]
				# CHECK-NEXT: <MCInst  #{{.*}} LBs9_NM
				# DISAS: {{.*}}  f5 a4 00 80  	lb	$a3, -256($s5)
	lb	$a3, 4($s0)	# CHECK: lb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  f0 84 04 00  	lb	$a3, 4($s0)
	lb	$a5, 255($s2)	# CHECK: lb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  32 85 ff 00  	lb	$a5, 255($s2)
	lb	$s3, 4095($t5)	# CHECK: lb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x0f]
				# CHECK-NEXT: <MCInst  #{{.*}} LB_NM
				# DISAS: {{.*}}  63 86 ff 0f  	lb	$s3, 4095($t5)

	lb	$a0, 256($gp)	# CHECK: lb $a0, 256($gp) # encoding: [0x80,0x44,0x00,0x01]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  80 44 00 01  	lb	$a0, 256($gp)
	lb	$s3, 4096($gp)	# CHECK: lb $s3, 4096($gp) # encoding: [0x60,0x46,0x00,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  60 46 00 10  	lb	$s3, 4096($gp)
	lb	$s0, 262143($gp)	# CHECK: lb $s0, 262143($gp) # encoding: [0x03,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} LBGP_NM
				# DISAS: {{.*}}  03 46 ff ff  	lb	$s0, 262143($gp)

	lbu	$a0, 0($s3)	# CHECK: lbu $a0, 0($s3) # encoding: [0x38,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  38 5e        	lbu	$a0, 0($s3)
	lbu	$a1, 1($s2)	# CHECK: lbu $a1, 1($s2) # encoding: [0xa9,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  a9 5e        	lbu	$a1, 1($s2)
	lbu	$a2, 2($s1)	# CHECK: lbu $a2, 2($s1) # encoding: [0x1a,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  1a 5f        	lbu	$a2, 2($s1)
	lbu	$s0, 3($s0)	# CHECK: lbu $s0, 3($s0) # encoding: [0x0b,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU16_NM
				# DISAS: {{.*}}  0b 5c        	lbu	$s0, 3($s0)

	lbu	$a2, -4($s1)	# CHECK: lbu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  d1 a4 fc 90  	lbu	$a2, -4($s1)
	lbu	$a3, -256($s5)	# CHECK: lbu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x90]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUs9_NM
				# DISAS: {{.*}}  f5 a4 00 90  	lbu	$a3, -256($s5)
	lbu	$a3, 4($s0)	# CHECK: lbu $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  f0 84 04 20  	lbu	$a3, 4($s0)
	lbu	$a5, 255($s2)	# CHECK: lbu $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x20]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  32 85 ff 20  	lbu	$a5, 255($s2)
	lbu	$s3, 4095($t5)	# CHECK: lbu $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x2f]
				# CHECK-NEXT: <MCInst  #{{.*}} LBU_NM
				# DISAS: {{.*}}  63 86 ff 2f  	lbu	$s3, 4095($t5)

	lbu	$a0, 0($gp)	# CHECK: lbu $a0, 0($gp) # encoding: [0x88,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  88 44 00 00  	lbu	$a0, 0($gp)
	lbu	$s3, 65536($gp)	# CHECK: lbu $s3, 65536($gp) # encoding: [0x69,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  69 46 00 00  	lbu	$s3, 65536($gp)
	lbu	$s0, 262143($gp)	# CHECK: lbu $s0, 262143($gp) # encoding: [0x0b,0x46,0xff,0xff]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUGP_NM
				# DISAS: {{.*}}  0b 46 ff ff  	lbu	$s0, 262143($gp)

	sb	$a0, 0($s3)	# CHECK: sb $a0, 0($s3) # encoding: [0x34,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  34 5e        	sb	$a0, 0($s3)
	sb	$a1, 1($s2)	# CHECK: sb $a1, 1($s2) # encoding: [0xa5,0x5e]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  a5 5e        	sb	$a1, 1($s2)
	sb	$a2, 2($s1)	# CHECK: sb $a2, 2($s1) # encoding: [0x16,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  16 5f        	sb	$a2, 2($s1)
	sb	$zero, 3($s0)	# CHECK: sb $zero, 3($s0) # encoding: [0x07,0x5c]
				# CHECK-NEXT: <MCInst  #{{.*}} SB16_NM
				# DISAS: {{.*}}  07 5c        	sb	$zero, 3($s0)

	sb	$a2, -4($s1)	# CHECK: sb $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  d1 a4 fc 88  	sb	$a2, -4($s1)
	sb	$a3, -256($s5)	# CHECK: sb $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0x88]
				# CHECK-NEXT: <MCInst  #{{.*}} SBs9_NM
				# DISAS: {{.*}}  f5 a4 00 88  	sb	$a3, -256($s5)
	sb	$a3, 4($s0)	# CHECK: sb $a3, 4($s0) # encoding: [0xf0,0x84,0x04,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  f0 84 04 10  	sb	$a3, 4($s0)
	sb	$a5, 255($s2)	# CHECK: sb $a5, 255($s2) # encoding: [0x32,0x85,0xff,0x10]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  32 85 ff 10  	sb	$a5, 255($s2)
	sb	$s3, 4095($t5)	# CHECK: sb $s3, 4095($t5) # encoding: [0x63,0x86,0xff,0x1f]
				# CHECK-NEXT: <MCInst  #{{.*}} SB_NM
				# DISAS: {{.*}}  63 86 ff 1f  	sb	$s3, 4095($t5)

	sb	$a0, 0($gp)	# CHECK: sb $a0, 0($gp) # encoding: [0x84,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
				# DISAS: {{.*}}  84 44 00 00  	sb	$a0, 0($gp)
	sb	$s3, 65536($gp)	# CHECK: sb $s3, 65536($gp) # encoding: [0x65,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
				# DISAS: {{.*}}  65 46 00 00  	sb	$s3, 65536($gp)
	sb	$s0, 262143($gp)	# CHECK: sb $s0, 262143($gp) # encoding: [0x07,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SBGP_NM
					# DISAS: {{.*}}  07 46 ff ff  	sb	$s0, 262143($gp)

	lh	$a0, 0($s3)	# CHECK: lh $a0, 0($s3) # encoding: [0x30,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  30 7e        	lh	$a0, 0($s3)
	lh	$a1, 2($s2)	# CHECK: lh $a1, 2($s2) # encoding: [0xa2,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  a2 7e        	lh	$a1, 2($s2)
	lh	$a2, 4($s1)	# CHECK: lh $a2, 4($s1) # encoding: [0x14,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  14 7f        	lh	$a2, 4($s1)
	lh	$s0, 6($s0)	# CHECK: lh $s0, 6($s0) # encoding: [0x06,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LH16_NM
				# DISAS: {{.*}}  06 7c        	lh	$s0, 6($s0)

	lh	$a2, -4($s1)	# CHECK: lh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  d1 a4 fc a0  	lh	$a2, -4($s1)
	lh	$a3, -256($s5)	# CHECK: lh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa0]
				# CHECK-NEXT: <MCInst  #{{.*}} LHs9_NM
				# DISAS: {{.*}}  f5 a4 00 a0  	lh	$a3, -256($s5)
	lh	$a4, 8($s0)	# CHECK: lh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  10 85 08 40  	lh	$a4, 8($s0)
	lh	$a5, 254($s2)	# CHECK: lh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x40]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  32 85 fe 40  	lh	$a5, 254($s2)
	lh	$s3, 4094($t5)	# CHECK: lh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x4f]
				# CHECK-NEXT: <MCInst  #{{.*}} LH_NM
				# DISAS: {{.*}}  63 86 fe 4f  	lh	$s3, 4094($t5)

	lh	$a0, 0($gp)	# CHECK: lh $a0, 0($gp) # encoding: [0x90,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  90 44 00 00  	lh	$a0, 0($gp)
	lh	$s3, 65536($gp)	# CHECK: lh $s3, 65536($gp) # encoding: [0x71,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  71 46 00 00  	lh	$s3, 65536($gp)
	lh	$s0, 262142($gp)	# CHECK: lh $s0, 262142($gp) # encoding: [0x13,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHGP_NM
				# DISAS: {{.*}}  13 46 fe ff  	lh	$s0, 262142($gp)
	lhu	$a0, 0($s3)	# CHECK: lhu $a0, 0($s3) # encoding: [0x38,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  38 7e        	lhu	$a0, 0($s3)
	lhu	$a1, 2($s2)	# CHECK: lhu $a1, 2($s2) # encoding: [0xaa,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  aa 7e        	lhu	$a1, 2($s2)
	lhu	$a2, 4($s1)	# CHECK: lhu $a2, 4($s1) # encoding: [0x1c,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  1c 7f        	lhu	$a2, 4($s1)
	lhu	$s0, 6($s0)	# CHECK: lhu $s0, 6($s0) # encoding: [0x0e,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} LHU16_NM
				# DISAS: {{.*}}  0e 7c        	lhu	$s0, 6($s0)
	lhu	$a2, -4($s1)	# CHECK: lhu $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
				# DISAS: {{.*}}  d1 a4 fc b0  	lhu	$a2, -4($s1)
	lhu	$a3, -256($s5)		# CHECK: lhu $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xb0]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUs9_NM
				# DISAS: {{.*}}  f5 a4 00 b0  	lhu	$a3, -256($s5)
	lhu	$a4, 8($s0)		# CHECK: lhu $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  10 85 08 60  	lhu	$a4, 8($s0)
	lhu	$a5, 254($s2)		# CHECK: lhu $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x60]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  32 85 fe 60  	lhu	$a5, 254($s2)
	lhu	$s3, 4094($t5)		# CHECK: lhu $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x6f]
					# CHECK-NEXT: <MCInst  #{{.*}} LHU_NM
					# DISAS: {{.*}}  63 86 fe 6f  	lhu	$s3, 4094($t5)
	lhu	$a0, 0($gp)	# CHECK: lhu $a0, 0($gp) # encoding: [0x90,0x44,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
				# DISAS: {{.*}}  90 44 01 00  	lhu	$a0, 0($gp)
	lhu	$s3, 65536($gp)	# CHECK: lhu $s3, 65536($gp) # encoding: [0x71,0x46,0x01,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
				# DISAS: {{.*}}  71 46 01 00  	lhu	$s3, 65536($gp)
	lhu	$s0, 262142($gp)	# CHECK: lhu $s0, 262142($gp) # encoding: [0x13,0x46,0xff,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} LHUGP_NM
					# DISAS: {{.*}}  13 46 ff ff  	lhu	$s0, 262142($gp)

	sh	$a0, 0($s3)	# CHECK: sh $a0, 0($s3) # encoding: [0x31,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  31 7e        	sh	$a0, 0($s3)
	sh	$a1, 2($s2)	# CHECK: sh $a1, 2($s2) # encoding: [0xa3,0x7e]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  a3 7e        	sh	$a1, 2($s2)
	sh	$a2, 4($s1)	# CHECK: sh $a2, 4($s1) # encoding: [0x15,0x7f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  15 7f        	sh	$a2, 4($s1)
	sh	$zero, 6($s0)	# CHECK: sh $zero, 6($s0) # encoding: [0x07,0x7c]
				# CHECK-NEXT: <MCInst  #{{.*}} SH16_NM
				# DISAS: {{.*}}  07 7c        	sh	$zero, 6($s0)

	sh	$a2, -4($s1)    # CHECK: sh $a2, -4($s1) # encoding: [0xd1,0xa4,0xfc,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  d1 a4 fc a8  	sh	$a2, -4($s1)
	sh	$a3, -256($s5)  # CHECK: sh $a3, -256($s5) # encoding: [0xf5,0xa4,0x00,0xa8]
				# CHECK-NEXT: <MCInst  #{{.*}} SHs9_NM
				# DISAS: {{.*}}  f5 a4 00 a8  	sh	$a3, -256($s5)
	sh	$a4, 8($s0)     # CHECK: sh $a4, 8($s0) # encoding: [0x10,0x85,0x08,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  10 85 08 50  	sh	$a4, 8($s0)
	sh	$a5, 254($s2)   # CHECK: sh $a5, 254($s2) # encoding: [0x32,0x85,0xfe,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  32 85 fe 50  	sh	$a5, 254($s2)
	sh	$s3, 4094($t5)  # CHECK: sh $s3, 4094($t5) # encoding: [0x63,0x86,0xfe,0x5f]
				# CHECK-NEXT: <MCInst  #{{.*}} SH_NM
				# DISAS: {{.*}}  63 86 fe 5f  	sh	$s3, 4094($t5)
	sh	$a0, 0($gp)	# CHECK: sh $a0, 0($gp) # encoding: [0x94,0x44,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
				# DISAS: {{.*}}  94 44 00 00  	sh	$a0, 0($gp)
	sh	$s3, 65536($gp)	# CHECK: sh $s3, 65536($gp) # encoding: [0x75,0x46,0x00,0x00]
				# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
				# DISAS: {{.*}}  75 46 00 00  	sh	$s3, 65536($gp)
	sh	$s0, 262142($gp)	# CHECK: sh $s0, 262142($gp) # encoding: [0x17,0x46,0xfe,0xff]
					# CHECK-NEXT: <MCInst  #{{.*}} SHGP_NM
					# DISAS: {{.*}}  17 46 fe ff  	sh	$s0, 262142($gp)

	lwx	$a0, $s0($t0)	# CHECK: lwx $a0, $s0($t0) # encoding: [0x90,0x21,0x07,0x24]
				# CHECK-NEXT: <MCInst  #{{.*}} LWX_NM
				# DISAS: {{.*}}  90 21 07 24  	lwx	$a0, $s0($t0)
	lbx	$a1, $s1($t1)	# CHECK: lbx $a1, $s1($t1) # encoding: [0xb1,0x21,0x07,0x28]
				# CHECK-NEXT: <MCInst  #{{.*}} LBX_NM
				# DISAS: {{.*}}  b1 21 07 28  	lbx	$a1, $s1($t1)
	lbux	$a2, $s2($t2)	# CHECK: lbux $a2, $s2($t2) # encoding: [0xd2,0x21,0x07,0x31]
				# CHECK-NEXT: <MCInst  #{{.*}} LBUX_NM
				# DISAS: {{.*}}  d2 21 07 31  	lbux	$a2, $s2($t2)
	lhx	$a3, $s3($t3)	# CHECK: lhx $a3, $s3($t3) # encoding: [0xf3,0x21,0x07,0x3a]
				# CHECK-NEXT: <MCInst  #{{.*}} LHX_NM
				# DISAS: {{.*}}  f3 21 07 3a  	lhx	$a3, $s3($t3)
	lhux	$a4, $s4($t4)	# CHECK: lhux $a4, $s4($t4) # encoding: [0x54,0x20,0x07,0x43]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUX_NM
				# DISAS: {{.*}}  54 20 07 43  	lhux	$a4, $s4($t4)
	swx	$a5, $s5($t5)	# CHECK: swx $a5, $s5($t5) # encoding: [0x75,0x20,0x87,0x4c]
				# CHECK-NEXT: <MCInst  #{{.*}} SWX_NM
				# DISAS: {{.*}}  75 20 87 4c  	swx	$a5, $s5($t5)
	sbx	$a6, $s6($k0)	# CHECK: sbx $a6, $s6($k0) # encoding: [0x56,0x23,0x87,0x50]
				# CHECK-NEXT: <MCInst  #{{.*}} SBX_NM
				# DISAS: {{.*}}  56 23 87 50  	sbx	$a6, $s6($k0)
	shx	$a7, $s7($k1)	# CHECK: shx $a7, $s7($k1) # encoding: [0x77,0x23,0x87,0x5a]
				# CHECK-NEXT: <MCInst  #{{.*}} SHX_NM
				# DISAS: {{.*}}  77 23 87 5a  	shx	$a7, $s7($k1)

	lwxs	$s1, $a1($a2)	# CHECK: lwxs $s1, $a1($a2) # encoding: [0x53,0x53]
				# CHECK-NEXT: <MCInst  #{{.*}} LWXS16_NM
				# DISAS: {{.*}}  53 53        	lwxs	$s1, $a1($a2)
	lwxs	$s7, $a0($t5)	# CHECK: lwxs $s7, $a0($t5) # encoding: [0x64,0x20,0x47,0xbc]
				# CHECK-NEXT: <MCInst  #{{.*}} LWXS_NM
				# DISAS: {{.*}}  64 20 47 bc  	lwxs	$s7, $a0($t5)
	swxs	$zero,$a1($s4)	# CHECK: swxs $zero, $a1($s4) # encoding: [0x85,0x22,0xc7,0x04]
				# CHECK-NEXT: <MCInst  #{{.*}} SWXS_NM
				# DISAS: {{.*}}  85 22 c7 04  	swxs	$zero, $a1($s4)
	lhxs	$s6, $a2($t0)	# CHECK: lhxs $s6, $a2($t0) # encoding: [0x86,0x21,0x47,0xb2]
				# CHECK-NEXT: <MCInst  #{{.*}} LHXS_NM
				# DISAS: {{.*}}  86 21 47 b2  	lhxs	$s6, $a2($t0)
	lhuxs	$s5, $a3($t1)	# CHECK: lhuxs $s5, $a3($t1) # encoding: [0xa7,0x21,0x47,0xab]
				# CHECK-NEXT: <MCInst  #{{.*}} LHUXS_NM
				# DISAS: {{.*}}  a7 21 47 ab  	lhuxs	$s5, $a3($t1)
	shxs	$zero,$a4($t2)	# CHECK: shxs $zero, $a4($t2) # encoding: [0xc8,0x21,0xc7,0x02]
				# CHECK-NEXT: <MCInst  #{{.*}} SHXS_NM
				# DISAS: {{.*}}  c8 21 c7 02  	shxs	$zero, $a4($t2)

	swpc	$t0, test	# CHECK: swpc $t0, test	# encoding: [0x8f,0x61,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
				# DISAS: {{.*}}  8f 61 00 00 00 00    	swpc	$t0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test
	swpc	$t0, test-1048576	# CHECK: swpc $t0, test-1048576	# encoding: [0x8f,0x61,A,A,A,A]
					# CHECK-NEXT: fixup A - offset: 2, value: test-1048576, kind: fixup_NANOMIPS_PC_I32
					# CHECK-NEXT: <MCInst  #{{.*}} SWPC_NM
					# DISAS: {{.*}}  8f 61 00 00 00 00    	swpc	$t0, 0x{{.*}}
					# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test-0x100000
	lwpc	$t5, test	# CHECK: lwpc $t5, test	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM
				# DISAS: {{.*}}  6b 60 00 00 00 00    	lwpc	$t5, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test
	lwpc	$t5, test+2147483647	# CHECK: lwpc $t5, test+2147483647	# encoding: [0x6b,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test+2147483647, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst  #{{.*}} LWPC_NM
				# DISAS: {{.*}}  6b 60 00 00 00 00    	lwpc	$t5, 0{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC_I32	test+0x7fffffff

	addiu	$t0, $gp, %gp_rel(test)	# CHECK: addiu.b	$t0, $gp, %gp_rel(test) # encoding: [0b100011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
					# DISAS: {{.*}}  8c 45 00 00  	addiu.b	$t0, $gp, 0
	addiu.b	$t1, $gp, %gp_rel(test)	# CHECK: addiu.b	$t1, $gp, %gp_rel(test) # encoding: [0b101011AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPB_NM
					# DISAS: {{.*}}  ac 45 00 00  	addiu.b	$t1, $gp, 0
	addiu.w	$t2, $gp, %gp_rel(test)	# CHECK: addiu.w	$t2, $gp, %gp_rel(test) # encoding: [0b11000AAA,0x41,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGPW_NM
					# DISAS: {{.*}}  c0 41 00 00  	addiu.w	$t2, $gp, 0
	addiu.b32 $t3, $gp, %gp_rel(test)	# CHECK: addiu.b32	$t3, $gp, %gp_rel(test) # encoding: [0xe2,0x61,A,A,A,A]
                                        # CHECK-NEXT: fixup A - offset: 2, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL_I32
                                        # CHECK-NEXT: <MCInst #{{.*}} ADDIUGP48_NM
					# DISAS: {{.*}}  e2 61 00 00 00 00    	addiu.b32	$t3, $gp, 0

	lw	$a0, %gp_rel(test)($gp)	# CHECK: lw	$a0, %gp_rel(test)($gp) # encoding: [0b0AAAAAAA,0x56]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP16_NM
					# DISAS: {{.*}}  00 56        	lw	$a0, 0($gp)
	sw	$s1, %gp_rel(test)($gp)	# CHECK: sw	$s1, %gp_rel(test)($gp) # encoding: [0b1AAAAAAA,0xd4]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL7_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP16_NM
					# DISAS: {{.*}}  80 d4        	sw	$s1, 0($gp)
	lw	$t0, %gp_rel(test)($gp)	# CHECK: lw	$t0, %gp_rel(test)($gp) # encoding: [0b10000AAA,0x41,0x02'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} LWGP_NM
					# DISAS: {{.*}}  80 41 02 00  	lw	$t0, 0($gp)
	lh	$a1, %gp_rel(test)($gp)	# CHECK: lh	$a1, %gp_rel(test)($gp) # encoding: [0b1011000A,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHGP_NM
					# DISAS: {{.*}}  b0 44 00 00  	lh	$a1, 0($gp)
	lhu	$a2, %gp_rel(test)($gp)	# CHECK: lhu	$a2, %gp_rel(test)($gp) # encoding: [0b1101000A,0x44,0x01'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} LHUGP_NM
					# DISAS: {{.*}}  d0 44 01 00  	lhu	$a2, 0($gp)
	lb	$a3, %gp_rel(test)($gp)	# CHECK: lb	$a3, %gp_rel(test)($gp) # encoding: [0b111000AA,0x44,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBGP_NM
					# DISAS: {{.*}}  e0 44 00 00  	lb	$a3, 0($gp)
	lbu	$a4, %gp_rel(test)($gp)	# CHECK: lbu	$a4, %gp_rel(test)($gp) # encoding: [0b000010AA,0x45,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} LBUGP_NM
					# DISAS: {{.*}}  08 45 00 00  	lbu	$a4, 0($gp)
	sw	$t1, %gp_rel(test)($gp)	# CHECK: sw	$t1, %gp_rel(test)($gp) # encoding: [0b10100AAA,0x41,0x03'A',A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL19_S2
                                        # CHECK-NEXT: <MCInst #{{.*}} SWGP_NM
					# DISAS: {{.*}}  a0 41 03 00  	sw	$t1, 0($gp)
	sh	$s1, %gp_rel(test)($gp)	# CHECK: sh	$s1, %gp_rel(test)($gp) # encoding: [0b0011010A,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL17_S1
                                        # CHECK-NEXT: <MCInst #{{.*}} SHGP_NM
					# DISAS: {{.*}}  34 46 00 00  	sh	$s1, 0($gp)
	sb	$s3, %gp_rel(test)($gp)	# CHECK: sb	$s3, %gp_rel(test)($gp) # encoding: [0b011001AA,0x46,A,A]
                                        # CHECK-NEXT: fixup A - offset: 0, value: %gp_rel(test), kind: fixup_NANOMIPS_GPREL18
                                        # CHECK-NEXT: <MCInst #{{.*}} SBGP_NM
					# DISAS: {{.*}}  64 46 00 00  	sb	$s3, 0($gp)

	lapc.h	$a0, test	# CHECK: lapc.h $a0, test # encoding: [0b100AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
				# DISAS: {{.*}}  80 04 00 00  	lapc.h	$a0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}:  R_NANOMIPS_PC21_S1	test
	lapc.h	$a1, test-16	# CHECK: lapc.h $a1, test-16 # encoding: [0b101AAAAA,0x04,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test-16, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
				# DISAS: {{.*}}  a0 04 00 00  	lapc.h	$a1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC21_S1	test-0x10
	lapc.b	$a3, test-16	# CHECK: lapc.b $a3, test-16 # encoding: [0xe3,0x60,A,A,A,A]
				# CHECK-NEXT: fixup A - offset: 2, value: test-16, kind: fixup_NANOMIPS_PC_I32
				# CHECK-NEXT: <MCInst #{{.*}} LAPC48_NM
				# DISAS: {{.*}}  e3 60 00 00 00 00    	lapc.b	$a3, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC_I32	test-0x10
	lapc	$a4, test+4	# CHECK: lapc.h $a4, test+4 # encoding: [0b000AAAAA,0x05,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: test+4, kind: fixup_NANOMIPS_PC21_S1
				# CHECK-NEXT: <MCInst #{{.*}} LAPC32_NM
				# DISAS: {{.*}}  00 05 00 00  	lapc.h	$a4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC21_S1	test+0x4
	
	aluipc $a0, %pcrel_hi(test)	# CHECK: aluipc $a0, %pcrel_hi(test) # encoding: [0b1000AAAA,0xe0,0x02'A',A]
				# CHECK-NEXT: fixup A - offset: 0, value: %pcrel_hi(test), kind: fixup_NANOMIPS_PCHI20
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
				# DISAS: {{.*}}  80 e0 02 00  	aluipc	$a0, %pcrel_hi({{.*}})
				# DISAS: {{.*}}  R_NANOMIPS_PCHI20	test
	aluipc $a1, %hi(0x87654321)	# CHECK: aluipc $a1, %pcrel_hi(0x87654) # encoding: [0xa5,0xe0,0xef,0x40]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
				# DISAS: {{.*}}  a5 e0 ef 40  	aluipc	$a1, %pcrel_hi({{.*}})
	aluipc $a2, 0x80000	# CHECK: aluipc $a2, %pcrel_hi(0x80000) # encoding: [0xc0,0xe0,0x03,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
				# DISAS: {{.*}} c0 e0 03 00   	aluipc	$a2, %pcrel_hi({{.*}})
	aluipc $a3, 0xfffff	# CHECK: aluipc $a3, %pcrel_hi(0xfffff) # encoding: [0xff,0xe0,0xff,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} ALUIPC_NM
				# DISAS: {{.*}}  ff e0 ff ff  	aluipc	$a3, %pcrel_hi({{.*}})
	lui $s0, %hi(test)	# CHECK: lui $s0, %hi(test) # encoding: [0b0000AAAA,0xe2,A,A]
				# CHECK-NEXT: fixup A - offset: 0, value: %hi(test), kind: fixup_NANOMIPS_HI20
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  00 e2 00 00  	lui	$s0, %hi(0x0)
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_HI20	test
	lui $s1, %hi(0x12345678)	# CHECK: lui $s1, %hi(0x12345) # encoding: [0x34,0xe2,0x44,0x52]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  34 e2 44 52  	lui	$s1, %hi(0x12345000)
	lui $s2, %hi(0x80001000)	# CHECK: lui $s2, %hi(0x80001) # encoding: [0x40,0xe2,0x01,0x10]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  40 e2 01 10  	lui	$s2, %hi(0x80001000)
	lui $s3, %hi(0xfffff000)	# CHECK: lui $s3, %hi(0xfffff) # encoding: [0x7f,0xe2,0xfd,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  7f e2 fd ff  	lui	$s3, %hi(0xfffff000)

	addiu $a0, $a0, %pcrel_lo(test)	# CHECK: addiu $a0, $a0, %lo(test) # encoding: [0x84,0x00,A,0b0000AAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: %lo(test), kind: fixup_NANOMIPS_LO12
				# CHECK-NEXT: <MCInst #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  84 00 00 00  	addiu	$a0, $a0, 0
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_LO12	test
	addiu $s0, $s0, %lo(test)	# CHECK: addiu $s0, $s0, %lo(test) # encoding: [0x10,0x02,A,0b0000AAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: %lo(test), kind: fixup_NANOMIPS_LO12
				# CHECK-NEXT: <MCInst #{{.*}} ADDIU_NM
				# DISAS: {{.*}}  10 02 00 00  	addiu	$s0, $s0, 0
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_LO12	test

1:
	brsc	$a1	# CHECK: brsc $a1	# encoding: [0x05,0x48,0x00,0x80]
			# CHECK-NEXT: # <MCInst #{{.*}} BRSC_NM
			# DISAS: {{.*}}  05 48 00 80  	brsc	$a1
	brsc	$a3	# CHECK: brsc $a3	# encoding: [0x07,0x48,0x00,0x80]
			# CHECK-NEXT: # <MCInst #{{.*}} BRSC_NM
			# DISAS: {{.*}}  07 48 00 80  	brsc	$a3
	balrsc  $ra, $t0	# CHECK: balrsc $ra, $t0	# encoding: [0xec,0x4b,0x00,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} BALRSC_NM
				# DISAS: {{.*}}  ec 4b 00 80  	balrsc	$ra, $t0
	balrsc  $sp, $gp	# CHECK: balrsc $sp, $gp	# encoding: [0xbc,0x4b,0x00,0x80]
				# CHECK-NEXT: # <MCInst #{{.*}} BALRSC_NM
				# DISAS: {{.*}}  bc 4b 00 80  	balrsc	$sp, $gp

	balc[16] test	# CHECK: balc[16] test # encoding: [A,0b001110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC16_NM
			# DISAS: {{.*}}  00 38        	balc[16]	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	test
	balc	test	# CHECK: balc test # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: test+0, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM
			# DISAS: {{.*}}  00 2a 00 00  	balc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC25_S1	test
	balc	0x1ff0001	# CHECK: balc 0x1ff0001 # encoding: [A,0b0010101A,A,A]
			# CHECK-NEXT: fixup A - offset: 0, value: 33488897, kind: fixup_NANOMIPS_PC25_S1
			# CHECK-NEXT: <MCInst #{{.*}} BALC_NM
			# DISAS: {{.*}}  00 2a 00 00  	balc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC25_S1	*ABS*+0x1ff0001

	bc	1b	# CHECK: bc .Ltmp0 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
			# DISAS: {{.*}}  00 18        	bc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	.Ltmp0
	bc	1f	# CHECK: bc .Ltmp1 # encoding: [A,0b000110AA]
			# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC10_S1
			# CHECK-NEXT: <MCInst #{{.*}} BC16_NM
			# DISAS: {{.*}}  00 18        	bc	0x{{.*}}
			# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC10_S1	.Ltmp1
	beqc	$a1, $a2, 2f	# CHECK: beqc $a2, $a1, .Ltmp2 # encoding: [0b0101AAAA,0xdb]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC16_NM
				# DISAS: {{.*}}  5f db        	beqc	$a1, $a2, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	beqc	$a2, $a1, 2f	# CHECK: beqc $a2, $a1, .Ltmp2 # encoding: [0b0101AAAA,0xdb]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC16_NM
				# DISAS: {{.*}}  5f db        	beqc	$a1, $a2, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	beqc	$a4, $a1, 1f	# CHECK: beqc $a4, $a1, .Ltmp1 # encoding: [0xa8,0x88,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC_NM
				# DISAS: {{.*}}  a8 88 00 00  	beqc	$a4, $a1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	bnec	$a2, $a0, 2f	# CHECK: bnec $a0, $a2, .Ltmp2 # encoding: [0b0110AAAA,0xda]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC16_NM
				# DISAS: {{.*}}  6f da        	bnec	$a2, $a0, {{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	bnec	$a0, $a2, 2f	# CHECK: bnec $a0, $a2, .Ltmp2 # encoding: [0b0110AAAA,0xda]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp2+0, kind: fixup_NANOMIPS_PC4_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC16_NM
				# DISAS: {{.*}}  6f da        	bnec	$a2, $a0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC4_S1	.Ltmp2
	bnec	$a0, $s5, 1f	# CHECK: bnec $a0, $s5, .Ltmp1 # encoding: [0xa4,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC_NM
				# DISAS: {{.*}}  a4 aa 00 00  	bnec	$a0, $s5, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
2:
	bgec	$k0, $t1, 1b	# CHECK: bgec $k0, $t1, .Ltmp0 # encoding: [0xba,0x89,A,0b10AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEC_NM
				# DISAS: {{.*}}  ba 89 00 80  	bgec	$k0, $t1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp0
	bltc	$a1, $s1, 1f	# CHECK: bltc $a1, $s1, .Ltmp1 # encoding: [0x25,0xaa,A,0b10AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTC_NM
				# DISAS: {{.*}}  25 aa 00 80  	bltc	$a1, $s1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	bltuc	$a2, $s2, 1b	# CHECK: bltuc $a2, $s2, .Ltmp0 # encoding: [0x46,0xaa,A,0b11AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTUC_NM
				# DISAS: {{.*}}  46 aa 00 c0  	bltuc	$a2, $s2, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp0
	beqzc	$a0, 1f		# CHECK: beqzc $a0, .Ltmp1 # encoding: [0b0AAAAAAA,0x9a]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC16_NM
				# DISAS: {{.*}}  00 9a        	beqzc	$a0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC7_S1	.Ltmp1
	beqzc	$a4, 1f		# CHECK: beqzc $a4, .Ltmp1 # encoding: [0x00,0x89,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQZC_NM
				# DISAS: {{.*}}  00 89 00 00  	beqzc	$a4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	bnezc	$s0, 1b		# CHECK: bnezc $s0, .Ltmp0 # encoding: [0b0AAAAAAA,0xb8]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC16_NM
				# DISAS: {{.*}}  00 b8        	bnezc	$s0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC7_S1	.Ltmp0
	bnezc	$s4, 1b		# CHECK: bnezc $s4, .Ltmp0 # encoding: [0x80,0xaa,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEZC_NM
				# DISAS: {{.*}}  80 aa 00 00  	bnezc	$s4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp0
	beqc	$a2, $zero, 1b	# CHECK: beqc $a2, $zero, .Ltmp0 # encoding: [0b0AAAAAAA,0x9b]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQCzero_NM
				# DISAS: {{.*}}  00 9b        	beqzc	$a2, 0x{{.*}}
	beqc	$a6, $zero, 1b	# CHECK: beqzc $a6, .Ltmp0 # encoding: [0x0a,0x88,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQC_NM
				# DISAS: {{.*}}  0a 88 00 00  	beqzc	$a6, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp0
	bnec	$s2, $zero, 1f	# CHECK: bnec $s2, $zero, .Ltmp1 # encoding: [0b0AAAAAAA,0xb9]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC7_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNECzero_NM
				# DISAS: {{.*}}  00 b9        	bnezc	$s2, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC7_S1	.Ltmp1
	bnec	$s4, $zero, 1f	# CHECK: bnezc $s4, .Ltmp1 # encoding: [0x14,0xa8,A,0b00AAAAAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC14_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEC_NM
				# DISAS: {{.*}}  14 a8 00 00  	bnezc	$s4, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC14_S1	.Ltmp1
	beqic	$a0, 0, 1f	# CHECK: beqic $a0, 0, .Ltmp1 # encoding: [0x80,0xc8,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BEQIC_NM
				# DISAS: {{.*}}  80 c8 00 00  	beqic	$a0, 0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp1
	bneic	$a2, 1, 1f	# CHECK: bneic $a2, 1, .Ltmp1 # encoding: [0xd0,0xc8,A,0b00001AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BNEIC_NM
				# DISAS: {{.*}}  d0 c8 00 08  	bneic	$a2, 1, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp1
	bgeic	$a4, 64, 1f	# CHECK: bgeic $a4, 64, .Ltmp1 # encoding: [0x0a,0xc9,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEIC_NM
				# DISAS: {{.*}}  0a c9 00 00  	bgeic	$a4, 64, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp1
	bgeiuc	$a6, 126, 1b	# CHECK: bgeiuc $a6, 126, .Ltmp0 # encoding: [0x4f,0xc9,A,0b11110AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BGEIUC_NM
				# DISAS: {{.*}}  4f c9 00 f0  	bgeiuc	$a6, 126, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp0
	bltic	$s2, 127, 1b	# CHECK: bltic $s2, 127, .Ltmp0 # encoding: [0x5b,0xca,A,0b11111AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTIC_NM
				# DISAS: {{.*}}  5b ca 00 f8  	bltic	$s2, 127, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp0
	bltiuc	$s4, 10, 1b	# CHECK: bltiuc $s4, 10, .Ltmp0 # encoding: [0x9c,0xca,A,0b01010AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BLTIUC_NM
				# DISAS: {{.*}}  9c ca 00 50  	bltiuc	$s4, 10, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp0
	bbnezc	$a1, 0, 1b	# CHECK: bbnezc $a1, 0, .Ltmp0 # encoding: [0xb4,0xc8,A,0b00000AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp0+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BBNEZC_NM
				# DISAS: {{.*}}  b4 c8 00 00  	bbnezc	$a1, 0, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp0
	bbeqzc	$s1, 31, 1f	# CHECK: bbeqzc $s1, 31, .Ltmp1 # encoding: [0x24,0xca,A,0b11111AAA]
				# CHECK-NEXT: fixup A - offset: 0, value: .Ltmp1+0, kind: fixup_NANOMIPS_PC11_S1
				# CHECK-NEXT: <MCInst #{{.*}} BBEQZC_NM
				# DISAS: {{.*}}  24 ca 00 f8  	bbeqzc	$s1, 31, 0x{{.*}}
				# DISAS-NEXT: {{.*}}  R_NANOMIPS_PC11_S1	.Ltmp1

	jalrc	$a1		# CHECK: jalrc $ra, $a1	# encoding: [0xb0,0xd8]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
				# DISAS: {{.*}}  b0 d8        	jalrc	$ra, $a1
	jalrc	$s1		# CHECK: jalrc $ra, $s1	# encoding: [0x30,0xda]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC16_NM
				# DISAS: {{.*}}  30 da        	jalrc	$ra, $s1
	jalrc	$t2, $s2	# CHECK: jalrc $t2, $s2	# encoding: [0xd2,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
				# DISAS: {{.*}}  d2 49 00 00  	jalrc	$t2, $s2
	jalrc	$a5, $s5	# CHECK: jalrc $a5, $s5	# encoding: [0x35,0x49,0x00,0x00]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRC_NM
				# DISAS: {{.*}}  35 49 00 00  	jalrc	$a5, $s5
	jalrc.hb $t0, $t1	# CHECK: jalrc.hb $t0, $t1	# encoding: [0x8d,0x49,0x00,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRCHB_NM
				# DISAS: {{.*}}  8d 49 00 10  	jalrc.hb	$t0, $t1
	jalrc.hb $s5, $a5	# CHECK: jalrc.hb $s5, $a5	# encoding: [0xa9,0x4a,0x00,0x10]
				# CHECK-NEXT: # <MCInst #{{.*}} JALRCHB_NM
				# DISAS: {{.*}}  a9 4a 00 10  	jalrc.hb	$s5, $a5
	jrc	$a1	# CHECK: jrc $a1	# encoding: [0xa0,0xd8]
			# CHECK-NEXT: # <MCInst #{{.*}} JRC_NM
			# DISAS: {{.*}}  a0 d8        	jrc	$a1
	jrc	$ra	# CHECK: jrc $ra	# encoding: [0xe0,0xdb]
			# CHECK-NEXT: # <MCInst #{{.*}} JRC_NM
			# DISAS: {{.*}}  e0 db        	jrc	$ra
	1:

	ll	$a2, -4($s4)	# CHECK: ll $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xd1]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
				# DISAS: {{.*}}  d4 a4 fc d1  	ll	$a2, -4($s4)
	ll	$a3, -256($s0)	# CHECK: ll $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xd1]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
				# DISAS: {{.*}}  f0 a4 00 d1  	ll	$a3, -256($s0)
	ll	$a4, 64($s3)	# CHECK: ll $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
				# DISAS: {{.*}}  13 a5 40 51  	ll	$a4, 64($s3)
	ll	$a1, 252($s2)	# CHECK: ll $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LL_NM
				# DISAS: {{.*}}  b2 a4 fc 51  	ll	$a1, 252($s2)
	sc	$a2, -4($s4)	# CHECK: sc $a2, -4($s4) # encoding: [0xd4,0xa4,0xfc,0xd9]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
				# DISAS: {{.*}}  d4 a4 fc d9  	sc	$a2, -4($s4)
	sc	$a3, -256($s0)	# CHECK: sc $a3, -256($s0) # encoding: [0xf0,0xa4,0x00,0xd9]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
				# DISAS: {{.*}}  f0 a4 00 d9  	sc	$a3, -256($s0)
	sc	$a4, 64($s3)	# CHECK: sc $a4, 64($s3) # encoding: [0x13,0xa5,0x40,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
				# DISAS: {{.*}}  13 a5 40 59  	sc	$a4, 64($s3)
	sc	$a1, 252($s2)	# CHECK: sc $a1, 252($s2) # encoding: [0xb2,0xa4,0xfc,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SC_NM
				# DISAS: {{.*}}  b2 a4 fc 59  	sc	$a1, 252($s2)
	llwp	$a0, $a1, ($s2)	# CHECK: llwp $a0, $a1, ($s2) # encoding: [0x92,0xa4,0x29,0x51]
				# CHECK-NEXT: <MCInst  #{{.*}} LLWP_NM
				# DISAS: {{.*}}  92 a4 29 51  	llwp $a0, $a1, ($s2)
	scwp	$t0, $t1, ($s6)	# CHECK: scwp $t0, $t1, ($s6) # encoding: [0x96,0xa5,0x69,0x59]
				# CHECK-NEXT: <MCInst  #{{.*}} SCWP_NM
				# DISAS: {{.*}}  96 a5 69 59  	scwp $t0, $t1, ($s6)

	pref	0, -4($s4)	# CHECK: pref 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  14 a4 fc 98  	pref	0, -4($s4)
	pref	1, -256($s0)	# CHECK: pref 1, -256($s0) # encoding: [0x30,0xa4,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} PREFs9_NM
				# DISAS: {{.*}}  30 a4 00 98  	pref	1, -256($s0)
	pref	12, 64($s3)	# CHECK: pref 12, 64($s3) # encoding: [0x93,0x85,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  93 85 40 30  	pref	12, 64($s3)
	pref	17, 252($s2)	# CHECK: pref 17, 252($s2) # encoding: [0x32,0x86,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  32 86 fc 30  	pref	17, 252($s2)
	pref	30, 4092($t5)	# CHECK: pref 30, 4092($t5) # encoding: [0xc3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} PREF_NM
				# DISAS: {{.*}}  c3 87 fc 3f  	pref	30, 4092($t5)

	synci	-4($s4)	# CHECK: synci -4($s4) # encoding: [0xf4,0xa7,0xfc,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f4 a7 fc 98  	synci	-4($s4)
	synci	-256($s0)	# CHECK: synci -256($s0) # encoding: [0xf0,0xa7,0x00,0x98]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCIs9_NM
				# DISAS: {{.*}}  f0 a7 00 98  	synci	-256($s0)
	synci	64($s3)	# CHECK: synci 64($s3) # encoding: [0xf3,0x87,0x40,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  f3 87 40 30  	synci	64($s3)
	synci	252($s2)	# CHECK: synci 252($s2) # encoding: [0xf2,0x87,0xfc,0x30]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  f2 87 fc 30  	synci	252($s2)
	synci	4092($t5)	# CHECK: synci 4092($t5) # encoding: [0xe3,0x87,0xfc,0x3f]
				# CHECK-NEXT: <MCInst  #{{.*}} SYNCI_NM
				# DISAS: {{.*}}  e3 87 fc 3f  	synci	4092($t5)

	cache	0, -4($s4)	# CHECK: cache 0, -4($s4) # encoding: [0x14,0xa4,0xfc,0xb9]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
				# DISAS: {{.*}}  14 a4 fc b9  	cache	0, -4($s4)
	cache	1, -256($s0)	# CHECK: cache 1, -256($s0) # encoding: [0x30,0xa4,0x00,0xb9]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
				# DISAS: {{.*}}  30 a4 00 b9  	cache	1, -256($s0)
	cache	12, 64($s3)	# CHECK: cache 12, 64($s3) # encoding: [0x93,0xa5,0x40,0x39]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
				# DISAS: {{.*}}  93 a5 40 39  	cache	12, 64($s3)
	cache	31, 252($s2)	# CHECK: cache 31, 252($s2) # encoding: [0xf2,0xa7,0xfc,0x39]
				# CHECK-NEXT: <MCInst  #{{.*}} CACHE_NM
				# DISAS: {{.*}}  f2 a7 fc 39  	cache	31, 252($s2)

	sync		# CHECK: sync	# encoding: [0x00,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  00 80 06 c0  	sync
	sync 0		# CHECK: sync	# encoding: [0x00,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  00 80 06 c0  	sync
	sync_wmb	# CHECK: sync_wmb	# encoding: [0x04,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  04 80 06 c0  	sync_wmb
	sync 4		# CHECK: sync_wmb	# encoding: [0x04,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  04 80 06 c0  	sync_wmb
	sync_mb		# CHECK: sync_mb	# encoding: [0x10,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  10 80 06 c0  	sync_mb
	sync 0x10	# CHECK: sync_mb	# encoding: [0x10,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  10 80 06 c0  	sync_mb
	sync_acquire	# CHECK: sync_acquire	# encoding: [0x11,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  11 80 06 c0  	sync_acquire
	sync 0x11	# CHECK: sync_acquire	# encoding: [0x11,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  11 80 06 c0  	sync_acquire
	sync_release	# CHECK: sync_release	# encoding: [0x12,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  12 80 06 c0  	sync_release
	sync 0x12	# CHECK: sync_release	# encoding: [0x12,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  12 80 06 c0  	sync_release
	sync_rmb	# CHECK: sync_rmb	# encoding: [0x13,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  13 80 06 c0  	sync_rmb
	sync 0x13	# CHECK: sync_rmb	# encoding: [0x13,0x80,0x06,0xc0]
			# CHECK-NEXT: # <MCInst #{{.*}} SYNC_NM
			# DISAS: {{.*}}  13 80 06 c0  	sync_rmb

	wait		# CHECK: wait	# encoding: [0x00,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
			# DISAS: {{.*}}  00 20 7f c3  	wait
	wait 0		# CHECK: wait	# encoding: [0x00,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
			# DISAS: {{.*}}  00 20 7f c3  	wait
	wait 1		# CHECK: wait 1	# encoding: [0x01,0x20,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
			# DISAS: {{.*}}  01 20 7f c3  	wait	1
	wait 1023	# CHECK: wait 1023	# encoding: [0xff,0x23,0x7f,0xc3]
			# CHECK-NEXT: # <MCInst #{{.*}} WAIT_NM
			# DISAS: {{.*}}  ff 23 7f c3  	wait	1023

	li	$t3, 65536	# CHECK: lui $t3, %hi(0x10) # encoding: [0xe1,0xe1,0x00,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  e1 e1 00 00  	lui	$t3, %hi(0x10000)
	li	$t4, -4096	# CHECK: lui $t4, %hi(-0x1) # encoding: [0x5f,0xe0,0xfd,0xff]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  5f e0 fd ff  	lui	$t4, %hi(0xfffff000)
	li	$t5, -2147483648	# CHECK: lui $t5, %hi(-0x80000) # encoding: [0x60,0xe0,0x01,0x00]
				# CHECK-NEXT: <MCInst #{{.*}} LUI_NM
				# DISAS: {{.*}}  60 e0 01 00  	lui	$t5, %hi(0x80000000)

	jrc $ra
	.type   g_8,@object
	.comm   g_8,16,16
