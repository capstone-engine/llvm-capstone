# RUN: not llvm-mc %s -triple=nanomips-elf -mattr +fix-hw110880 -show-encoding -show-inst 2>&1 |\
# RUN: FileCheck %s --check-prefixes CHECK-FIX,CHECK-ANY

# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst |\
# RUN: FileCheck %s --check-prefixes CHECK,CHECK-ANY

  .globl foo
  .text
  .ent	foo
  .type	foo, @function
foo:
# Candidate patterns to trigger the transform
  addiu[48] $a0, $a0, 0x56f1bf00 # CHECK-FIX: error: immediate value in 48-bit instruction requires transform for 110880
				# CHECK: addiu $a0, $a0, 1458683648 # encoding: [0x81,0x60,0x00,0xbf,0xf1,0x56]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU48_NM
  addiu $a0, $a0, 0xd2012400	# CHECK-FIX: error: immediate value in 48-bit instruction requires transform for 110880
				# CHECK: addiu $a0, $a0, -771677184 # encoding: [0x81,0x60,0x00,0x24,0x01,0xd2]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIU48_NM
  li[48] $a0, 0xf101a4ff		# CHECK-FIX: error: immediate value in 48-bit instruction requires transform for 110880
				# CHECK: li $a0, 0xf101a4ff # encoding: [0x80,0x60,0xff,0xa4,0x01,0xf1]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
  li $a0, 0x51012400		# CHECK-FIX: error: immediate value in 48-bit instruction requires transform for hw110880
				# CHECK: li $a0, 0x51012400  # encoding: [0x80,0x60,0x00,0x24,0x01,0x51]
				# CHECK-NEXT: # <MCInst #{{.*}} LI48_NM
  addiu[gp48] $a0, $gp, 0x73f1acaa # CHECK-FIX: error: immediate value in 48-bit instruction requires transform for hw110880
				# CHECK: addiu.b32 $a0, $gp, 1945218218 # encoding: [0x82,0x60,0xaa,0xac,0xf1,0x73]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUGP48_NM
  addiu $a0, $gp, 0x5ef1bd55	# CHECK-FIX: error: immediate value in 48-bit instruction requires transform for hw110880
				# CHECK: addiu.b32 $a0, $gp, 1592900949 # encoding: [0x82,0x60,0x55,0xbd,0xf1,0x5e]
				# CHECK-NEXT: # <MCInst #{{.*}} ADDIUGP48_NM

  # ADDIUPC/LAPC/SWPC are always emitted with PC-relative relocation
  # so checking/handling is deffered to the linker.
  addiupc $a0, 0x52012400	# CHECK-ANY: lapc.b $a0, 0x52012400	# encoding: [0x83,0x60,A,A,A,A]
				# CHECK-ANY-NEXT: fixup A - offset: 2, value: 1375806464, kind: fixup_NANOMIPS_PC_I32
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} LAPC48_NM
  lapc.b $a0, 0x51012400	# CHECK-ANY: lapc.b $a0, 0x51012400	# encoding: [0x83,0x60,A,A,A,A]
				# CHECK-ANY-NEXT: fixup A - offset: 2, value: 1359029248, kind: fixup_NANOMIPS_PC_I32
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} LAPC48_NM
  swpc $a0, 0x51012400		# CHECK-ANY: swpc $a0, 0x51012400	# encoding: [0x8f,0x60,A,A,A,A]
				# CHECK-ANY-NEXT: fixup A - offset: 2, value: 1359029248, kind: fixup_NANOMIPS_PC_I32
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} SWPC_NM

  # Non-candidate patterns - part of the required pattern
  # masked off.
  addiu[48] $a0, $a0, 0xb6f12f00  # CHECK-ANY: addiu $a0, $a0, -1225707776 # encoding: [0x81,0x60,0x00,0x2f,0xf1,0xb6]
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} ADDIU48_NM
  addiu $a0, $a0, 0x55016c00	# CHECK-ANY: addiu $a0, $a0, 1426156544	# encoding: [0x81,0x60,0x00,0x6c,0x01,0x55]
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} ADDIU48_NM
  li[48] $a0, 0xe101a4ff	# CHECK-ANY: li $a0, 0xe101a4ff	# encoding: [0x80,0x60,0xff,0xa4,0x01,0xe1]
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} LI48_NM
  li $a0, 0x51002400		# CHECK-ANY: li $a0, 0x51002400	# encoding: [0x80,0x60,0x00,0x24,0x00,0x51]
				# CHECK-ANY-NEXT: # <MCInst #{{.*}} LI48_NM
  .end foo
