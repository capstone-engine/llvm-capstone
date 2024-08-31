# RUN: llvm-mc %s -triple=nanomips-elf -show-encoding -show-inst \
# RUN: -mattr=+tlb | FileCheck %s

	tlbinv		# CHECK: tlbinv	# encoding: [0x00,0x20,0x7f,0x07]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBINV_NM
	tlbinvf		# CHECK: tlbinvf	# encoding: [0x00,0x20,0x7f,0x17]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBINVF_NM
	tlbp		# CHECK: tlbp	# encoding: [0x00,0x20,0x7f,0x03]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBP_NM
	tlbr		# CHECK: tlbr	# encoding: [0x00,0x20,0x7f,0x13]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBR_NM
	tlbwi		# CHECK: tlbwi	# encoding: [0x00,0x20,0x7f,0x23]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBWI_NM
	tlbwr		# CHECK: tlbwr	# encoding: [0x00,0x20,0x7f,0x33]
			# CHECK-NEXT: # <MCInst #{{.*}} TLBWR_NM
