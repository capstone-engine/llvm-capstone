# The file testing Nop insertion with R_NANOMIPS_ALIGN for relaxation.

# Relaxation enabled:
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+relax < %s \
# RUN:     | llvm-objdump -d - \
# RUN:     | FileCheck -check-prefix=RELAX-INST %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+relax < %s \
# RUN:     | llvm-readelf -r - | FileCheck -check-prefix=ALIGN-RELOC %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -mattr=+relax < %s \
# RUN:     | llvm-readelf -s - | FileCheck -check-prefix=ALIGN-RELOC-SYMS %s

# Relaxation disabled: since this is controlled soley by the .linkrelax
# directive in case of assembly, use an assembler define to modulate.
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -defsym norelax=1 < %s \
# RUN:     | llvm-objdump -dx - \
# RUN:     | FileCheck -check-prefix=NORELAX-INST %s
# RUN: llvm-mc -filetype=obj -triple nanomips-elf -defsym norelax=1 < %s \
# RUN:     | llvm-readelf -r - | FileCheck -check-prefix=NOALIGN-RELOC %s

# We need to insert N bytes of NOPs and R_NANOMIPS_ALIGN relocation
# type for .align N directive when linker relaxation enabled.
# Linker could satisfy alignment by removing NOPs after linker relaxation.

# The first R_NANOMIPS_ALIGN come from
# MCELFStreamer::InitSections() emitCodeAlignment(getTextSectionAligntment()).
.ifndef norelax
	.linkrelax
.endif
test:
	.p2align 2
# ALIGN-RELOC: {{0*}}0 {{.*}} R_NANOMIPS_ALIGN       00000002   .Ltmp{{.}} + 0
# ALIGN-RELOC-SYMS: {{.*}}: {{0*}}2     0 NOTYPE  LOCAL  DEFAULT   ABS .Ltmp{{.}}
	move	$a0, $zero
	.p2align 3
# ALIGN-RELOC: {{0*}}2 {{.*}} R_NANOMIPS_ALIGN       00000003   .Ltmp{{.}} + 0
# ALIGN-RELOC-SYMS: {{.*}}: {{0*}}3     6 NOTYPE  LOCAL  DEFAULT   ABS .Ltmp{{.}}
# RELAX-INST:  nop
# RELAX-INST:  nop32
	addu	$a0, $a0, $a1
	.align 4
# ALIGN-RELOC: {{0*}}a {{.*}} R_NANOMIPS_ALIGN       00000004   .Ltmp{{.}} + 0
# ALIGN-RELOC-SYMS: {{.*}}: {{0*}}4     6 NOTYPE  LOCAL  DEFAULT   ABS .Ltmp{{.}}
.LBB0_2:
# RELAX-INST:  nop
# RELAX-INST:  nop32
# NORELAX-INST: nop32
	add	$a0, $a0, $a1
	.p2align 3
# ALIGN-RELOC: {{0*}}14 {{.*}} R_NANOMIPS_ALIGN       00000003   .Ltmp{{.}} + 0
# ALIGN-RELOC-SYMS: {{.*}}: {{0*}}3     4 NOTYPE  LOCAL  DEFAULT   ABS .Ltmp{{.}}
# RELAX-INST:  nop32
# NORELAX-INST: nop32
.constant_pool:
.long	3126770193
	add	$a0, $a0, $a1
	nop32
# Alignment directive with specific padding value 0x01.
# We will not emit R_NANOMIPS_ALIGN in this case.
# The behavior is the same as GNU assembler.
	.p2align 3, 1
# ALIGN-RELOC-NOT: {{0*}}28 {{.*}} R_NANOMIPS_ALIGN       00000003   .Ltmp{{.}} + 0
# RELAX-INST:  01 01 01 01
	jr $ra
# NOALIGN-RELOC-NOT: R_NANOMIPS_ALIGN
# Code alignment of a byte size less than the size of a nop must be treated
# as no alignment. This used to trigger a fatal error with relaxation enabled
# as the calculation to emit the worst-case sequence of nops would overflow.
	.p2align        1
	add	$a0, $a0, $a1
	.p2align        0
	add	$a0, $a0, $a1
# We only need to insert R_NANOMIPS_ALIGN for code section
# when the linker relaxation enabled.
        .data
	.p2align        3
# ALIGN-RELOC-NOT: {{0*}}0 {{.*}} R_NANOMIPS_ALIGN       00000003   .Ltmp{{.}} + 0
data1:
	.word 7
	.p2align        4
# ALIGN-RELOC-NOT: {{0*}}10 {{.*}} R_NANOMIPS_ALIGN       00000004   .Ltmp{{.}} + 0
data2:
	.word 9
