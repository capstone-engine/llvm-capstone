# RUN: not llvm-mc -arch=nanomips < %s 2>%t1
# RUN: FileCheck %s < %t1
        .text
        .globl foo
foo:
        ll  $a4, 66($s3) # CHECK: error: invalid operand for instruction
        sc  $a2, -6($s4) # CHECK: error: invalid operand for instruction
        ll  $a4, 33($s3) # CHECK: error: invalid operand for instruction
        sc  $a2, -1($s4) # CHECK: error: invalid operand for instruction
