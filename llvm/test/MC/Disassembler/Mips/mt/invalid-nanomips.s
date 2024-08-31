# RUN: not llvm-mc -arch=nanomips -mattr=+mt < %s 2>%t1
# RUN: FileCheck %s < %t1
  dmt 4                   # CHECK: error: invalid operand for instruction
  dmt $a0, $a1              # CHECK: error: invalid operand for instruction
  dmt $a1, 0($a0)           # CHECK: error: invalid operand for instruction
  emt 4                   # CHECK: error: invalid operand for instruction
  emt $a0, $a1              # CHECK: error: invalid operand for instruction
  emt $a1, 0($a1)           # CHECK: error: invalid operand for instruction
  dvpe 4                  # CHECK: error: invalid operand for instruction
  dvpe $a0, $a1             # CHECK: error: invalid operand for instruction
  dvpe $a1, 0($a0)          # CHECK: error: invalid operand for instruction
  evpe 4                  # CHECK: error: invalid operand for instruction
  evpe $a0, $a1             # CHECK: error: invalid operand for instruction
  evpe $a1, 0($a1)          # CHECK: error: invalid operand for instruction
  # FIXME: add tests for mftr/mttr.
