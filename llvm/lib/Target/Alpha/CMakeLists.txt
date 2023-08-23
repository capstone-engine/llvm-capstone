add_llvm_component_group(Alpha)

set(LLVM_TARGET_DEFINITIONS Alpha.td)

tablegen(LLVM AlphaGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM AlphaGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM AlphaGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM AlphaGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM AlphaGenCallingConv.inc -gen-callingconv)
tablegen(LLVM AlphaGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(AlphaCommonTableGen)

add_llvm_target(AlphaCodeGen
  AlphaAsmPrinter.cpp
  AlphaBranchSelector.cpp
  AlphaInstrInfo.cpp
  AlphaISelDAGToDAG.cpp
  AlphaISelLowering.cpp
  AlphaFrameLowering.cpp
  AlphaLLRP.cpp
  AlphaRegisterInfo.cpp
  AlphaSubtarget.cpp
  AlphaTargetMachine.cpp
  AlphaSelectionDAGInfo.cpp

  LINK_COMPONENTS
  AlphaInfo
  AlphaDesc
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  Support
  Target

  ADD_TO_COMPONENT
  Alpha 
  )

# add_llvm_library_dependencies(LLVMAlphaCodeGen
#   LLVMAlphaDesc
#   LLVMAlphaInfo
#   LLVMAsmPrinter
#   LLVMCodeGen
#   LLVMCore
#   LLVMMC
#   LLVMSelectionDAG
#   LLVMSupport
#   LLVMTarget
#   )

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
