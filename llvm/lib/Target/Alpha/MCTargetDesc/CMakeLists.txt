add_llvm_library(LLVMAlphaDesc
  AlphaMCTargetDesc.cpp
  AlphaMCAsmInfo.cpp

  LINK_COMPONENTS
  AlphaInfo
  MC

  ADD_TO_COMPONENT
  Alpha
  )

# add_llvm_library_dependencies(LLVMAlphaDesc
#   LLVMAlphaInfo
#   LLVMMC
#   )

add_dependencies(LLVMAlphaDesc AlphaCommonTableGen)
