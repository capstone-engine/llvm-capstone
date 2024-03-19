#!/bin/sh

# Compare the generated tables of our refactored TableGen to the original ones.

# We skip Alpha because it is no longer supported by upstream LLVM
archs="AArch64 ARM PPC"
file_names="GenAsmWriter GenDisassemblerTables GenInstrInfo GenRegisterInfo GenSubtargetInfo GenSystemOperands"
release="18"
gen_dir="output_tmp"

if [ ! -d $gen_dir ]; then
  mkdir "$gen_dir"
fi

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
  echo "$0 [--rebuild]"
  echo "\trebuild - Rebuild Capstone llvm-tblgen after upstream LLVM tables were generated."
  exit 0
fi

gen_all()
{
  cd $gen_dir
  table_type="$1"
  postfix="$2"
  echo "Generate inc files with $table_type as $postfix"

  for arch in $archs; do
    for file_name in $file_names; do
      out_file="$arch$file_name""_$postfix.inc"
      if [ "$arch" = "PPC" ]; then
        arch_include="../llvm/lib/Target/PowerPC"
      else
        arch_include="../llvm/lib/Target/$arch"
      fi

      echo "\t$arch - $out_file"

      if [ $file_name = "GenAsmWriter" ]; then
        ../build/bin/llvm-tblgen --gen-asm-writer "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
      elif [ $file_name = "GenDisassemblerTables" ]; then
        ../build/bin/llvm-tblgen --gen-disassembler "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
      elif [ $file_name = "GenInstrInfo" ]; then
        ../build/bin/llvm-tblgen --gen-instr-info "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
      elif [ $file_name = "GenRegisterInfo" ]; then
        ../build/bin/llvm-tblgen --gen-register-info "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
      elif [ $file_name = "GenSubtargetInfo" ]; then
        ../build/bin/llvm-tblgen --gen-subtarget "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
      elif [ $file_name = "GenSystemOperands" ]; then
        if [ $arch != "PPC" ] ; then
          ../build/bin/llvm-tblgen --gen-searchable-tables "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
        fi
      else
        echo "File $file_name not handled."
        exit 1
      fi
      if [ $? -ne 0 ]; then
        echo "Generation of $out_file failed."
        return 0
      fi
    done
  done

  cd ..
  return 1
}

# Generate patched
if gen_all "--printerLang=C++" "CPP_CS"; then
  exit 1
fi

if gen_all "--printerLang=CCS" "C_CS"; then
  exit 1
fi

