#!/bin/sh

# Compare the generated tables of our refactored TableGen to the original ones.

# We skip Alpha because it is no longer supported by upstream LLVM
archs="AArch64 ARM PPC"
file_names="GenAsmWriter GenDisassemblerTables GenInstrInfo GenRegisterInfo GenSubtargetInfo GenSystemOperands"
release="18"
gen_dir="output_tmp"
mkdir "$gen_dir"

build_upstream_llvm()
{
  echo "Build upstream llvm-tblgen"
  git checkout "auto-sync-$release-base"
  cd build
  cmake -G Ninja -DCMAKE_BUILD_TYPE=Release ../llvm
  cmake --build . --target llvm-tblgen --config Release
  cd ..
  git checkout "auto-sync-$release"
}

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
      elif [ $file_name = "GenSystemOperands" ] && [ $arch != "PPC" ]; then
        ../build/bin/llvm-tblgen --gen-searchable-tables "$table_type" -o "$out_file" -I "$arch_include" -I "../llvm/include" "$arch_include/$arch.td"
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

# Build original LLVM
build_upstream_llvm
if gen_all "--color" "CPP_LLVM"; then
  exit 1
fi

echo "Diff LLVM files"
mismatch="false"
for arch in $archs; do
  for file_name in $file_names; do
    out_CPP_LLVM="$gen_dir/$arch$file_name""_CPP_LLVM.inc"
    out_CPP_CS="$gen_dir/$arch$file_name""_CPP_CS.inc"

    if [ ! -e "$out_CPP_CS" ]; then
      continue
    fi

    diff "$out_CPP_LLVM" "$out_CPP_CS" > /dev/null
    if [ $? -ne 0 ]; then
      echo "The following files mismatch, but they should be the same: $out_files"
      mismatch="true"
    fi
  done
done

echo "C table syntax check"
malformed_syntax="false"
for arch in $archs; do
  for file_name in $file_names; do
    out_file="$gen_dir/$arch"$file_name"_C_CS.inc"

    if [ ! -e "$out_file" ]; then
      continue
    fi

    gcc -fsyntax-only $out_file 2> /dev/null
    if [ $? -ne 0 ]; then
      echo "Invalid C syntax in file: $out_file"
      malformed_syntax="true"
    fi
  done
done

if [ $mismatch = "true" ] || [ $malformed_syntax = "true" ]; then
  exit 1
fi
