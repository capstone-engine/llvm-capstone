#!/bin/sh

# Compare the generated tables of our refactored TableGen to the original ones.
archs="AArch64 ARM PPC"
file_names="GenAsmWriter GenDisassemblerTables GenInstrInfo GenRegisterInfo GenSubtargetInfo GenSystemOperands"
release="18"
repo_root=$(git rev-parse --show-toplevel)
gen_dir="$repo_root/output_tmp"

# Requires that LLVM tables were generated before.
echo "Diff LLVM files (blanks and empty lines are ignored)"
for arch in $archs; do
  for file_name in $file_names; do
    out_CPP_LLVM="$gen_dir/$arch$file_name""_CPP_LLVM.inc"
    out_CPP_CS="$gen_dir/$arch$file_name""_CPP_CS.inc"

    if [ ! -e "$out_CPP_CS" ]; then
      continue
    fi

    diff -w -B "$out_CPP_LLVM" "$out_CPP_CS" > /dev/null
    if [ $? -ne 0 ]; then
      echo "The following files mismatch: $out_CPP_LLVM $out_CPP_CS"
      mismatch="true"
    fi
  done
done

echo "C table syntax check"
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

if [ "$1" = "--rebuild" ]; then
  build_capstone_llvm
fi

if [ -n "$mismatch" ] || [ -n "$malformed_syntax" ]; then
  exit 1
fi
