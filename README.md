# Capstone's LLVM with refactored TableGen backends

This LLVM version has the purpose to generate code for the
[Capstone disassembler](https://github.com/capstone-engine/capstone).

It refactors the TableGen emitter backends, so they can emit C code
in addition to the C++ code they normally emit.

Please note that within LLVM we speak of a `Target` if we refer to an architecture.

## Code generation

### Relevant files

The TableGen emitter backends are located in `llvm/utils/TableGen/`.

The target definition files (`.td`), which define the
instructions, operands, features etc., can be
found in `llvm/lib/Target/<ARCH>/`.

### Code generation overview

Generating code for a target has 6 steps:

```
                                                                         5                 6
                                                                    ┌──────────┐      ┌──────────┐
                                                                    │Printer   │      │CS .inc   │
    1               2                 3                4        ┌──►│Capstone  ├─────►│files     │
┌───────┐     ┌───────────┐     ┌───────────┐     ┌──────────┐  │   └──────────┘      └──────────┘
│ .td   │     │           │     │           │     │ Code-    │  │
│ files ├────►│ TableGen  ├────►│  CodeGen  ├────►│ Emitter  │◄─┤
└───────┘     └──────┬────┘     └───────────┘     └──────────┘  │
                     │                                 ▲        │   ┌──────────┐      ┌──────────┐
                     └─────────────────────────────────┘        └──►│Printer   ├─────►│LLVM .inc │
                                                                    │LLVM      │      │files     │
                                                                    └──────────┘      └──────────┘
```

1. LLVM targets are defined in `.td` files. They describe instructions, operands,
features and other properties.

2. [LLVM TableGen](https://llvm.org/docs/TableGen/index.html) parses these files
and converts them to an internal representation of [Classes, Records, DAGs](https://llvm.org/docs/TableGen/ProgRef.html)
 and other types.

3. In the second step a TableGen component called [CodeGen](https://llvm.org/docs/CodeGenerator.html)
abstracts this even further.
The result is a representation which is _not_ specific to any target
(e.g. the `CodeGenInstruction` class can represent a machine instruction of any target).

4. Different code emitter backends use the result of the former two components to
generated code.

5. Whenever the emitter emits code it calls a `Printer`. Either the `PrinterCapstone` to emit C or `PrinterLLVM` to emit C++.
Which one is controlled by the `--printerLang=[CCS,C++]` option passed to `llvm-tblgen`.

6. After the emitter backend is done, the `Printer` writes the `output_stream` content into the `.inc` files.

### Emitter backends and their use cases

We use the following emitter backends

| Name | Generated Code | Note |
|------|----------------|------|
| AsmMatcherEmitter | Mapping tables for Capstone | |
| AsmWriterEmitter | State machine to decode the asm-string for a `MCInst` | |
| DecoderEmitter | State machine which decodes bytes to a `MCInst`. | |
| InstrInfoEmitter | Tables with instruction information (instruction enum, instr. operand information...) | |
| RegisterInfoEmitter | Tables with register information (register enum, register type info...) | |
| SubtargetEmitter | Table about the target features. | |
| SearchableTablesEmitter | Usually used to generate tables and decoding functions for system registers. | **1.** Not all targets use this. |
| | | **2.** Backend can't access the target name. Wherever the target name is needed `__ARCH__` or `##ARCH##` is printed and later replaced. |

## Developer notes

- If you find C++ code within the generated files check the `.td` files again.
You can replace any C++ code except calls to template functions (those are used by `auto-sync's` `TemplateCollector`).
If the `PrinterCapstone` is used the calls to template functions are patched in `PrinterCapstone::resolveTemplateCall()`.
Syntax to define C++ code in `.td` files is `[{ code }]`
Also see: TableGen syntax for [TokCode](https://llvm.org/docs/TableGen/ProgRef.html#grammar-token-tokcode) (usually used for code snippets).

- If the mapping files miss operand types or access information, then the `.td` files are incomplete (happens surprisingly often).
You need to search for the instruction or operands with missing or incorrect values and fix them.
  ```
    Wrong access attributes for:
      - Registers, Immediates: The instructions defines "out" and "in" operands incorrectly.
      - Memory: The "mayLoad" or "mayStore" variable is not set for the instruction.

    Operand type is invalid:
      - The "OperandType" variable is unset for this operand type.
  ```

- If certain target features (e.g. architecture extensions) were removed from LLVM or you want to add your own,
checkout [DeprecatedFeatures.md](DeprecatedFeatures.md).