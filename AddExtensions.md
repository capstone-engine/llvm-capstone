# Adding new extensions

Some architectures have processor extensions not defined in upstream LLVM.
If we want to have them in Capstone, we can write the `.td` files for them here.

The process is rather straight forward.

1. Write all definitions into a new `llvm/lib/Target/<ARCH>/<ARCH>NewExtension.td` file.
2. Define a feature and property for the new extension:
    ```python
    def FeatureExtensionName : SubtargetFeature<"extenion_name", "HasExtensionName", "true",
      "Enable the new extension bla bla bla.">;

    def HasExtensionName : Predicate<"Subtarget->hasExtensionName()">,
                           AssemblerPredicate<(all_of FeatureExtensionName), "extenion_name">;  
    ```
3. Ensure the newly defined instructions have `let Predicates = [HasExtensionName];` set.
4. Add or exclude the extension from existing processor models:
    - Search for `list<Predicate> UnsupportedFeatures` in the target directory.
      Add `HasExtensionName` to all processor models which don't support it.
    - The processor models which do support it, need the instructions in the scheduler.
      Check how it is done in the processor's scheduler file.
5. Include the `<ARCH>NewExtension.td` at the bottom of `<ARCH>InstrInfo.td` with `include "<ARCH>NewExtension.td"`.

**Note:** As an example you can refer to `AArch64AppleProprietary.td`.
Also search for `HasAMX` and `FeatureAMX` to see how those flags are used in the files.

# Deprecated Features

Capstone needs to support features which were removed by LLVM in the past.
Here we explain how to reintroduce them.

## Reintroduction

To get the old features back we copy them from the old `.td` files and include them in the new ones.

To include removed features from previous LLVM versions do the following:

1. Checkout the last LLVM version the feature was present.
2. Copy all feature related definitions into a `<ARCH>Deprecated.td` file.
3. Checkout the newest LLVM version again.
4. Wrap the different definition types in include guards. For example the `InstrInfo` definitions could be included in:

    ```
    #ifndef INCLUDED_CAPSTONE_DEPR_INSTR
    #ifdef CAPSTONE_DEPR_INSTR
    #define INCLUDED_CAPSTONE_DEPR_INSTR // Ensures it is only included once
    
    [Instruction definitions of removed feature]
    
    #endif // INCLUDED_CAPSTONE_DEPR_INSTR
    #endif // CAPSTONE_DEPR_INSTR
    ```

    _Note that the order of `#ifndef` and `#ifdef` matters (otherwise you'll get an error from `tblgen`)._

    **This step is somewhat optional. It might be enough to write all definitions in a single file and `#include` them once into `ARCHInstrInfo.td` at the bottom.**

5. Include the definitions in the current definition files with:

    ```
    #define CAPSTONE_DEPR_INSTR
    include "<ARCH>Deprecated.td"
    ```

## Notes
- It is possible that you have to change some definitions slightly.
Because certain classes no longer exist or were replaced (e.g.: `GCCBuiltin` -> `ClangBuiltin`).
- Some processors might need the feature flag (`Has<DeprecatedFeature>`) added
in their `UnsupportedFeatures` list.
