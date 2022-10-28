# Decoder emitter

The decoder emitter generates the code for the disassembler of several architectures.

The general design is split into three classes.

- `DecoderEmitter`: Controls code generation and pre-processes `Records`.
- `FilterChooser`: Builds the decoding state machine.
- `Filter`: Represents a state in the decoding state machine.

The rough process of the code generation is described in the following diagram.
The details about the state machine creation (the `recurse` step) are explained below.

```
DecoderEmitter                  FilterChooser                           Filter          PrinterInterface           Output Stream

    ┌───┐                           ┌───┐                               ┌───┐                ┌───┐                 ┌───┐
    │   ├────────┐                  │   │                               │   │                │   │                 │   │
    │   │        │Separate instr.   │   │                               │   │                │   │                 │   │
    │   │        │into groups       │   │                               │   │                │   │                 │   │
    │   │        │                  │   │                               │   │                │   │                 │   │
    │   │◄───────┘                  │   │                               │   │                │   │                 │   │
    │   │                           │   │                               │   │                │   │                 │   │
    │   │ Start separation process  │   │ Separate                      │   │                │   │                 │   │
    │   │ of instr. groups          │   │ into                          │   │                │   │                 │   │
    │   ├──────────────────────────►│   │ subsets                       │   │                │   │                 │   │
    │   │                           │   ├───────────┐                   │   │                │   │                 │   │
    │   │                           │   │           │ Add Filter to     │   │                │   │                 │   │
    │   │                           │   │           │ Filterlist        │   │                │   │                 │   │
    │   │                           │   │    R      ├──────────────────►│   │                │   │                 │   │
    │   │                           │   │    E      │                   │   │                │   │                 │   │
    │   │                           │   │    C      │                   │   │                │   │                 │   │
    │   │                           │   │    U      │                   └───┘                │   │                 │   │
    │   │                           │   │    R      │                                        │   │                 │   │
    │   │                           │   │    S      │ Request Filter decoder string          │   │                 │   │
    │   │                           │   │    E      ├───────────────────────────────────────►│   │                 │   │
    │   │                           │   │           │                                        │   │                 │   │
    │   │                           │   │           │◄───────────────────────────────────────┤   │                 │   │
    │   │                           │   │           │ Return decoder string                  │   │                 │   │
    │   │                           │   │◄──────────┘                                        │   │                 │   │
    │   │                           │   │                                                    │   │                 │   │
    │   │                           │   │                                                    │   │                 │   │
    │   │◄──────────────────────────┤   │                                                    │   │                 │   │
    │   │  Return list of decoder   │   │                                                    │   │                 │   │
    │   │  strings                  └───┘                                                    │   │                 │   │
    │   │                                                                                    │   │                 │   │
    │   │                                                                                    │   │                 │   │
    │   │                                                                                    │   │                 │   │
    │   │                                                                                    │   │                 │   │
    │   │  Request to print decoders into file                                               │   │ Print           │   │
    │   ├───────────────────────────────────────────────────────────────────────────────────►│   │ decoders        │   │
    │   │                                                                                    │   ├────────────────►│   │
    │   │                                                                                    │   │                 │   │
    │   │  Request to print helper functions into file                                       │   │ Print           │   │
    │   ├───────────────────────────────────────────────────────────────────────────────────►│   │ functions       │   │
    │   │                                                                                    │   ├────────────────►│   │
    │   │                                                                                    │   │                 │   │
    └───┘                                                                                    │   │                 │   │
                                                                                             └───┘                 └───┘
```

# Instruction Decoding

The disassembler of LLVM decodes instructions with the help of a non-cyclic [state machine](https://en.wikipedia.org/wiki/Finite-state_machine).

Reading this will help you a lot to understand the code!
Because we describe here how this state machine is generated.

## The general idea

We start with a single set off all instructions of an architecture.
These instructions are put in groups. Each group holds the instructions of a specific CPU extension or mode
(think of `Thumb` mode in ARM, or vector extension of some processors).
For each group a state machine is generated. It decodes instructions of this group.

To generate the state machine we first take all instructions of a group.
This set is split into subsets each of which can be distinguished to the other subsets by a certain bit field in its encoding.
This and other separation information is called `Filter`.

The subsets are further split into smaller subsets until only single instructions are left.

_Each subset represents a state in our decoding state machine._

<hr>

This generator will build the states for several instruction groups (the `uint8_t decoder<InstrGroup>[]` tables)
and a decoder function which walks over those states until it fails or identifies an instruction.

## Step 1 - Determine best bits for set separation

In this step we determine which bits are most suited to separate the current set of instructions into subsets.

Lets assume we have four instructions with a width of 8 bits (all instructions of a group have the same bit width).

```text
Bit   0 1 2 3 4 5 6 7

IA:   0 1 0 ? ? 1 ? ?
IB:   ? 1 0 0 0 ? ? ?
IC:   0 0 0 0 1 ? ? ?
ID:   ? 0 1 ? ? ? ? ?

`?` = unset bit position (holds variable bits)
```

Now we have a `BitAttr` mask which saves which bits are suitable for filtering the set into subsets.

```
BitAttr:  . . _ _ _ _ _ _

"." = Bit already used by previous Filter.
"_" = Bit unset/None (not used by previous Filter)
```

In the beginning all bits are unset (`_`) but the more instructions become filtered the more bits are set to `.`.

To determine which bits in instruction `IA` to `ID` can be used to distinguish them, we define a automaton (see below).
The automaton gets the value at `BitAttr[i]` (`Filtered` or `Unset`/`None`) as start state and as input the bits at `IA[i]`, `IB[i]`, `IC[i]`, `ID[i]`.

The result can be: `AllSet` (`S`), `All_unset` (`U`), `Mixed` (`M`) or `Filtered` (`F`)
The result is written to `BitAttr[i]`.

```
                           0,1
                          ┌─────┐
                          │     │
                          │     │
                          │     │
                          │     ▼
                        ┌─┴───────┐
         0,1            │         │                      0,1,?
     ┌─────────────────►│All_Set  │                     ┌────┐
     │                  │         │                     │    │
     │                  └────┬────┘                     │    │
     │                       │                          │    │
     │                       │?                         │    ▼
     │                       │                      ┌───┴──────┐
     │                       ▼                      │          │
┌────┴───┐             ┌───────────┐                │ Filtered │
│        │             │           ├─────┐          │          │
│ None   │             │ Mixed     │     │0,1,?     └──────────┘
│        │             │           │◄────┘
└───┬────┘             └───────────┘
    │                        ▲
    │                        │0,1
    │                        │
    │                   ┌────┴─────┐
    │   ?               │          │
    └──────────────────►│ All_unset│
                        │          │
                        └─┬────────┘
                          │     ▲
                          │     │
                          │     │
                          │     │
                          └─────┘
                             ?
```

Here is how our little example would play out.

```
IA:       0 1 0 ? ? 1 ? ?
IB:       ? 1 0 0 0 ? ? ?
IC:       0 0 0 0 1 ? ? ?
ID:       ? 0 1 ? ? ? ? ?

BitAttr:  . . _ _ _ _ _ _

becomes

BitAttr:  . . S M M M U U
```

## Step 2 - Determine potential Filters

Now we report regions in the `BitAttr` which might be suitable as a `Filter`.
A `region` is the index of the start bit and the length.

There are three region reporting strategies:

1. Report only successive `S` bits.
2. Report only successive `S` or successive `M` bits.
3. A special and rare case (explained in code).

The strategies are tried from 1 to 3.
If non of them works we get a conflict (the set of instructions are indistinguishable). This case is explained in the code.

If we continue with our example we get the following regions:

```
region = (startIndex, length)

Bit       0 1 2 3 4 5 6 7
BitAttr:  . . S M M M U U

strategy 1 = R1 = (2, 1)
strategy 2 = R1 = (2, 1), R2 = (3, 3)
```

## Step 3 - Select the best region as Filter

We determine the best `Filter` by checking which region distinguishes the most instructions.
Lets assume we used strategy 2 here, so we have:

```
Bit  0 1 2 3 4 5 6 7

IA:  0 1 0 ? ? 1 ? ?
IB:  ? 1 0 0 0 ? ? ?
IC:  0 0 0 0 1 ? ? ?
ID:  ? 0 1 ? ? ? ? ?

R1 = (2, 1)
R2 = (3, 3)
```

`R1` can create two subsets: `(IA, IB, IC)` and `(ID)`.
`R2` can create only one subset `(IA, IB, IC, ID)`.

`R1` can separate instructions by bit 2 (which is either `0` or `1` in all instructions).

`R2` has not a single bit position where all instruction bits are known (remember: `?` are variable bits, we can't distinguish them).
Therefore it can not separate instructions.

Because `R1` creates the most subsets it is chosen as the Filter.

## Recurse

The `BitAttr` bits are reset to either `Filtered` (if used) or `Unset` and passed on to the inferior `FilterChooser` of each subset.
For each subset we start the process again from `Step 1` (not for subsets of size 1 obviously).

