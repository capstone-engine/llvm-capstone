//===- AsmMatcherEmitter.cpp - Generate an assembly matcher ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits a target specifier matcher for converting parsed
// assembly operands in the MCInst structures. It also emits a matcher for
// custom operand parsing.
//
// Converting assembly operands into MCInst structures
// ---------------------------------------------------
//
// The input to the target specific matcher is a list of literal tokens and
// operands. The target specific parser should generally eliminate any syntax
// which is not relevant for matching; for example, comma tokens should have
// already been consumed and eliminated by the parser. Most instructions will
// end up with a single literal token (the instruction name) and some number of
// operands.
//
// Some example inputs, for X86:
//   'addl' (immediate ...) (register ...)
//   'add' (immediate ...) (memory ...)
//   'call' '*' %epc
//
// The assembly matcher is responsible for converting this input into a precise
// machine instruction (i.e., an instruction with a well defined encoding). This
// mapping has several properties which complicate matching:
//
//  - It may be ambiguous; many architectures can legally encode particular
//    variants of an instruction in different ways (for example, using a smaller
//    encoding for small immediates). Such ambiguities should never be
//    arbitrarily resolved by the assembler, the assembler is always responsible
//    for choosing the "best" available instruction.
//
//  - It may depend on the subtarget or the assembler context. Instructions
//    which are invalid for the current mode, but otherwise unambiguous (e.g.,
//    an SSE instruction in a file being assembled for i486) should be accepted
//    and rejected by the assembler front end. However, if the proper encoding
//    for an instruction is dependent on the assembler context then the matcher
//    is responsible for selecting the correct machine instruction for the
//    current mode.
//
// The core matching algorithm attempts to exploit the regularity in most
// instruction sets to quickly determine the set of possibly matching
// instructions, and the simplify the generated code. Additionally, this helps
// to ensure that the ambiguities are intentionally resolved by the user.
//
// The matching is divided into two distinct phases:
//
//   1. Classification: Each operand is mapped to the unique set which (a)
//      contains it, and (b) is the largest such subset for which a single
//      instruction could match all members.
//
//      For register classes, we can generate these subgroups automatically. For
//      arbitrary operands, we expect the user to define the classes and their
//      relations to one another (for example, 8-bit signed immediates as a
//      subset of 32-bit immediates).
//
//      By partitioning the operands in this way, we guarantee that for any
//      tuple of classes, any single instruction must match either all or none
//      of the sets of operands which could classify to that tuple.
//
//      In addition, the subset relation amongst classes induces a partial order
//      on such tuples, which we use to resolve ambiguities.
//
//   2. The input can now be treated as a tuple of classes (static tokens are
//      simple singleton sets). Each such tuple should generally map to a single
//      instruction (we currently ignore cases where this isn't true, whee!!!),
//      which we can emit a simple matcher for.
//
// Custom Operand Parsing
// ----------------------
//
//  Some targets need a custom way to parse operands, some specific instructions
//  can contain arguments that can represent processor flags and other kinds of
//  identifiers that need to be mapped to specific values in the final encoded
//  instructions. The target specific custom operand parsing works in the
//  following way:
//
//   1. A operand match table is built, each entry contains a mnemonic, an
//      operand class, a mask for all operand positions for that same
//      class/mnemonic and target features to be checked while trying to match.
//
//   2. The operand matcher will try every possible entry with the same
//      mnemonic and will check if the target feature for this mnemonic also
//      matches. After that, if the operand to be matched has its index
//      present in the mask, a successful match occurs. Otherwise, fallback
//      to the regular operand parsing.
//
//   3. For a match success, each operand class that has a 'ParserMethod'
//      becomes part of a switch from where the custom method is called.
//
//===----------------------------------------------------------------------===//

#include "AsmMatcherEmitterTypes.h"
#include "Printer.h"

using namespace llvm;

#define DEBUG_TYPE "asm-matcher-emitter"

cl::OptionCategory AsmMatcherEmitterCat("Options for -gen-asm-matcher");

static cl::opt<std::string>
    MatchPrefix("match-prefix", cl::init(""),
                cl::desc("Only match instructions with the given prefix"),
                cl::cat(AsmMatcherEmitterCat));

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)

class AsmMatcherEmitter {
  RecordKeeper &Records;
  PrinterLLVM &PI;

public:
  AsmMatcherEmitter(RecordKeeper &R, PrinterLLVM &PI) : Records(R), PI(PI) {}

  void run();
};

//
// ClassInfo implementation
//

bool ClassInfo::isRelatedTo(const ClassInfo &RHS) const {
    // Tokens are only related to tokens.
    if (Kind == Token || RHS.Kind == Token)
        return Kind == Token && RHS.Kind == Token;
    
    // Registers classes are only related to registers classes, and only if
    // their intersection is non-empty.
    if (isRegisterClass() || RHS.isRegisterClass()) {
        if (!isRegisterClass() || !RHS.isRegisterClass())
            return false;
    
        RegisterSet Tmp;
        std::insert_iterator<RegisterSet> II(Tmp, Tmp.begin());
        std::set_intersection(Registers.begin(), Registers.end(),
                              RHS.Registers.begin(), RHS.Registers.end(), II,
                              LessRecordByID());
    
        return !Tmp.empty();
    }
    
    // Otherwise we have two users operands; they are related if they are in the
    // same class hierarchy.
    //
    // FIXME: This is an oversimplification, they should only be related if they
    // intersect, however we don't have that information.
    assert(isUserClass() && RHS.isUserClass() && "Unexpected class!");
    const ClassInfo *Root = this;
    while (!Root->SuperClasses.empty())
        Root = Root->SuperClasses.front();
    
    const ClassInfo *RHSRoot = &RHS;
    while (!RHSRoot->SuperClasses.empty())
        RHSRoot = RHSRoot->SuperClasses.front();
    
    return Root == RHSRoot;
}

bool ClassInfo::isSubsetOf(const ClassInfo &RHS) const {
    // This is a subset of RHS if it is the same class...
    if (this == &RHS)
        return true;
    
    // ... or if any of its super classes are a subset of RHS.
    SmallVector<const ClassInfo *, 16> Worklist(SuperClasses.begin(),
                                                SuperClasses.end());
    SmallPtrSet<const ClassInfo *, 16> Visited;
    while (!Worklist.empty()) {
        auto *CI = Worklist.pop_back_val();
        if (CI == &RHS)
            return true;
        for (auto *Super : CI->SuperClasses)
            if (Visited.insert(Super).second)
                Worklist.push_back(Super);
    }
    
    return false;
}

int ClassInfo::getTreeDepth() const {
    int Depth = 0;
    const ClassInfo *Root = this;
    while (!Root->SuperClasses.empty()) {
        Depth++;
        Root = Root->SuperClasses.front();
    }
    return Depth;
}

const ClassInfo *ClassInfo::findRoot() const {
    const ClassInfo *Root = this;
    while (!Root->SuperClasses.empty())
        Root = Root->SuperClasses.front();
    return Root;
}

bool ClassInfo::operator<(const ClassInfo &RHS) const {
    if (this == &RHS)
        return false;

    // First, enforce the ordering between the three different types of class.
    // Tokens sort before registers, which sort before user classes.
    if (Kind == Token) {
        if (RHS.Kind != Token)
            return true;
        assert(RHS.Kind == Token);
    } else if (isRegisterClass()) {
        if (RHS.Kind == Token)
            return false;
        else if (RHS.isUserClass())
            return true;
        assert(RHS.isRegisterClass());
    } else if (isUserClass()) {
        if (!RHS.isUserClass())
            return false;
        assert(RHS.isUserClass());
    } else {
        llvm_unreachable("Unknown ClassInfoKind");
    }

    if (Kind == Token || isUserClass()) {
        // Related tokens and user classes get sorted by depth in the inheritence
        // tree (so that subclasses are before their parents).
        if (isRelatedTo(RHS)) {
            if (getTreeDepth() > RHS.getTreeDepth())
                return true;
            if (getTreeDepth() < RHS.getTreeDepth())
                return false;
      } else {
          // Unrelated tokens and user classes are ordered by the name of their
          // root nodes, so that there is a consistent ordering between
          // unconnected trees.
          return findRoot()->ValueName < RHS.findRoot()->ValueName;
      }
    } else if (isRegisterClass()) {
        // For register sets, sort by number of registers. This guarantees that
        // a set will always sort before all of it's strict supersets.
        if (Registers.size() != RHS.Registers.size())
            return Registers.size() < RHS.Registers.size();
    } else {
        llvm_unreachable("Unknown ClassInfoKind");
    }

    // FIXME: We should be able to just return false here, as we only need a
    // partial order (we use stable sorts, so this is deterministic) and the
    // name of a class shouldn't be significant. However, some of the backends
    // accidentally rely on this behaviour, so it will have to stay like this
    // until they are fixed.
    return ValueName < RHS.ValueName;
}

//
// MatchableInfo implementation
//
int MatchableInfo::findAsmOperand(StringRef N, int SubOpIdx) const {
    auto I = find_if(AsmOperands, [&](const AsmOperand &Op) {
        return Op.SrcOpName == N && Op.SubOpIdx == SubOpIdx;
    });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
}

int MatchableInfo::findAsmOperandNamed(StringRef N, int LastIdx) const {
    auto I =
        llvm::find_if(llvm::drop_begin(AsmOperands, LastIdx + 1),
                      [&](const AsmOperand &Op) { return Op.SrcOpName == N; });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
}

int MatchableInfo::findAsmOperandOriginallyNamed(StringRef N) const {
    auto I = find_if(AsmOperands, [&](const AsmOperand &Op) {
        return Op.OrigSrcOpName == N;
    });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
}

bool MatchableInfo::operator<(const MatchableInfo &RHS) const {
    // The primary comparator is the instruction mnemonic.
    if (int Cmp = Mnemonic.compare_insensitive(RHS.Mnemonic))
        return Cmp == -1;
    
    if (AsmOperands.size() != RHS.AsmOperands.size())
        return AsmOperands.size() < RHS.AsmOperands.size();
    
    // Compare lexicographically by operand. The matcher validates that other
    // orderings wouldn't be ambiguous using \see couldMatchAmbiguouslyWith().
    for (unsigned i = 0, e = AsmOperands.size(); i != e; ++i) {
        if (*AsmOperands[i].Class < *RHS.AsmOperands[i].Class)
            return true;
        if (*RHS.AsmOperands[i].Class < *AsmOperands[i].Class)
            return false;
    }
    
    // Give matches that require more features higher precedence. This is useful
    // because we cannot define AssemblerPredicates with the negation of
    // processor features. For example, ARM v6 "nop" may be either a HINT or
    // MOV. With v6, we want to match HINT. The assembler has no way to
    // predicate MOV under "NoV6", but HINT will always match first because it
    // requires V6 while MOV does not.
    if (RequiredFeatures.size() != RHS.RequiredFeatures.size())
        return RequiredFeatures.size() > RHS.RequiredFeatures.size();
    
    // For X86 AVX/AVX512 instructions, we prefer vex encoding because the
    // vex encoding size is smaller. Since X86InstrSSE.td is included ahead
    // of X86InstrAVX512.td, the AVX instruction ID is less than AVX512 ID.
    // We use the ID to sort AVX instruction before AVX512 instruction in
    // matching table.
    if (TheDef->isSubClassOf("Instruction") &&
        TheDef->getValueAsBit("HasPositionOrder"))
        return TheDef->getID() < RHS.TheDef->getID();
    
    return false;
}

bool MatchableInfo::couldMatchAmbiguouslyWith(const MatchableInfo &RHS) const {
    // The primary comparator is the instruction mnemonic.
    if (Mnemonic != RHS.Mnemonic)
        return false;
    
    // Different variants can't conflict.
    if (AsmVariantID != RHS.AsmVariantID)
        return false;
    
    // The number of operands is unambiguous.
    if (AsmOperands.size() != RHS.AsmOperands.size())
        return false;
    
    // Otherwise, make sure the ordering of the two instructions is unambiguous
    // by checking that either (a) a token or operand kind discriminates them,
    // or (b) the ordering among equivalent kinds is consistent.
    
    // Tokens and operand kinds are unambiguous (assuming a correct target
    // specific parser).
    for (unsigned i = 0, e = AsmOperands.size(); i != e; ++i)
        if (AsmOperands[i].Class->Kind != RHS.AsmOperands[i].Class->Kind ||
            AsmOperands[i].Class->Kind == ClassInfo::Token)
            if (*AsmOperands[i].Class < *RHS.AsmOperands[i].Class ||
                *RHS.AsmOperands[i].Class < *AsmOperands[i].Class)
                return false;
    
    // Otherwise, this operand could commute if all operands are equivalent, or
    // there is a pair of operands that compare less than and a pair that
    // compare greater than.
    bool HasLT = false, HasGT = false;
    for (unsigned i = 0, e = AsmOperands.size(); i != e; ++i) {
        if (*AsmOperands[i].Class < *RHS.AsmOperands[i].Class)
            HasLT = true;
        if (*RHS.AsmOperands[i].Class < *AsmOperands[i].Class)
            HasGT = true;
    }
    
    return HasLT == HasGT;
}

//
// OperandMatchEntry implementation
//
OperandMatchEntry OperandMatchEntry::create(const MatchableInfo *mi, ClassInfo *ci,
                                            unsigned opMask) {
    OperandMatchEntry X;
    X.OperandMask = opMask;
    X.CI = ci;
    X.MI = mi;
    return X;
}

//
// AsmMatcherInfo implementations
//
const SubtargetFeatureInfo *AsmMatcherInfo::getSubtargetFeature(Record *Def) const {
    assert(Def->isSubClassOf("Predicate") && "Invalid predicate type!");
    const auto &I = SubtargetFeatures.find(Def);
    return I == SubtargetFeatures.end() ? nullptr : &I->second;
}

bool AsmMatcherInfo::hasOptionalOperands() const {
return any_of(Classes,
              [](const ClassInfo &Class) { return Class.IsOptional; });
}

LLVM_DUMP_METHOD void MatchableInfo::dump() const {
  errs() << TheDef->getName() << " -- " << "flattened:\"" << AsmString <<"\"\n";

  errs() << "  variant: " << AsmVariantID << "\n";

  for (unsigned i = 0, e = AsmOperands.size(); i != e; ++i) {
    const AsmOperand &Op = AsmOperands[i];
    errs() << "  op[" << i << "] = " << Op.Class->ClassName << " - ";
    errs() << '\"' << Op.Token << "\"\n";
  }
}
#endif

static std::pair<StringRef, StringRef>
parseTwoOperandConstraint(StringRef S, ArrayRef<SMLoc> Loc) {
  // Split via the '='.
  std::pair<StringRef, StringRef> Ops = S.split('=');
  if (Ops.second == "")
    PrintFatalError(Loc, "missing '=' in two-operand alias constraint");
  // Trim whitespace and the leading '$' on the operand names.
  size_t start = Ops.first.find_first_of('$');
  if (start == std::string::npos)
    PrintFatalError(Loc, "expected '$' prefix on asm operand name");
  Ops.first = Ops.first.slice(start + 1, std::string::npos);
  size_t end = Ops.first.find_last_of(" \t");
  Ops.first = Ops.first.slice(0, end);
  // Now the second operand.
  start = Ops.second.find_first_of('$');
  if (start == std::string::npos)
    PrintFatalError(Loc, "expected '$' prefix on asm operand name");
  Ops.second = Ops.second.slice(start + 1, std::string::npos);
  end = Ops.second.find_last_of(" \t");
  Ops.first = Ops.first.slice(0, end);
  return Ops;
}

void MatchableInfo::formTwoOperandAlias(StringRef Constraint) {
  // Figure out which operands are aliased and mark them as tied.
  std::pair<StringRef, StringRef> Ops =
    parseTwoOperandConstraint(Constraint, TheDef->getLoc());

  // Find the AsmOperands that refer to the operands we're aliasing.
  int SrcAsmOperand = findAsmOperandNamed(Ops.first);
  int DstAsmOperand = findAsmOperandNamed(Ops.second);
  if (SrcAsmOperand == -1)
    PrintFatalError(TheDef->getLoc(),
                    "unknown source two-operand alias operand '" + Ops.first +
                    "'.");
  if (DstAsmOperand == -1)
    PrintFatalError(TheDef->getLoc(),
                    "unknown destination two-operand alias operand '" +
                    Ops.second + "'.");

  // Find the ResOperand that refers to the operand we're aliasing away
  // and update it to refer to the combined operand instead.
  for (ResOperand &Op : ResOperands) {
    if (Op.Kind == ResOperand::RenderAsmOperand &&
        Op.AsmOperandNum == (unsigned)SrcAsmOperand) {
      Op.AsmOperandNum = DstAsmOperand;
      break;
    }
  }
  // Remove the AsmOperand for the alias operand.
  AsmOperands.erase(AsmOperands.begin() + SrcAsmOperand);
  // Adjust the ResOperand references to any AsmOperands that followed
  // the one we just deleted.
  for (ResOperand &Op : ResOperands) {
    switch(Op.Kind) {
    default:
      // Nothing to do for operands that don't reference AsmOperands.
      break;
    case ResOperand::RenderAsmOperand:
      if (Op.AsmOperandNum > (unsigned)SrcAsmOperand)
        --Op.AsmOperandNum;
      break;
    }
  }
}

/// extractSingletonRegisterForAsmOperand - Extract singleton register,
/// if present, from specified token.
static void
extractSingletonRegisterForAsmOperand(MatchableInfo::AsmOperand &Op,
                                      const AsmMatcherInfo &Info,
                                      StringRef RegisterPrefix) {
  StringRef Tok = Op.Token;

  // If this token is not an isolated token, i.e., it isn't separated from
  // other tokens (e.g. with whitespace), don't interpret it as a register name.
  if (!Op.IsIsolatedToken)
    return;

  if (RegisterPrefix.empty()) {
    std::string LoweredTok = Tok.lower();
    if (const CodeGenRegister *Reg = Info.Target.getRegisterByName(LoweredTok))
      Op.SingletonReg = Reg->TheDef;
    return;
  }

  if (!Tok.startswith(RegisterPrefix))
    return;

  StringRef RegName = Tok.substr(RegisterPrefix.size());
  if (const CodeGenRegister *Reg = Info.Target.getRegisterByName(RegName))
    Op.SingletonReg = Reg->TheDef;

  // If there is no register prefix (i.e. "%" in "%eax"), then this may
  // be some random non-register token, just ignore it.
}

void MatchableInfo::initialize(const AsmMatcherInfo &Info,
                               SmallPtrSetImpl<Record*> &SingletonRegisters,
                               AsmVariantInfo const &Variant,
                               bool HasMnemonicFirst) {
  AsmVariantID = Variant.AsmVariantNo;
  AsmString =
    CodeGenInstruction::FlattenAsmStringVariants(AsmString,
                                                 Variant.AsmVariantNo);

  tokenizeAsmString(Info, Variant);

  // The first token of the instruction is the mnemonic, which must be a
  // simple string, not a $foo variable or a singleton register.
  if (AsmOperands.empty())
    PrintFatalError(TheDef->getLoc(),
                  "Instruction '" + TheDef->getName() + "' has no tokens");

  assert(!AsmOperands[0].Token.empty());
  if (HasMnemonicFirst) {
    Mnemonic = AsmOperands[0].Token;
    if (Mnemonic[0] == '$')
      PrintFatalError(TheDef->getLoc(),
                      "Invalid instruction mnemonic '" + Mnemonic + "'!");

    // Remove the first operand, it is tracked in the mnemonic field.
    AsmOperands.erase(AsmOperands.begin());
  } else if (AsmOperands[0].Token[0] != '$')
    Mnemonic = AsmOperands[0].Token;

  // Compute the require features.
  for (Record *Predicate : TheDef->getValueAsListOfDefs("Predicates"))
    if (const SubtargetFeatureInfo *Feature =
            Info.getSubtargetFeature(Predicate))
      RequiredFeatures.push_back(Feature);

  // Collect singleton registers, if used.
  for (MatchableInfo::AsmOperand &Op : AsmOperands) {
    extractSingletonRegisterForAsmOperand(Op, Info, Variant.RegisterPrefix);
    if (Record *Reg = Op.SingletonReg)
      SingletonRegisters.insert(Reg);
  }

  const RecordVal *DepMask = TheDef->getValue("DeprecatedFeatureMask");
  if (!DepMask)
    DepMask = TheDef->getValue("ComplexDeprecationPredicate");

  HasDeprecation =
      DepMask ? !DepMask->getValue()->getAsUnquotedString().empty() : false;
}

/// Append an AsmOperand for the given substring of AsmString.
void MatchableInfo::addAsmOperand(StringRef Token, bool IsIsolatedToken) {
  AsmOperands.push_back(AsmOperand(IsIsolatedToken, Token));
}

/// tokenizeAsmString - Tokenize a simplified assembly string.
void MatchableInfo::tokenizeAsmString(const AsmMatcherInfo &Info,
                                      AsmVariantInfo const &Variant) {
  StringRef String = AsmString;
  size_t Prev = 0;
  bool InTok = false;
  bool IsIsolatedToken = true;
  for (size_t i = 0, e = String.size(); i != e; ++i) {
    char Char = String[i];
    if (Variant.BreakCharacters.find(Char) != std::string::npos) {
      if (InTok) {
        addAsmOperand(String.slice(Prev, i), false);
        Prev = i;
        IsIsolatedToken = false;
      }
      InTok = true;
      continue;
    }
    if (Variant.TokenizingCharacters.find(Char) != std::string::npos) {
      if (InTok) {
        addAsmOperand(String.slice(Prev, i), IsIsolatedToken);
        InTok = false;
        IsIsolatedToken = false;
      }
      addAsmOperand(String.slice(i, i + 1), IsIsolatedToken);
      Prev = i + 1;
      IsIsolatedToken = true;
      continue;
    }
    if (Variant.SeparatorCharacters.find(Char) != std::string::npos) {
      if (InTok) {
        addAsmOperand(String.slice(Prev, i), IsIsolatedToken);
        InTok = false;
      }
      Prev = i + 1;
      IsIsolatedToken = true;
      continue;
    }

    switch (Char) {
    case '\\':
      if (InTok) {
        addAsmOperand(String.slice(Prev, i), false);
        InTok = false;
        IsIsolatedToken = false;
      }
      ++i;
      assert(i != String.size() && "Invalid quoted character");
      addAsmOperand(String.slice(i, i + 1), IsIsolatedToken);
      Prev = i + 1;
      IsIsolatedToken = false;
      break;

    case '$': {
      if (InTok) {
        addAsmOperand(String.slice(Prev, i), IsIsolatedToken);
        InTok = false;
        IsIsolatedToken = false;
      }

      // If this isn't "${", start new identifier looking like "$xxx"
      if (i + 1 == String.size() || String[i + 1] != '{') {
        Prev = i;
        break;
      }

      size_t EndPos = String.find('}', i);
      assert(EndPos != StringRef::npos &&
             "Missing brace in operand reference!");
      addAsmOperand(String.slice(i, EndPos+1), IsIsolatedToken);
      Prev = EndPos + 1;
      i = EndPos;
      IsIsolatedToken = false;
      break;
    }

    default:
      InTok = true;
      break;
    }
  }
  if (InTok && Prev != String.size())
    addAsmOperand(String.substr(Prev), IsIsolatedToken);
}

bool MatchableInfo::validate(StringRef CommentDelimiter, bool IsAlias) const {
  // Reject matchables with no .s string.
  if (AsmString.empty())
    PrintFatalError(TheDef->getLoc(), "instruction with empty asm string");

  // Reject any matchables with a newline in them, they should be marked
  // isCodeGenOnly if they are pseudo instructions.
  if (AsmString.find('\n') != std::string::npos)
    PrintFatalError(TheDef->getLoc(),
                  "multiline instruction is not valid for the asmparser, "
                  "mark it isCodeGenOnly");

  // Remove comments from the asm string.  We know that the asmstring only
  // has one line.
  if (!CommentDelimiter.empty() &&
      StringRef(AsmString).contains(CommentDelimiter))
    PrintFatalError(TheDef->getLoc(),
                  "asmstring for instruction has comment character in it, "
                  "mark it isCodeGenOnly");

  // Reject matchables with operand modifiers, these aren't something we can
  // handle, the target should be refactored to use operands instead of
  // modifiers.
  //
  // Also, check for instructions which reference the operand multiple times,
  // if they don't define a custom AsmMatcher: this implies a constraint that
  // the built-in matching code would not honor.
  std::set<std::string> OperandNames;
  for (const AsmOperand &Op : AsmOperands) {
    StringRef Tok = Op.Token;
    if (Tok[0] == '$' && Tok.contains(':'))
      PrintFatalError(TheDef->getLoc(),
                      "matchable with operand modifier '" + Tok +
                      "' not supported by asm matcher.  Mark isCodeGenOnly!");
    // Verify that any operand is only mentioned once.
    // We reject aliases and ignore instructions for now.
    if (!IsAlias && TheDef->getValueAsString("AsmMatchConverter").empty() &&
        Tok[0] == '$' && !OperandNames.insert(std::string(Tok)).second) {
      LLVM_DEBUG({
        errs() << "warning: '" << TheDef->getName() << "': "
               << "ignoring instruction with tied operand '"
               << Tok << "'\n";
      });
      return false;
    }
  }

  return true;
}

static std::string getEnumNameForToken(StringRef Str) {
  std::string Res;

  for (char C : Str) {
    switch (C) {
    case '*': Res += "_STAR_"; break;
    case '%': Res += "_PCT_"; break;
    case ':': Res += "_COLON_"; break;
    case '!': Res += "_EXCLAIM_"; break;
    case '.': Res += "_DOT_"; break;
    case '<': Res += "_LT_"; break;
    case '>': Res += "_GT_"; break;
    case '-': Res += "_MINUS_"; break;
    case '#': Res += "_HASH_"; break;
    default:
      if (isAlnum(C))
        Res += C;
      else
        Res += "_" + utostr((unsigned)C) + "_";
    }
  }

  return Res;
}

ClassInfo *AsmMatcherInfo::getTokenClass(StringRef Token) {
  ClassInfo *&Entry = TokenClasses[std::string(Token)];

  if (!Entry) {
    Classes.emplace_front();
    Entry = &Classes.front();
    Entry->Kind = ClassInfo::Token;
    Entry->ClassName = "Token";
    Entry->Name = "MCK_" + getEnumNameForToken(Token);
    Entry->ValueName = std::string(Token);
    Entry->PredicateMethod = "<invalid>";
    Entry->RenderMethod = "<invalid>";
    Entry->ParserMethod = "";
    Entry->DiagnosticType = "";
    Entry->IsOptional = false;
    Entry->DefaultMethod = "<invalid>";
  }

  return Entry;
}

ClassInfo *
AsmMatcherInfo::getOperandClass(const CGIOperandList::OperandInfo &OI,
                                int SubOpIdx) {
  Record *Rec = OI.Rec;
  if (SubOpIdx != -1)
    Rec = cast<DefInit>(OI.MIOperandInfo->getArg(SubOpIdx))->getDef();
  return getOperandClass(Rec, SubOpIdx);
}

ClassInfo *
AsmMatcherInfo::getOperandClass(Record *Rec, int SubOpIdx) {
  if (Rec->isSubClassOf("RegisterOperand")) {
    // RegisterOperand may have an associated ParserMatchClass. If it does,
    // use it, else just fall back to the underlying register class.
    const RecordVal *R = Rec->getValue("ParserMatchClass");
    if (!R || !R->getValue())
      PrintFatalError(Rec->getLoc(),
                      "Record `" + Rec->getName() +
                          "' does not have a ParserMatchClass!\n");

    if (DefInit *DI= dyn_cast<DefInit>(R->getValue())) {
      Record *MatchClass = DI->getDef();
      if (ClassInfo *CI = AsmOperandClasses[MatchClass])
        return CI;
    }

    // No custom match class. Just use the register class.
    Record *ClassRec = Rec->getValueAsDef("RegClass");
    if (!ClassRec)
      PrintFatalError(Rec->getLoc(), "RegisterOperand `" + Rec->getName() +
                    "' has no associated register class!\n");
    if (ClassInfo *CI = RegisterClassClasses[ClassRec])
      return CI;
    PrintFatalError(Rec->getLoc(), "register class has no class info!");
  }

  if (Rec->isSubClassOf("RegisterClass")) {
    if (ClassInfo *CI = RegisterClassClasses[Rec])
      return CI;
    PrintFatalError(Rec->getLoc(), "register class has no class info!");
  }

  if (!Rec->isSubClassOf("Operand"))
    PrintFatalError(Rec->getLoc(), "Operand `" + Rec->getName() +
                  "' does not derive from class Operand!\n");
  Record *MatchClass = Rec->getValueAsDef("ParserMatchClass");
  if (ClassInfo *CI = AsmOperandClasses[MatchClass])
    return CI;

  PrintFatalError(Rec->getLoc(), "operand has no match class!");
}

struct LessRegisterSet {
  bool operator() (const RegisterSet &LHS, const RegisterSet & RHS) const {
    // std::set<T> defines its own compariso "operator<", but it
    // performs a lexicographical comparison by T's innate comparison
    // for some reason. We don't want non-deterministic pointer
    // comparisons so use this instead.
    return std::lexicographical_compare(LHS.begin(), LHS.end(),
                                        RHS.begin(), RHS.end(),
                                        LessRecordByID());
  }
};

void AsmMatcherInfo::
buildRegisterClasses(SmallPtrSetImpl<Record*> &SingletonRegisters) {
  const auto &Registers = Target.getRegBank().getRegisters();
  auto &RegClassList = Target.getRegBank().getRegClasses();

  typedef std::set<RegisterSet, LessRegisterSet> RegisterSetSet;

  // The register sets used for matching.
  RegisterSetSet RegisterSets;

  // Gather the defined sets.
  for (const CodeGenRegisterClass &RC : RegClassList)
    RegisterSets.insert(
        RegisterSet(RC.getOrder().begin(), RC.getOrder().end()));

  // Add any required singleton sets.
  for (Record *Rec : SingletonRegisters) {
    RegisterSets.insert(RegisterSet(&Rec, &Rec + 1));
  }

  // Introduce derived sets where necessary (when a register does not determine
  // a unique register set class), and build the mapping of registers to the set
  // they should classify to.
  std::map<Record*, RegisterSet> RegisterMap;
  for (const CodeGenRegister &CGR : Registers) {
    // Compute the intersection of all sets containing this register.
    RegisterSet ContainingSet;

    for (const RegisterSet &RS : RegisterSets) {
      if (!RS.count(CGR.TheDef))
        continue;

      if (ContainingSet.empty()) {
        ContainingSet = RS;
        continue;
      }

      RegisterSet Tmp;
      std::swap(Tmp, ContainingSet);
      std::insert_iterator<RegisterSet> II(ContainingSet,
                                           ContainingSet.begin());
      std::set_intersection(Tmp.begin(), Tmp.end(), RS.begin(), RS.end(), II,
                            LessRecordByID());
    }

    if (!ContainingSet.empty()) {
      RegisterSets.insert(ContainingSet);
      RegisterMap.insert(std::make_pair(CGR.TheDef, ContainingSet));
    }
  }

  // Construct the register classes.
  std::map<RegisterSet, ClassInfo*, LessRegisterSet> RegisterSetClasses;
  unsigned Index = 0;
  for (const RegisterSet &RS : RegisterSets) {
    Classes.emplace_front();
    ClassInfo *CI = &Classes.front();
    CI->Kind = ClassInfo::RegisterClass0 + Index;
    CI->ClassName = "Reg" + utostr(Index);
    CI->Name = "MCK_Reg" + utostr(Index);
    CI->ValueName = "";
    CI->PredicateMethod = ""; // unused
    CI->RenderMethod = "addRegOperands";
    CI->Registers = RS;
    // FIXME: diagnostic type.
    CI->DiagnosticType = "";
    CI->IsOptional = false;
    CI->DefaultMethod = ""; // unused
    RegisterSetClasses.insert(std::make_pair(RS, CI));
    ++Index;
  }

  // Find the superclasses; we could compute only the subgroup lattice edges,
  // but there isn't really a point.
  for (const RegisterSet &RS : RegisterSets) {
    ClassInfo *CI = RegisterSetClasses[RS];
    for (const RegisterSet &RS2 : RegisterSets)
      if (RS != RS2 &&
          std::includes(RS2.begin(), RS2.end(), RS.begin(), RS.end(),
                        LessRecordByID()))
        CI->SuperClasses.push_back(RegisterSetClasses[RS2]);
  }

  // Name the register classes which correspond to a user defined RegisterClass.
  for (const CodeGenRegisterClass &RC : RegClassList) {
    // Def will be NULL for non-user defined register classes.
    Record *Def = RC.getDef();
    if (!Def)
      continue;
    ClassInfo *CI = RegisterSetClasses[RegisterSet(RC.getOrder().begin(),
                                                   RC.getOrder().end())];
    if (CI->ValueName.empty()) {
      CI->ClassName = RC.getName();
      CI->Name = "MCK_" + RC.getName();
      CI->ValueName = RC.getName();
    } else
      CI->ValueName = CI->ValueName + "," + RC.getName();

    Init *DiagnosticType = Def->getValueInit("DiagnosticType");
    if (StringInit *SI = dyn_cast<StringInit>(DiagnosticType))
      CI->DiagnosticType = std::string(SI->getValue());

    Init *DiagnosticString = Def->getValueInit("DiagnosticString");
    if (StringInit *SI = dyn_cast<StringInit>(DiagnosticString))
      CI->DiagnosticString = std::string(SI->getValue());

    // If we have a diagnostic string but the diagnostic type is not specified
    // explicitly, create an anonymous diagnostic type.
    if (!CI->DiagnosticString.empty() && CI->DiagnosticType.empty())
      CI->DiagnosticType = RC.getName();

    RegisterClassClasses.insert(std::make_pair(Def, CI));
  }

  // Populate the map for individual registers.
  for (auto &It : RegisterMap)
    RegisterClasses[It.first] = RegisterSetClasses[It.second];

  // Name the register classes which correspond to singleton registers.
  for (Record *Rec : SingletonRegisters) {
    ClassInfo *CI = RegisterClasses[Rec];
    assert(CI && "Missing singleton register class info!");

    if (CI->ValueName.empty()) {
      CI->ClassName = std::string(Rec->getName());
      CI->Name = "MCK_" + Rec->getName().str();
      CI->ValueName = std::string(Rec->getName());
    } else
      CI->ValueName = CI->ValueName + "," + Rec->getName().str();
  }
}

void AsmMatcherInfo::buildOperandClasses() {
  std::vector<Record*> AsmOperands =
    Records.getAllDerivedDefinitions("AsmOperandClass");

  // Pre-populate AsmOperandClasses map.
  for (Record *Rec : AsmOperands) {
    Classes.emplace_front();
    AsmOperandClasses[Rec] = &Classes.front();
  }

  unsigned Index = 0;
  for (Record *Rec : AsmOperands) {
    ClassInfo *CI = AsmOperandClasses[Rec];
    CI->Kind = ClassInfo::UserClass0 + Index;

    ListInit *Supers = Rec->getValueAsListInit("SuperClasses");
    for (Init *I : Supers->getValues()) {
      DefInit *DI = dyn_cast<DefInit>(I);
      if (!DI) {
        PrintError(Rec->getLoc(), "Invalid super class reference!");
        continue;
      }

      ClassInfo *SC = AsmOperandClasses[DI->getDef()];
      if (!SC)
        PrintError(Rec->getLoc(), "Invalid super class reference!");
      else
        CI->SuperClasses.push_back(SC);
    }
    CI->ClassName = std::string(Rec->getValueAsString("Name"));
    CI->Name = "MCK_" + CI->ClassName;
    CI->ValueName = std::string(Rec->getName());

    // Get or construct the predicate method name.
    Init *PMName = Rec->getValueInit("PredicateMethod");
    if (StringInit *SI = dyn_cast<StringInit>(PMName)) {
      CI->PredicateMethod = std::string(SI->getValue());
    } else {
      assert(isa<UnsetInit>(PMName) && "Unexpected PredicateMethod field!");
      CI->PredicateMethod = "is" + CI->ClassName;
    }

    // Get or construct the render method name.
    Init *RMName = Rec->getValueInit("RenderMethod");
    if (StringInit *SI = dyn_cast<StringInit>(RMName)) {
      CI->RenderMethod = std::string(SI->getValue());
    } else {
      assert(isa<UnsetInit>(RMName) && "Unexpected RenderMethod field!");
      CI->RenderMethod = "add" + CI->ClassName + "Operands";
    }

    // Get the parse method name or leave it as empty.
    Init *PRMName = Rec->getValueInit("ParserMethod");
    if (StringInit *SI = dyn_cast<StringInit>(PRMName))
      CI->ParserMethod = std::string(SI->getValue());

    // Get the diagnostic type and string or leave them as empty.
    Init *DiagnosticType = Rec->getValueInit("DiagnosticType");
    if (StringInit *SI = dyn_cast<StringInit>(DiagnosticType))
      CI->DiagnosticType = std::string(SI->getValue());
    Init *DiagnosticString = Rec->getValueInit("DiagnosticString");
    if (StringInit *SI = dyn_cast<StringInit>(DiagnosticString))
      CI->DiagnosticString = std::string(SI->getValue());
    // If we have a DiagnosticString, we need a DiagnosticType for use within
    // the matcher.
    if (!CI->DiagnosticString.empty() && CI->DiagnosticType.empty())
      CI->DiagnosticType = CI->ClassName;

    Init *IsOptional = Rec->getValueInit("IsOptional");
    if (BitInit *BI = dyn_cast<BitInit>(IsOptional))
      CI->IsOptional = BI->getValue();

    // Get or construct the default method name.
    Init *DMName = Rec->getValueInit("DefaultMethod");
    if (StringInit *SI = dyn_cast<StringInit>(DMName)) {
      CI->DefaultMethod = std::string(SI->getValue());
    } else {
      assert(isa<UnsetInit>(DMName) && "Unexpected DefaultMethod field!");
      CI->DefaultMethod = "default" + CI->ClassName + "Operands";
    }

    ++Index;
  }
}

AsmMatcherInfo::AsmMatcherInfo(Record *asmParser,
                               CodeGenTarget &target,
                               RecordKeeper &records)
  : Records(records), AsmParser(asmParser), Target(target) {
}

/// buildOperandMatchInfo - Build the necessary information to handle user
/// defined operand parsing methods.
void AsmMatcherInfo::buildOperandMatchInfo() {

  /// Map containing a mask with all operands indices that can be found for
  /// that class inside a instruction.
  typedef std::map<ClassInfo *, unsigned, deref<std::less<>>> OpClassMaskTy;
  OpClassMaskTy OpClassMask;

  bool CallCustomParserForAllOperands =
      AsmParser->getValueAsBit("CallCustomParserForAllOperands");
  for (const auto &MI : Matchables) {
    OpClassMask.clear();

    // Keep track of all operands of this instructions which belong to the
    // same class.
    unsigned NumOptionalOps = 0;
    for (unsigned i = 0, e = MI->AsmOperands.size(); i != e; ++i) {
      const MatchableInfo::AsmOperand &Op = MI->AsmOperands[i];
      if (CallCustomParserForAllOperands || !Op.Class->ParserMethod.empty()) {
        unsigned &OperandMask = OpClassMask[Op.Class];
        OperandMask |= maskTrailingOnes<unsigned>(NumOptionalOps + 1)
                       << (i - NumOptionalOps);
      }
      if (Op.Class->IsOptional)
        ++NumOptionalOps;
    }

    // Generate operand match info for each mnemonic/operand class pair.
    for (const auto &OCM : OpClassMask) {
      unsigned OpMask = OCM.second;
      ClassInfo *CI = OCM.first;
      OperandMatchInfo.push_back(OperandMatchEntry::create(MI.get(), CI,
                                                           OpMask));
    }
  }
}

void AsmMatcherInfo::buildInfo() {
  // Build information about all of the AssemblerPredicates.
  const std::vector<std::pair<Record *, SubtargetFeatureInfo>>
      &SubtargetFeaturePairs = SubtargetFeatureInfo::getAll(Records);
  SubtargetFeatures.insert(SubtargetFeaturePairs.begin(),
                           SubtargetFeaturePairs.end());
#ifndef NDEBUG
  for (const auto &Pair : SubtargetFeatures)
    LLVM_DEBUG(Pair.second.dump());
#endif // NDEBUG

  bool HasMnemonicFirst = AsmParser->getValueAsBit("HasMnemonicFirst");
  bool ReportMultipleNearMisses =
      AsmParser->getValueAsBit("ReportMultipleNearMisses");

  // Parse the instructions; we need to do this first so that we can gather the
  // singleton register classes.
  SmallPtrSet<Record*, 16> SingletonRegisters;
  unsigned VariantCount = Target.getAsmParserVariantCount();
  for (unsigned VC = 0; VC != VariantCount; ++VC) {
    Record *AsmVariant = Target.getAsmParserVariant(VC);
    StringRef CommentDelimiter =
        AsmVariant->getValueAsString("CommentDelimiter");
    AsmVariantInfo Variant;
    Variant.RegisterPrefix = AsmVariant->getValueAsString("RegisterPrefix");
    Variant.TokenizingCharacters =
        AsmVariant->getValueAsString("TokenizingCharacters");
    Variant.SeparatorCharacters =
        AsmVariant->getValueAsString("SeparatorCharacters");
    Variant.BreakCharacters =
        AsmVariant->getValueAsString("BreakCharacters");
    Variant.Name = AsmVariant->getValueAsString("Name");
    Variant.AsmVariantNo = AsmVariant->getValueAsInt("Variant");

    for (const CodeGenInstruction *CGI : Target.getInstructionsByEnumValue()) {

      // If the tblgen -match-prefix option is specified (for tblgen hackers),
      // filter the set of instructions we consider.
      if (!StringRef(CGI->TheDef->getName()).startswith(MatchPrefix))
        continue;

      // Ignore "codegen only" instructions.
      if (CGI->TheDef->getValueAsBit("isCodeGenOnly"))
        continue;

      // Ignore instructions for different instructions
      StringRef V = CGI->TheDef->getValueAsString("AsmVariantName");
      if (!V.empty() && V != Variant.Name)
        continue;

      auto II = std::make_unique<MatchableInfo>(*CGI);

      II->initialize(*this, SingletonRegisters, Variant, HasMnemonicFirst);

      // Ignore instructions which shouldn't be matched and diagnose invalid
      // instruction definitions with an error.
      if (!II->validate(CommentDelimiter, false))
        continue;

      Matchables.push_back(std::move(II));
    }

    // Parse all of the InstAlias definitions and stick them in the list of
    // matchables.
    std::vector<Record*> AllInstAliases =
      Records.getAllDerivedDefinitions("InstAlias");
    for (Record *InstAlias : AllInstAliases) {
      auto Alias = std::make_unique<CodeGenInstAlias>(InstAlias, Target);

      // If the tblgen -match-prefix option is specified (for tblgen hackers),
      // filter the set of instruction aliases we consider, based on the target
      // instruction.
      if (!StringRef(Alias->ResultInst->TheDef->getName())
            .startswith( MatchPrefix))
        continue;

      StringRef V = Alias->TheDef->getValueAsString("AsmVariantName");
      if (!V.empty() && V != Variant.Name)
        continue;

      auto II = std::make_unique<MatchableInfo>(std::move(Alias));

      II->initialize(*this, SingletonRegisters, Variant, HasMnemonicFirst);

      // Validate the alias definitions.
      II->validate(CommentDelimiter, true);

      Matchables.push_back(std::move(II));
    }
  }

  // Build info for the register classes.
  buildRegisterClasses(SingletonRegisters);

  // Build info for the user defined assembly operand classes.
  buildOperandClasses();

  // Build the information about matchables, now that we have fully formed
  // classes.
  std::vector<std::unique_ptr<MatchableInfo>> NewMatchables;
  for (auto &II : Matchables) {
    // Parse the tokens after the mnemonic.
    // Note: buildInstructionOperandReference may insert new AsmOperands, so
    // don't precompute the loop bound.
    for (unsigned i = 0; i != II->AsmOperands.size(); ++i) {
      MatchableInfo::AsmOperand &Op = II->AsmOperands[i];
      StringRef Token = Op.Token;

      // Check for singleton registers.
      if (Record *RegRecord = Op.SingletonReg) {
        Op.Class = RegisterClasses[RegRecord];
        assert(Op.Class && Op.Class->Registers.size() == 1 &&
               "Unexpected class for singleton register");
        continue;
      }

      // Check for simple tokens.
      if (Token[0] != '$') {
        Op.Class = getTokenClass(Token);
        continue;
      }

      if (Token.size() > 1 && isdigit(Token[1])) {
        Op.Class = getTokenClass(Token);
        continue;
      }

      // Otherwise this is an operand reference.
      StringRef OperandName;
      if (Token[1] == '{')
        OperandName = Token.substr(2, Token.size() - 3);
      else
        OperandName = Token.substr(1);

      if (II->DefRec.is<const CodeGenInstruction*>())
        buildInstructionOperandReference(II.get(), OperandName, i);
      else
        buildAliasOperandReference(II.get(), OperandName, Op);
    }

    if (II->DefRec.is<const CodeGenInstruction*>()) {
      II->buildInstructionResultOperands();
      // If the instruction has a two-operand alias, build up the
      // matchable here. We'll add them in bulk at the end to avoid
      // confusing this loop.
      StringRef Constraint =
          II->TheDef->getValueAsString("TwoOperandAliasConstraint");
      if (Constraint != "") {
        // Start by making a copy of the original matchable.
        auto AliasII = std::make_unique<MatchableInfo>(*II);

        // Adjust it to be a two-operand alias.
        AliasII->formTwoOperandAlias(Constraint);

        // Add the alias to the matchables list.
        NewMatchables.push_back(std::move(AliasII));
      }
    } else
      // FIXME: The tied operands checking is not yet integrated with the
      // framework for reporting multiple near misses. To prevent invalid
      // formats from being matched with an alias if a tied-operands check
      // would otherwise have disallowed it, we just disallow such constructs
      // in TableGen completely.
      II->buildAliasResultOperands(!ReportMultipleNearMisses);
  }
  if (!NewMatchables.empty())
    Matchables.insert(Matchables.end(),
                      std::make_move_iterator(NewMatchables.begin()),
                      std::make_move_iterator(NewMatchables.end()));

  // Process token alias definitions and set up the associated superclass
  // information.
  std::vector<Record*> AllTokenAliases =
    Records.getAllDerivedDefinitions("TokenAlias");
  for (Record *Rec : AllTokenAliases) {
    ClassInfo *FromClass = getTokenClass(Rec->getValueAsString("FromToken"));
    ClassInfo *ToClass = getTokenClass(Rec->getValueAsString("ToToken"));
    if (FromClass == ToClass)
      PrintFatalError(Rec->getLoc(),
                    "error: Destination value identical to source value.");
    FromClass->SuperClasses.push_back(ToClass);
  }

  // Reorder classes so that classes precede super classes.
  Classes.sort();

#ifdef EXPENSIVE_CHECKS
  // Verify that the table is sorted and operator < works transitively.
  for (auto I = Classes.begin(), E = Classes.end(); I != E; ++I) {
    for (auto J = I; J != E; ++J) {
      assert(!(*J < *I));
      assert(I == J || !J->isSubsetOf(*I));
    }
  }
#endif
}

/// buildInstructionOperandReference - The specified operand is a reference to a
/// named operand such as $src.  Resolve the Class and OperandInfo pointers.
void AsmMatcherInfo::
buildInstructionOperandReference(MatchableInfo *II,
                                 StringRef OperandName,
                                 unsigned AsmOpIdx) {
  const CodeGenInstruction &CGI = *II->DefRec.get<const CodeGenInstruction*>();
  const CGIOperandList &Operands = CGI.Operands;
  MatchableInfo::AsmOperand *Op = &II->AsmOperands[AsmOpIdx];

  // Map this token to an operand.
  unsigned Idx;
  if (!Operands.hasOperandNamed(OperandName, Idx))
    PrintFatalError(II->TheDef->getLoc(),
                    "error: unable to find operand: '" + OperandName + "'");

  // If the instruction operand has multiple suboperands, but the parser
  // match class for the asm operand is still the default "ImmAsmOperand",
  // then handle each suboperand separately.
  if (Op->SubOpIdx == -1 && Operands[Idx].MINumOperands > 1) {
    Record *Rec = Operands[Idx].Rec;
    assert(Rec->isSubClassOf("Operand") && "Unexpected operand!");
    Record *MatchClass = Rec->getValueAsDef("ParserMatchClass");
    if (MatchClass && MatchClass->getValueAsString("Name") == "Imm") {
      // Insert remaining suboperands after AsmOpIdx in II->AsmOperands.
      StringRef Token = Op->Token; // save this in case Op gets moved
      for (unsigned SI = 1, SE = Operands[Idx].MINumOperands; SI != SE; ++SI) {
        MatchableInfo::AsmOperand NewAsmOp(/*IsIsolatedToken=*/true, Token);
        NewAsmOp.SubOpIdx = SI;
        II->AsmOperands.insert(II->AsmOperands.begin()+AsmOpIdx+SI, NewAsmOp);
      }
      // Replace Op with first suboperand.
      Op = &II->AsmOperands[AsmOpIdx]; // update the pointer in case it moved
      Op->SubOpIdx = 0;
    }
  }

  // Set up the operand class.
  Op->Class = getOperandClass(Operands[Idx], Op->SubOpIdx);
  Op->OrigSrcOpName = OperandName;

  // If the named operand is tied, canonicalize it to the untied operand.
  // For example, something like:
  //   (outs GPR:$dst), (ins GPR:$src)
  // with an asmstring of
  //   "inc $src"
  // we want to canonicalize to:
  //   "inc $dst"
  // so that we know how to provide the $dst operand when filling in the result.
  int OITied = -1;
  if (Operands[Idx].MINumOperands == 1)
    OITied = Operands[Idx].getTiedRegister();
  if (OITied != -1) {
    // The tied operand index is an MIOperand index, find the operand that
    // contains it.
    std::pair<unsigned, unsigned> Idx = Operands.getSubOperandNumber(OITied);
    OperandName = Operands[Idx.first].Name;
    Op->SubOpIdx = Idx.second;
  }

  Op->SrcOpName = OperandName;
}

/// buildAliasOperandReference - When parsing an operand reference out of the
/// matching string (e.g. "movsx $src, $dst"), determine what the class of the
/// operand reference is by looking it up in the result pattern definition.
void AsmMatcherInfo::buildAliasOperandReference(MatchableInfo *II,
                                                StringRef OperandName,
                                                MatchableInfo::AsmOperand &Op) {
  const CodeGenInstAlias &CGA = *II->DefRec.get<const CodeGenInstAlias*>();

  // Set up the operand class.
  for (unsigned i = 0, e = CGA.ResultOperands.size(); i != e; ++i)
    if (CGA.ResultOperands[i].isRecord() &&
        CGA.ResultOperands[i].getName() == OperandName) {
      // It's safe to go with the first one we find, because CodeGenInstAlias
      // validates that all operands with the same name have the same record.
      Op.SubOpIdx = CGA.ResultInstOperandIndex[i].second;
      // Use the match class from the Alias definition, not the
      // destination instruction, as we may have an immediate that's
      // being munged by the match class.
      Op.Class = getOperandClass(CGA.ResultOperands[i].getRecord(),
                                 Op.SubOpIdx);
      Op.SrcOpName = OperandName;
      Op.OrigSrcOpName = OperandName;
      return;
    }

  PrintFatalError(II->TheDef->getLoc(),
                  "error: unable to find operand: '" + OperandName + "'");
}

void MatchableInfo::buildInstructionResultOperands() {
  const CodeGenInstruction *ResultInst = getResultInst();

  // Loop over all operands of the result instruction, determining how to
  // populate them.
  for (const CGIOperandList::OperandInfo &OpInfo : ResultInst->Operands) {
    // If this is a tied operand, just copy from the previously handled operand.
    int TiedOp = -1;
    if (OpInfo.MINumOperands == 1)
      TiedOp = OpInfo.getTiedRegister();
    if (TiedOp != -1) {
      int TiedSrcOperand = findAsmOperandOriginallyNamed(OpInfo.Name);
      if (TiedSrcOperand != -1 &&
          ResOperands[TiedOp].Kind == ResOperand::RenderAsmOperand)
        ResOperands.push_back(ResOperand::getTiedOp(
            TiedOp, ResOperands[TiedOp].AsmOperandNum, TiedSrcOperand));
      else
        ResOperands.push_back(ResOperand::getTiedOp(TiedOp, 0, 0));
      continue;
    }

    int SrcOperand = findAsmOperandNamed(OpInfo.Name);
    if (OpInfo.Name.empty() || SrcOperand == -1) {
      // This may happen for operands that are tied to a suboperand of a
      // complex operand.  Simply use a dummy value here; nobody should
      // use this operand slot.
      // FIXME: The long term goal is for the MCOperand list to not contain
      // tied operands at all.
      ResOperands.push_back(ResOperand::getImmOp(0));
      continue;
    }

    // Check if the one AsmOperand populates the entire operand.
    unsigned NumOperands = OpInfo.MINumOperands;
    if (AsmOperands[SrcOperand].SubOpIdx == -1) {
      ResOperands.push_back(ResOperand::getRenderedOp(SrcOperand, NumOperands));
      continue;
    }

    // Add a separate ResOperand for each suboperand.
    for (unsigned AI = 0; AI < NumOperands; ++AI) {
      assert(AsmOperands[SrcOperand+AI].SubOpIdx == (int)AI &&
             AsmOperands[SrcOperand+AI].SrcOpName == OpInfo.Name &&
             "unexpected AsmOperands for suboperands");
      ResOperands.push_back(ResOperand::getRenderedOp(SrcOperand + AI, 1));
    }
  }
}

void MatchableInfo::buildAliasResultOperands(bool AliasConstraintsAreChecked) {
  const CodeGenInstAlias &CGA = *DefRec.get<const CodeGenInstAlias*>();
  const CodeGenInstruction *ResultInst = getResultInst();

  // Map of:  $reg -> #lastref
  //   where $reg is the name of the operand in the asm string
  //   where #lastref is the last processed index where $reg was referenced in
  //   the asm string.
  SmallDenseMap<StringRef, int> OperandRefs;

  // Loop over all operands of the result instruction, determining how to
  // populate them.
  unsigned AliasOpNo = 0;
  unsigned LastOpNo = CGA.ResultInstOperandIndex.size();
  for (unsigned i = 0, e = ResultInst->Operands.size(); i != e; ++i) {
    const CGIOperandList::OperandInfo *OpInfo = &ResultInst->Operands[i];

    // If this is a tied operand, just copy from the previously handled operand.
    int TiedOp = -1;
    if (OpInfo->MINumOperands == 1)
      TiedOp = OpInfo->getTiedRegister();
    if (TiedOp != -1) {
      unsigned SrcOp1 = 0;
      unsigned SrcOp2 = 0;

      // If an operand has been specified twice in the asm string,
      // add the two source operand's indices to the TiedOp so that
      // at runtime the 'tied' constraint is checked.
      if (ResOperands[TiedOp].Kind == ResOperand::RenderAsmOperand) {
        SrcOp1 = ResOperands[TiedOp].AsmOperandNum;

        // Find the next operand (similarly named operand) in the string.
        StringRef Name = AsmOperands[SrcOp1].SrcOpName;
        auto Insert = OperandRefs.try_emplace(Name, SrcOp1);
        SrcOp2 = findAsmOperandNamed(Name, Insert.first->second);

        // Not updating the record in OperandRefs will cause TableGen
        // to fail with an error at the end of this function.
        if (AliasConstraintsAreChecked)
          Insert.first->second = SrcOp2;

        // In case it only has one reference in the asm string,
        // it doesn't need to be checked for tied constraints.
        SrcOp2 = (SrcOp2 == (unsigned)-1) ? SrcOp1 : SrcOp2;
      }

      // If the alias operand is of a different operand class, we only want
      // to benefit from the tied-operands check and just match the operand
      // as a normal, but not copy the original (TiedOp) to the result
      // instruction. We do this by passing -1 as the tied operand to copy.
      if (ResultInst->Operands[i].Rec->getName() !=
          ResultInst->Operands[TiedOp].Rec->getName()) {
        SrcOp1 = ResOperands[TiedOp].AsmOperandNum;
        int SubIdx = CGA.ResultInstOperandIndex[AliasOpNo].second;
        StringRef Name = CGA.ResultOperands[AliasOpNo].getName();
        SrcOp2 = findAsmOperand(Name, SubIdx);
        ResOperands.push_back(
            ResOperand::getTiedOp((unsigned)-1, SrcOp1, SrcOp2));
      } else {
        ResOperands.push_back(ResOperand::getTiedOp(TiedOp, SrcOp1, SrcOp2));
        continue;
      }
    }

    // Handle all the suboperands for this operand.
    const std::string &OpName = OpInfo->Name;
    for ( ; AliasOpNo <  LastOpNo &&
            CGA.ResultInstOperandIndex[AliasOpNo].first == i; ++AliasOpNo) {
      int SubIdx = CGA.ResultInstOperandIndex[AliasOpNo].second;

      // Find out what operand from the asmparser that this MCInst operand
      // comes from.
      switch (CGA.ResultOperands[AliasOpNo].Kind) {
      case CodeGenInstAlias::ResultOperand::K_Record: {
        StringRef Name = CGA.ResultOperands[AliasOpNo].getName();
        int SrcOperand = findAsmOperand(Name, SubIdx);
        if (SrcOperand == -1)
          PrintFatalError(TheDef->getLoc(), "Instruction '" +
                        TheDef->getName() + "' has operand '" + OpName +
                        "' that doesn't appear in asm string!");

        // Add it to the operand references. If it is added a second time, the
        // record won't be updated and it will fail later on.
        OperandRefs.try_emplace(Name, SrcOperand);

        unsigned NumOperands = (SubIdx == -1 ? OpInfo->MINumOperands : 1);
        ResOperands.push_back(ResOperand::getRenderedOp(SrcOperand,
                                                        NumOperands));
        break;
      }
      case CodeGenInstAlias::ResultOperand::K_Imm: {
        int64_t ImmVal = CGA.ResultOperands[AliasOpNo].getImm();
        ResOperands.push_back(ResOperand::getImmOp(ImmVal));
        break;
      }
      case CodeGenInstAlias::ResultOperand::K_Reg: {
        Record *Reg = CGA.ResultOperands[AliasOpNo].getRegister();
        ResOperands.push_back(ResOperand::getRegOp(Reg));
        break;
      }
      }
    }
  }

  // Check that operands are not repeated more times than is supported.
  for (auto &T : OperandRefs) {
    if (T.second != -1 && findAsmOperandNamed(T.first, T.second) != -1)
      PrintFatalError(TheDef->getLoc(),
                      "Operand '" + T.first + "' can never be matched");
  }
}

static unsigned
getConverterOperandID(const std::string &Name,
                      SmallSetVector<CachedHashString, 16> &Table,
                      bool &IsNew) {
  IsNew = Table.insert(CachedHashString(Name));

  unsigned ID = IsNew ? Table.size() - 1 : find(Table, Name) - Table.begin();

  assert(ID < Table.size());

  return ID;
}

static unsigned
emitConvertFuncs(CodeGenTarget &Target, StringRef ClassName,
                 std::vector<std::unique_ptr<MatchableInfo>> &Infos,
                 bool HasMnemonicFirst, bool HasOptionalOperands,
                 PrinterLLVM &PI) {
  SmallSetVector<CachedHashString, 16> OperandConversionKinds;
  SmallSetVector<CachedHashString, 16> InstructionConversionKinds;
  std::vector<std::vector<uint8_t> > ConversionTable;
  size_t MaxRowLength = 2; // minimum is custom converter plus terminator.

  // TargetOperandClass - This is the target's operand class, like X86Operand.
  std::string TargetOperandClass = Target.getName().str() + "Operand";

  // Start the unified conversion function.
  size_t MaxNumOperands = 0;
  for (const auto &MI : Infos) {
    MaxNumOperands = std::max(MaxNumOperands, MI->AsmOperands.size());
  }
  PI.asmMatcherEmitConversionFunctionI(Target.getName(), ClassName,
                                       TargetOperandClass, HasOptionalOperands,
                                       MaxNumOperands);

  // Start the operand number lookup function.
  PI.asmMatcherEmitOperandFunctionI(Target.getName(), ClassName);

  // Pre-populate the operand conversion kinds with the standard always
  // available entries.
  OperandConversionKinds.insert(CachedHashString("CVT_Done"));
  OperandConversionKinds.insert(CachedHashString("CVT_Reg"));
  OperandConversionKinds.insert(CachedHashString("CVT_Tied"));
  enum { CVT_Done, CVT_Reg, CVT_Tied };

  // Map of e.g. <0, 2, 3> -> "Tie_0_2_3" enum label.
  std::map<std::tuple<uint8_t, uint8_t, uint8_t>, std::string>
  TiedOperandsEnumMap;

  for (auto &II : Infos) {
    // Check if we have a custom match function.
    StringRef AsmMatchConverter =
        II->getResultInst()->TheDef->getValueAsString("AsmMatchConverter");
    if (!AsmMatchConverter.empty() && II->UseInstAsmMatchConverter) {
      std::string Signature = ("ConvertCustom_" + AsmMatchConverter).str();
      II->ConversionFnKind = Signature;

      // Check if we have already generated this signature.
      if (!InstructionConversionKinds.insert(CachedHashString(Signature)))
        continue;

      // Remember this converter for the kind enum.
      unsigned KindID = OperandConversionKinds.size();
      OperandConversionKinds.insert(
          CachedHashString("CVT_" + getEnumNameForToken(AsmMatchConverter)));

      // Add the converter row for this instruction.
      ConversionTable.emplace_back();
      ConversionTable.back().push_back(KindID);
      ConversionTable.back().push_back(CVT_Done);

      // Add the handler to the conversion driver function.
      PI.asmMatcherEmitConversionFunctionII(
              getEnumNameForToken(AsmMatchConverter),
              AsmMatchConverter);

      // FIXME: Handle the operand number lookup for custom match functions.
      continue;
    }

    // Build the conversion function signature.
    std::string Signature = "Convert";

    std::vector<uint8_t> ConversionRow;

    // Compute the convert enum and the case body.
    MaxRowLength = std::max(MaxRowLength, II->ResOperands.size()*2 + 1 );

    for (unsigned i = 0, e = II->ResOperands.size(); i != e; ++i) {
      const MatchableInfo::ResOperand &OpInfo = II->ResOperands[i];

      // Generate code to populate each result operand.
      switch (OpInfo.Kind) {
      case MatchableInfo::ResOperand::RenderAsmOperand: {
        // This comes from something we parsed.
        const MatchableInfo::AsmOperand &Op =
          II->AsmOperands[OpInfo.AsmOperandNum];

        // Registers are always converted the same, don't duplicate the
        // conversion function based on them.
        Signature += "__";
        std::string Class;
        Class = Op.Class->isRegisterClass() ? "Reg" : Op.Class->ClassName;
        Signature += Class;
        Signature += utostr(OpInfo.MINumOperands);
        Signature += "_" + itostr(OpInfo.AsmOperandNum);

        // Add the conversion kind, if necessary, and get the associated ID
        // the index of its entry in the vector).
        std::string Name = "CVT_" + (Op.Class->isRegisterClass() ? "Reg" :
                                     Op.Class->RenderMethod);
        if (Op.Class->IsOptional) {
          // For optional operands we must also care about DefaultMethod
          assert(HasOptionalOperands);
          Name += "_" + Op.Class->DefaultMethod;
        }
        Name = getEnumNameForToken(Name);

        bool IsNewConverter = false;
        unsigned ID = getConverterOperandID(Name, OperandConversionKinds,
                                            IsNewConverter);

        // Add the operand entry to the instruction kind conversion row.
        ConversionRow.push_back(ID);
        ConversionRow.push_back(OpInfo.AsmOperandNum + HasMnemonicFirst);

        if (!IsNewConverter)
          break;

        // This is a new operand kind. Add a handler for it to the
        // converter driver.
        PI.asmMatcherEmitConversionFunctionIII(
                Name, TargetOperandClass, HasOptionalOperands,
                Op, OpInfo);

        // Add a handler for the operand number lookup.
        PI.asmMatcherEmitOperandFunctionII(Name, Op, OpInfo);
        break;
      }
      case MatchableInfo::ResOperand::TiedOperand: {
        // If this operand is tied to a previous one, just copy the MCInst
        // operand from the earlier one.We can only tie single MCOperand values.
        assert(OpInfo.MINumOperands == 1 && "Not a singular MCOperand");
        uint8_t TiedOp = OpInfo.TiedOperands.ResOpnd;
        uint8_t SrcOp1 =
            OpInfo.TiedOperands.SrcOpnd1Idx + HasMnemonicFirst;
        uint8_t SrcOp2 =
            OpInfo.TiedOperands.SrcOpnd2Idx + HasMnemonicFirst;
        assert((i > TiedOp || TiedOp == (uint8_t)-1) &&
               "Tied operand precedes its target!");
        auto TiedTupleName = std::string("Tie") + utostr(TiedOp) + '_' +
                             utostr(SrcOp1) + '_' + utostr(SrcOp2);
        Signature += "__" + TiedTupleName;
        ConversionRow.push_back(CVT_Tied);
        ConversionRow.push_back(TiedOp);
        ConversionRow.push_back(SrcOp1);
        ConversionRow.push_back(SrcOp2);

        // Also create an 'enum' for this combination of tied operands.
        auto Key = std::make_tuple(TiedOp, SrcOp1, SrcOp2);
        TiedOperandsEnumMap.emplace(Key, TiedTupleName);
        break;
      }
      case MatchableInfo::ResOperand::ImmOperand: {
        int64_t Val = OpInfo.ImmVal;
        std::string Ty = "imm_" + itostr(Val);
        Ty = getEnumNameForToken(Ty);
        Signature += "__" + Ty;

        std::string Name = "CVT_" + Ty;
        bool IsNewConverter = false;
        unsigned ID = getConverterOperandID(Name, OperandConversionKinds,
                                            IsNewConverter);
        // Add the operand entry to the instruction kind conversion row.
        ConversionRow.push_back(ID);
        ConversionRow.push_back(0);

        if (!IsNewConverter)
          break;

        PI.asmMatcherEmitConversionFunctionIV(Name, Val);
        PI.asmMatcherEmitOperandFunctionIII(Name);

        break;
      }
      case MatchableInfo::ResOperand::RegOperand: {
        std::string Reg, Name;
        if (!OpInfo.Register) {
          Name = "reg0";
          Reg = "0";
        } else {
          Reg = getQualifiedName(OpInfo.Register);
          Name = "reg" + OpInfo.Register->getName().str();
        }
        Signature += "__" + Name;
        Name = "CVT_" + Name;
        bool IsNewConverter = false;
        unsigned ID = getConverterOperandID(Name, OperandConversionKinds,
                                            IsNewConverter);
        // Add the operand entry to the instruction kind conversion row.
        ConversionRow.push_back(ID);
        ConversionRow.push_back(0);

        if (!IsNewConverter)
          break;
        PI.asmMatcherEmitConversionFunctionV(Name, Reg);
        PI.asmMatcherEmitOperandFunctionIV(Name);
      }
      }
    }

    // If there were no operands, add to the signature to that effect
    if (Signature == "Convert")
      Signature += "_NoOperands";

    II->ConversionFnKind = Signature;

    // Save the signature. If we already have it, don't add a new row
    // to the table.
    if (!InstructionConversionKinds.insert(CachedHashString(Signature)))
      continue;

    // Add the row to the table.
    ConversionTable.push_back(std::move(ConversionRow));
  }

  // Finish up the converter driver function.
  PI.asmMatcherEmitConversionFunctionVI();

  // Finish up the operand number lookup function.
  PI.asmMatcherEmitOperandFunctionV();

  // Output a static table for tied operands.
  if (TiedOperandsEnumMap.size()) {
    // The number of tied operand combinations will be small in practice,
    // but just add the assert to be sure.
    assert(TiedOperandsEnumMap.size() <= 254 &&
           "Too many tied-operand combinations to reference with "
           "an 8bit offset from the conversion table, where index "
           "'255' is reserved as operand not to be copied.");

    PI.asmMatcherEmitTiedOperandEnum(TiedOperandsEnumMap);

    PI.asmMatcherEmitTiedOpTable(TiedOperandsEnumMap);
  } else
    PI.asmMatcherEmitTiedOpEmptyTable();

  PI.emitNamespace("", true);

  // Output the operand conversion kind enum.
  PI.asmMatcherEmitOperandConvKindEnum(OperandConversionKinds);

  // Output the instruction conversion kind enum.
  PI.asmMatcherEmitInstrConvKindEnum(InstructionConversionKinds);

  PI.emitNamespace("", false);

  // Output the conversion table.
  PI.asmMatcherEmitConversionTable(MaxRowLength,
                                   ConversionTable,
                                   InstructionConversionKinds,
                                   OperandConversionKinds,
                                   TiedOperandsEnumMap);

  // Spit out the conversion driver function.
   PI.asmMatcherWriteCvtOSToOS();
  // Spit out the operand number lookup function.
   PI.asmMatcherWriteOpOSToOS();

  return ConversionTable.size();
}

/// emitMatchClassEnumeration - Emit the enumeration for match class kinds.
static void emitMatchClassEnumeration(CodeGenTarget &Target,
                                      std::forward_list<ClassInfo> &Infos,
                                      PrinterLLVM const &PI) {
  PI.emitNamespace("", true);
  PI.asmMatcherEmitMatchClassKindEnum(Infos);
  PI.emitNamespace("", false);
}

/// emitMatchClassDiagStrings - Emit a function to get the diagnostic text to be
/// used when an assembly operand does not match the expected operand class.
static void emitOperandMatchErrorDiagStrings(AsmMatcherInfo &Info, PrinterLLVM const &PI) {
  // If the target does not use DiagnosticString for any operands, don't emit
  // an unused function.
  if (llvm::all_of(Info.Classes, [](const ClassInfo &CI) {
        return CI.DiagnosticString.empty();
      }))
    return;
  PI.asmMatcherEmitMatchClassDiagStrings(Info);
}

/// emitIsSubclass - Emit the subclass predicate function.
static void emitIsSubclass(CodeGenTarget &Target,
                           std::forward_list<ClassInfo> &Infos,
                           PrinterLLVM const &PI) {
  PI.asmMatcherEmitIsSubclassI();
  bool EmittedSwitch = false;
  for (const auto &A : Infos) {
    std::vector<StringRef> SuperClasses;
    if (A.IsOptional)
      SuperClasses.push_back("OptionalMatchClass");
    for (const auto &B : Infos) {
      if (&A != &B && A.isSubsetOf(B))
        SuperClasses.push_back(B.Name);
    }

    if (SuperClasses.empty())
      continue;

    // If this is the first SuperClass, emit the switch header.
    EmittedSwitch = PI.asmMatcherEmitIsSubclassII(EmittedSwitch, A.Name);

    if (SuperClasses.size() == 1) {
      PI.asmMatcherEmitIsSubclassIII(SuperClasses.back());
      continue;
    }

    PI.asmMatcherEmitIsSubclassIV(SuperClasses);
  }

  // If there were case statements emitted into the string stream write the
  // default.
  PI.asmMatcherEmitIsSubclassV(EmittedSwitch);
}

/// emitMatchTokenString - Emit the function to match a token string to the
/// appropriate match class value.
static void emitMatchTokenString(CodeGenTarget &Target,
                                 std::forward_list<ClassInfo> &Infos,
                                 PrinterLLVM const &PI) {
  // Construct the match list.
  std::vector<StringMatcher::StringPair> Matches;
  for (const auto &CI : Infos) {
    if (CI.Kind == ClassInfo::Token)
      Matches.emplace_back(CI.ValueName, "return " + CI.Name + ";");
  }

  PI.asmMatcherEmitMatchTokenString(Matches);
}

/// emitMatchRegisterName - Emit the function to match a string to the target
/// specific register enum.
static void emitMatchRegisterName(CodeGenTarget &Target, Record *AsmParser,
                                  PrinterLLVM const &PI) {
  // Construct the match list.
  std::vector<StringMatcher::StringPair> Matches;
  const auto &Regs = Target.getRegBank().getRegisters();
  for (const CodeGenRegister &Reg : Regs) {
    if (Reg.TheDef->getValueAsString("AsmName").empty())
      continue;

    Matches.emplace_back(std::string(Reg.TheDef->getValueAsString("AsmName")),
                         "return " + utostr(Reg.EnumValue) + ";");
  }

  PI.asmMatcherEmitMatchRegisterName(AsmParser, Matches);
}

/// Emit the function to match a string to the target
/// specific register enum.
static void emitMatchRegisterAltName(CodeGenTarget &Target, Record *AsmParser,
                                     PrinterLLVM const &PI) {
  // Construct the match list.
  std::vector<StringMatcher::StringPair> Matches;
  const auto &Regs = Target.getRegBank().getRegisters();
  for (const CodeGenRegister &Reg : Regs) {

    auto AltNames = Reg.TheDef->getValueAsListOfStrings("AltNames");

    for (auto AltName : AltNames) {
      AltName = StringRef(AltName).trim();

      // don't handle empty alternative names
      if (AltName.empty())
        continue;

      Matches.emplace_back(std::string(AltName),
                           "return " + utostr(Reg.EnumValue) + ";");
    }
  }
  PI.asmMatcherEmitMatchRegisterAltName(AsmParser, Matches);
}

/// emitOperandDiagnosticTypes - Emit the operand matching diagnostic types.
static void emitOperandDiagnosticTypes(AsmMatcherInfo &Info, PrinterLLVM const &PI) {
  // Get the set of diagnostic types from all of the operand classes.
  std::set<StringRef> Types;
  for (const auto &OpClassEntry : Info.AsmOperandClasses) {
    if (!OpClassEntry.second->DiagnosticType.empty())
      Types.insert(OpClassEntry.second->DiagnosticType);
  }
  for (const auto &OpClassEntry : Info.RegisterClassClasses) {
    if (!OpClassEntry.second->DiagnosticType.empty())
      Types.insert(OpClassEntry.second->DiagnosticType);
  }

  if (Types.empty()) return;

  // Now emit the enum entries.
  PI.asmMatcherEmitOperandDiagTypes(Types);
}

static std::string GetAliasRequiredFeatures(Record *R,
                                            const AsmMatcherInfo &Info) {
  std::vector<Record*> ReqFeatures = R->getValueAsListOfDefs("Predicates");
  std::string Result;

  if (ReqFeatures.empty())
    return Result;

  for (unsigned i = 0, e = ReqFeatures.size(); i != e; ++i) {
    const SubtargetFeatureInfo *F = Info.getSubtargetFeature(ReqFeatures[i]);

    if (!F)
      PrintFatalError(R->getLoc(), "Predicate '" + ReqFeatures[i]->getName() +
                    "' is not marked as an AssemblerPredicate!");

    if (i)
      Result += " && ";

    Result += "Features.test(" + F->getEnumBitName() + ')';
  }

  return Result;
}

static void emitMnemonicAliasVariant(PrinterLLVM const &PI, const AsmMatcherInfo &Info,
                                     std::vector<Record*> &Aliases,
                                     unsigned Indent = 0,
                                  StringRef AsmParserVariantName = StringRef()){
  // Keep track of all the aliases from a mnemonic.  Use an std::map so that the
  // iteration order of the map is stable.
  std::map<std::string, std::vector<Record*> > AliasesFromMnemonic;

  for (Record *R : Aliases) {
    // FIXME: Allow AssemblerVariantName to be a comma separated list.
    StringRef AsmVariantName = R->getValueAsString("AsmVariantName");
    if (AsmVariantName != AsmParserVariantName)
      continue;
    AliasesFromMnemonic[R->getValueAsString("FromMnemonic").lower()]
        .push_back(R);
  }
  if (AliasesFromMnemonic.empty())
    return;

  // Process each alias a "from" mnemonic at a time, building the code executed
  // by the string remapper.
  std::vector<StringMatcher::StringPair> Cases;
  for (const auto &AliasEntry : AliasesFromMnemonic) {
    const std::vector<Record*> &ToVec = AliasEntry.second;

    // Loop through each alias and emit code that handles each case.  If there
    // are two instructions without predicates, emit an error.  If there is one,
    // emit it last.
    std::string MatchCode;
    int AliasWithNoPredicate = -1;

    for (unsigned i = 0, e = ToVec.size(); i != e; ++i) {
      Record *R = ToVec[i];
      std::string FeatureMask = GetAliasRequiredFeatures(R, Info);

      // If this unconditionally matches, remember it for later and diagnose
      // duplicates.
      if (FeatureMask.empty()) {
        if (AliasWithNoPredicate != -1 &&
            R->getValueAsString("ToMnemonic") !=
                ToVec[AliasWithNoPredicate]->getValueAsString("ToMnemonic")) {
          // We can't have two different aliases from the same mnemonic with no
          // predicate.
          PrintError(
              ToVec[AliasWithNoPredicate]->getLoc(),
              "two different MnemonicAliases with the same 'from' mnemonic!");
          PrintFatalError(R->getLoc(), "this is the other MnemonicAlias.");
        }

        AliasWithNoPredicate = i;
        continue;
      }
      if (R->getValueAsString("ToMnemonic") == AliasEntry.first)
        PrintFatalError(R->getLoc(), "MnemonicAlias to the same string");

      PI.asmMatcherAppendMnemonicAlias(R, FeatureMask, MatchCode);
    }

    if (AliasWithNoPredicate != -1) {
      Record *R = ToVec[AliasWithNoPredicate];
      PI.asmMatcherAppendMnemonic(R, MatchCode);
    }

    PI.asmMatcherAppendMnemonicAliasEnd(MatchCode);

    Cases.push_back(std::make_pair(AliasEntry.first, MatchCode));
  }
  PI.asmMatcherEmitMnemonicAliasVariant(Cases, Indent);
}

/// emitMnemonicAliases - If the target has any MnemonicAlias<> definitions,
/// emit a function for them and return true, otherwise return false.
static bool emitMnemonicAliases(PrinterLLVM const &PI, const AsmMatcherInfo &Info,
                                CodeGenTarget &Target) {
  // Ignore aliases when match-prefix is set.
  if (!MatchPrefix.empty())
    return false;

  std::vector<Record*> Aliases =
    Info.getRecords().getAllDerivedDefinitions("MnemonicAlias");
  if (Aliases.empty()) return false;

  PI.asmMatcherEmitApplyMnemonicAliasesI();
  unsigned VariantCount = Target.getAsmParserVariantCount();
  for (unsigned VC = 0; VC != VariantCount; ++VC) {
    Record *AsmVariant = Target.getAsmParserVariant(VC);
    int AsmParserVariantNo = AsmVariant->getValueAsInt("Variant");
    StringRef AsmParserVariantName = AsmVariant->getValueAsString("Name");
    PI.asmMatcherEmitApplyMnemonicAliasesII(AsmParserVariantNo);
    emitMnemonicAliasVariant(PI, Info, Aliases, /*Indent=*/2,
                             AsmParserVariantName);
    PI.asmMatcherEmitApplyMnemonicAliasesIII();
  }
  PI.asmMatcherEmitApplyMnemonicAliasesIV();

  // Emit aliases that apply to all variants.
  emitMnemonicAliasVariant(PI, Info, Aliases);

  PI.asmMatcherEmitApplyMnemonicAliasesV();

  return true;
}

void AsmMatcherEmitter::run() {
  PI.asmMatcherEmitSourceFileHeader("Assembly Matcher Source Fragment");
  CodeGenTarget Target(Records);
  Record *AsmParser = Target.getAsmParser();
  StringRef ClassName = AsmParser->getValueAsString("AsmParserClassName");

  // Compute the information on the instructions to match.
  AsmMatcherInfo Info(AsmParser, Target, Records);
  Info.buildInfo();

  // Sort the instruction table using the partial order on classes. We use
  // stable_sort to ensure that ambiguous instructions are still
  // deterministically ordered.
  llvm::stable_sort(
      Info.Matchables,
      [](const std::unique_ptr<MatchableInfo> &a,
         const std::unique_ptr<MatchableInfo> &b) { return *a < *b; });

#ifdef EXPENSIVE_CHECKS
  // Verify that the table is sorted and operator < works transitively.
  for (auto I = Info.Matchables.begin(), E = Info.Matchables.end(); I != E;
       ++I) {
    for (auto J = I; J != E; ++J) {
      assert(!(**J < **I));
    }
  }
#endif

  DEBUG_WITH_TYPE("instruction_info", {
      for (const auto &MI : Info.Matchables)
        MI->dump();
    });

  // Check for ambiguous matchables.
  DEBUG_WITH_TYPE("ambiguous_instrs", {
    unsigned NumAmbiguous = 0;
    for (auto I = Info.Matchables.begin(), E = Info.Matchables.end(); I != E;
         ++I) {
      for (auto J = std::next(I); J != E; ++J) {
        const MatchableInfo &A = **I;
        const MatchableInfo &B = **J;

        if (A.couldMatchAmbiguouslyWith(B)) {
          errs() << "warning: ambiguous matchables:\n";
          A.dump();
          errs() << "\nis incomparable with:\n";
          B.dump();
          errs() << "\n\n";
          ++NumAmbiguous;
        }
      }
    }
    if (NumAmbiguous)
      errs() << "warning: " << NumAmbiguous
             << " ambiguous matchables!\n";
  });

  // Compute the information on the custom operand parsing.
  Info.buildOperandMatchInfo();

  bool HasMnemonicFirst = AsmParser->getValueAsBit("HasMnemonicFirst");
  bool HasOptionalOperands = Info.hasOptionalOperands();
  bool ReportMultipleNearMisses =
      AsmParser->getValueAsBit("ReportMultipleNearMisses");

  // Write the output.

  // Information for the class declaration.
  PI.emitIncludeToggle("GET_ASSEMBLER_HEADER", true);
  PI.asmMatcherEmitDeclarations(HasOptionalOperands, ReportMultipleNearMisses, !Info.OperandMatchInfo.empty());
  PI.emitIncludeToggle("GET_ASSEMBLER_HEADER", false);

  // Emit the operand match diagnostic enum names.
  PI.emitIncludeToggle("GET_OPERAND_DIAGNOSTIC_TYPES", true);
  emitOperandDiagnosticTypes(Info, PI);
  PI.emitIncludeToggle("GET_OPERAND_DIAGNOSTIC_TYPES", false);

  PI.emitIncludeToggle("GET_REGISTER_MATCHER", true);
  // Emit the subtarget feature enumeration.
  PI.asmMatcherEmitSTFBitEnum(Info);

  // Emit the function to match a register name to number.
  // This should be omitted for Mips target
  if (AsmParser->getValueAsBit("ShouldEmitMatchRegisterName"))
    emitMatchRegisterName(Target, AsmParser, PI);

  if (AsmParser->getValueAsBit("ShouldEmitMatchRegisterAltName"))
    emitMatchRegisterAltName(Target, AsmParser, PI);
  PI.emitIncludeToggle("GET_REGISTER_MATCHER", false);

  PI.emitIncludeToggle("GET_SUBTARGET_FEATURE_NAME", true);
  // Generate the helper function to get the names for subtarget features.
  PI.asmMatcherEmitGetSubtargetFeatureName(Info.SubtargetFeatures);
  PI.emitIncludeToggle("GET_SUBTARGET_FEATURE_NAME", false);

  PI.emitIncludeToggle("GET_MATCHER_IMPLEMENTATION", true);
  // Generate the function that remaps for mnemonic aliases.
  bool HasMnemonicAliases = emitMnemonicAliases(PI, Info, Target);

  // Generate the convertToMCInst function to convert operands into an MCInst.
  // Also, generate the convertToMapAndConstraints function for MS-style inline
  // assembly.  The latter doesn't actually generate a MCInst.
  unsigned NumConverters = emitConvertFuncs(Target, ClassName, Info.Matchables,
                                            HasMnemonicFirst,
                                            HasOptionalOperands, PI);

  // Emit the enumeration for classes which participate in matching.
  emitMatchClassEnumeration(Target, Info.Classes, PI);

  // Emit a function to get the user-visible string to describe an operand
  // match failure in diagnostics.
  emitOperandMatchErrorDiagStrings(Info, PI);

  // Emit a function to map register classes to operand match failure codes.
  PI.asmMatcherEmitRegisterMatchErrorFunc(Info);

  // Emit the routine to match token strings to their match class.
  emitMatchTokenString(Target, Info.Classes, PI);

  // Emit the subclass predicate routine.
  emitIsSubclass(Target, Info.Classes, PI);

  // Emit the routine to validate an operand against a match class.
  PI.asmMatcherEmitValidateOperandClass(Info);

  PI.asmMatcherEmitMatchClassKindNames(Info.Classes);

  // Emit the available features compute function.
  PI.asmMatcherEmitComputeAssemblerAvailableFeatures(Info, ClassName);

  if (!ReportMultipleNearMisses)
    PI.asmMatcherEmitAsmTiedOperandConstraints(Target, Info);

  StringToOffsetTable StringTable(PrinterLLVM::getLanguage());

  size_t MaxNumOperands = 0;
  unsigned MaxMnemonicIndex = 0;
  bool HasDeprecation = false;
  for (const auto &MI : Info.Matchables) {
    MaxNumOperands = std::max(MaxNumOperands, MI->AsmOperands.size());
    HasDeprecation |= MI->HasDeprecation;

    // Store a pascal-style length byte in the mnemonic.
    std::string LenMnemonic = char(MI->Mnemonic.size()) + MI->Mnemonic.lower();
    MaxMnemonicIndex = std::max(MaxMnemonicIndex,
                        StringTable.GetOrAddStringOffset(LenMnemonic, false));
  }

  PI.asmMatcherEmitMnemonicTable(StringTable);

  std::vector<std::vector<Record *>> FeatureBitsets;
  for (const auto &MI : Info.Matchables) {
    if (MI->RequiredFeatures.empty())
      continue;
    FeatureBitsets.emplace_back();
    for (unsigned I = 0, E = MI->RequiredFeatures.size(); I != E; ++I)
      FeatureBitsets.back().push_back(MI->RequiredFeatures[I]->TheDef);
  }

  llvm::sort(FeatureBitsets, [&](const std::vector<Record *> &A,
                                 const std::vector<Record *> &B) {
    if (A.size() < B.size())
      return true;
    if (A.size() > B.size())
      return false;
    for (auto Pair : zip(A, B)) {
      if (std::get<0>(Pair)->getName() < std::get<1>(Pair)->getName())
        return true;
      if (std::get<0>(Pair)->getName() > std::get<1>(Pair)->getName())
        return false;
    }
    return false;
  });
  FeatureBitsets.erase(
      std::unique(FeatureBitsets.begin(), FeatureBitsets.end()),
      FeatureBitsets.end());
  PI.asmMatcherEmitFeatureBitsetEnum(FeatureBitsets);
  PI.asmMatcherEmitFeatureBitsets(FeatureBitsets, Info);

  // Emit the static match table; unused classes get initialized to 0 which is
  // guaranteed to be InvalidMatchClass.
  //
  // FIXME: We can reduce the size of this table very easily. First, we change
  // it so that store the kinds in separate bit-fields for each index, which
  // only needs to be the max width used for classes at that index (we also need
  // to reject based on this during classification). If we then make sure to
  // order the match kinds appropriately (putting mnemonics last), then we
  // should only end up using a few bits for each class, especially the ones
  // following the mnemonic.
  PI.emitNamespace("", true);
  PI.asmMatcherEmitMatchEntryStruct(MaxMnemonicIndex, NumConverters, MaxNumOperands,
          FeatureBitsets, Info);
  PI.emitNamespace("", false);

  unsigned VariantCount = Target.getAsmParserVariantCount();
  PI.asmMatcherEmitMatchTable(Target, Info, StringTable, VariantCount);

  PI.asmMatcherEmitIncludes();

  // Finally, build the match function.
  PI.asmMatcherEmitMatchFunction(
      Target, AsmParser, ClassName, HasMnemonicFirst, HasOptionalOperands,
      ReportMultipleNearMisses, HasMnemonicAliases, MaxNumOperands,
      HasDeprecation, VariantCount);

  if (!Info.OperandMatchInfo.empty()) {
      unsigned MaxMask = 0;
      for (const OperandMatchEntry &OMI : Info.OperandMatchInfo) {
        MaxMask |= OMI.OperandMask;
      }
    PI.asmMatcherEmitCustomOperandParsing(MaxMask, Target, Info, ClassName,
                             StringTable, MaxMnemonicIndex, FeatureBitsets.size(),
                             HasMnemonicFirst, *AsmParser);
  }
  PI.emitIncludeToggle("GET_MATCHER_IMPLEMENTATION", false);

  PI.emitIncludeToggle("GET_MNEMONIC_SPELL_CHECKER", true);
  PI.asmMatcherEmitMnemonicSpellChecker(Target, VariantCount);
  PI.emitIncludeToggle("GET_MNEMONIC_SPELL_CHECKER", false);

  PI.emitIncludeToggle("GET_MNEMONIC_CHECKER", true);
  PI.asmMatcherEmitMnemonicChecker(Target, VariantCount,
                      HasMnemonicFirst, HasMnemonicAliases);
  PI.emitIncludeToggle("GET_MNEMONIC_CHECKER", false);
}

namespace llvm {

void EmitAsmMatcher(RecordKeeper &RK, raw_ostream &OS) {
  formatted_raw_ostream FOS(OS);
  PrinterLanguage const PLang = PrinterLLVM::getLanguage();
  PrinterLLVM *PI = nullptr;
  switch (PLang) {
  default:
    PrintFatalNote(
        "AsmMatcher backend does not support the selected ouput language.");
    return;
  case PRINTER_LANG_CPP:
    PI = new PrinterLLVM(FOS);
    break;
  case PRINTER_LANG_CAPSTONE_C:
    PI = new PrinterCapstone(FOS);
    break;
  }

  AsmMatcherEmitter(RK, *PI).run();
  delete PI;
}

} // end namespace llvm
