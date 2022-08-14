//===-- CapstoneGenMapper.h - Mapper Generation Module --------\*- C++ -\*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// Created by Phosphorus15 on 2021/6/3.
//

#ifndef LLVM_UTILS_TABLEGEN_CAPSTONEGENMAPPER_H
#define LLVM_UTILS_TABLEGEN_CAPSTONEGENMAPPER_H

#include "CodeGenInstruction.h"
#include "CodeGenTarget.h"
#include "SubtargetFeatureInfo.h"
#include "Types.h"
#include "llvm/ADT/CachedHashString.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/StringMatcher.h"
#include "llvm/TableGen/StringToOffsetTable.h"
#include "llvm/TableGen/TableGenBackend.h"
#include <cassert>
#include <cctype>
#include <forward_list>
#include <map>
#include <set>

using namespace llvm;

namespace {
class AsmMatcherInfo;

// Register sets are used as keys in some second-order sets TableGen creates
// when generating its data structures. This means that the order of two
// RegisterSets can be seen in the outputted AsmMatcher tables occasionally, and
// can even affect compiler output (at least seen in diagnostics produced when
// all matches fail). So we use a type that sorts them consistently.
typedef std::set<Record *, LessRecordByID> RegisterSet;

class AsmMatcherEmitter {
  RecordKeeper &Records;

public:
  AsmMatcherEmitter(RecordKeeper &R) : Records(R) {}

  void run(raw_ostream &o);
};

/// ClassInfo - Helper class for storing the information about a particular
/// class of operands which can be matched.
struct ClassInfo {
  enum ClassInfoKind {
    /// Invalid kind, for use as a sentinel value.
    Invalid = 0,

    /// The class for a particular token.
    Token,

    /// The (first) register class, subsequent register classes are
    /// RegisterClass0+1, and so on.
    RegisterClass0,

    /// The (first) user defined class, subsequent user defined classes are
    /// UserClass0+1, and so on.
    UserClass0 = 1 << 16
  };

  /// Kind - The class kind, which is either a predefined kind, or (UserClass0 +
  /// N) for the Nth user defined class.
  unsigned Kind;

  /// SuperClasses - The super classes of this class. Note that for simplicities
  /// sake user operands only record their immediate super class, while register
  /// operands include all superclasses.
  std::vector<ClassInfo *> SuperClasses;

  /// Name - The full class name, suitable for use in an enum.
  std::string Name;

  /// ClassName - The unadorned generic name for this class (e.g., Token).
  std::string ClassName;

  /// ValueName - The name of the value this class represents; for a token this
  /// is the literal token string, for an operand it is the TableGen class (or
  /// empty if this is a derived class).
  std::string ValueName;

  /// PredicateMethod - The name of the operand method to test whether the
  /// operand matches this class; this is not valid for Token or register kinds.
  std::string PredicateMethod;

  /// RenderMethod - The name of the operand method to add this operand to an
  /// MCInst; this is not valid for Token or register kinds.
  std::string RenderMethod;

  /// ParserMethod - The name of the operand method to do a target specific
  /// parsing on the operand.
  std::string ParserMethod;

  /// For register classes: the records for all the registers in this class.
  RegisterSet Registers;

  /// For custom match classes: the diagnostic kind for when the predicate
  /// fails.
  std::string DiagnosticType;

  /// For custom match classes: the diagnostic string for when the predicate
  /// fails.
  std::string DiagnosticString;

  /// Is this operand optional and not always required.
  bool IsOptional;

  /// DefaultMethod - The name of the method that returns the default operand
  /// for optional operand
  std::string DefaultMethod;

public:
  /// isRegisterClass() - Check if this is a register class.
  bool isRegisterClass() const {
    return Kind >= RegisterClass0 && Kind < UserClass0;
  }

  /// isUserClass() - Check if this is a user defined class.
  bool isUserClass() const { return Kind >= UserClass0; }

  /// isRelatedTo - Check whether this class is "related" to \p RHS. Classes
  /// are related if they are in the same class hierarchy.
  bool isRelatedTo(const ClassInfo &RHS) const {
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

  /// isSubsetOf - Test whether this class is a subset of \p RHS.
  bool isSubsetOf(const ClassInfo &RHS) const {
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

  int getTreeDepth() const {
    int Depth = 0;
    const ClassInfo *Root = this;
    while (!Root->SuperClasses.empty()) {
      Depth++;
      Root = Root->SuperClasses.front();
    }
    return Depth;
  }

  const ClassInfo *findRoot() const {
    const ClassInfo *Root = this;
    while (!Root->SuperClasses.empty())
      Root = Root->SuperClasses.front();
    return Root;
  }

  /// Compare two classes. This does not produce a total ordering, but does
  /// guarantee that subclasses are sorted before their parents, and that the
  /// ordering is transitive.
  bool operator<(const ClassInfo &RHS) const {
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
};

class AsmVariantInfo {
public:
  StringRef RegisterPrefix;
  StringRef TokenizingCharacters;
  StringRef SeparatorCharacters;
  StringRef BreakCharacters;
  StringRef Name;
  int AsmVariantNo;
};

/// MatchableInfo - Helper class for storing the necessary information for an
/// instruction or alias which is capable of being matched.
struct MatchableInfo {
  struct AsmOperand {
    /// Token - This is the token that the operand came from.
    StringRef Token;

    /// The unique class instance this operand should match.
    ClassInfo *Class;

    /// The operand name this is, if anything.
    StringRef SrcOpName;

    /// The operand name this is, before renaming for tied operands.
    StringRef OrigSrcOpName;

    /// The suboperand index within SrcOpName, or -1 for the entire operand.
    int SubOpIdx;

    /// Whether the token is "isolated", i.e., it is preceded and followed
    /// by separators.
    bool IsIsolatedToken;

    /// Register record if this token is singleton register.
    Record *SingletonReg;

    explicit AsmOperand(bool IsIsolatedToken, StringRef T)
        : Token(T), Class(nullptr), SubOpIdx(-1),
          IsIsolatedToken(IsIsolatedToken), SingletonReg(nullptr) {}
  };

  /// ResOperand - This represents a single operand in the result instruction
  /// generated by the match.  In cases (like addressing modes) where a single
  /// assembler operand expands to multiple MCOperands, this represents the
  /// single assembler operand, not the MCOperand.
  struct ResOperand {
    enum {
      /// RenderAsmOperand - This represents an operand result that is
      /// generated by calling the render method on the assembly operand.  The
      /// corresponding AsmOperand is specified by AsmOperandNum.
      RenderAsmOperand,

      /// TiedOperand - This represents a result operand that is a duplicate of
      /// a previous result operand.
      TiedOperand,

      /// ImmOperand - This represents an immediate value that is dumped into
      /// the operand.
      ImmOperand,

      /// RegOperand - This represents a fixed register that is dumped in.
      RegOperand
    } Kind;

    /// Tuple containing the index of the (earlier) result operand that should
    /// be copied from, as well as the indices of the corresponding (parsed)
    /// operands in the asm string.
    struct TiedOperandsTuple {
      unsigned ResOpnd;
      unsigned SrcOpnd1Idx;
      unsigned SrcOpnd2Idx;
    };

    union {
      /// This is the operand # in the AsmOperands list that this should be
      /// copied from.
      unsigned AsmOperandNum;

      /// Description of tied operands.
      TiedOperandsTuple TiedOperands;

      /// ImmVal - This is the immediate value added to the instruction.
      int64_t ImmVal;

      /// Register - This is the register record.
      Record *Register;
    };

    /// MINumOperands - The number of MCInst operands populated by this
    /// operand.
    unsigned MINumOperands;

    static ResOperand getRenderedOp(unsigned AsmOpNum, unsigned NumOperands) {
      ResOperand X;
      X.Kind = RenderAsmOperand;
      X.AsmOperandNum = AsmOpNum;
      X.MINumOperands = NumOperands;
      return X;
    }

    static ResOperand getTiedOp(unsigned TiedOperandNum, unsigned SrcOperand1,
                                unsigned SrcOperand2) {
      ResOperand X;
      X.Kind = TiedOperand;
      X.TiedOperands = {TiedOperandNum, SrcOperand1, SrcOperand2};
      X.MINumOperands = 1;
      return X;
    }

    static ResOperand getImmOp(int64_t Val) {
      ResOperand X;
      X.Kind = ImmOperand;
      X.ImmVal = Val;
      X.MINumOperands = 1;
      return X;
    }

    static ResOperand getRegOp(Record *Reg) {
      ResOperand X;
      X.Kind = RegOperand;
      X.Register = Reg;
      X.MINumOperands = 1;
      return X;
    }
  };

  /// AsmVariantID - Target's assembly syntax variant no.
  int AsmVariantID;

  /// AsmString - The assembly string for this instruction (with variants
  /// removed), e.g. "movsx $src, $dst".
  std::string AsmString;

  /// TheDef - This is the definition of the instruction or InstAlias that this
  /// matchable came from.
  Record *const TheDef;

  /// DefRec - This is the definition that it came from.
  PointerUnion<const CodeGenInstruction *, const CodeGenInstAlias *> DefRec;

  const CodeGenInstruction *getResultInst() const {
    if (DefRec.is<const CodeGenInstruction *>())
      return DefRec.get<const CodeGenInstruction *>();
    return DefRec.get<const CodeGenInstAlias *>()->ResultInst;
  }

  /// ResOperands - This is the operand list that should be built for the result
  /// MCInst.
  SmallVector<ResOperand, 8> ResOperands;

  /// Mnemonic - This is the first token of the matched instruction, its
  /// mnemonic.
  StringRef Mnemonic;

  /// AsmOperands - The textual operands that this instruction matches,
  /// annotated with a class and where in the OperandList they were defined.
  /// This directly corresponds to the tokenized AsmString after the mnemonic is
  /// removed.
  SmallVector<AsmOperand, 8> AsmOperands;

  /// Predicates - The required subtarget features to match this instruction.
  SmallVector<const SubtargetFeatureInfo *, 4> RequiredFeatures;

  /// ConversionFnKind - The enum value which is passed to the generated
  /// convertToMCInst to convert parsed operands into an MCInst for this
  /// function.
  std::string ConversionFnKind;

  /// If this instruction is deprecated in some form.
  bool HasDeprecation;

  /// If this is an alias, this is use to determine whether or not to using
  /// the conversion function defined by the instruction's AsmMatchConverter
  /// or to use the function generated by the alias.
  bool UseInstAsmMatchConverter;

  MatchableInfo(const CodeGenInstruction &CGI)
      : AsmVariantID(0), AsmString(CGI.AsmString), TheDef(CGI.TheDef),
        DefRec(&CGI), UseInstAsmMatchConverter(true) {}

  MatchableInfo(std::unique_ptr<const CodeGenInstAlias> Alias)
      : AsmVariantID(0), AsmString(Alias->AsmString), TheDef(Alias->TheDef),
        DefRec(Alias.release()), UseInstAsmMatchConverter(TheDef->getValueAsBit(
                                     "UseInstAsmMatchConverter")) {}

  // Could remove this and the dtor if PointerUnion supported unique_ptr
  // elements with a dynamic failure/assertion (like the one below) in the case
  // where it was copied while being in an owning state.
  MatchableInfo(const MatchableInfo &RHS)
      : AsmVariantID(RHS.AsmVariantID), AsmString(RHS.AsmString),
        TheDef(RHS.TheDef), DefRec(RHS.DefRec), ResOperands(RHS.ResOperands),
        Mnemonic(RHS.Mnemonic), AsmOperands(RHS.AsmOperands),
        RequiredFeatures(RHS.RequiredFeatures),
        ConversionFnKind(RHS.ConversionFnKind),
        HasDeprecation(RHS.HasDeprecation),
        UseInstAsmMatchConverter(RHS.UseInstAsmMatchConverter) {
    assert(!DefRec.is<const CodeGenInstAlias *>());
  }

  ~MatchableInfo() { delete DefRec.dyn_cast<const CodeGenInstAlias *>(); }

  // Two-operand aliases clone from the main matchable, but mark the second
  // operand as a tied operand of the first for purposes of the assembler.
  void formTwoOperandAlias(StringRef Constraint);

  void initialize(const AsmMatcherInfo &Info,
                  SmallPtrSetImpl<Record *> &SingletonRegisters,
                  AsmVariantInfo const &Variant, bool HasMnemonicFirst);

  /// validate - Return true if this matchable is a valid thing to match against
  /// and perform a bunch of validity checking.
  bool validate(StringRef CommentDelimiter, bool IsAlias) const;

  /// findAsmOperand - Find the AsmOperand with the specified name and
  /// suboperand index.
  int findAsmOperand(StringRef N, int SubOpIdx) const {
    auto I = find_if(AsmOperands, [&](const AsmOperand &Op) {
      return Op.SrcOpName == N && Op.SubOpIdx == SubOpIdx;
    });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
  }

  /// findAsmOperandNamed - Find the first AsmOperand with the specified name.
  /// This does not check the suboperand index.
  int findAsmOperandNamed(StringRef N, int LastIdx = -1) const {
    auto I =
        std::find_if(AsmOperands.begin() + LastIdx + 1, AsmOperands.end(),
                     [&](const AsmOperand &Op) { return Op.SrcOpName == N; });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
  }

  int findAsmOperandOriginallyNamed(StringRef N) const {
    auto I = find_if(AsmOperands, [&](const AsmOperand &Op) {
      return Op.OrigSrcOpName == N;
    });
    return (I != AsmOperands.end()) ? I - AsmOperands.begin() : -1;
  }

  void buildInstructionResultOperands();
  void buildAliasResultOperands(bool AliasConstraintsAreChecked);

  /// operator< - Compare two matchables.
  bool operator<(const MatchableInfo &RHS) const {
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

    return false;
  }

  /// couldMatchAmbiguouslyWith - Check whether this matchable could
  /// ambiguously match the same set of operands as \p RHS (without being a
  /// strictly superior match).
  bool couldMatchAmbiguouslyWith(const MatchableInfo &RHS) const {
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

  void dump() const;

private:
  void tokenizeAsmString(AsmMatcherInfo const &Info,
                         AsmVariantInfo const &Variant);
  void addAsmOperand(StringRef Token, bool IsIsolatedToken = false);
};

struct OperandMatchEntry {
  unsigned OperandMask;
  const MatchableInfo *MI;
  ClassInfo *CI;

  static OperandMatchEntry create(const MatchableInfo *mi, ClassInfo *ci,
                                  unsigned opMask) {
    OperandMatchEntry X;
    X.OperandMask = opMask;
    X.CI = ci;
    X.MI = mi;
    return X;
  }
};

class AsmMatcherInfo {
public:
  /// Tracked Records
  RecordKeeper &Records;

  /// The tablegen AsmParser record.
  Record *AsmParser;

  /// Target - The target information.
  CodeGenTarget &Target;

  /// The classes which are needed for matching.
  std::forward_list<ClassInfo> Classes;

  /// The information on the matchables to match.
  std::vector<std::unique_ptr<MatchableInfo>> Matchables;

  /// Info for custom matching operands by user defined methods.
  std::vector<OperandMatchEntry> OperandMatchInfo;

  /// Map of Register records to their class information.
  typedef std::map<Record *, ClassInfo *, LessRecordByID> RegisterClassesTy;
  RegisterClassesTy RegisterClasses;

  /// Map of Predicate records to their subtarget information.
  std::map<Record *, SubtargetFeatureInfo, LessRecordByID> SubtargetFeatures;

  /// Map of AsmOperandClass records to their class information.
  std::map<Record *, ClassInfo *> AsmOperandClasses;

  /// Map of RegisterClass records to their class information.
  std::map<Record *, ClassInfo *> RegisterClassClasses;

private:
  /// Map of token to class information which has already been constructed.
  std::map<std::string, ClassInfo *> TokenClasses;

private:
  /// getTokenClass - Lookup or create the class for the given token.
  ClassInfo *getTokenClass(StringRef Token);

  /// getOperandClass - Lookup or create the class for the given operand.
  ClassInfo *getOperandClass(const CGIOperandList::OperandInfo &OI,
                             int SubOpIdx);
  ClassInfo *getOperandClass(Record *Rec, int SubOpIdx);

  /// buildRegisterClasses - Build the ClassInfo* instances for register
  /// classes.
  void buildRegisterClasses(SmallPtrSetImpl<Record *> &SingletonRegisters);

  /// buildOperandClasses - Build the ClassInfo* instances for user defined
  /// operand classes.
  void buildOperandClasses();

  void buildInstructionOperandReference(MatchableInfo *II, StringRef OpName,
                                        unsigned AsmOpIdx);
  void buildAliasOperandReference(MatchableInfo *II, StringRef OpName,
                                  MatchableInfo::AsmOperand &Op);

public:
  AsmMatcherInfo(Record *AsmParser, CodeGenTarget &Target,
                 RecordKeeper &Records);

  /// Construct the various tables used during matching.
  void buildInfo();

  /// buildOperandMatchInfo - Build the necessary information to handle user
  /// defined operand parsing methods.
  void buildOperandMatchInfo();

  /// getSubtargetFeature - Lookup or create the subtarget feature info for the
  /// given operand.
  const SubtargetFeatureInfo *getSubtargetFeature(Record *Def) const {
    assert(Def->isSubClassOf("Predicate") && "Invalid predicate type!");
    const auto &I = SubtargetFeatures.find(Def);
    return I == SubtargetFeatures.end() ? nullptr : &I->second;
  }

  RecordKeeper &getRecords() const { return Records; }

  bool hasOptionalOperands() const {
    return any_of(Classes,
                  [](const ClassInfo &Class) { return Class.IsOptional; });
  }
};

} // end anonymous namespace

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
    switch (Op.Kind) {
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
static void extractSingletonRegisterForAsmOperand(MatchableInfo::AsmOperand &Op,
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
                               SmallPtrSetImpl<Record *> &SingletonRegisters,
                               AsmVariantInfo const &Variant,
                               bool HasMnemonicFirst) {
  AsmVariantID = Variant.AsmVariantNo;
  AsmString = CodeGenInstruction::FlattenAsmStringVariants(
      AsmString, Variant.AsmVariantNo);

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
      addAsmOperand(String.slice(i, EndPos + 1), IsIsolatedToken);
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
      StringRef(AsmString).find(CommentDelimiter) != StringRef::npos)
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
    if (Tok[0] == '$' && Tok.find(':') != StringRef::npos)
      PrintFatalError(
          TheDef->getLoc(),
          "matchable with operand modifier '" + Tok +
              "' not supported by asm matcher.  Mark isCodeGenOnly!");
    // Verify that any operand is only mentioned once.
    // We reject aliases and ignore instructions for now.
    if (!IsAlias && TheDef->getValueAsString("AsmMatchConverter").empty() &&
        Tok[0] == '$' && !OperandNames.insert(std::string(Tok)).second) {
      LLVM_DEBUG({
        errs() << "warning: '" << TheDef->getName() << "': "
               << "ignoring instruction with tied operand '" << Tok << "'\n";
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
    case '*':
      Res += "_STAR_";
      break;
    case '%':
      Res += "_PCT_";
      break;
    case ':':
      Res += "_COLON_";
      break;
    case '!':
      Res += "_EXCLAIM_";
      break;
    case '.':
      Res += "_DOT_";
      break;
    case '<':
      Res += "_LT_";
      break;
    case '>':
      Res += "_GT_";
      break;
    case '-':
      Res += "_MINUS_";
      break;
    case '#':
      Res += "_HASH_";
      break;
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

ClassInfo *AsmMatcherInfo::getOperandClass(Record *Rec, int SubOpIdx) {
  if (Rec->isSubClassOf("RegisterOperand")) {
    // RegisterOperand may have an associated ParserMatchClass. If it does,
    // use it, else just fall back to the underlying register class.
    const RecordVal *R = Rec->getValue("ParserMatchClass");
    if (!R || !R->getValue())
      PrintFatalError(Rec->getLoc(),
                      "Record `" + Rec->getName() +
                          "' does not have a ParserMatchClass!\n");

    if (DefInit *DI = dyn_cast<DefInit>(R->getValue())) {
      Record *MatchClass = DI->getDef();
      if (ClassInfo *CI = AsmOperandClasses[MatchClass])
        return CI;
    }

    // No custom match class. Just use the register class.
    Record *ClassRec = Rec->getValueAsDef("RegClass");
    if (!ClassRec)
      PrintFatalError(Rec->getLoc(),
                      "RegisterOperand `" + Rec->getName() +
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
    PrintFatalError(Rec->getLoc(),
                    "Operand `" + Rec->getName() +
                        "' does not derive from class Operand!\n");
  Record *MatchClass = Rec->getValueAsDef("ParserMatchClass");
  if (ClassInfo *CI = AsmOperandClasses[MatchClass])
    return CI;

  PrintFatalError(Rec->getLoc(), "operand has no match class!");
}

struct LessRegisterSet {
  bool operator()(const RegisterSet &LHS, const RegisterSet &RHS) const {
    // std::set<T> defines its own compariso "operator<", but it
    // performs a lexicographical comparison by T's innate comparison
    // for some reason. We don't want non-deterministic pointer
    // comparisons so use this instead.
    return std::lexicographical_compare(LHS.begin(), LHS.end(), RHS.begin(),
                                        RHS.end(), LessRecordByID());
  }
};

void AsmMatcherInfo::buildRegisterClasses(
    SmallPtrSetImpl<Record *> &SingletonRegisters) {
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
  std::map<Record *, RegisterSet> RegisterMap;
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
  std::map<RegisterSet, ClassInfo *, LessRegisterSet> RegisterSetClasses;
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
      if (RS != RS2 && std::includes(RS2.begin(), RS2.end(), RS.begin(),
                                     RS.end(), LessRecordByID()))
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
  for (std::map<Record *, RegisterSet>::iterator it = RegisterMap.begin(),
                                                 ie = RegisterMap.end();
       it != ie; ++it)
    RegisterClasses[it->first] = RegisterSetClasses[it->second];

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
  std::vector<Record *> AsmOperands =
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

AsmMatcherInfo::AsmMatcherInfo(Record *asmParser, CodeGenTarget &target,
                               RecordKeeper &records)
    : Records(records), AsmParser(asmParser), Target(target) {}

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
  SmallPtrSet<Record *, 16> SingletonRegisters;
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
    Variant.BreakCharacters = AsmVariant->getValueAsString("BreakCharacters");
    Variant.Name = AsmVariant->getValueAsString("Name");
    Variant.AsmVariantNo = AsmVariant->getValueAsInt("Variant");

    for (const CodeGenInstruction *CGI : Target.getInstructionsByEnumValue()) {

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
    std::vector<Record *> AllInstAliases =
        Records.getAllDerivedDefinitions("InstAlias");
    for (unsigned i = 0, e = AllInstAliases.size(); i != e; ++i) {
      auto Alias =
          std::make_unique<CodeGenInstAlias>(AllInstAliases[i], Target);

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

      if (II->DefRec.is<const CodeGenInstruction *>())
        buildInstructionOperandReference(II.get(), OperandName, i);
      else
        buildAliasOperandReference(II.get(), OperandName, Op);
    }

    if (II->DefRec.is<const CodeGenInstruction *>()) {
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
  std::vector<Record *> AllTokenAliases =
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
void AsmMatcherInfo::buildInstructionOperandReference(MatchableInfo *II,
                                                      StringRef OperandName,
                                                      unsigned AsmOpIdx) {
  const CodeGenInstruction &CGI = *II->DefRec.get<const CodeGenInstruction *>();
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
        II->AsmOperands.insert(II->AsmOperands.begin() + AsmOpIdx + SI,
                               NewAsmOp);
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
  const CodeGenInstAlias &CGA = *II->DefRec.get<const CodeGenInstAlias *>();

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
      Op.Class =
          getOperandClass(CGA.ResultOperands[i].getRecord(), Op.SubOpIdx);
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
      assert(AsmOperands[SrcOperand + AI].SubOpIdx == (int)AI &&
             AsmOperands[SrcOperand + AI].SrcOpName == OpInfo.Name &&
             "unexpected AsmOperands for suboperands");
      ResOperands.push_back(ResOperand::getRenderedOp(SrcOperand + AI, 1));
    }
  }
}

void MatchableInfo::buildAliasResultOperands(bool AliasConstraintsAreChecked) {
  const CodeGenInstAlias &CGA = *DefRec.get<const CodeGenInstAlias *>();
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
    for (; AliasOpNo < LastOpNo &&
           CGA.ResultInstOperandIndex[AliasOpNo].first == i;
         ++AliasOpNo) {
      int SubIdx = CGA.ResultInstOperandIndex[AliasOpNo].second;

      // Find out what operand from the asmparser that this MCInst operand
      // comes from.
      switch (CGA.ResultOperands[AliasOpNo].Kind) {
      case CodeGenInstAlias::ResultOperand::K_Record: {
        StringRef Name = CGA.ResultOperands[AliasOpNo].getName();
        int SrcOperand = findAsmOperand(Name, SubIdx);
        if (SrcOperand == -1)
          PrintFatalError(TheDef->getLoc(),
                          "Instruction '" + TheDef->getName() +
                              "' has operand '" + OpName +
                              "' that doesn't appear in asm string!");

        // Add it to the operand references. If it is added a second time, the
        // record won't be updated and it will fail later on.
        OperandRefs.try_emplace(Name, SrcOperand);

        unsigned NumOperands = (SubIdx == -1 ? OpInfo->MINumOperands : 1);
        ResOperands.push_back(
            ResOperand::getRenderedOp(SrcOperand, NumOperands));
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

namespace {

class CapstoneGenMapper {
  RecordKeeper &Records;

public:
  CapstoneGenMapper(RecordKeeper &R) : Records(R) {}

  void run(raw_ostream &O);
};

std::string truncForCapstone(std::string flag) {
  if (flag.find("IS") == 0 || flag.find("IN") == 0) {
    return flag.substr(2);
  }
  if (flag.find("HAS") == 0)
    return flag.substr(3);
  return flag;
}

void emitInstrMatchTable(raw_ostream &OS, CodeGenTarget &Target,
                         const AsmMatcherInfo &Info) {
  OS << "static const insn_map insns[] = {\n"
     << "\t// dummy item\n"
     << "\t{\n"
     << "\t\t0, 0,\n"
     << "#ifndef CAPSTONE_DIET\n"
     << "\t\t{ 0 }, { 0 }, { 0 }, 0, 0\n"
     << "#endif\n"
     << "\t},\n"
     << "\n";

  std::set<std::string> InstructClass;

  unsigned VariantCount = Target.getAsmParserVariantCount();
  for (unsigned VC = 0; VC != VariantCount; ++VC) {
    Record *AsmVariant = Target.getAsmParserVariant(VC);
    int AsmVariantNo = AsmVariant->getValueAsInt("Variant");

    for (const auto &MI : Info.Matchables) {
      if (MI->AsmVariantID != AsmVariantNo)
        continue;

      const auto *R = MI->getResultInst();

      bool IsBranch = R->isBranch;
      bool IsIndirectBranch = R->isIndirectBranch;

      std::string CapstoneMnemonic = MI->Mnemonic.upper();
      auto DotPos = CapstoneMnemonic.find('.');
      if (DotPos != std::string::npos) {
        // truncate the tailing part
        CapstoneMnemonic = CapstoneMnemonic.substr(0, DotPos);
      }

      CapstoneMnemonic =
          Target.getInstNamespace().upper() + "_INS_" + CapstoneMnemonic;

      OS << "\t{\n"
         << " /* " << MI->Mnemonic << " */ \n\t\t" << Target.getInstNamespace()
         << "_" << MI->getResultInst()->TheDef->getName() << ", "
         << CapstoneMnemonic << ","
         << "\n";

      InstructClass.insert(CapstoneMnemonic);

      OS << "#ifndef CAPSTONE_DIET\n";

      // Write the required features mask.
      OS << "\t\t{ 0 }, { 0 },";
      OS << "{ ";
      for (unsigned i = 0, e = MI->RequiredFeatures.size(); i != e; ++i)
        OS << Target.getInstNamespace().upper() << "_GRP_"
           << truncForCapstone(
                  MI->RequiredFeatures[i]->TheDef->getName().upper())
           << ", ";
      OS << "0 }, ";
      OS << uint16_t(IsBranch) << ", " << uint16_t(IsIndirectBranch);
      OS << "\n#endif\n\t},\n";
    }

    OS << "};\n\n\n\n";
  }

  OS << "typedef enum " << Target.getInstNamespace().lower()
     << "_insn {\n"
        "  "
     << Target.getInstNamespace().upper() << "_INS_INVALID = 0";
  for (auto Mnemonic : InstructClass) {
    OS << ",\n  " << Mnemonic;
  }
  OS << "\n} " << Target.getInstNamespace().lower() << "_insn;";
}

void CapstoneGenMapper::run(raw_ostream &O) {
  CodeGenTarget Target(Records);
  Record *AsmParser = Target.getAsmParser();

  AsmMatcherInfo Info(AsmParser, Target, Records);
  Info.buildInfo();

  // Sort the instruction table using the partial order on classes. We use
  // stable_sort to ensure that ambiguous instructions are still
  // deterministically ordered.
  llvm::stable_sort(
      Info.Matchables,
      [](const std::unique_ptr<MatchableInfo> &a,
         const std::unique_ptr<MatchableInfo> &b) { return *a < *b; });
  emitInstrMatchTable(O, Target, Info);
}

} // namespace

#endif // LLVM_UTILS_TABLEGEN_CAPSTONEGENMAPPER_H
