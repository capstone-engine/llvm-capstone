//===----- AsmMatcherEmitterTypes.h - Asm Matcher Types -*- C++ ----*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===-----------------------------------------------------------------===//

#ifndef LLVM_UTILS_TABLEGEN_ASMMATCHEREMITTERTYPES_H
#define LLVM_UTILS_TABLEGEN_ASMMATCHEREMITTERTYPES_H

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

class AsmMatcherInfo;

// Register sets are used as keys in some second-order sets TableGen creates
// when generating its data structures. This means that the order of two
// RegisterSets can be seen in the outputted AsmMatcher tables occasionally, and
// can even affect compiler output (at least seen in diagnostics produced when
// all matches fail). So we use a type that sorts them consistently.
typedef std::set<Record *, LessRecordByID> RegisterSet;

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
  bool isRelatedTo(const ClassInfo &RHS) const;

  /// isSubsetOf - Test whether this class is a subset of \p RHS.
  bool isSubsetOf(const ClassInfo &RHS) const;

  int getTreeDepth() const;

  const ClassInfo *findRoot() const;

  /// Compare two classes. This does not produce a total ordering, but does
  /// guarantee that subclasses are sorted before their parents, and that the
  /// ordering is transitive.
  bool operator<(const ClassInfo &RHS) const;
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
  int findAsmOperand(StringRef N, int SubOpIdx) const;

  /// findAsmOperandNamed - Find the first AsmOperand with the specified name.
  /// This does not check the suboperand index.
  int findAsmOperandNamed(StringRef N, int LastIdx = -1) const;

  int findAsmOperandOriginallyNamed(StringRef N) const;

  void buildInstructionResultOperands();
  void buildAliasResultOperands(bool AliasConstraintsAreChecked);

  /// operator< - Compare two matchables.
  bool operator<(const MatchableInfo &RHS) const;

  /// couldMatchAmbiguouslyWith - Check whether this matchable could
  /// ambiguously match the same set of operands as \p RHS (without being a
  /// strictly superior match).
  bool couldMatchAmbiguouslyWith(const MatchableInfo &RHS) const;

  void dump() const;

  void tokenizeAsmString(AsmMatcherInfo const &Info,
                         AsmVariantInfo const &Variant);
private:
  void addAsmOperand(StringRef Token, bool IsIsolatedToken = false);
};

struct OperandMatchEntry {
  unsigned OperandMask;
  const MatchableInfo *MI;
  ClassInfo *CI;

  static OperandMatchEntry create(const MatchableInfo *mi, ClassInfo *ci,
                                  unsigned opMask);
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
  const SubtargetFeatureInfo *getSubtargetFeature(Record *Def) const;

  RecordKeeper &getRecords() const { return Records; }

  bool hasOptionalOperands() const;
};

#endif // LLVM_UTILS_TABLEGEN_ASMMATCHEREMITTERTYPES_H
