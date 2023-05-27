//===- SearchableTableEmitter.cpp - Generate efficiently searchable tables -==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits a generic array initialized by specified fields,
// together with companion index tables and lookup functions (binary search,
// currently).
//
//===----------------------------------------------------------------------===//

#include "Printer.h"
#include "PrinterTypes.h"
#include "SearchableTablesTypes.h"

using namespace llvm;

#define DEBUG_TYPE "searchable-table-emitter"

int getAsInt(Init *B) {
  return cast<IntInit>(
             B->convertInitializerTo(IntRecTy::get(B->getRecordKeeper())))
      ->getValue();
}

namespace {

int getInt(Record *R, StringRef Field) {
  return getAsInt(R->getValueInit(Field));
}


class SearchableTableEmitter {
  RecordKeeper &Records;
  DenseMap<Init *, std::unique_ptr<CodeGenIntrinsic>> Intrinsics;
  std::vector<std::unique_ptr<GenericEnum>> Enums;
  DenseMap<Record *, GenericEnum *> EnumMap;
  std::set<std::string> PreprocessorGuards;
  PrinterLLVM &PI;

public:
  SearchableTableEmitter(RecordKeeper &R, PrinterLLVM &PI) : Records(R), PI(PI) {}

  void run();

private:
  typedef std::pair<Init *, int> SearchTableEntry;

  bool isIntrinsic(Init *I) {
    if (DefInit *DI = dyn_cast<DefInit>(I))
      return DI->getDef()->isSubClassOf("Intrinsic");
    return false;
  }

  CodeGenIntrinsic &getIntrinsic(Init *I) {
    std::unique_ptr<CodeGenIntrinsic> &Intr = Intrinsics[I];
    if (!Intr)
      Intr = std::make_unique<CodeGenIntrinsic>(dyn_cast<DefInit>(I)->getDef(),
                                                std::vector<Record *>());
    return *Intr;
  }

  bool compareBy(Record *LHS, Record *RHS, const SearchIndex &Index);

  void emitGenericTable(const GenericTable &Table);
  void emitGenericEnum(const GenericEnum &Enum, raw_ostream &OS);
  void emitLookupFunction(const GenericTable &Table, const SearchIndex &Index,
                          bool IsPrimary);
  void emitIfdef(StringRef Guard, raw_ostream &OS);

  bool parseFieldType(GenericField &Field, Init *II);
  std::unique_ptr<SearchIndex>
  parseSearchIndex(GenericTable &Table, const RecordVal *RecVal, StringRef Name,
                   const std::vector<StringRef> &Key, bool EarlyOut);
  void collectEnumEntries(GenericEnum &Enum, StringRef NameField,
                          StringRef ValueField,
                          const std::vector<Record *> &Items);
  void collectTableEntries(GenericTable &Table,
                           const std::vector<Record *> &Items);
};

} // End anonymous namespace.

// For search indices that consists of a single field whose numeric value is
// known, return that numeric value.
static int64_t getNumericKey(const SearchIndex &Index, Record *Rec) {
  assert(Index.Fields.size() == 1);

  if (Index.Fields[0].Enum) {
    Record *EnumEntry = Rec->getValueAsDef(Index.Fields[0].Name);
    return Index.Fields[0].Enum->EntryMap[EnumEntry]->second;
  }

  return getInt(Rec, Index.Fields[0].Name);
}

/// Less-than style comparison between \p LHS and \p RHS according to the
/// key of \p Index.
bool SearchableTableEmitter::compareBy(Record *LHS, Record *RHS,
                                       const SearchIndex &Index) {
  for (const auto &Field : Index.Fields) {
    Init *LHSI = LHS->getValueInit(Field.Name);
    Init *RHSI = RHS->getValueInit(Field.Name);

    if (isa<BitsRecTy>(Field.RecType) || isa<IntRecTy>(Field.RecType)) {
      int64_t LHSi = getAsInt(LHSI);
      int64_t RHSi = getAsInt(RHSI);
      if (LHSi < RHSi)
        return true;
      if (LHSi > RHSi)
        return false;
    } else if (Field.IsIntrinsic) {
      CodeGenIntrinsic &LHSi = getIntrinsic(LHSI);
      CodeGenIntrinsic &RHSi = getIntrinsic(RHSI);
      if (std::tie(LHSi.TargetPrefix, LHSi.Name) <
          std::tie(RHSi.TargetPrefix, RHSi.Name))
        return true;
      if (std::tie(LHSi.TargetPrefix, LHSi.Name) >
          std::tie(RHSi.TargetPrefix, RHSi.Name))
        return false;
    } else if (Field.IsInstruction) {
      // This does not correctly compare the predefined instructions!
      Record *LHSr = cast<DefInit>(LHSI)->getDef();
      Record *RHSr = cast<DefInit>(RHSI)->getDef();

      bool LHSpseudo = LHSr->getValueAsBit("isPseudo");
      bool RHSpseudo = RHSr->getValueAsBit("isPseudo");
      if (LHSpseudo && !RHSpseudo)
        return true;
      if (!LHSpseudo && RHSpseudo)
        return false;

      int comp = LHSr->getName().compare(RHSr->getName());
      if (comp < 0)
        return true;
      if (comp > 0)
        return false;
    } else if (Field.Enum) {
      auto LHSr = cast<DefInit>(LHSI)->getDef();
      auto RHSr = cast<DefInit>(RHSI)->getDef();
      int64_t LHSv = Field.Enum->EntryMap[LHSr]->second;
      int64_t RHSv = Field.Enum->EntryMap[RHSr]->second;
      if (LHSv < RHSv)
        return true;
      if (LHSv > RHSv)
        return false;
    } else {
      StringRef LHSIEnum = (Field.IsIntrinsic ? getIntrinsic(LHSI).EnumName : "");
      StringRef RHSIEnum = (Field.IsIntrinsic ? getIntrinsic(LHSI).EnumName : "");
      std::string LHSs = PI.searchableTablesPrimaryRepresentation(Index.Loc, Field, LHSI, LHSIEnum);
      std::string RHSs = PI.searchableTablesPrimaryRepresentation(Index.Loc, Field, RHSI, RHSIEnum);

      if (isa<StringRecTy>(Field.RecType)) {
        LHSs = StringRef(LHSs).upper();
        RHSs = StringRef(RHSs).upper();
      }

      int comp = LHSs.compare(RHSs);
      if (comp < 0)
        return true;
      if (comp > 0)
        return false;
    }
  }
  return false;
}

void SearchableTableEmitter::emitLookupFunction(const GenericTable &Table,
                                                const SearchIndex &Index,
                                                bool IsPrimary) {
  PI.searchableTablesEmitLookupDeclaration(Table, Index, ST_IMPL_OS);

  std::vector<Record *> IndexRowsStorage;
  ArrayRef<Record *> IndexRows;
  StringRef IndexTypeName;
  StringRef IndexName;

  if (IsPrimary) {
    IndexTypeName = Table.CppTypeName;
    IndexName = Table.Name;
    IndexRows = Table.Entries;
  } else {
    PI.searchableTablesEmitIndexTypeStruct(Table, Index);

    PI.searchableTablesEmitIndexArrayI();

    std::vector<std::pair<Record *, unsigned>> Entries;
    Entries.reserve(Table.Entries.size());
    for (unsigned i = 0; i < Table.Entries.size(); ++i)
      Entries.emplace_back(Table.Entries[i], i);

    llvm::stable_sort(Entries, [&](const std::pair<Record *, unsigned> &LHS,
                                   const std::pair<Record *, unsigned> &RHS) {
      return compareBy(LHS.first, RHS.first, Index);
    });

    IndexRowsStorage.reserve(Entries.size());
    for (const auto &Entry : Entries) {
      IndexRowsStorage.push_back(Entry.first);

      PI.searchableTablesEmitIndexArrayII();
      ListSeparator LS;
      for (const auto &Field : Index.Fields) {
        StringRef EnumName = (Field.IsIntrinsic ? getIntrinsic(Entry.first->getValueInit(Field.Name)).EnumName : "");
        std::string Repr = PI.searchableTablesPrimaryRepresentation(
            Index.Loc, Field, Entry.first->getValueInit(Field.Name),
            EnumName);
        if (isa<StringRecTy>(Field.RecType))
          Repr = StringRef(Repr).upper();
        PI.searchableTablesEmitIndexArrayIII(LS, Repr);
      }
      PI.searchableTablesEmitIndexArrayIV(Entry);
    }
    PI.searchableTablesEmitIndexArrayV();

    IndexTypeName = "IndexType";
    IndexName = "Index";
    IndexRows = IndexRowsStorage;
  }

  bool IsContiguous = false;

  if (Index.Fields.size() == 1 &&
      (Index.Fields[0].Enum || isa<BitsRecTy>(Index.Fields[0].RecType))) {
    IsContiguous = true;
    for (unsigned i = 0; i < IndexRows.size(); ++i) {
      if (getNumericKey(Index, IndexRows[i]) != i) {
        IsContiguous = false;
        break;
      }
    }
  }

  if (IsContiguous) {
    PI.searchableTablesEmitIsContiguousCase(IndexName, Table, Index, IsPrimary);
    return;
  }

  if (Index.EarlyOut) {
    const GenericField &Field = Index.Fields[0];
    StringRef EnumNameA = (Field.IsIntrinsic ?
      getIntrinsic(IndexRows[0]->getValueInit(Field.Name)).EnumName :
      "");
    std::string FirstRepr = PI.searchableTablesPrimaryRepresentation(
        Index.Loc, Field, IndexRows[0]->getValueInit(Field.Name),
        EnumNameA);

    StringRef EnumNameB = (Field.IsIntrinsic ?
      getIntrinsic(IndexRows.back()->getValueInit(Field.Name)).EnumName :
      "");
    std::string LastRepr = PI.searchableTablesPrimaryRepresentation(
        Index.Loc, Field, IndexRows.back()->getValueInit(Field.Name),
        EnumNameB);
    PI.searchableTablesEmitIfFieldCase(Field, FirstRepr, LastRepr);
  }

  PI.searchableTablesEmitKeyTypeStruct(Table, Index);
  PI.searchableTablesEmitKeyArray(Table, Index, IsPrimary);
  PI.searchableTablesEmitIndexLamda(Index, IndexName, IndexTypeName);
  PI.searchableTablesEmitReturns(Table, Index, IsPrimary);
}

void SearchableTableEmitter::emitGenericTable(const GenericTable &Table) {
  PI.searchableTablesEmitIfdef((Twine("GET_") + Table.PreprocessorGuard + "_DECL").str(),
    ST_DECL_OS);

  // Emit the declarations for the functions that will perform lookup.
  if (Table.PrimaryKey) {
    PI.searchableTablesEmitLookupDeclaration(Table, *Table.PrimaryKey, ST_DECL_OS);
  }
  for (const auto &Index : Table.Indices) {
    PI.searchableTablesEmitLookupDeclaration(Table, *Index, ST_DECL_OS);
  }
  PI.searchableTablesEmitEndif(ST_DECL_OS);

  PI.searchableTablesEmitIfdef((Twine("GET_") + Table.PreprocessorGuard + "_IMPL").str(),
    ST_IMPL_OS);

  PI.searchableTablesEmitMapI(Table);
  // The primary data table contains all the fields defined for this map.
  for (unsigned i = 0; i < Table.Entries.size(); ++i) {
    Record *Entry = Table.Entries[i];
    PI.searchableTablesEmitMapII();

    ListSeparator LS;
    for (const auto &Field : Table.Fields) {
      StringRef EnumName = (Field.IsIntrinsic ? getIntrinsic(Entry->getValueInit(Field.Name)).EnumName : "");
      PI.searchableTablesEmitMapIII(Table, LS, Field,
        EnumName, Entry);
    }

    PI.searchableTablesEmitMapIV(i);
  }
  PI.searchableTablesEmitMapV();

  // Indexes are sorted "{ Thing, PrimaryIdx }" arrays, so that a binary
  // search can be performed by "Thing".
  if (Table.PrimaryKey)
    emitLookupFunction(Table, *Table.PrimaryKey, true);
  for (const auto &Index : Table.Indices)
    emitLookupFunction(Table, *Index, false);

  PI.searchableTablesEmitEndif(ST_IMPL_OS);
}

bool SearchableTableEmitter::parseFieldType(GenericField &Field, Init *TypeOf) {
  if (auto Type = dyn_cast<StringInit>(TypeOf)) {
    if (Type->getValue() == "code") {
      Field.IsCode = true;
      return true;
    } else {
      if (Record *TypeRec = Records.getDef(Type->getValue())) {
        if (TypeRec->isSubClassOf("GenericEnum")) {
          Field.Enum = EnumMap[TypeRec];
          Field.RecType = RecordRecTy::get(Field.Enum->Class);
          return true;
        }
      }
    }
  }

  return false;
}

std::unique_ptr<SearchIndex> SearchableTableEmitter::parseSearchIndex(
    GenericTable &Table, const RecordVal *KeyRecVal, StringRef Name,
    const std::vector<StringRef> &Key, bool EarlyOut) {
  auto Index = std::make_unique<SearchIndex>();
  Index->Name = std::string(Name);
  Index->Loc = KeyRecVal->getLoc();
  Index->EarlyOut = EarlyOut;

  for (const auto &FieldName : Key) {
    const GenericField *Field = Table.getFieldByName(FieldName);
    if (!Field)
      PrintFatalError(
          KeyRecVal,
          Twine("In table '") + Table.Name +
              "', 'PrimaryKey' or 'Key' refers to nonexistent field '" +
              FieldName + "'");
                      
    Index->Fields.push_back(*Field);
  }

  if (EarlyOut && isa<StringRecTy>(Index->Fields[0].RecType)) {
    PrintFatalError(
        KeyRecVal, Twine("In lookup method '") + Name + "', early-out is not " +
                       "supported for a first key field of type string");
  }

  return Index;
}

void SearchableTableEmitter::collectEnumEntries(
    GenericEnum &Enum, StringRef NameField, StringRef ValueField,
    const std::vector<Record *> &Items) {
  for (auto *EntryRec : Items) {
    StringRef Name;
    if (NameField.empty())
      Name = EntryRec->getName();
    else
      Name = EntryRec->getValueAsString(NameField);

    int64_t Value = 0;
    if (!ValueField.empty())
      Value = getInt(EntryRec, ValueField);

    Enum.Entries.push_back(std::make_unique<GenericEnum::Entry>(Name, Value));
    Enum.EntryMap.insert(std::make_pair(EntryRec, Enum.Entries.back().get()));
  }

  if (ValueField.empty()) {
    llvm::stable_sort(Enum.Entries,
                      [](const std::unique_ptr<GenericEnum::Entry> &LHS,
                         const std::unique_ptr<GenericEnum::Entry> &RHS) {
                        return LHS->first < RHS->first;
                      });

    for (size_t i = 0; i < Enum.Entries.size(); ++i)
      Enum.Entries[i]->second = i;
  }
}

void SearchableTableEmitter::collectTableEntries(
    GenericTable &Table, const std::vector<Record *> &Items) {
  if (Items.empty())
    PrintFatalError(Table.Locs,
                    Twine("Table '") + Table.Name + "' has no entries");

  for (auto *EntryRec : Items) {
    for (auto &Field : Table.Fields) {
      auto TI = dyn_cast<TypedInit>(EntryRec->getValueInit(Field.Name));
      if (!TI || !TI->isComplete()) {
        PrintFatalError(EntryRec, Twine("Record '") + EntryRec->getName() +
                                      "' for table '" + Table.Name +
                                      "' is missing field '" + Field.Name +
                                      "'");
      }
      if (!Field.RecType) {
        Field.RecType = TI->getType();
      } else {
        RecTy *Ty = resolveTypes(Field.RecType, TI->getType());
        if (!Ty)
          PrintFatalError(EntryRec->getValue(Field.Name), 
                          Twine("Field '") + Field.Name + "' of table '" +
                          Table.Name + "' entry has incompatible type: " +
                          TI->getType()->getAsString() + " vs. " +
                          Field.RecType->getAsString());
        Field.RecType = Ty;
      }
    }

    Table.Entries.push_back(EntryRec); // Add record to table's record list.
  }

  Record *IntrinsicClass = Records.getClass("Intrinsic");
  Record *InstructionClass = Records.getClass("Instruction");
  for (auto &Field : Table.Fields) {
    if (!Field.RecType)
      PrintFatalError(Twine("Cannot determine type of field '") + Field.Name +
                      "' in table '" + Table.Name + "'. Maybe it is not used?");

    if (auto RecordTy = dyn_cast<RecordRecTy>(Field.RecType)) {
      if (IntrinsicClass && RecordTy->isSubClassOf(IntrinsicClass))
        Field.IsIntrinsic = true;
      else if (InstructionClass && RecordTy->isSubClassOf(InstructionClass))
        Field.IsInstruction = true;
    }
  }

  SearchIndex Idx;
  std::copy(Table.Fields.begin(), Table.Fields.end(),
            std::back_inserter(Idx.Fields));
  llvm::sort(Table.Entries, [&](Record *LHS, Record *RHS) {
    return compareBy(LHS, RHS, Idx);
  });
}

void SearchableTableEmitter::run() {
  // Emit tables in a deterministic order to avoid needless rebuilds.
  SmallVector<std::unique_ptr<GenericTable>, 4> Tables;
  DenseMap<Record *, GenericTable *> TableMap;

  // Collect all definitions first.
  for (auto *EnumRec : Records.getAllDerivedDefinitions("GenericEnum")) {
    StringRef NameField;
    if (!EnumRec->isValueUnset("NameField"))
      NameField = EnumRec->getValueAsString("NameField");

    StringRef ValueField;
    if (!EnumRec->isValueUnset("ValueField"))
      ValueField = EnumRec->getValueAsString("ValueField");

    auto Enum = std::make_unique<GenericEnum>();
    Enum->Name = std::string(EnumRec->getName());
    Enum->PreprocessorGuard = std::string(EnumRec->getName());

    StringRef FilterClass = EnumRec->getValueAsString("FilterClass");
    Enum->Class = Records.getClass(FilterClass);
    if (!Enum->Class)
      PrintFatalError(EnumRec->getValue("FilterClass"), 
                      Twine("Enum FilterClass '") + FilterClass +
                          "' does not exist");

    collectEnumEntries(*Enum, NameField, ValueField,
                       Records.getAllDerivedDefinitions(FilterClass));
    EnumMap.insert(std::make_pair(EnumRec, Enum.get()));
    Enums.emplace_back(std::move(Enum));
  }

  for (auto *TableRec : Records.getAllDerivedDefinitions("GenericTable")) {
    auto Table = std::make_unique<GenericTable>();
    Table->Name = std::string(TableRec->getName());
    Table->Locs = TableRec->getLoc();
    Table->PreprocessorGuard = std::string(TableRec->getName());
    Table->CppTypeName = std::string(TableRec->getValueAsString("CppTypeName"));

    std::vector<StringRef> Fields = TableRec->getValueAsListOfStrings("Fields");
    for (const auto &FieldName : Fields) {
      Table->Fields.emplace_back(FieldName); // Construct a GenericField.

      if (auto TypeOfRecordVal = TableRec->getValue(("TypeOf_" + FieldName).str())) {
        if (!parseFieldType(Table->Fields.back(), TypeOfRecordVal->getValue())) {
          PrintError(TypeOfRecordVal, 
                     Twine("Table '") + Table->Name +
                         "' has invalid 'TypeOf_" + FieldName +
                         "': " + TypeOfRecordVal->getValue()->getAsString());
          PrintFatalNote("The 'TypeOf_xxx' field must be a string naming a "
                         "GenericEnum record, or \"code\"");
        }
      }
    }

    StringRef FilterClass = TableRec->getValueAsString("FilterClass");
    if (!Records.getClass(FilterClass))
      PrintFatalError(TableRec->getValue("FilterClass"), 
                      Twine("Table FilterClass '") +
                          FilterClass + "' does not exist");

    collectTableEntries(*Table, Records.getAllDerivedDefinitions(FilterClass));

    if (!TableRec->isValueUnset("PrimaryKey")) {
      Table->PrimaryKey =
          parseSearchIndex(*Table, TableRec->getValue("PrimaryKey"),
                           TableRec->getValueAsString("PrimaryKeyName"),
                           TableRec->getValueAsListOfStrings("PrimaryKey"),
                           TableRec->getValueAsBit("PrimaryKeyEarlyOut"));

      llvm::stable_sort(Table->Entries, [&](Record *LHS, Record *RHS) {
        return compareBy(LHS, RHS, *Table->PrimaryKey);
      });
    }

    TableMap.insert(std::make_pair(TableRec, Table.get()));
    Tables.emplace_back(std::move(Table));
  }

  for (Record *IndexRec : Records.getAllDerivedDefinitions("SearchIndex")) {
    Record *TableRec = IndexRec->getValueAsDef("Table");
    auto It = TableMap.find(TableRec);
    if (It == TableMap.end())
      PrintFatalError(IndexRec->getValue("Table"), 
                      Twine("SearchIndex '") + IndexRec->getName() +
                          "' refers to nonexistent table '" +
                          TableRec->getName());

    GenericTable &Table = *It->second;
    Table.Indices.push_back(
        parseSearchIndex(Table, IndexRec->getValue("Key"), IndexRec->getName(), 
                         IndexRec->getValueAsListOfStrings("Key"),
                         IndexRec->getValueAsBit("EarlyOut")));
  }

  // Translate legacy tables.
  Record *SearchableTable = Records.getClass("SearchableTable");
  for (auto &NameRec : Records.getClasses()) {
    Record *Class = NameRec.second.get();
    if (Class->getSuperClasses().size() != 1 ||
        !Class->isSubClassOf(SearchableTable))
      continue;

    StringRef TableName = Class->getName();
    std::vector<Record *> Items = Records.getAllDerivedDefinitions(TableName);
    if (!Class->isValueUnset("EnumNameField")) {
      StringRef NameField = Class->getValueAsString("EnumNameField");
      StringRef ValueField;
      if (!Class->isValueUnset("EnumValueField"))
        ValueField = Class->getValueAsString("EnumValueField");

      auto Enum = std::make_unique<GenericEnum>();
      Enum->Name = (Twine(Class->getName()) + "Values").str();
      Enum->PreprocessorGuard = Class->getName().upper();
      Enum->Class = Class;

      collectEnumEntries(*Enum, NameField, ValueField, Items);

      Enums.emplace_back(std::move(Enum));
    }

    auto Table = std::make_unique<GenericTable>();
    Table->Name = (Twine(Class->getName()) + "sList").str();
    Table->Locs = Class->getLoc();
    Table->PreprocessorGuard = Class->getName().upper();
    Table->CppTypeName = std::string(Class->getName());

    for (const RecordVal &Field : Class->getValues()) {
      std::string FieldName = std::string(Field.getName());

      // Skip uninteresting fields: either special to us, or injected
      // template parameters (if they contain a ':').
      if (FieldName.find(':') != std::string::npos ||
          FieldName == "SearchableFields" || FieldName == "EnumNameField" ||
          FieldName == "EnumValueField")
        continue;

      Table->Fields.emplace_back(FieldName);
    }

    collectTableEntries(*Table, Items);

    for (const auto &Field :
         Class->getValueAsListOfStrings("SearchableFields")) {
      std::string Name =
          (Twine("lookup") + Table->CppTypeName + "By" + Field).str();
      Table->Indices.push_back(parseSearchIndex(*Table, Class->getValue(Field),
                                                Name, {Field}, false));
    }

    Tables.emplace_back(std::move(Table));
  }

  // Emit everything.
  for (const auto &Enum : Enums) {
    std::string Guard = (Twine("GET_") + Enum->PreprocessorGuard + "_DECL").str();
    PreprocessorGuards.insert(Guard);
    PI.searchableTablesEmitIfdef(Guard, ST_DECL_OS);
    PI.searchableTablesEmitGenericEnum(*Enum);
    PI.searchableTablesEmitEndif(ST_DECL_OS);
  }

  for (const auto &Table : Tables)
    emitGenericTable(*Table);

  // Put all #undefs last, to allow multiple sections guarded by the same
  // define.
  for (const auto &Guard : PreprocessorGuards)
    PI.searchableTablesEmitUndef(Guard);
}

namespace llvm {

void EmitSearchableTables(RecordKeeper &RK, raw_ostream &OS) {
  formatted_raw_ostream FOS(OS);
  PrinterLanguage const PLang = PrinterLLVM::getLanguage();
  PrinterLLVM *PI = nullptr;
  switch (PLang) {
  default:
    PrintFatalNote(
        "RegisterInfo backend does not support the selected ouput language.");
    return;
  case PRINTER_LANG_CPP:
    PI = new PrinterLLVM(FOS);
    break;
  case PRINTER_LANG_CAPSTONE_C:
    Record *IDef = RK.getClass("I");
    if (!IDef)
      // If this is reached we need to implement the search for other classes which have Namespace set.
      llvm_unreachable("Base instruction class \"I\" does not exist for this target.");
    if (!IDef->getValue("Namespace"))
      llvm_unreachable("Field \"Namespace\" does not exist.");
    std::string TName = IDef->getValueAsString("Namespace").str();
    PI = new PrinterCapstone(FOS, TName);
    break;
  }

  SearchableTableEmitter(RK, *PI).run();
  PI->searchableTablesWriteFiles();
  delete PI;
}

} // End llvm namespace.
