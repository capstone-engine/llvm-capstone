//===- SparcASITags.td -------------------------------------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the symbolic operands permitted for various kinds of
// SPARCv9 ASI.
//
//===----------------------------------------------------------------------===//

include "llvm/TableGen/SearchableTable.td"

// Capstone edit:
// This is changed to a Searchable Table.
// Simply because emitting generic tables with the PrinterCapstone class
// is not implemented (and is annoying to do so).

class ASITag<string name, string alt_name, bits<8> op> : SearchableTable {
  string Name = name;
  // A maximum of one alias is supported right now.
  string AltName = alt_name;
  bits<8> Encoding = op;
  let EnumValueField = "Encoding";

  let SearchableFields = ["Name", "AltName", "Encoding"];
}

def : ASITag<"ASI_N", "ASI_NUCLEUS", 0x4>;
def : ASITag<"ASI_N_L", "ASI_NUCLEUS_LITTLE", 0xC>;
def : ASITag<"ASI_AIUP", "ASI_AS_IF_USER_PRIMARY", 0x10>;
def : ASITag<"ASI_AIUS", "ASI_AS_IF_USER_SECONDARY", 0x11>;
def : ASITag<"ASI_AIUP_L", "ASI_AS_IF_USER_PRIMARY_LITTLE", 0x18>;
def : ASITag<"ASI_AIUS_L", "ASI_AS_IF_USER_SECONDARY_LITTLE", 0x19>;
def : ASITag<"ASI_P", "ASI_PRIMARY", 0x80>;
def : ASITag<"ASI_S", "ASI_SECONDARY", 0x81>;
def : ASITag<"ASI_PNF", "ASI_PRIMARY_NOFAULT", 0x82>;
def : ASITag<"ASI_SNF", "ASI_SECONDARY_NOFAULT", 0x83>;
def : ASITag<"ASI_P_L", "ASI_PRIMARY_LITTLE", 0x88>;
def : ASITag<"ASI_S_L", "ASI_SECONDARY_LITTLE", 0x89>;
def : ASITag<"ASI_PNF_L", "ASI_PRIMARY_NOFAULT_LITTLE", 0x8A>;
def : ASITag<"ASI_SNF_L", "ASI_SECONDARY_NOFAULT_LITTLE", 0x8B>;
