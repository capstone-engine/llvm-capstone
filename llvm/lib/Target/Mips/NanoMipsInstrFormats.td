class InstNM<dag outs, dag ins, string asmstr, list<dag> pattern,
             InstrItinClass itin = NoItinerary>:
  Instruction, PredicateControl, ISA_NANOMIPS
{
  let Namespace = "Mips";
  let DecoderNamespace = "NanoMips";
  let EncodingPredicates = [HasNanoMips];
  string Arch = "nanomips";

  let OutOperandList = outs;
  let InOperandList  = ins;
  let AsmVariantName = "nanomips";
  let AsmString = asmstr;
  let Pattern   = pattern;
  let FastISelShouldIgnore = 1; // disabled for all NanoMips
  bit isCTI = 0; // Any form of Control Transfer Instruction.

  let Itinerary = itin;
}

class InstAsmNM<dag outs, dag ins, string asmstr, list<dag> pattern,
               InstrItinClass itin = NoItinerary>:
	       InstNM<outs, ins, asmstr, [], itin> {
  list<dag> UnusedPattern = pattern;
}

class InstSize16 {
  field bits<16> Inst;
  int Size = 2;
  field bits<16> SoftFail = 0;
}

class InstSize32 {
  field bits<32> Inst;
  int Size = 4;
  field bits<32> SoftFail = 0;
}

class InstSize48 {
  field bits<48> Inst;
  int Size = 6;
  field bits<48> SoftFail = 0;
}

// nanoMIPS Pseudo Instructions Format
class PseudoInstNM<dag outs, dag ins, list<dag> pattern,
		   InstrItinClass itin = IIPseudo> :
    InstNM<outs, ins, "", pattern, itin> {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
}

class NanoMipsAsmPseudoInst<dag outs, dag ins, string asmstr, list<dag> pattern = []> :
	 InstNM<outs, ins, asmstr, pattern, IIPseudo> {
  let isPseudo = 1;
  let hasNoSchedulingInfo = 1;
}

class NMPat<dag pattern, dag result> : Pat<pattern, result>, PredicateControl,
                                       ISA_NANOMIPS;

class NanoMipsInstAlias<string asm, dag result, bit emit = 0b1>:
	MipsInstAlias<asm, result, emit>, ISA_NANOMIPS;

class NanoMipsMTInst : InstNM<(outs), (ins), "", [], NoItinerary>, InstSize32 {
  bit hasSideEffects = 1;
  bit isNotDuplicable = 1;
}
