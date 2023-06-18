#include "Alpha.h"
#include "AlphaRegisterInfo.h"
#include "AlphaSubtarget.h"
#include "MCTargetDesc/AlphaMCTargetDesc.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "alpha-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {

/// A disassembler class for Alpha.
class AlphaDisassembler : public MCDisassembler {
public:
  AlphaDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}
  virtual ~AlphaDisassembler() = default;

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};
} // namespace

static MCDisassembler *createAlphaDisassembler(const Target &T,
                                             const MCSubtargetInfo &STI,
                                             MCContext &Ctx) {
  return new AlphaDisassembler(STI, Ctx);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeAlphaDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(getTheAlphaTarget(),
                                         createAlphaDisassembler);
}

static const uint16_t GPRDecoderTable[] = {
    Alpha::R0,  Alpha::R1,  Alpha::R2,  Alpha::R3,  Alpha::R4,  Alpha::R5,  Alpha::R6,
    Alpha::R7,  Alpha::R8,  Alpha::R9,  Alpha::R10, Alpha::R11, Alpha::R12, Alpha::R13,
    Alpha::R14, Alpha::R15, Alpha::R16, Alpha::R17, Alpha::R18, Alpha::R19, Alpha::R20,
    Alpha::R21, Alpha::R22, Alpha::R23, Alpha::R24, Alpha::R25, Alpha::R26, Alpha::R27,
    Alpha::R28, Alpha::R29, Alpha::R30, Alpha::R31,
};

