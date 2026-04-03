use crate::core::Byte;
use crate::core::cpu::Status;
use crate::core::microcode::Instruction;
use crate::core::microcode::addressing_modes::IMMEDIATE;
use crate::core::microcode::operations::ARR_RICOH;
use crate::core::variants::nmos_6502_full::NMOS_6502_FULL;

use super::traits::{ALUOuput, VariantQuirks};
use super::variant::*;

pub static RICOH_2A03: Variant = Variant {
    rules: &[DecodeRule {
        matches: |op| op == 0x6B,
        decode: |_| {
            let addr = &IMMEDIATE;
            let opn = &ARR_RICOH;
            Some(Instruction::new(addr, opn))
        },
    }],
    parent: Some(&NMOS_6502_FULL),
    quirks: &RICOH_QUIRKS,
};

#[allow(non_snake_case)]
static RICOH_QUIRKS: VariantQuirks = VariantQuirks {
    adc: |cpu, value| {
        let carry = cpu.flags.contains(Status::CARRY) as u16;
        let A = cpu.a as u16;
        let M = value as u16;
        let result = A + M + carry;

        cpu.set_binarymode_flags(A, M, result);
        ALUOuput::Done(result as Byte)
    },

    sbc: |cpu, value| {
        let carry = cpu.flags.contains(Status::CARRY) as u16;
        let A = cpu.a as u16;
        let M = (value ^ 0xFF) as u16;
        let result = A + M + carry;

        cpu.set_binarymode_flags(A, M, result);
        ALUOuput::Done(result as Byte)
    },

    ind_addr_inc: |addr| {
        let lo = addr & 0x00FF;
        let hi = addr & 0xFF00;

        // Bug: Wrap within page: $12FF → $1200
        ALUOuput::Done(hi | ((lo + 1) & 0x00FF))
    },
};
