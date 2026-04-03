use crate::core::Byte;
use crate::core::microcode::Instruction;
use crate::core::microcode::addressing_modes::*;
use crate::core::microcode::operations::*;

use super::helpers::*;
use super::nmos_6502::{NMOS_6502, NMOS_QUIRKS};
use super::variant::*;

pub static NMOS_6502_FULL: Variant = Variant {
    rules: &[
        DecodeRule {
            matches: |op| lnibble(op) == 0xA,
            decode: decode_implied_nops,
        },
        DecodeRule {
            matches: |op| hnibble(op) == 0x9,
            decode: decode_store_with_hmask,
        },
        DecodeRule {
            matches: |op| lnibble(op) == 0x2,
            decode: decode_jam,
        },
        DecodeRule {
            matches: |op| lnibble(op) == 0xB,
            decode: decode_undocumented_immediate,
        },
        DecodeRule {
            matches: |op| cc(op) == 0b11,
            decode: decode_group_11,
        },
    ],
    parent: Some(&NMOS_6502),
    quirks: &NMOS_QUIRKS,
};

fn decode_jam(op: Byte) -> Option<Instruction> {
    let addr = &NONE;

    let opn = match hnibble(op) {
        0x8 | 0xA | 0xC | 0xE => return None,

        _ => &JAM,
    };

    Some(Instruction::new(addr, opn))
}

// Unlike NOPs with other addressing modes, undocumented implied NOPs don't come about naturally as part of software decoding process.
// Hence they need to be handled manually here
fn decode_implied_nops(op: Byte) -> Option<Instruction> {
    let addr = &IMPLIED;

    let opn = match hnibble(op) {
        0x1 | 0x3 | 0x5 | 0x7 | 0xD | 0xF => &NOP,

        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_undocumented_immediate(op: Byte) -> Option<Instruction> {
    let addr = &IMMEDIATE;

    let opn = match hnibble(op) {
        0x0 => &ANC,
        0x2 => &ANC,
        0x4 => &ALR,
        0x6 => &ARR_WITH_DECIMAL,
        0x8 => &ANE,
        0xA => &LXA,
        0xC => &SBX,
        0xE => &USBC,

        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_group_11(op: Byte) -> Option<Instruction> {
    let mut addr = match bbb(op) {
        0 => &IDX_IND,
        1 => &ZERO_PAGE,
        3 => &ABSOLUTE,
        4 => &IND_IDX,
        5 => &ZERO_PAGE_X,
        6 => &ABSOLUTE_Y,
        7 => &ABSOLUTE_X,
        _ => return None,
    };

    let opn = match aaa(op) {
        0 => &SLO,
        1 => &RLA,
        2 => &SRE,
        3 => &RRA,
        4 => &SAX,
        5 => &LAX,
        6 => &DCP,
        7 => &ISC,
        _ => return None,
    };

    if aaa(op) == 4 || aaa(op) == 5 {
        addr = match bbb(op) {
            5 => &ZERO_PAGE_Y,
            7 => &ABSOLUTE_Y,
            _ => addr,
        };
    }

    if op == 0xBB {
        Some(Instruction::new(&ABSOLUTE_Y, &LAS))
    } else {
        Some(Instruction::new(addr, opn))
    }
}

fn decode_store_with_hmask(op: Byte) -> Option<Instruction> {
    let (addr, opn) = match lnibble(op) {
        0x3 => (&IND_IDX, &SHA),
        0xB => (&ABSOLUTE_Y, &TAS),
        0xC => (&ABSOLUTE_X, &SHY),
        0xE => (&ABSOLUTE_Y, &SHX),
        0xF => (&ABSOLUTE_Y, &SHA),
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}
