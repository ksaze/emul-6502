#[allow(clippy::wildcard_imports)]
use crate::operations::*;
use crate::shared::Byte;

#[inline]
fn aaa(op: u8) -> u8 {
    op & 0b111
}

#[inline]
fn bbb(op: u8) -> u8 {
    (op >> 3) & 0b111
}

#[inline]
fn cc(op: u8) -> u8 {
    (op >> 6) & 0b11
}

pub trait Decoder {
    fn decode(&self, opcode: Byte) -> Option<Instruction>;
}

// Decode should only return None to delegate decoding to parent variant
// Base variants should always return some Instruction
pub struct DecodeRule {
    pub matches: fn(u8) -> bool,
    pub decode: fn(u8) -> Option<Instruction>,
}

#[derive(Copy, Clone)]
pub struct Variant {
    pub rules: &'static [DecodeRule],
    pub parent: Option<&'static Variant>,
}

impl Decoder for Variant {
    fn decode(&self, opcode: u8) -> Option<Instruction> {
        for rule in self.rules {
            if (rule.matches)(opcode) {
                if let Some(desc) = (rule.decode)(opcode) {
                    return Some(desc);
                }
            }
        }

        self.parent.and_then(|p| p.decode(opcode))
    }
}

// #[inline]
// fn is_alu(op: Byte) -> bool {
//     cc(op) == 0b01
// }
//
// fn decode_alu(op: Byte) -> Option<Instruction> {
//     let addr = match aaa(op) {
//         0 => &IDX_IND,
//         1 => &ZP,
//         2 => &IMM,
//         3 => &ABS,
//         4 => &IND_IDX,
//         5 => &ZP_X,
//         6 => &ABS_Y,
//         7 => &ABS_X,
//         _ => return None,
//     };
//
//     let opn = match bbb(op) {
//         0 => &ORA,
//         1 => &AND,
//         2 => &EOR,
//         3 => &ADC,
//         4 => &STA,
//         5 => &LDA,
//         6 => &CMP,
//         7 => &SBC,
//         _ => return None,
//     };
//
//     Some(Instruction::new(addr, opn))
// }

pub static NMOS_6502: Variant = Variant {
    rules: &[],
    parent: None,
};
