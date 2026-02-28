#[allow(clippy::wildcard_imports)]
use crate::operations::*;
use crate::{
    cpu::{CPUCore, Status},
    shared::{Byte, Word},
};

#[inline]
fn aaa(op: u8) -> u8 {
    (op & 0xE0) >> 5
}

#[inline]
fn bbb(op: u8) -> u8 {
    (op & 0x1C) >> 2
}

#[inline]
fn cc(op: u8) -> u8 {
    op & 0x03
}

#[inline]
fn lnibble(op: u8) -> u8 {
    op & 0x0F
}

#[inline]
fn hnibble(op: u8) -> u8 {
    (op & 0xF0) >> 4
}

pub enum ALUOuput<T> {
    Done(T),
    Penalty(T),
}

pub struct VariantQuirks {
    pub adc: fn(&mut CPUCore, Byte) -> ALUOuput<Byte>,
    pub sbc: fn(&mut CPUCore, Byte) -> ALUOuput<Byte>,
    pub ind_addr_inc: fn(Word) -> ALUOuput<Word>,
}

pub trait Decoder {
    fn decode(&self, opcode: Byte) -> Option<Instruction>;
}

pub trait Quirks {
    fn quirks(&self) -> &'static VariantQuirks;
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
    pub quirks: &'static VariantQuirks,
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

impl Quirks for Variant {
    fn quirks(&self) -> &'static VariantQuirks {
        self.quirks
    }
}

fn decode_gr1(op: Byte) -> Option<Instruction> {
    let addr = match bbb(op) {
        0 => &IDX_IND,
        1 => &ZERO_PAGE,
        2 => &IMMEDIATE,
        3 => &ABSOLUTE,
        4 => &IND_IDX,
        5 => &ZERO_PAGE_X,
        6 => &ABSOLUTE_Y,
        7 => &ABSOLUTE_X,
        _ => return None,
    };

    let opn = match aaa(op) {
        0 => &ORA,
        1 => &AND,
        2 => &EOR,
        3 => &ADC,
        4 => &STA,
        5 => &LDA,
        6 => &CMP,
        7 => &SBC,
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_gr2(op: Byte) -> Option<Instruction> {
    let mut addr = match bbb(op) {
        0 => &IMMEDIATE,
        1 => &ZERO_PAGE,
        2 => &ACCUMULATOR,
        3 => &ABSOLUTE,
        5 => &ZERO_PAGE_X,
        7 => &ABSOLUTE_X,
        _ => return None,
    };

    let opn = match aaa(op) {
        0 => &ASL,
        1 => &ROL,
        2 => &LSR,
        3 => &ROR,
        4 => &STX,
        5 => &LDX,
        6 => &DEC,
        7 => &INC,
        _ => return None,
    };

    if aaa(op) == 4 || aaa(op) == 5 {
        addr = match bbb(op) {
            5 => &ZERO_PAGE_Y,
            7 => &ABSOLUTE_Y,
            _ => addr,
        };
    }

    Some(Instruction::new(addr, opn))
}

fn decode_gr3(op: Byte) -> Option<Instruction> {
    let mut addr = match bbb(op) {
        0 => &IMMEDIATE,
        1 => &ZERO_PAGE,
        3 => &ABSOLUTE,
        5 => &ZERO_PAGE_X,
        7 => &ABSOLUTE_X,
        _ => return None,
    };

    let opn = match aaa(op) {
        1 => &BIT,
        2 => &JMP,
        3 => {
            if op == 0x6C {
                addr = &ABS_IND
            }
            &JMP
        }
        4 => &STY,
        5 => &LDY,
        6 => &CPY,
        7 => &CPX,
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_branch(op: Byte) -> Option<Instruction> {
    let addr = &RELATIVE;

    let opn = match aaa(op) {
        0 => &BPL,
        1 => &BMI,
        2 => &BVC,
        3 => &BVS,
        4 => &BCC,
        5 => &BCS,
        6 => &BNE,
        7 => &BEQ,
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_sb1(op: Byte) -> Option<Instruction> {
    let addr = &IMPLIED;

    let opn = match hnibble(op) {
        0x0 => &PHP,
        0x1 => &CLC,
        0x2 => &PLP,
        0x3 => &SEC,
        0x4 => &PHA,
        0x5 => &CLI,
        0x6 => &PLA,
        0x7 => &SEI,
        0x8 => &DEY,
        0x9 => &TYA,
        0xA => &TAY,
        0xB => &CLV,
        0xC => &INY,
        0xD => &CLD,
        0xE => &INX,
        0xF => &SED,
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_sb2(op: Byte) -> Option<Instruction> {
    let addr = &IMPLIED;

    let opn = match hnibble(op) {
        0x8 => &TXA,
        0x9 => &TXS,
        0xA => &TAX,
        0xB => &TSX,
        0xC => &DEX,
        0xE => &NOP,
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn decode_sbr_and_int(op: Byte) -> Option<Instruction> {
    let (addr, opn) = match hnibble(op) {
        0x0 => (&NONE, &BRK),
        // JSR is absolute but addressing is interleaved in the operation cycles
        // Thus in this case JSR owns the addressing mode inside its operation
        // This fixed in later version, hence conditionally altering the abs microps is dismissed
        0x2 => (&NONE, &JSR),
        0x4 => (&IMPLIED, &RTI),
        0x6 => (&IMPLIED, &RTS),
        _ => return None,
    };

    Some(Instruction::new(addr, opn))
}

fn set_binarymode_flags(cpu: &mut CPUCore, a: u16, m: u16, result: u16) {
    cpu.flags.set(Status::CARRY, result > 0xFF);
    cpu.flags
        .set(Status::OVERFLOW, (!(a ^ m) & (a ^ result) & 0x80) != 0);

    cpu.flags.set_nz(result as Byte);
}

// Reference: https://forums.atariage.com/topic/163876-flags-on-decimal-mode-on-the-nmos-6502
#[allow(non_snake_case)]
static NMOS_QUIRKS: VariantQuirks = VariantQuirks {
    adc: |cpu, value| {
        // Values in ALU are of 9 bits
        // Represented here using u16
        let carry = cpu.flags.contains(Status::CARRY) as u16;
        let A = cpu.a as u16;
        let M = value as u16;
        let binary_result = A + M + carry;

        if !cpu.flags.contains(Status::DECIMAL) {
            set_binarymode_flags(cpu, A, M, binary_result);
            ALUOuput::Done(binary_result as Byte)
        } else {
            // Z from 8-bit binary sum
            cpu.flags.set(Status::ZERO, (binary_result & 0xFF) == 0);

            // ---- low nibble ----
            let mut lo = (A & 0x0F) + (M & 0x0F) + carry;
            if lo > 9 {
                lo = lo.wrapping_add(6);
            }
            let half_carry = (lo > 0x0F) as u16;

            let pre = (A & 0xF0) + (M & 0xF0) + (half_carry << 4) + (lo & 0x0F);

            // N and V from pre-adjust value
            cpu.flags.set(Status::NEGATIVE, (pre & 0x80) != 0);
            cpu.flags
                .set(Status::OVERFLOW, (!(A ^ M) & (pre ^ A) & 0x80) != 0);

            // ---- final BCD correction ----
            let mut result = pre;
            if result > 0x9F {
                result = result.wrapping_add(0x60);
            }
            cpu.flags.set(Status::CARRY, result > 0xFF);

            ALUOuput::Done(result as Byte)
        }
    },

    sbc: |cpu, value| {
        // Values in ALU are of 9 bits
        // Represented here using u16
        let carry = cpu.flags.contains(Status::CARRY) as u16;
        let A = cpu.a as u16;
        let M = (value ^ 0xFF) as u16;
        let mut result = A + M + carry;

        // SBC flags come from binary result on NMOS
        set_binarymode_flags(cpu, A, M, result);

        if cpu.flags.contains(Status::DECIMAL) {
            let carry_out = result > 0xFF;

            // if a carry propogated to bit 4
            if ((A & 0x0F) + (M & 0x0F) + carry) <= 0x0F {
                result = (result & 0xF0) | ((result + 0x0A) & 0x0F);
            }

            // high digit borrow (independent!)
            if !carry_out {
                result = result.wrapping_add(0xA0);
            }
        }

        ALUOuput::Done(result as u8)
    },

    ind_addr_inc: |addr| {
        let lo = addr & 0x00FF;
        let hi = addr & 0xFF00;

        // Bug: Wrap within page: $12FF → $1200
        ALUOuput::Done(hi | ((lo + 1) & 0x00FF))
    },
};

pub static NMOS_6502: Variant = Variant {
    rules: &[
        DecodeRule {
            matches: |op| lnibble(op) == 0x8,
            decode: decode_sb1,
        },
        DecodeRule {
            matches: |op| lnibble(op) == 0xA,
            decode: decode_sb2,
        },
        DecodeRule {
            matches: |op| (op & 0x1F) == 0x10,
            decode: decode_branch,
        },
        DecodeRule {
            matches: |op| lnibble(op) == 0x0,
            decode: decode_sbr_and_int,
        },
        DecodeRule {
            matches: |op| cc(op) == 0b01,
            decode: decode_gr1,
        },
        DecodeRule {
            matches: |op| cc(op) == 0b10,
            decode: decode_gr2,
        },
        DecodeRule {
            matches: |op| cc(op) == 0b00,
            decode: decode_gr3,
        },
        // If no rule matches including incompatible addr_mode & op is found, then JAM is triggered
        DecodeRule {
            matches: |_| true,
            decode: |_| Some(Instruction::new(&NONE, &JAM)),
        },
    ],
    parent: None,
    quirks: &NMOS_QUIRKS,
};
