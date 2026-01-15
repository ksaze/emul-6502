#[allow(clippy::wildcard_imports)]
use crate::operations::*;
use crate::{
    cpu::{ALUImpl, ALUOuput, CPUCore, Status},
    shared::Byte,
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

pub trait Decoder {
    fn decode(&self, opcode: Byte) -> Option<Instruction>;
}

pub trait ALUVariant {
    fn alu(&self) -> &'static ALUImpl;
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
    pub alu_impl: &'static ALUImpl,
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

impl ALUVariant for Variant {
    fn alu(&self) -> &'static ALUImpl {
        self.alu_impl
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

fn adc_binary(cpu: &mut CPUCore, value: Byte) -> Byte {
    let carry = cpu.flags.contains(Status::CARRY) as Byte;
    let result = cpu.a.wrapping_add(value).wrapping_add(carry);

    cpu.flags.set(Status::CARRY, result < cpu.a);
    cpu.flags.set(
        Status::OVERFLOW,
        (!(cpu.a ^ value) & (cpu.a ^ result) & 0x80) != 0,
    );

    cpu.flags.set_nz(result);
    result
}

// Reference: https://forums.atariage.com/topic/163876-flags-on-decimal-mode-on-the-nmos-6502
static NMOS_ALU: ALUImpl = ALUImpl {
    adc: |cpu, value| {
        if !(cpu.flags.contains(Status::DECIMAL)) {
            ALUOuput::Done(adc_binary(cpu, value))
        } else {
            let carry = cpu.flags.contains(Status::CARRY) as Byte;

            let mut low_nibble = (cpu.a & 0x0F)
                .wrapping_add(value & 0x0F)
                .wrapping_add(carry);

            let half_carry: Byte = if low_nibble > 9 { 1 } else { 0 };

            let mut high_nibble = (cpu.a >> 4)
                .wrapping_add(value >> 4)
                .wrapping_add(half_carry);

            let intermediate = (high_nibble << 4) | low_nibble;

            cpu.flags.set_nz(intermediate);
            cpu.flags.set(
                Status::OVERFLOW,
                (!(cpu.a ^ value) & (cpu.a ^ intermediate) & 0x80) != 0,
            );

            if low_nibble > 9 {
                low_nibble = low_nibble.wrapping_sub(10) & 0x0F;
            };

            cpu.flags.set(Status::CARRY, high_nibble > 9);

            if high_nibble > 9 {
                high_nibble = high_nibble.wrapping_sub(10) & 0x0F;
            }

            let result = (high_nibble << 4) | low_nibble;

            ALUOuput::Done(result)
        }
    },

    sbc: |cpu, value| {
        if !(cpu.flags.contains(Status::DECIMAL)) {
            // SBC = A + ~M + (1 - C)
            cpu.flags.toggle(Status::CARRY);
            ALUOuput::Done(adc_binary(cpu, value ^ 0xFF))
        } else {
            let carry = cpu.flags.contains(Status::CARRY) as Byte;

            let mut low_nibble = (cpu.a & 0x0F)
                .wrapping_sub(value & 0x0F)
                .wrapping_sub(1 - carry);

            let half_borrow: Byte = if low_nibble & 0x10 != 0 { 1 } else { 0 };

            let mut high_nibble = (cpu.a >> 4)
                .wrapping_sub(value >> 4)
                .wrapping_sub(half_borrow);

            let intermediate = (high_nibble << 4) | low_nibble;

            cpu.flags.set_nz(intermediate);
            cpu.flags.set(
                Status::OVERFLOW,
                ((cpu.a ^ value) & (cpu.a ^ intermediate) & 0x80) != 0,
            );

            if (low_nibble & 0x10) != 0 {
                low_nibble = low_nibble.wrapping_add(10) & 0x0F;
            }

            cpu.flags.set(Status::CARRY, (high_nibble & 0x10) != 0);

            if (high_nibble & 0x10) != 0 {
                high_nibble = high_nibble.wrapping_add(10) & 0x0F;
            }

            let result = (high_nibble << 4) | low_nibble;
            cpu.flags.set_nz(result);

            ALUOuput::Done(result)
        }
    },
};

pub static NMOS_6502: Variant = Variant {
    rules: &[
        DecodeRule {
            matches: |op| cc(op) == 0b01,
            decode: decode_gr1,
        },
        DecodeRule {
            matches: |op| cc(op) == 0b10,
            decode: decode_gr2,
        },
    ],
    parent: None,
    alu_impl: &NMOS_ALU,
};
