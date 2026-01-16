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
        0 => &BIT,
        1 => &JMP,
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

fn set_binarymode_flags(cpu: &mut CPUCore, a: u16, m: u16, result: u16) {
    cpu.flags.set(Status::CARRY, result > 0xFF);
    cpu.flags
        .set(Status::OVERFLOW, (!(a ^ m) & (a ^ result) & 0x80) != 0);

    cpu.flags.set_nz(result as Byte);
}

// Reference: https://forums.atariage.com/topic/163876-flags-on-decimal-mode-on-the-nmos-6502
#[allow(non_snake_case)]
static NMOS_ALU: ALUImpl = ALUImpl {
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
                lo += 6;
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
                result += 0x60;
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
        let binary_result = A + M + carry;

        if !cpu.flags.contains(Status::DECIMAL) {
            set_binarymode_flags(cpu, A, M, binary_result);
            ALUOuput::Done(binary_result as Byte)
        } else {
            // Z from 8-bit binary sum
            cpu.flags.set(Status::ZERO, (binary_result & 0xFF) == 0);

            // ---- low nibble ----
            let mut lo = (A & 0x0F) + (M & 0x0F) + carry;
            if lo <= 0x0F {
                lo -= 6;
            }
            let half_carry = (lo > 0x0F) as u16;

            let pre = (A & 0xF0) + (M & 0xF0) + (half_carry << 4) + (lo & 0x0F);

            // N and V from pre-adjust value
            cpu.flags.set(Status::NEGATIVE, (pre & 0x80) != 0);
            cpu.flags
                .set(Status::OVERFLOW, (!(A ^ M) & (pre ^ A) & 0x80) != 0);

            // ---- final BCD correction ----
            let mut result = pre;
            if result <= 0xFF {
                result -= 0x60;
            }
            cpu.flags.set(Status::CARRY, result > 0xFF);
            ALUOuput::Done(result as Byte)
        }
    },

    ind_addr_inc: |addr| {
        let lo = addr & 0x00FF;
        let hi = addr & 0xFF00;

        // Bug: Wrap within page: $12FF → $1200
        ALUOuput::Done(hi | ((lo + 1) & 0x00FF))
    },
};

#[cfg(test)]
mod adc_sbc_tests {
    use super::*;
    use crate::cpu::CPU;

    macro_rules! alu_test {
        (
            $name:ident,
            op = $op:ident,
            a = $a:expr,
            carry = $carry:expr,
            value = $value:expr,
            flags = $flags:expr,
            result = $result:expr
        ) => {
            #[test]
            fn $name() {
                let mut cpu = CPU::new(NMOS_6502);

                cpu.core.a = $a;
                cpu.core.flags |= Status::DECIMAL;
                cpu.core.flags.set(Status::CARRY, $carry);

                let result = match cpu.core.$op($value) {
                    ALUOuput::Done(val) => val,
                    _ => panic!(concat!(stringify!($op), " did not complete")),
                };

                assert_eq!(cpu.core.flags.bits(), $flags, "Flags test failed");
                assert_eq!(result, $result, "Result does not match");
            }
        };
    }

    alu_test!(
        adc_00_00_c0,
        op = adc,
        a = 0x00,
        carry = false,
        value = 0x00,
        flags = 0b00101010,
        result = 0x00
    );

    alu_test!(
        adc_79_00_c1,
        op = adc,
        a = 0x79,
        carry = true,
        value = 0x00,
        flags = 0b11101000,
        result = 0x80
    );

    alu_test!(
        adc_24_56_c0,
        op = adc,
        a = 0x24,
        carry = false,
        value = 0x56,
        flags = 0b11101000,
        result = 0x80
    );

    alu_test!(
        adc_93_82_c0,
        op = adc,
        a = 0x93,
        carry = false,
        value = 0x82,
        flags = 0b01101001,
        result = 0x75
    );

    alu_test!(
        adc_89_76_c0,
        op = adc,
        a = 0x89,
        carry = false,
        value = 0x76,
        flags = 0b00101001,
        result = 0x65
    );

    alu_test!(
        adc_89_76_c1,
        op = adc,
        a = 0x89,
        carry = true,
        value = 0x76,
        flags = 0b00101011,
        result = 0x66
    );

    alu_test!(
        adc_80_f0_c0,
        op = adc,
        a = 0x80,
        carry = false,
        value = 0xF0,
        flags = 0b01101001,
        result = 0xD0
    );

    alu_test!(
        adc_80_fa_c0,
        op = adc,
        a = 0x80,
        carry = false,
        value = 0xFA,
        flags = 0b10101001,
        result = 0xE0
    );

    alu_test!(
        adc_2f_4f_c0,
        op = adc,
        a = 0x2F,
        carry = false,
        value = 0x4F,
        flags = 0b00101000,
        result = 0x74
    );

    alu_test!(
        adc_6f_00_c1,
        op = adc,
        a = 0x6F,
        carry = true,
        value = 0x00,
        flags = 0b00101000,
        result = 0x76
    );

    alu_test!(
        sbc_00_00_c0,
        op = sbc,
        a = 0x00,
        carry = false,
        value = 0x00,
        flags = 0b10101000,
        result = 0x99
    );

    alu_test!(
        sbc_00_00_c1,
        op = sbc,
        a = 0x00,
        carry = true,
        value = 0x00,
        flags = 0b00101011,
        result = 0x00
    );

    alu_test!(
        sbc_00_01_c1,
        op = sbc,
        a = 0x00,
        carry = true,
        value = 0x01,
        flags = 0b10101000,
        result = 0x99
    );

    alu_test!(
        sbc_0a_00_c1,
        op = sbc,
        a = 0x0a,
        carry = true,
        value = 0x0,
        flags = 0b00101001,
        result = 0x0A
    );

    alu_test!(
        sbc_0b_00_c0,
        op = sbc,
        a = 0x0B,
        carry = false,
        value = 0x00,
        flags = 0b00101001,
        result = 0x0A
    );

    alu_test!(
        sbc_9a_00_c1,
        op = sbc,
        a = 0x9A,
        carry = true,
        value = 0x00,
        flags = 0b10101001,
        result = 0x9A
    );

    alu_test!(
        sbc_9b_00_c0,
        op = sbc,
        a = 0x9B,
        carry = false,
        value = 0x00,
        flags = 0b10101001,
        result = 0x9A
    );
}

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
        DecodeRule {
            matches: |op| cc(op) == 0b00,
            decode: decode_gr3,
        },
    ],
    parent: None,
    alu_impl: &NMOS_ALU,
};
