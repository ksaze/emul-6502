use crate::core::microcode::{Instruction, MicroOp};
use crate::core::variants::{ALUOuput, VariantQuirks};
use crate::shared::{Byte, Word};

use super::{CPUState, Signals, StackPointer, Status};

pub struct CPUCore {
    pub pc: Word,
    pub sp: StackPointer,

    pub a: Byte,
    pub x: Byte,
    pub y: Byte,
    pub flags: Status,

    pub data_bus: Byte,
    pub addr_bus: Word,
    pub rw: bool,

    pub quirks: &'static VariantQuirks,

    pub ir: Byte,
    pub tmp8: Byte,
    pub tmp16: Word,
    pub eff_addr: Word,
    pub crossed: bool,

    pub signals: Signals,

    pub instr: Instruction,
    pub micro_iter: Option<
        std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>,
    >,
    pub micro_op: Option<&'static MicroOp>,
    pub state: CPUState,
}

impl CPUCore {
    #[inline]
    pub fn adc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.quirks.adc)(self, value)
    }

    #[inline]
    pub fn sbc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.quirks.sbc)(self, value)
    }

    #[inline]
    pub fn ind_addr_inc(&self, addr: Word) -> ALUOuput<Word> {
        (self.quirks.ind_addr_inc)(addr)
    }

    #[inline]
    pub fn alu_shl(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, value & 0x80 != 0);

        let result = value << 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_shr(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = value >> 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_rol(&mut self, value: u8) -> u8 {
        let carry_in = self.flags.contains(Status::CARRY) as Byte;
        let carry_out = (value & 0x80) != 0;

        let result = (value << 1) | carry_in;

        self.flags.set(Status::CARRY, carry_out);
        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_ror(&mut self, value: u8) -> u8 {
        let carry_in = (self.flags.contains(Status::CARRY) as Byte) << 7;

        // Carry gets bit 0
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = (value >> 1) | carry_in;

        self.flags.set_nz(result);

        result
    }
}
