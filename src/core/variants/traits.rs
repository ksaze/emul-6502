use crate::core::cpu::CPUCore;
use crate::core::microcode::Instruction;
use crate::core::{Byte, Word};

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
