use crate::operations::{Instruction, NOP_INSTR, Operation};
use crate::shared::Byte;

pub trait Variant {
    fn init_instr_table();
    fn decode(&self, opcode: Byte) -> Instruction;
}

pub struct Nmos6502 {
    operation_table: [Vec<Operation>; 8],
    addr_modes: [Operation; 13],
}

impl Nmos6502 {
    pub fn new() -> Self {
        Nmos6502 {
            operation_table: std::array::from_fn(|_| Vec::new()),
            addr_modes: [NOP_INSTR; 13], // tmp fix
        }
    }
}

impl Variant for Nmos6502 {
    fn init_instr_table() {
        todo!();
    }

    fn decode(&self, _opcode: Byte) -> Instruction {
        Instruction::empty()
    }
}
