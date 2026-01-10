use crate::shared::{Byte, Word};

#[derive(Clone, Copy)]
pub enum BusOp {
    Read(Word),
    Write(Word, Byte),
    Internal,
}

pub trait Bus {
    fn read(&mut self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);
    fn tick(&mut self) -> BusOp;
    fn rdy(&self) -> bool;
}

const MEM_ADDRESS_LO: Word = 0x0000;
const MEM_ADDRESS_HI: Word = 0xFFFF;
const STACK_ADDRESS_LO: Word = 0x0100;
const STACK_ADDRESS_HI: Word = 0x01FF;

pub struct MemoryBus {
    operation: BusOp,
    memory: [Byte; (MEM_ADDRESS_HI as usize) + 1], // 64 KB
}

impl Bus for MemoryBus {
    fn read(&mut self, addr: Word) -> Byte {
        self.operation = BusOp::Read(addr);
        self.memory[addr as usize]
    }

    fn write(&mut self, addr: Word, val: Byte) {
        self.operation = BusOp::Write(addr, val);
        self.memory[addr as usize] = val;
    }

    fn tick(&mut self) -> BusOp {
        let performed = self.operation;
        self.operation = BusOp::Internal; // Reset operation for next cycle
        performed
    }

    fn rdy(&self) -> bool {
        true
    }
}

impl MemoryBus {
    #[must_use]
    #[allow(clippy::large_stack_arrays)]
    pub fn new() -> Self {
        MemoryBus {
            operation: BusOp::Internal,
            memory: [0; (MEM_ADDRESS_HI as usize) + 1],
        }
    }
}
