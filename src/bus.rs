use crate::shared::{Byte, Word};

pub trait Bus {
    fn read(&self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);
}

const MEM_ADDRESS_LO: Word = 0x0000;
const MEM_ADDRESS_HI: Word = 0xFFFF;
const STACK_ADDRESS_LO: Word = 0x0100;
const STACK_ADDRESS_HI: Word = 0x01FF;

pub struct MemoryBus {
    memory: [Byte; (MEM_ADDRESS_HI as usize) + 1], // 64 KB
}

impl Bus for MemoryBus {
    fn read(&self, addr: Word) -> Byte {
        self.memory[addr as usize]
    }

    fn write(&mut self, addr: Word, val: Byte) {
        self.memory[addr as usize] = val;
    }
}

impl MemoryBus {
    #[must_use]
    #[allow(clippy::large_stack_arrays)]
    pub fn new() -> Self {
        MemoryBus {
            memory: [0; (MEM_ADDRESS_HI as usize) + 1],
        }
    }
}
