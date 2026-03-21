use bitflags::bitflags;

use crate::core::cpu::CPUCore;
use crate::shared::{Byte, Word};

#[derive(Copy, Clone)]
pub enum StepCtl {
    Next,
    End,
    Skip,
    Merge,
}

#[derive(Copy, Clone, Debug)]
pub enum BusOpSpec {
    Read {
        addr: for<'a> fn(&'a mut CPUCore) -> Word,
    },
    Write {
        addr: for<'a> fn(&'a mut CPUCore) -> Word,
        data: for<'a> fn(&'a mut CPUCore) -> Byte,
    },
    Internal,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum OperationType {
    Read,
    RMW,
    Store,
    Interrupt,
    Control,
    Register,
    Timing,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct AddressingModeFlag: u16 {
        const NONE        = 0b0000_0000_0000_0001;
        const IMPLIED     = 0b0000_0000_0000_0010;
        const ACCUMULATOR = 0b0000_0000_0000_0100;
        const IMMEDIATE   = 0b0000_0000_0000_1000;
        const ZERO_PAGE   = 0b0000_0000_0001_0000;
        const ZERO_PAGE_X = 0b0000_0000_0010_0000;
        const ZERO_PAGE_Y = 0b0000_0000_0100_0000;
        const ABSOLUTE    = 0b0000_0000_1000_0000;
        const ABSOLUTE_X  = 0b0000_0001_0000_0000;
        const ABSOLUTE_Y  = 0b0000_0010_0000_0000;
        const IDX_IND     = 0b0000_0100_0000_0000;
        const IND_IDX     = 0b0000_1000_0000_0000;
        const ABS_IND     = 0b0001_0000_0000_0000;
        const RELATIVE    = 0b0010_0000_0000_0000;
    }
}

impl AddressingModeFlag {
    pub const fn combine(flags: &[AddressingModeFlag]) -> AddressingModeFlag {
        let mut bits = 0;
        let mut i = 0;
        while i < flags.len() {
            bits |= flags[i].bits();
            i += 1;
        }
        AddressingModeFlag::from_bits_retain(bits)
    }

    pub const fn clear(&self, flags: &[AddressingModeFlag]) -> Self {
        let bits = AddressingModeFlag::combine(flags).bits();

        AddressingModeFlag::from_bits_retain(self.bits() & !bits)
    }
}

macro_rules! combine {
    ($($flag:path),+ $(,)?) => {
        AddressingModeFlag::combine(&[$($flag),+])
    };
}

pub(super) use combine;
