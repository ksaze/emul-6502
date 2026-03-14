use bitflags::bitflags;

use crate::shared::Byte;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Status: u8 {
        const CARRY     = 0b0000_0001; // C
        const ZERO      = 0b0000_0010; // Z
        const IRQ_DISABLE = 0b0000_0100; // I
        const DECIMAL   = 0b0000_1000; // D
        // bit 4: B flag is phantom
        const UNUSED   = 0b0010_0000; // always set on 6502
        const OVERFLOW = 0b0100_0000; // V
        const NEGATIVE = 0b1000_0000; // N
    }
}

impl Status {
    #[inline]
    pub fn set_nz(&mut self, value: Byte) {
        self.set(Status::ZERO, value == 0);
        self.set(Status::NEGATIVE, value & 0x80 != 0);
    }
}
