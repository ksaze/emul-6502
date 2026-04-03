use crate::core::{Byte, Word};

pub struct StackPointer {
    pub value: Byte,
}

impl StackPointer {
    #[must_use]
    pub const fn to_word(&self) -> Word {
        Word::from_le_bytes([self.value, 0x01])
    }

    pub const fn decrement(&mut self) {
        self.value = self.value.wrapping_sub(1);
    }

    pub const fn increment(&mut self) {
        self.value = self.value.wrapping_add(1);
    }
}
