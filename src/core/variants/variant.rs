use crate::core::microcode::Instruction;

use super::traits::*;

// Decode should only return None to delegate decoding to parent variant
// Base variants should always return some Instruction
pub struct DecodeRule {
    pub(super) matches: fn(u8) -> bool,
    pub(super) decode: fn(u8) -> Option<Instruction>,
}

#[derive(Copy, Clone)]
pub struct Variant {
    pub(super) rules: &'static [DecodeRule],
    pub parent: Option<&'static Variant>,
    pub quirks: &'static VariantQuirks,
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

impl Quirks for Variant {
    fn quirks(&self) -> &'static VariantQuirks {
        self.quirks
    }
}
