use crate::bus::Bus;
use crate::operations::{Instruction, RESET, StepCtl};
use crate::shared::{Byte, UNUSED_BIT_POS, Word};
use crate::variants::Variant;

pub struct StackPointer(pub Byte);

impl StackPointer {
    #[must_use]
    pub const fn to_word(&self) -> Word {
        let sp_value = self.0;
        Word::from_le_bytes([sp_value, 0x01])
    }

    pub const fn decrement(&mut self) {
        self.0 = self.0.wrapping_sub(1);
    }

    pub const fn increment(&mut self) {
        self.0 = self.0.wrapping_add(1);
    }
}

pub struct CPUCore {
    pub pc: Word,
    pub sp: StackPointer,

    a: Byte,
    x: Byte,
    y: Byte,
    pub flags: Byte,

    pub ir: Byte,
    pub tmp8: Byte,
    pub tmp16: Word,
    pub eff: Word,
    pub crossed: bool,

    pub instr: Instruction,
    pub step: usize,
    pub ready: bool,
}

impl CPUCore {
    pub fn set_flag_bit(&mut self, pos: u8) {
        self.flags |= 1 << pos;
    }

    pub fn clear_flag_bit(&mut self, pos: u8) {
        self.flags &= 1 << pos;
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU<V>
where
    V: Variant,
{
    pub core: CPUCore,
    pub instr_table: V,
}

impl<V: Variant> CPU<V> {
    pub fn new(variant: V) -> Self {
        CPU {
            core: CPUCore {
                pc: 0,
                sp: StackPointer(0),
                a: 0,
                x: 0,
                y: 0,
                flags: (1 << UNUSED_BIT_POS),

                ir: 0,
                tmp8: 0,
                tmp16: 0,
                eff: 0,
                crossed: false,

                instr: Instruction::empty(),
                step: 0,
                ready: true,
            },

            instr_table: variant,
        }
    }

    pub fn reset(&mut self) {
        self.core.instr = Instruction::prepend_with_null(&RESET);
        self.core.step = 0;
        self.core.ready = false;
    }

    pub fn tick(&mut self, bus: &mut dyn Bus) {
        if self.core.ready {
            let opcode = bus.read(self.core.pc);
            self.core.pc = self.core.pc.wrapping_add(1);
            self.core.ir = opcode;
            self.core.instr = self.instr_table.decode(opcode);
            self.core.step = 0;
            self.core.ready = false;
            return;
        }

        if let Some(&micro_op) = self.core.instr.pipeline.next() {
            match micro_op(&mut self.core, bus) {
                StepCtl::Next => {}
                StepCtl::End => {
                    self.core.ready = true;
                }
                StepCtl::Skip => {
                    // Skip the penalty cycle
                    let _ignore = self.core.instr.pipeline.next();
                }
            }
        } else {
            panic!("Instruction sequence ended without StepCtl::End");
        }
    }
}
