use crate::bus::Bus;
use crate::operations::{Instruction, MicroOp, NONE, RESET, StepCtl};
use crate::shared::{Byte, UNUSED_BIT_POS, Word};
use crate::variants::Decoder;

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
    pub micro_iter: Option<
        std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>,
    >,
    pub ready: bool,
}

impl CPUCore {
    pub fn set_flag_bit(&mut self, pos: u8) {
        self.flags |= 1 << pos;
    }

    pub fn clear_flag_bit(&mut self, pos: u8) {
        self.flags &= !(1 << pos);
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU<V: Decoder>
// where
//    V: Decoder,
{
    pub core: CPUCore,
    pub decoder: V,
}

impl<V: Decoder> CPU<V> {
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

                instr: Instruction::default(),
                micro_iter: None,
                ready: true,
            },

            decoder: variant,
        }
    }

    pub fn reset(&mut self) {
        self.core.instr = Instruction::new(&NONE, &RESET);
        self.core.micro_iter = Some(self.core.instr.pipeline());
        self.core.ready = false;
    }

    pub fn tick(&mut self, bus: &mut dyn Bus) {
        // --- Fetch & Decode Opcode Phase
        if self.core.ready {
            let opcode = bus.read(self.core.pc);
            self.core.pc = self.core.pc.wrapping_add(1);
            self.core.ir = opcode;

            let instr = self.decoder.decode(opcode).unwrap_or_else(|| {
                panic!(
                    "Decode failed for opcode: ${:02X} at PC=${:04X}",
                    opcode,
                    self.core.pc.wrapping_sub(1)
                )
            });

            self.core.instr = instr;
            self.core.micro_iter = Some(self.core.instr.pipeline());
            self.core.ready = false;
            return;
        }

        // --- Execute Micro-op Phase

        // Fetch micro-op
        let micro = {
            let iter = match &mut self.core.micro_iter {
                Some(it) => it,
                None => {
                    self.core.ready = true;
                    return;
                }
            };

            iter.next()
        };

        // If end of iterator, skip to next instruction
        let Some(micro) = micro else {
            self.core.ready = true;
            self.core.micro_iter = None;
            return;
        };

        // Execute micro-op
        match micro(&mut self.core, bus) {
            StepCtl::Next => {}

            StepCtl::End => {
                self.core.ready = true;
                self.core.micro_iter = None;
            }

            StepCtl::Skip => {
                if let Some(iter) = &mut self.core.micro_iter {
                    iter.next(); // skip fake stall micro-op
                }
            }
        }
    }
}
