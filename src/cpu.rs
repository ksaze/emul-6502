use crate::bus::Bus;
use crate::operations::{Instruction, MicroOp, NONE, RESET, StepCtl};
use crate::shared::{Byte, Word};
use crate::variants::{ALUVariant, Decoder};

use bitflags::bitflags;

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
        self.set(Status::NEGATIVE, value & 0x80 != 9);
    }
}

pub enum ALUOuput<T> {
    Done(T),
    Penalty(T),
}

pub struct ALUImpl {
    pub adc: fn(&mut CPUCore, Byte) -> ALUOuput<Byte>,
    pub sbc: fn(&mut CPUCore, Byte) -> ALUOuput<Byte>,
    pub ind_addr_inc: fn(Word) -> ALUOuput<Word>,
}

pub struct CPUCore {
    pub pc: Word,
    pub sp: StackPointer,

    pub a: Byte,
    pub x: Byte,
    pub y: Byte,
    pub flags: Status,

    pub alu: &'static ALUImpl,

    pub ir: Byte,
    pub tmp8: Byte,
    pub tmp16: Word,
    pub crossed: bool,

    pub instr: Instruction,
    pub micro_iter: Option<
        std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>,
    >,
    pub ready: bool,
}

impl CPUCore {
    #[inline]
    pub fn adc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.alu.adc)(self, value)
    }

    #[inline]
    pub fn sbc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.alu.sbc)(self, value)
    }

    #[inline]
    pub fn ind_addr_inc(&self, addr: Word) -> ALUOuput<Word> {
        (self.alu.ind_addr_inc)(addr)
    }

    #[inline]
    pub fn alu_shl(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, value & 0x80 != 0);

        let result = value << 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_shr(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = value >> 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_rol(&mut self, value: u8) -> u8 {
        let carry_in = self.flags.contains(Status::CARRY) as Byte;
        let carry_out = (value & 0x80) != 0;

        let result = (value << 1) | carry_in;

        self.flags.set(Status::CARRY, carry_out);
        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_ror(&mut self, value: u8) -> u8 {
        let carry_in = (self.flags.contains(Status::CARRY) as Byte) << 7;

        // Carry gets bit 0
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = (value >> 1) | carry_in;

        self.flags.set_nz(result);

        result
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU<V: Decoder + ALUVariant> {
    pub core: CPUCore,
    pub decoder: V,
}

impl<V: Decoder + ALUVariant> CPU<V> {
    pub fn new(variant: V) -> Self {
        CPU {
            core: CPUCore {
                pc: 0,
                sp: StackPointer { value: 0 },
                a: 0,
                x: 0,
                y: 0,
                flags: Status::UNUSED,

                alu: variant.alu(),

                ir: 0,
                tmp8: 0,
                tmp16: 0,
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

    pub fn tick(&mut self, bus: &mut Bus) {
        if !bus.rdy() {
            return;
        }

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
        loop {
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
                StepCtl::Next => {
                    break;
                }

                StepCtl::End => {
                    self.core.ready = true;
                    self.core.micro_iter = None;
                    break;
                }

                StepCtl::Skip => {
                    if let Some(iter) = &mut self.core.micro_iter {
                        iter.next(); // skip fake stall micro-op
                    }
                    break;
                }

                StepCtl::Merge => {
                    continue;
                }

                StepCtl::SkipMerge => {
                    if let Some(iter) = &mut self.core.micro_iter {
                        iter.next(); // skip fake stall micro-op
                    }
                    continue;
                }
            }
        }
    }
}
