use bitflags::bitflags;

use crate::bus::Bus;
use crate::cpu::{CPUCore, StackPointer, Status};
use crate::shared::{Byte, Word};

pub type MicroOp = for<'a, 'b> fn(&'a mut CPUCore, &'b mut dyn Bus) -> StepCtl;

#[derive(Copy, Clone)]
pub enum StepCtl {
    Next,
    End,
    Skip,
    Merge,
    SkipMerge,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct AddressingModeFlag: u16 {
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
    }
}

const G1_MODES: AddressingModeFlag = AddressingModeFlag::from_bits_retain(
    AddressingModeFlag::ACCUMULATOR.bits()
        | AddressingModeFlag::IMMEDIATE.bits()
        | AddressingModeFlag::ZERO_PAGE.bits()
        | AddressingModeFlag::ZERO_PAGE_X.bits()
        | AddressingModeFlag::ABSOLUTE.bits()
        | AddressingModeFlag::ABSOLUTE_X.bits()
        | AddressingModeFlag::ABSOLUTE_Y.bits()
        | AddressingModeFlag::IDX_IND.bits()
        | AddressingModeFlag::IND_IDX.bits(),
);

#[derive(Copy, Clone, PartialEq, Eq)]
enum OperationType {
    Read,
    RMW,
    Store,
    Interrupt,
    Control,
    Implied,
}

pub struct Operation {
    name: &'static str,
    valid_modes: AddressingModeFlag,
    typ: OperationType,
    micro: &'static [MicroOp],
}

pub struct AddressingMode {
    name: &'static str,
    flag: AddressingModeFlag,
    micro: &'static [MicroOp],
}

pub struct Instruction {
    pub name: String,
    typ: OperationType,
    addressing: &'static [MicroOp],
    operation: &'static [MicroOp],
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            name: String::default(),
            typ: OperationType::Implied,
            addressing: &[],
            operation: &[],
        }
    }
}

impl Instruction {
    #[must_use]
    pub fn new(addressing: &'static AddressingMode, operation: &'static Operation) -> Self {
        if !(operation.valid_modes.contains(addressing.flag)) {
            return Instruction {
                name: String::from("NOP"),
                typ: OperationType::Implied,
                addressing: &IMPLIED.micro,
                operation: &NOP.micro,
            };
        }

        let name = if addressing.name == "none" {
            String::from(operation.name)
        } else {
            format!("{}_{}", addressing.name, operation.name)
        };

        Instruction {
            name,
            typ: operation.typ,
            addressing: addressing.micro,
            operation: operation.micro,
        }
    }

    pub fn pipeline(
        &self,
    ) -> std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>
    {
        self.addressing.iter().chain(self.operation.iter())
    }
}

static EMPTY_MICROOP: MicroOp = |_cpu, _bus| StepCtl::Next;

static DUMMY_READ: MicroOp = |_cpu, bus| {
    bus.read(0x00FF);
    StepCtl::Next
};

static READ_LO_BYTE: MicroOp = |cpu, bus| {
    cpu.tmp8 = bus.read(cpu.pc);
    cpu.pc = cpu.pc.wrapping_add(1);
    StepCtl::Next
};

static READ_HIGH_BYTE: MicroOp = |cpu, bus| {
    cpu.tmp16 = ((bus.read(cpu.pc) as Word) << 8) | (cpu.tmp8 as Word);
    cpu.pc = cpu.pc.wrapping_add(1);
    StepCtl::Next
};

pub static NOP: Operation = Operation {
    name: "NOP",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Implied,
    micro: &[],
};

/* --- ADDRESSING MODES --- */
// Resultant Operand stored in tmp8
pub static NONE: AddressingMode = AddressingMode {
    name: "NONE",
    flag: AddressingModeFlag::NONE,
    micro: &[],
};

pub static IMPLIED: AddressingMode = AddressingMode {
    name: "IMPLIED",
    flag: AddressingModeFlag::IMPLIED,
    micro: &[|cpu, bus| {
        bus.read(cpu.pc);
        StepCtl::Merge
    }],
};

pub static ACCUMULATOR: AddressingMode = AddressingMode {
    name: "ACCUMULATOR",
    flag: AddressingModeFlag::ACCUMULATOR,
    micro: &[|cpu, bus| {
        bus.read(cpu.pc);
        cpu.tmp8 = cpu.a;
        StepCtl::Merge
    }],
};

pub static IMMEDIATE: AddressingMode = AddressingMode {
    name: "IMMEDIATE",
    flag: AddressingModeFlag::IMMEDIATE,
    micro: &[|cpu, bus| {
        if cpu.instr.typ == OperationType::Store {
            cpu.tmp16 = cpu.pc;
        } else {
            cpu.tmp8 = bus.read(cpu.pc);
        }
        cpu.pc = cpu.pc.wrapping_add(1);
        StepCtl::Merge
    }],
};

pub static ZERO_PAGE: AddressingMode = AddressingMode {
    name: "ZERO_PAGE",
    flag: AddressingModeFlag::ZERO_PAGE,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            if cpu.instr.typ == OperationType::Store {
                cpu.tmp16 = cpu.tmp8 as Word;
            } else {
                cpu.tmp8 = bus.read(cpu.tmp8 as Word);
            }
            StepCtl::Merge
        }
    ],
};

pub static ZERO_PAGE_X: AddressingMode = AddressingMode {
    name: "ZERO_PAGE_X",
    flag: AddressingModeFlag::ZERO_PAGE_X,
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            bus.read(cpu.tmp8 as Word);
            cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.x);
            StepCtl::Next
        },
        |cpu, bus| {
            if cpu.instr.typ == OperationType::Store {
                cpu.tmp16 = cpu.tmp8 as Word;
            } else {
                cpu.tmp8 = bus.read(cpu.tmp8 as Word);
            }
            StepCtl::Merge
        },
    ],
};

pub static ZERO_PAGE_Y: AddressingMode = AddressingMode {
    name: "ZERO_PAGE_Y",
    flag: AddressingModeFlag::ZERO_PAGE_Y,
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            bus.read(cpu.tmp8 as Word);
            cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.y);
            StepCtl::Next
        },
        |cpu, bus| {
            if cpu.instr.typ == OperationType::Store {
                cpu.tmp16 = cpu.tmp8 as Word 
            } else {
                cpu.tmp8 = bus.read(cpu.tmp8 as Word);
            }
            StepCtl::Merge
        },
    ],
};

pub static ABSOLUTE: AddressingMode = AddressingMode {
    name: "ABSOLUTE",
    flag: AddressingModeFlag::ABSOLUTE,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            if cpu.instr.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

pub static ABSOLUTE_X: AddressingMode = AddressingMode {
    name: "ABSOLUTE_X",
    flag: AddressingModeFlag::ABSOLUTE_X,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            cpu.tmp8 = (cpu.tmp16 >> 8) as Byte;
            cpu.crossed = cpu.tmp8.wrapping_add(cpu.x) < cpu.tmp8;
            cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.x) as Word);

            if cpu.crossed {
                StepCtl::Next
            } else {
                if cpu.instr.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::SkipMerge
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

pub static ABSOLUTE_Y: AddressingMode = AddressingMode {
    name: "ABSOLUTE_Y",
    flag: AddressingModeFlag::ABSOLUTE_Y,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            cpu.tmp8 = (cpu.tmp16 >> 8) as Byte;
            cpu.crossed = cpu.tmp8.wrapping_add(cpu.y) < cpu.tmp8;
            cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.y) as Word);

            if cpu.crossed {
                StepCtl::Next
            } else {
                if cpu.instr.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::Skip
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

pub static IDX_IND: AddressingMode = AddressingMode {
    name: "IDX_IND",
    flag: AddressingModeFlag::IDX_IND,
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            bus.read(cpu.tmp8 as Word);
            cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.x);
            StepCtl::Next
        },
        |cpu, bus| {
            cpu.tmp16 = bus.read(cpu.tmp8 as Word) as Word;
            cpu.tmp8 = cpu.tmp8.wrapping_add(1);
            StepCtl::Next
        },
        |cpu, bus| {
            cpu.tmp16 |= (bus.read(cpu.tmp8 as Word) as Word) << 8;
            StepCtl::Next
        },
        |cpu, bus| {
            if cpu.instr.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

pub static IND_IDX: AddressingMode = AddressingMode {
    name: "IND_IDX",
    flag: AddressingModeFlag::IND_IDX,
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            cpu.tmp16 = bus.read(cpu.tmp8 as Word) as Word;
            cpu.tmp8 = cpu.tmp8.wrapping_add(1);
            StepCtl::Next
        },
        |cpu, bus| {
            cpu.tmp16 |= (bus.read(cpu.tmp8 as Word) as Word) << 8;
            StepCtl::Next
        },
        |cpu, bus| {
            cpu.tmp8 = (cpu.tmp16 >> 8) as Byte;
            cpu.crossed = cpu.tmp8.wrapping_add(cpu.y) < cpu.tmp8;
            cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.y) as Word);

            if cpu.crossed {
                StepCtl::Next
            } else {
                if cpu.instr.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::SkipMerge
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

/* --- Group 1 (ALU) --- */
pub static ORA: Operation = Operation {
    name: "ORA",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a |= cpu.tmp8;
        StepCtl::End
    }],
};

pub static AND: Operation = Operation {
    name: "AND",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a &= cpu.tmp8;
        StepCtl::End
    }],
};

pub static EOR: Operation = Operation {
    name: "EOR",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a ^= cpu.tmp8;
        StepCtl::End
    }],
};

pub static ADC: Operation = Operation {
    name: "ADC",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        let accum_before_op = cpu.a;
        let carry = cpu.flags.contains(Status::CARRY) as Byte;

        cpu.a = cpu.a.wrapping_add(cpu.tmp8.wrapping_add(carry));

        cpu.update_flag(Status::CARRY, accum_before_op > cpu.a);
        cpu.update_flag(Status::ZERO, cpu.a == 0);
        cpu.update_flag(Status::NEGATIVE, cpu.a & 0x80 != 0);

        let overflow = (accum_before_op ^ cpu.a) & (cpu.tmp8 ^ cpu.a) & 0x80 != 0;
        cpu.update_flag(Status::OVERFLOW, overflow);
        StepCtl::End
    }],
};

pub static STA: Operation = Operation {
    name: "STA",
    valid_modes: AddressingModeFlag::from_bits_retain(
        G1_MODES.bits() & !AddressingModeFlag::IMMEDIATE.bits(),
    ),
    typ: OperationType::Store,
    micro: &[|cpu, bus| {
        bus.write(cpu.tmp16, cpu.a);
        StepCtl::End
    }],
};

pub static LDA: Operation = Operation {
    name: "LDA",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a = cpu.tmp8;
        StepCtl::End
    }],
};

pub static CMP: Operation = Operation {
    name: "CMP",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.update_flag(Status::CARRY, cpu.a >= cpu.tmp8);
        cpu.update_flag(Status::ZERO, cpu.a == cpu.tmp8);
        cpu.update_flag(Status::NEGATIVE, cpu.a.wrapping_sub(cpu.tmp8) & 0x80 != 0);
        StepCtl::End
    }],
};

pub static SBC: Operation = Operation {
    name: "SBC",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        let accum_before_op = cpu.a;
        let carry = cpu.flags.contains(Status::CARRY) as Byte;

        cpu.a = cpu.a.wrapping_add((!cpu.tmp8).wrapping_add(carry));

        cpu.update_flag(Status::CARRY, accum_before_op > cpu.a);
        cpu.update_flag(Status::ZERO, cpu.a == 0);
        cpu.update_flag(Status::NEGATIVE, cpu.a & 0x80 != 0);

        let overflow = (accum_before_op ^ cpu.a) & (cpu.tmp8 ^ cpu.a) & 0x80 != 0;
        cpu.update_flag(Status::OVERFLOW, overflow);
        StepCtl::End
    }],
};

/* --- INTERRUPTS --- */
// Adapted from https://www.pagetable.com/?p=410
pub static RESET: Operation = Operation {
    name: "RESET",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Interrupt,
    micro: &[
        // Cycle 0: Initialise sp and ir
        |cpu, bus| {
            cpu.sp = StackPointer(0);
            cpu.ir = 0;
            bus.read(0x00FF);
            StepCtl::Next
        },
        // Internal Cycles
        DUMMY_READ,
        DUMMY_READ,
        // Cycle 3: fake stack push of PCH -> actually a READ from 0x0100+SP
        |cpu, bus| {
            let addr = 0x0100u16.wrapping_add(cpu.sp.to_word());
            bus.read(addr); // discard
            cpu.sp.decrement();
            StepCtl::Next
        },
        // Cycle 4: fake stack push of PCL
        |cpu, bus| {
            let addr = 0x0100u16.wrapping_add(cpu.sp.to_word());
            bus.read(addr);
            cpu.sp.decrement();
            StepCtl::Next
        },
        // Cycle 5: fake stack push of P
        |cpu, bus| {
            let addr = 0x0100u16.wrapping_add(cpu.sp.to_word());
            bus.read(addr);
            cpu.sp.decrement();
            StepCtl::Next
        },
        // Cycle 6: fetch low byte of RESET vector at $FFFC
        // Also disable interrupts and clear decimal flag
        |cpu, bus| {
            cpu.set_flag(Status::IRQ_DISABLE);
            cpu.clear_flag(Status::DECIMAL);

            let lo = bus.read(0xFFFC);
            cpu.tmp8 = lo;
            StepCtl::Next
        },
        // Cycle 7: fetch high byte of RESET vector at $FFFD
        |cpu, bus| {
            cpu.set_flag(Status::BREAK);
            let hi = bus.read(0xFFFD);
            cpu.pc = Word::from_le_bytes([cpu.tmp8, hi]);
            StepCtl::Next
        },
        // Cycle 8: fetch first real opcode into IR
        |cpu, bus| {
            let opcode = bus.read(cpu.pc);
            cpu.ir = opcode;
            cpu.pc = cpu.pc.wrapping_add(1);
            cpu.ready = true; // now we’re ready for the next real instruction
            StepCtl::End
        },
    ],
};
