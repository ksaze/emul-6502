use bitflags::bitflags;

use crate::bus::Bus;
use crate::cpu::{ALUOuput, CPUCore, Status};
use crate::shared::{Byte, Word};

pub type MicroOp = for<'a, 'b> fn(&'a mut CPUCore, &'b mut Bus) -> StepCtl;

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
        const ABS_IND     = 0b0001_0000_0000_0000;
        const RELATIVE    = 0b0010_0000_0000_0000;
    }
}

impl AddressingModeFlag {
    const fn combine(flags: &[AddressingModeFlag]) -> AddressingModeFlag {
        let mut bits = 0;
        let mut i = 0;
        while i < flags.len() {
            bits |= flags[i].bits();
            i += 1;
        }
        AddressingModeFlag::from_bits_retain(bits)
    }

    const fn clear(&self, flags: &[AddressingModeFlag]) -> Self {
        let bits = AddressingModeFlag::combine(flags).bits();
        
        AddressingModeFlag::from_bits_retain(self.bits() & !bits)
    }
}

macro_rules! combine {
    ($($flag:path),+ $(,)?) => {
        AddressingModeFlag::combine(&[$($flag),+])
    };
}

const G1_MODES: AddressingModeFlag = combine!(
    AddressingModeFlag::ACCUMULATOR,
    AddressingModeFlag::IMMEDIATE, 
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ABSOLUTE_X,
    AddressingModeFlag::ABSOLUTE_Y,
    AddressingModeFlag::IDX_IND,
    AddressingModeFlag::IND_IDX,
);

const G2_MODES: AddressingModeFlag = combine!(
    AddressingModeFlag::IMMEDIATE,
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ACCUMULATOR,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE_X,
);

const G3_MODES: AddressingModeFlag = combine! (
    AddressingModeFlag::IMMEDIATE,
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE_X,
);

#[derive(Copy, Clone, PartialEq, Eq)]
enum OperationType {
    Read,
    RMW,
    Store,
    Interrupt,
    Control,
    Register,
    Timing,
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
    addressing: &'static AddressingMode,
    operation: &'static Operation,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            name: String::default(),
            addressing: &NONE,
            operation: &NOP,
        }
    }
}

impl Instruction {
    #[must_use]
    pub fn new(addressing: &'static AddressingMode, operation: &'static Operation) -> Self {
        if !(operation.valid_modes.contains(addressing.flag)) {
            return Instruction {
                name: String::from("NOP"),
                addressing: &IMPLIED,
                operation: &NOP,
            };
        }

        let name = if addressing.name == "none" {
            String::from(operation.name)
        } else {
            format!("{}_{}", addressing.name, operation.name)
        };

        Instruction {
            name,
            addressing: addressing,
            operation: operation,
        }
    }

    pub fn pipeline(
        &self,
    ) -> std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>
    {
        self.addressing.micro.iter().chain(self.operation.micro.iter())
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
    cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.pc)]);
    cpu.pc = cpu.pc.wrapping_add(1);
    StepCtl::Next
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
        if cpu.instr.operation.typ == OperationType::Store {
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
            // Required for RMW instructions
            cpu.tmp16 = cpu.tmp8 as Word;
            if cpu.instr.operation.typ == OperationType::Store {}
            else {
                cpu.tmp8 = bus.read(cpu.tmp8 as Word);
            }
            StepCtl::Merge
        }
    ],
};

pub static RELATIVE: AddressingMode = AddressingMode {
    name: "RELATIVE",
    flag: AddressingModeFlag::RELATIVE,
    micro: &[|cpu, bus| {
        cpu.tmp8 = bus.read(cpu.pc);
        cpu.pc = cpu.pc.wrapping_add(1);
        StepCtl::Merge
    }],
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
            // Required for RMW instructions
            cpu.tmp16 = cpu.tmp8 as Word;
            if cpu.instr.operation.typ == OperationType::Store {}
            else {
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
            // Required for RMW instructions
            cpu.tmp16 = cpu.tmp8 as Word;
            if cpu.instr.operation.typ == OperationType::Store {}
            else {
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
        |cpu, bus| {
            cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.pc)]);
            cpu.pc = cpu.pc.wrapping_add(1);
            if cpu.instr.operation.typ == OperationType::Control {
                StepCtl::SkipMerge
            } else { StepCtl::Next}
        },

        |cpu, bus| {
            if cpu.instr.operation.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

pub static ABS_IND: AddressingMode = AddressingMode {
    name: "ABS_IND",
    flag: AddressingModeFlag::ABS_IND,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            cpu.tmp8 = bus.read(cpu.tmp16);
            match cpu.ind_addr_inc(cpu.tmp16) {
                ALUOuput::Done(addr) => {
                    cpu.tmp16 = addr;
                    StepCtl::Skip
                }
                ALUOuput::Penalty(addr) => {
                    cpu.tmp16 = addr;
                    StepCtl::Next
                }
            }
        },
        |cpu, bus| {
            bus.read(cpu.tmp16);
            // Fix page in case of page wrap
            if cpu.tmp16 & 0xFF == 0 {cpu.tmp16 += 1 << 8};
            StepCtl::Next
        },
        |cpu, bus| {
            cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.tmp16)]);
            StepCtl::Merge
        }
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
                if cpu.instr.operation.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::SkipMerge
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.operation.typ == OperationType::Store {} 
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
                if cpu.instr.operation.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::Skip
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.operation.typ == OperationType::Store {} 
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
            if cpu.instr.operation.typ == OperationType::Store {} 
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
                if cpu.instr.operation.typ == OperationType::Store {} 
                else {
                    cpu.tmp8 = bus.read(cpu.tmp16);
                }
                StepCtl::SkipMerge
            }
        },
        |cpu, bus| {
            // Fix high byte
            cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
            if cpu.instr.operation.typ == OperationType::Store {} 
            else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }
            StepCtl::Merge
        },
    ],
};

/* --- Misc Operation --- */
pub static NOP: Operation = Operation {
    name: "NOP",
        valid_modes: combine!(
            AddressingModeFlag::IMPLIED,
            AddressingModeFlag::IMMEDIATE,
            AddressingModeFlag::ZERO_PAGE, 
            AddressingModeFlag::ZERO_PAGE_X, 
            AddressingModeFlag::ABSOLUTE, 
            AddressingModeFlag::ABSOLUTE_X),
    typ: OperationType::Timing,
    micro: &[],
};

/* --- Group 1  --- */
pub static ORA: Operation = Operation {
    name: "ORA",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a |= cpu.tmp8;
        cpu.flags.set_nz(cpu.a);
        StepCtl::End
    }],
};

pub static AND: Operation = Operation {
    name: "AND",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a &= cpu.tmp8;
        cpu.flags.set_nz(cpu.a);
        StepCtl::End
    }],
};

pub static EOR: Operation = Operation {
    name: "EOR",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.a ^= cpu.tmp8;
        cpu.flags.set_nz(cpu.a);
        StepCtl::End
    }],
};

pub static ADC: Operation = Operation {
    name: "ADC",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[
        |cpu, _bus| {
            match cpu.adc(cpu.tmp8) {
                ALUOuput::Done(value) => {
                    cpu.a = value;
                    StepCtl::End
                }
                ALUOuput::Penalty(value) => {
                    cpu.tmp8 = value;
                    StepCtl::Next
                }
            }
         },

        |_cpu, _bus| {
            todo!()
        },
    ],
};

pub static STA: Operation = Operation {
    name: "STA",
    valid_modes: G1_MODES.clear(&[AddressingModeFlag::IMMEDIATE]),
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
        cpu.flags.set_nz(cpu.a);
        StepCtl::End
    }],
};

pub static CMP: Operation = Operation {
    name: "CMP",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.flags.set(Status::CARRY, cpu.a >= cpu.tmp8);
        cpu.flags.set_nz(cpu.a.wrapping_sub(cpu.tmp8));
        StepCtl::End
    }],
};

pub static SBC: Operation = Operation {
    name: "SBC",
    valid_modes: G1_MODES,
    typ: OperationType::Read,
    micro: &[
        |cpu, _bus| {
            match cpu.sbc(cpu.tmp8) {
                ALUOuput::Done(value) => {
                    cpu.a = value;
                    StepCtl::End
                }
                ALUOuput::Penalty(value) => {
                    cpu.tmp8 = value;
                    StepCtl::Next
                }
            }
         },

        |_cpu, _bus| {
            todo!()
        },
    ],
};

/* --- Group 2 --- */
macro_rules! shift {
    ($name:literal, $alu_fn:ident) => {
        Operation {
            name: $name,
            valid_modes: G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE]),
            typ: OperationType::RMW,
            micro: &[
                // Accumulator form
                |cpu, _bus| {
                    if cpu.instr.addressing.flag.contains(AddressingModeFlag::ACCUMULATOR) {
                        cpu.a = cpu.$alu_fn(cpu.a);
                        StepCtl::End
                    } else {
                        StepCtl::Next
                    }
                },

                // Dummy write + modify
                |cpu, bus| {
                    bus.write(cpu.tmp16, cpu.tmp8);
                    cpu.tmp8 = cpu.$alu_fn(cpu.tmp8);
                    StepCtl::Next
                },

                // Final writeback
                |cpu, bus| {
                    bus.write(cpu.tmp16, cpu.tmp8);
                    StepCtl::End
                },
            ],
        }
    };
}

pub static ASL: Operation = shift!("ASL", alu_shl);
pub static LSR: Operation = shift!("LSR", alu_shr);
pub static ROL: Operation = shift!("ROL", alu_rol);
pub static ROR: Operation = shift!("ROR", alu_ror);

pub static STX: Operation = Operation {
    name: "STX",
    valid_modes: combine!(AddressingModeFlag::ZERO_PAGE, AddressingModeFlag::ABSOLUTE, AddressingModeFlag::ZERO_PAGE_Y),
    typ: OperationType::Store,
    micro: &[|cpu, bus| {
        bus.write(cpu.tmp16, cpu.x);
        StepCtl::End
    }],
};


pub static LDX: Operation = Operation {
    name: "LDX",
    valid_modes: combine!(
        AddressingModeFlag::IMMEDIATE,
        AddressingModeFlag::ZERO_PAGE,
        AddressingModeFlag::ABSOLUTE,
        AddressingModeFlag::ZERO_PAGE_Y,
        AddressingModeFlag::ABSOLUTE_Y),
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.x = cpu.tmp8;
        cpu.flags.set_nz(cpu.x);
        StepCtl::End
    }],
};

pub static INC: Operation = Operation {
    name: "INC",
    valid_modes: G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ACCUMULATOR]),
    typ: OperationType::RMW,
    micro: &[
        |_cpu, _bus| {
            StepCtl::Next
        },

        // Dummy write + modify
        |cpu, bus| {
            bus.write(cpu.tmp16, cpu.tmp8);
            cpu.tmp8 = cpu.tmp8.wrapping_add(1);
            cpu.flags.set_nz(cpu.tmp8);
            StepCtl::Next
        },

        // Final writeback
        |cpu, bus| {
            bus.write(cpu.tmp16, cpu.tmp8);
            StepCtl::End
        },
    ],
};


pub static DEC: Operation = Operation {
    name: "DEC",
    valid_modes: G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ACCUMULATOR]),
    typ: OperationType::RMW,
    micro: &[
        |_cpu, _bus| {
            StepCtl::Next
        },

        // Dummy write + modify
        |cpu, bus| {
            bus.write(cpu.tmp16, cpu.tmp8);
            cpu.tmp8 = cpu.tmp8.wrapping_sub(1);
            cpu.flags.set_nz(cpu.tmp8);
            StepCtl::Next
        },

        // Final writeback
        |cpu, bus| {
            bus.write(cpu.tmp16, cpu.tmp8);
            StepCtl::End
        },
    ],
};

/* --- Group 3 --- */
pub static BIT: Operation = Operation {
    name: "BIT",
    valid_modes: combine!(AddressingModeFlag::ZERO_PAGE, AddressingModeFlag::ABSOLUTE),
    typ: OperationType::Read,
    micro: &[
        |cpu, _bus| {
            cpu.flags.set_nz(cpu.a & cpu.tmp8);
            // V Flag => Copy bit 6 from memory
            cpu.flags.set(Status::OVERFLOW, cpu.tmp8 & 0x40 != 0);
            StepCtl::End
        }
    ],
};

pub static JMP: Operation = Operation {
    name: "JMP",
    valid_modes: combine!(AddressingModeFlag::ABSOLUTE, AddressingModeFlag::ABS_IND),
    typ: OperationType::Control,
    micro: &[
        |cpu, _bus| {
            cpu.pc = cpu.tmp16;
            StepCtl::End
        }
    ]
};


pub static STY: Operation = Operation {
    name: "STY",
    valid_modes: G3_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ABSOLUTE_X]),
    typ: OperationType::Store,
    micro: &[|cpu, bus| {
        bus.write(cpu.tmp16, cpu.y);
        StepCtl::End
    }],
};


pub static LDY: Operation = Operation {
    name: "LDY",
    valid_modes: G3_MODES,
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.y = cpu.tmp8;
        cpu.flags.set_nz(cpu.y);
        StepCtl::End
    }],
};


pub static CPY: Operation = Operation {
    name: "CPY",
    valid_modes: G3_MODES.clear(&[AddressingModeFlag::ZERO_PAGE_X, AddressingModeFlag::ZERO_PAGE_Y]),
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.flags.set(Status::CARRY, cpu.y >= cpu.tmp8);
        cpu.flags.set_nz(cpu.y.wrapping_sub(cpu.tmp8));
        StepCtl::End
    }],
};

pub static CPX: Operation = Operation {
    name: "CPX",
    valid_modes: G3_MODES.clear(&[AddressingModeFlag::ZERO_PAGE_X, AddressingModeFlag::ZERO_PAGE_Y]),
    typ: OperationType::Read,
    micro: &[|cpu, _bus| {
        cpu.flags.set(Status::CARRY, cpu.x >= cpu.tmp8);
        cpu.flags.set_nz(cpu.x.wrapping_sub(cpu.tmp8));
        StepCtl::End
    }],
};

/* --- Branch Instructinos --- */
macro_rules! branch {
    ($name:literal, $flag:path, $flag_value:literal) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::RELATIVE,
            typ: OperationType::Control,
            micro: &[
                |cpu, _bus| {
                    if cpu.flags.contains($flag) == $flag_value {
                        StepCtl::Next
                    } else {
                        StepCtl::End
                    }
                },

                |cpu, bus| {
                    bus.read(cpu.pc);
                    let old_pcl = cpu.pc as u8;
                    let new_pcl = old_pcl.wrapping_add(cpu.tmp8);
                    cpu.pc = (cpu.pc & 0xFF00) | new_pcl as Word;

                    let offset_is_negative = cpu.tmp8 & 0x80 != 0;
                    let page_wrap = if offset_is_negative {
                        cpu.tmp8 = 0xFF;
                        new_pcl > old_pcl
                    } else {
                        cpu.tmp8 = 0x1;
                        old_pcl > new_pcl
                    };
                    
                    if page_wrap {
                        StepCtl::Next
                    } else {
                        StepCtl::Skip
                    }
                },

                // page correction cycle
                // cpu.tmp8 holds carry value
                |cpu, bus| {
                    cpu.pc = cpu.pc.wrapping_add((cpu.tmp8 as Word) << 8);
                    bus.read(cpu.pc);
                    StepCtl::End
                }
            ]
        }
    };
}

pub static BEQ: Operation = branch!("BEQ", Status::ZERO, true);
pub static BNE: Operation = branch!("BNE", Status::ZERO, false);

pub static BCS: Operation = branch!("BCS", Status::CARRY, true);
pub static BCC: Operation = branch!("BCC", Status::CARRY, false);

pub static BMI: Operation = branch!("BMI", Status::NEGATIVE, true);
pub static BPL: Operation = branch!("BPL", Status::NEGATIVE, false);

pub static BVS: Operation = branch!("BVS", Status::OVERFLOW, true);
pub static BVC: Operation = branch!("BVC", Status::OVERFLOW, false);

macro_rules! reg_set {
    // PUBLIC INTERFACES
    ($name:literal, $to:ident <- sp) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.sp.value)
    };

    ($name:literal, $to:ident <- $from:ident) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$from)
    };


    ($name:literal, $to:ident <- $r:ident + 1) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$r.wrapping_add(1))
    };

    ($name:literal, $to:ident <- $r:ident - 1) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$r.wrapping_sub(1))
    };

    // INTERNAL IMPLEMENTATION
    (@impl $name:literal, $to:ident, $rhs:expr) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                |cpu, _bus| {
                    cpu.$to = ($rhs)(cpu);
                    cpu.flags.set_nz(cpu.$to);
                    StepCtl::End
                }
            ]
        }
    };
}


macro_rules! flag {
    // INTERNAL IMPLEMENTATION
    (@impl $name:literal, $op:ident, $flag:ident) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                |cpu, _bus| {
                    cpu.flags.$op(Status::$flag);
                    StepCtl::End
                }
            ]
        }
    };

    // PUBLIC INTERFACES 
    ($name:literal, set $flag:ident) => {
        flag!(@impl $name, insert, $flag)
    };

    ($name:literal, clear $flag:ident) => {
        flag!(@impl $name, remove, $flag)
    };
}

macro_rules! stack {
    // PUSH INTERFACE
    ($name:literal, push a) => {
        stack!(@push $name, |cpu: &mut CPUCore| cpu.a)
    };

    ($name:literal, push p) => {
        // Set UNUSED & BREAK in pushed status byte
        stack!(@push $name, |cpu: &mut CPUCore| cpu.flags.bits() | 0x30)
    };

    // PULL INTERFACE
    ($name:literal, pull a) => {
        stack!(@pull $name, |cpu: &mut CPUCore, v: Byte| {
            cpu.a = v;
            cpu.flags.set_nz(cpu.a);
        })
    };

    ($name:literal, pull p) => {
        stack!(@pull $name, |cpu: &mut CPUCore, v: Byte| {
            cpu.flags = Status::from_bits_retain(v);
            cpu.flags.insert(Status::UNUSED);
        })
    };

    // INTERNAL PUSH IMPLEMENTATION
    (@push $name:literal, $value:expr) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                // cycle 1: dummy read (opcode fetch already done)
                |_, _| StepCtl::Next,

                // cycle 2: write to stack
                |cpu, bus| {
                    let v: Byte = ($value)(cpu);
                    bus.write(cpu.sp.to_word(), v);
                    cpu.sp.decrement();
                    StepCtl::End
                }
            ]
        }
    };

    // INTERNAL PULL IMPLEMENTATION
    (@pull $name:literal, $assign:expr) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                // cycle 1: dummy read
                |_cpu, _bus| StepCtl::Next,

                // cycle 2: increment SP
                |cpu, bus| {
                    bus.read(cpu.sp.to_word());
                    cpu.sp.increment();
                    StepCtl::Next
                },

                // cycle 3: read from stack
                |cpu, bus| {
                    let v = bus.read(cpu.sp.to_word());
                    ($assign)(cpu, v);
                    StepCtl::End
                }
            ]
        }
    };
}

pub static PHA: Operation = stack!("PHA", push a);
pub static PHP: Operation = stack!("PHP", push p);
pub static PLA: Operation = stack!("PLA", pull a);
pub static PLP: Operation = stack!("PLP", pull p);

pub static CLC: Operation = flag!("CLC", clear CARRY);
pub static SEC: Operation = flag!("SEC", set CARRY);
pub static CLI: Operation = flag!("CLI", clear IRQ_DISABLE);
pub static SEI: Operation = flag!("SEI", set IRQ_DISABLE);
pub static CLV: Operation = flag!("CLV", clear OVERFLOW);
pub static CLD: Operation = flag!("CLD", clear DECIMAL);
pub static SED: Operation = flag!("SED", set DECIMAL);

pub static DEY: Operation = reg_set!("DEY", y <- y-1);
pub static INY: Operation = reg_set!("INY", y <- y+1);
pub static INX: Operation = reg_set!("INX", x <- x+1);
pub static DEX: Operation = reg_set!("DEX", x <- x-1);
pub static TAY: Operation = reg_set!("TAY", y <- a);
pub static TYA: Operation = reg_set!("TYA", y <- a);
pub static TXA: Operation = reg_set!("TXA", a <- x);
pub static TAX: Operation = reg_set!("TAX", x <- a);
pub static TSX: Operation = reg_set!("TSX", x <- sp);

// reg_set! macro isn't used because TXS doesn't set NZ flags like others
pub static TXS: Operation = Operation {
    name: "TXS",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Register,
    micro: &[
        |cpu, _bus| {
            cpu.sp.value = cpu.x;
            StepCtl::End
        } 
    ]
};

/* --- INTERRUPTS --- */
// Adapted from https://www.pagetable.com/?p=410
pub static RESET: Operation = Operation {
    name: "RESET",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Interrupt,
    micro: &[
        // Cycle 1 & 2: Internal Cycles
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
            cpu.flags.set(Status::IRQ_DISABLE, true);

            let lo = bus.read(0xFFFC);
            cpu.tmp8 = lo;
            StepCtl::Next
        },
        // Cycle 7: fetch high byte of RESET vector at $FFFD
        |cpu, bus| {
            let hi = bus.read(0xFFFD);
            cpu.pc = Word::from_le_bytes([cpu.tmp8, hi]);
            StepCtl::End
        },
        // Cycle 8: fetch first opcode & decode
    ],
};
