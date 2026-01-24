use bitflags::{bitflags};

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
                name: format!("{} {}", "NOP", addressing.name),
                addressing: addressing,
                operation: &NOP,
            };
        }
        
        let name = if combine!(AddressingModeFlag::NONE, AddressingModeFlag::IMPLIED, AddressingModeFlag::RELATIVE).contains(addressing.flag) { 
            String::from(operation.name)
        } else {
            format!("{} {}", operation.name, addressing.name)
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
    name: "A",
    flag: AddressingModeFlag::ACCUMULATOR,
    micro: &[|cpu, bus| {
        bus.read(cpu.pc);
        cpu.tmp8 = cpu.a;
        StepCtl::Merge
    }],
};

pub static IMMEDIATE: AddressingMode = AddressingMode {
    name: "#imm",
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
    name: "zp",
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
    name: "offset",
    flag: AddressingModeFlag::RELATIVE,
    micro: &[|cpu, bus| {
        cpu.tmp8 = bus.read(cpu.pc);
        cpu.pc = cpu.pc.wrapping_add(1);
        StepCtl::Merge
    }],
};

pub static ZERO_PAGE_X: AddressingMode = AddressingMode {
    name: "zp,X",
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
    name: "zp,Y",
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
    name: "abs",
    flag: AddressingModeFlag::ABSOLUTE,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        |cpu, bus| {
            cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.pc)]);
            cpu.pc = cpu.pc.wrapping_add(1);
            // for JMP 
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
    name: "(abs)",
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
    name: "abs,X",
    flag: AddressingModeFlag::ABSOLUTE_X,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            cpu.tmp8 = (cpu.tmp16 >> 8) as Byte;
            cpu.crossed = cpu.tmp8.wrapping_add(cpu.x) < cpu.tmp8;
            cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.x) as Word);
            
            if cpu.instr.operation.typ == OperationType::Store {
                // Dummy read in case of Store
                bus.read(cpu.tmp16);
            } else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }

            if cpu.crossed {
                StepCtl::Next
            } else {
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
    name: "abs,Y",
    flag: AddressingModeFlag::ABSOLUTE_Y,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        |cpu, bus| {
            cpu.tmp8 = (cpu.tmp16 >> 8) as Byte;
            cpu.crossed = cpu.tmp8.wrapping_add(cpu.y) < cpu.tmp8;
            cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.y) as Word);

            if cpu.instr.operation.typ == OperationType::Store {
                // Dummy read in case of Store
                bus.read(cpu.tmp16);
            } else {
                cpu.tmp8 = bus.read(cpu.tmp16);
            }

            if cpu.crossed {
                StepCtl::Next
            } else {
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

pub static IDX_IND: AddressingMode = AddressingMode {
    name: "(zp,X)",
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
    name: "(zp),X",
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

            let value = bus.read(cpu.tmp16);
            if cpu.instr.operation.typ != OperationType::Store {
                cpu.tmp8 = value;
            } 

            if cpu.crossed {
                StepCtl::Next
            } else {
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
macro_rules! store {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Store,
            micro: &[|cpu, bus| {
                bus.write(cpu.tmp16, cpu.$register);
                StepCtl::End
            }],
        }
    };
}

macro_rules! load {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Read,
            micro: &[|cpu, _bus| {
                cpu.$register = cpu.tmp8;
                cpu.flags.set_nz(cpu.$register);
                StepCtl::End
            }],
        }
    };
}

macro_rules! compare {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Read,
            micro: &[|cpu, _bus| {
                cpu.flags.set(Status::CARRY, cpu.$register >= cpu.tmp8);
                cpu.flags.set_nz(cpu.$register.wrapping_sub(cpu.tmp8));
                StepCtl::End
            }],
        }
    };
}

macro_rules! alu {
    ($name:literal, $op:tt) => {
        Operation {
            name: $name,
            valid_modes: G1_MODES,
            typ: OperationType::Read,
            micro: &[|cpu, _bus| {
                cpu.a = cpu.a $op cpu.tmp8;
                cpu.flags.set_nz(cpu.a);
                StepCtl::End
            }],
        }
    };
}

pub static ORA: Operation = alu!("ORA", |);
pub static AND: Operation = alu!("AND", &);
pub static EOR: Operation = alu!("EOR", ^);

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

pub static STA: Operation = store!("STA", a, G1_MODES.clear(&[AddressingModeFlag::IMMEDIATE]));

pub static LDA: Operation = load!("LDA", a, G1_MODES);

pub static CMP: Operation = compare!("CMP", a, G1_MODES);

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
macro_rules! alu_rmw {
    // RMW operations with accumulator mode support
    ($name:literal, with_acc, $modify:expr) => {
        alu_rmw!(@impl $name,
             G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE]),
             true,
             $modify)
    };
    
    // RMW operations without accumulator mode (INC/DEC)
    ($name:literal, no_acc, $modify:expr) => {
        alu_rmw!(@impl $name,
             G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ACCUMULATOR]),
             false,
             $modify)
    };
    
    // Internal implementation
    (@impl $name:literal, $modes:expr, $has_acc:expr, $modify:expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::RMW,
            micro: &[
                |cpu, _bus| {
                    if $has_acc && cpu.instr.addressing.flag.contains(AddressingModeFlag::ACCUMULATOR) {
                        cpu.a = { cpu.tmp8 = cpu.a; ($modify)(cpu); cpu.tmp8 };
                        StepCtl::End
                    } else {
                        StepCtl::Next
                    }
                },
                |cpu, bus| {
                    bus.write(cpu.tmp16, cpu.tmp8);
                    ($modify)(cpu);
                    StepCtl::Next
                },
                |cpu, bus| {
                    bus.write(cpu.tmp16, cpu.tmp8);
                    StepCtl::End
                },
            ],
        }
    };
}

pub static ASL: Operation = alu_rmw!("ASL", with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shl(cpu.tmp8)
});

pub static LSR: Operation = alu_rmw!("LSR", with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shr(cpu.tmp8)
});

pub static ROL: Operation = alu_rmw!("ROL", with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_rol(cpu.tmp8)
});

pub static ROR: Operation = alu_rmw!("ROR", with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_ror(cpu.tmp8)
});


pub static STX: Operation = store!("STX", x, combine!(AddressingModeFlag::ZERO_PAGE, AddressingModeFlag::ABSOLUTE, AddressingModeFlag::ZERO_PAGE_Y));

pub static LDX: Operation = load!(
    "LDX", 
    x, 
    combine!(
        AddressingModeFlag::IMMEDIATE,
        AddressingModeFlag::ZERO_PAGE, 
        AddressingModeFlag::ABSOLUTE,
        AddressingModeFlag::ZERO_PAGE_Y,
        AddressingModeFlag::ABSOLUTE_Y
    )
);

// Inc/Dec operations don't support accumulator addressing mode
pub static INC: Operation = alu_rmw!("INC", no_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_add(1);
    cpu.flags.set_nz(cpu.tmp8);
});

pub static DEC: Operation = alu_rmw!("DEC", no_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_sub(1);
    cpu.flags.set_nz(cpu.tmp8);
});

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

pub static STY: Operation = store!("STY", y, G3_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ABSOLUTE_X]));

pub static LDY: Operation = load!("LDY", y, G3_MODES);

pub static CPY: Operation = compare!("CPY", y, G3_MODES.clear(&[AddressingModeFlag::ZERO_PAGE_X, AddressingModeFlag::ZERO_PAGE_Y]));
pub static CPX: Operation = compare!("CPX", x, G3_MODES.clear(&[AddressingModeFlag::ZERO_PAGE_X, AddressingModeFlag::ZERO_PAGE_Y]));

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


/* --- Single Byte --- */
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

/* --- SUBROUTINE --- */
pub static JSR: Operation = Operation {
    name: "JSR abs",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Control,
    micro: &[
        READ_LO_BYTE,
        |_cpu, _bus| {
            // For return address, the address of next instruction - 1 is pushed
            // Buffer ADL
            StepCtl::Next
        },

        |cpu, bus| {
            bus.write(cpu.sp.to_word(), (cpu.pc >> 8) as Byte);
            cpu.sp.decrement();
            StepCtl::Next
        },

        |cpu, bus| {
            bus.write(cpu.sp.to_word(), (cpu.pc & 0xFF) as Byte);
            cpu.sp.decrement();
            StepCtl::Next
        },

        |cpu, bus| {
            cpu.pc = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.pc)]);
            // Merge with instruction fetch
            cpu.ready = true;
            StepCtl::Merge
        },
    ],
};

pub static RTS: Operation = Operation {
    name: "RTS",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Control,
    micro: &[
        // cycle 2: dummy read handled by implied addressing mode already, skip to next cycle
        |_cpu, _bus| StepCtl::Next,

        // cycle 3: increment sp
        |cpu, bus| {
            bus.read(cpu.sp.to_word());
            cpu.sp.increment();
            StepCtl::Next
        },

        // cycle 4: pull PCL
        |cpu, bus| {
            cpu.tmp8 = bus.read(cpu.sp.to_word());
            cpu.sp.increment();
            StepCtl::Next
        },

        // cycle 5: pull PCH
        |cpu, bus| {
            cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.sp.to_word())]);
            StepCtl::Next
        },

        // cycle 6: increment PC to next instruction
        |cpu, bus| {
            cpu.pc = cpu.tmp16; bus.read(cpu.pc); cpu.pc = cpu.pc.wrapping_add(1); StepCtl::End
        }
    ],
};

/* --- INTERRUPTS --- */
// Adapted from https://www.pagetable.com/?p=410
// Goes through the operations of BRK but doesn't perform any actual writes
// Instead bus pin is set to read, resulting in fake stack pushes of PCH, PCL, and P
pub static RESET: Operation = Operation {
    name: "RESET",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Interrupt,
    micro: &[
        |_cpu, bus| {
            bus.read(bus.addr_bus);
            StepCtl::Next
        },
        |_cpu, bus| {
            bus.read(bus.addr_bus.wrapping_add(1));
            StepCtl::Next
        },
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
        |cpu, bus| {
            cpu.flags.insert(Status::IRQ_DISABLE);
            cpu.flags.remove(Status::DECIMAL);
            cpu.flags.insert(Status::UNUSED);

            cpu.tmp8 = bus.read(0xFFFC);
            StepCtl::Next
        },
        // Cycle 7: fetch high byte of RESET vector at $FFFD
        |cpu, bus| {
            cpu.pc = Word::from_le_bytes([cpu.tmp8, bus.read(0xFFFD)]);
            StepCtl::End
        },
        // Cycle 8: fetch first opcode & decode
    ],
};

pub static RTI: Operation = Operation {
    name: "RTI",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Control,
    micro: &[
        // cycle 2: dummy read handled by implied addressing mode already, skip to next cycle
        |_cpu, _bus| StepCtl::Next,

        // cycle 3: increment sp
        |cpu, bus| {
            bus.read(cpu.sp.to_word());
            cpu.sp.increment();
            StepCtl::Next
        },

        |cpu, bus| {
            cpu.flags = Status::from_bits_truncate(bus.read(cpu.sp.to_word()));
            cpu.flags.insert(Status::UNUSED);
            cpu.sp.increment();
            StepCtl::Next
        },

        // cycle 4: pull PCL
        |cpu, bus| {
            cpu.tmp8 = bus.read(cpu.sp.to_word());
            cpu.sp.increment();
            StepCtl::Next
        },

        // cycle 5: pull PCH
        |cpu, bus| {
            cpu.pc = Word::from_le_bytes([cpu.tmp8, bus.read(cpu.sp.to_word())]);
            StepCtl::Next
        },
    ],
};

macro_rules! interrupt {
    (
        $name:literal,
        vector = $vector:expr,
        push_b = $push_b:expr,
        prefetch = $prefetch:expr
    ) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::NONE,
            typ: OperationType::Interrupt,

            micro: &[
                // optional padding fetch (BRK only)
                |cpu, bus| {
                    if $prefetch {
                        bus.read(cpu.pc);
                        cpu.pc = cpu.pc.wrapping_add(1);
                        StepCtl::Next
                    } else {
                        StepCtl::Merge
                    }
                },

                // push PCH
                |cpu, bus| {
                    bus.write(cpu.sp.to_word(), cpu.pc.to_le_bytes()[1]);
                    cpu.sp.decrement();
                    StepCtl::Next
                },

                // push PCL
                |cpu, bus| {
                    bus.write(cpu.sp.to_word(), cpu.pc.to_le_bytes()[0]);
                    cpu.sp.decrement();
                    StepCtl::Next
                },

                // push P
                |cpu, bus| {
                    let mut p = cpu.flags.bits();
                    p |= 0b0010_0000; // bit 5 always set

                    if $push_b {
                        p |= 0b0001_0000;
                    } else {
                        p &= !0b0001_0000;
                    }

                    bus.write(cpu.sp.to_word(), p);
                    cpu.flags.insert(Status::IRQ_DISABLE);
                    StepCtl::Next
                },

                // vector low
                |cpu, bus| {
                    cpu.tmp8 = bus.read($vector);
                    StepCtl::Next
                },

                // vector high
                |cpu, bus| {
                    cpu.pc = Word::from_le_bytes([cpu.tmp8, bus.read($vector + 1)]);
                    StepCtl::End
                },
            ],
        }
    };
}

pub static BRK: Operation = interrupt!(
    "BRK",
    vector = 0xFFFE,
    push_b = true,
    prefetch = true
);

pub static IRQ: Operation = interrupt!(
    "IRQ",
    vector = 0xFFFE,
    push_b = false,
    prefetch = false
);

pub static NMI: Operation = interrupt!(
    "NMI",
    vector = 0xFFFA,
    push_b = false,
    prefetch = false
);
