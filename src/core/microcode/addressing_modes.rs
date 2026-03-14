use crate::shared::{Byte, Word};
use crate::core::variants::ALUOuput;

use super::instruction::AddressingMode;
use super::types::*;
use super::micro_op::*;

static READ_LO_BYTE: MicroOp = micro_op!(
    (READ pc) 
    |cpu| {
        cpu.tmp8 = cpu.data_bus;
        cpu.pc = cpu.pc.wrapping_add(1);
        StepCtl::Next
    }
);

static READ_HIGH_BYTE: MicroOp = micro_op!(
    (READ pc)
    |cpu| {
        cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
        cpu.pc = cpu.pc.wrapping_add(1);
        StepCtl::Next
    }
);

/* --- ADDRESSING MODES --- */
pub static NONE: AddressingMode = AddressingMode {
    name: "NONE",
    flag: AddressingModeFlag::NONE,
    micro: &[],
};

pub static IMPLIED: AddressingMode = AddressingMode {
    name: "IMPLIED",
    flag: AddressingModeFlag::IMPLIED,
    micro: &[micro_op!(
        (INTERNAL)
        |cpu| {
            // DUMMY READ
            cpu.eff_addr = cpu.pc;
            StepCtl::Merge
        }
    )],
};

pub static ACCUMULATOR: AddressingMode = AddressingMode {
    name: "A",
    flag: AddressingModeFlag::ACCUMULATOR,
    micro: &[micro_op!(
        (INTERNAL)
        |cpu| {
            // DUMMY READ
            cpu.eff_addr = cpu.pc;
            StepCtl::Merge
        }
    )],
};

pub static IMMEDIATE: AddressingMode = AddressingMode {
    name: "#imm",
    flag: AddressingModeFlag::IMMEDIATE,
    micro: &[micro_op!(
        (INTERNAL)
        |cpu| {
            cpu.eff_addr = cpu.pc;
            cpu.pc = cpu.pc.wrapping_add(1);
            StepCtl::Merge
        }
    )],
};

pub static ZERO_PAGE: AddressingMode = AddressingMode {
    name: "zp",
    flag: AddressingModeFlag::ZERO_PAGE,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (INTERNAL)
            |cpu| {
                cpu.eff_addr = cpu.tmp8 as Word;
                StepCtl::Merge
            }
        )
    ],
};

pub static RELATIVE: AddressingMode = AddressingMode {
    name: "rel",
    flag: AddressingModeFlag::RELATIVE,
    micro: &[micro_op!(
        (READ pc)
        |cpu| {
            cpu.tmp8 = cpu.data_bus;
            cpu.pc = cpu.pc.wrapping_add(1);
            StepCtl::Merge
        }
    )],
};

pub static ZERO_PAGE_X: AddressingMode = AddressingMode {
    name: "zp,X",
    flag: AddressingModeFlag::ZERO_PAGE_X,
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ tmp8) // DUMMY
            |cpu| {
                cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.x);
                cpu.eff_addr = cpu.tmp8 as Word;
                StepCtl::Next
            }
        ),
    ],
};

pub static ZERO_PAGE_Y: AddressingMode = AddressingMode {
    name: "zp,Y",
    flag: AddressingModeFlag::ZERO_PAGE_Y,
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ tmp8) // DUMMY
            |cpu| {
                cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.y);
                cpu.eff_addr = cpu.tmp8 as Word;
                StepCtl::Next
            }
        ),
    ],
};

pub static ABSOLUTE: AddressingMode = AddressingMode {
    name: "abs",
    flag: AddressingModeFlag::ABSOLUTE,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.eff_addr = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                cpu.pc = cpu.pc.wrapping_add(1);
                // for JMP 
                if cpu.instr.operation.typ == OperationType::Control {
                    StepCtl::Merge
                } else { 
                    StepCtl::Next 
                }
            }
        ),
    ],
};

pub static ABS_IND: AddressingMode = AddressingMode {
    name: "(abs)",
    flag: AddressingModeFlag::ABS_IND,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        READ_HIGH_BYTE,
        micro_op!(
            (READ tmp16)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                match cpu.ind_addr_inc(cpu.tmp16) {
                    ALUOuput::Done(addr) => {
                        cpu.tmp16 = addr;
                        StepCtl::Skip(1)
                    }
                    ALUOuput::Penalty(addr) => {
                        cpu.tmp16 = addr;
                        StepCtl::Next
                    }
                }
            }
        ),

        micro_op!(
            (READ tmp16) // DUMMY
            |cpu| {
                // Fix page in case of page wrap
                if cpu.tmp16 & 0xFF == 0 {cpu.tmp16 += 1 << 8};
                StepCtl::Next
            }
        ),

        micro_op!(
            (READ tmp16)
            |cpu| {
                cpu.eff_addr = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                StepCtl::Merge
            }
        )
    ],
};

pub static ABSOLUTE_X: AddressingMode = AddressingMode {
    name: "abs,X",
    flag: AddressingModeFlag::ABSOLUTE_X,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                cpu.pc = cpu.pc.wrapping_add(1);

                cpu.tmp8 = (cpu.tmp16 & 0xFF) as Byte;
                cpu.crossed = cpu.tmp8.wrapping_add(cpu.x) < cpu.tmp8;
                cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.x) as Word);

                if !cpu.crossed && cpu.instr.operation.typ == OperationType::Read {
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(2)
                } else {
                    StepCtl::Next
                }
            }
        ),

        micro_op!(
            (READ tmp16) // DUMMY READ
            |cpu| {
                if cpu.crossed || cpu.instr.operation.typ == OperationType::RMW {
                    StepCtl::Next
                } else if !cpu.crossed && cpu.instr.operation.typ == OperationType::Store {
                    // read already done this cycle. Next cycle required for store
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(1)
                } else {
                    StepCtl::SkipMerge
                }
            }
        ),
    
        micro_op!(
            (INTERNAL)
            |cpu| {
                if cpu.crossed {
                    // Fix high byte
                    cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
                    cpu.crossed = false;
                }
                cpu.eff_addr = cpu.tmp16;
                StepCtl::Merge
            }
        )
    ],
};

pub static ABSOLUTE_Y: AddressingMode = AddressingMode {
    name: "abs,Y",
    flag: AddressingModeFlag::ABSOLUTE_Y,
    #[rustfmt::skip]
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                cpu.pc = cpu.pc.wrapping_add(1);

                cpu.tmp8 = (cpu.tmp16 & 0xFF) as Byte;
                cpu.crossed = cpu.tmp8.wrapping_add(cpu.y) < cpu.tmp8;
                cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.y) as Word);

                if !cpu.crossed && cpu.instr.operation.typ == OperationType::Read {
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(2)
                } else {
                    StepCtl::Next
                }
            }
        ),

        micro_op!(
            (READ tmp16) // DUMMY READ
            |cpu| {
                if cpu.crossed || cpu.instr.operation.typ == OperationType::RMW {
                    StepCtl::Next
                } else if !cpu.crossed && cpu.instr.operation.typ == OperationType::Store {
                    // read already done this cycle. Next cycle required for store
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(1)
                } else {
                    StepCtl::SkipMerge
                }
            }
        ),
    
        micro_op!(
            (INTERNAL)
            |cpu| {
                if cpu.crossed {
                    // Fix high byte
                    cpu.tmp16 = cpu.tmp16.wrapping_add(1 << 8);
                    cpu.crossed = false;
                }
                cpu.eff_addr = cpu.tmp16;
                StepCtl::Merge
            }
        )
    ],
};


pub static IDX_IND: AddressingMode = AddressingMode {
    name: "(zp,X)",
    flag: AddressingModeFlag::IDX_IND,
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ tmp8) // DUMMY
            |cpu| {
                cpu.tmp8 = cpu.tmp8.wrapping_add(cpu.x);
                StepCtl::Next
            }
        ),
        micro_op!(
            (READ tmp8)
            |cpu| {
                cpu.tmp16 = cpu.data_bus as Word;
                cpu.tmp8 = cpu.tmp8.wrapping_add(1);
                StepCtl::Next
            }
        ),
        micro_op!(
            (READ tmp8)
            |cpu| {
                cpu.tmp16 |= (cpu.data_bus as Word) << 8;
                cpu.eff_addr= cpu.tmp16;
                StepCtl::Next
            }
        ),
    ],
};

pub static IND_IDX: AddressingMode = AddressingMode {
    name: "(zp),Y",
    flag: AddressingModeFlag::IND_IDX,
    micro: &[
        READ_LO_BYTE,
        micro_op!(
            (READ tmp8)
            |cpu| {
                cpu.tmp16 = cpu.data_bus as Word;
                cpu.tmp8 = cpu.tmp8.wrapping_add(1);
                StepCtl::Next
            }
        ),
        micro_op!(
            (READ tmp8)
            |cpu| {
                cpu.tmp16 |= (cpu.data_bus as Word) << 8;

                cpu.tmp8 = (cpu.tmp16 & 0xFF) as Byte;
                cpu.crossed = cpu.tmp8.wrapping_add(cpu.y) < cpu.tmp8;
                cpu.tmp16 = (cpu.tmp16 & 0xFF00) | (cpu.tmp8.wrapping_add(cpu.y) as Word);

                if !cpu.crossed && cpu.instr.operation.typ == OperationType::Read {
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(2)
                } else {
                    StepCtl::Next
                }
            }
        ),

        micro_op!(
            (READ tmp16) // DUMMY
            |cpu| {
                if cpu.crossed {
                    StepCtl::Next
                } else if !cpu.crossed && cpu.instr.operation.typ == OperationType::Store {
                    // read already done this cycle. Next cycle required for store
                    cpu.eff_addr = cpu.tmp16;
                    StepCtl::Skip(1)
                } else {
                    StepCtl::SkipMerge
                }
            }
        ),
        micro_op!(
            (INTERNAL)
            |cpu| {
                // Fix high byte
                cpu.eff_addr = cpu.tmp16.wrapping_add(1 << 8);
                StepCtl::Merge
            }
        )
    ],
};
