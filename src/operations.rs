use bitflags::bitflags;

use crate::bus::Bus;
use crate::cpu::{CPUCore, StackPointer};
use crate::shared::{BREAK_FLAG_POS, DECIMAL_FLAG_POS, IRQ_DISABLE_FLAG_POS, Word};

pub type MicroOp = for<'a, 'b> fn(&'a mut CPUCore, &'b mut dyn Bus) -> StepCtl;

#[derive(Copy, Clone)]
pub enum StepCtl {
    Next,
    End,
    Skip,
}

// pub enum AddressingMode {
//     None, // For interrupts
//     Implied,
//     Accumulator,
//     Immediate,
//     ZeroPage,
//     ZeroPageX,
//     ZeroPageY,
//     Absolute,
//     AbsoluteX,
//     AbsoluteY,
//     Indirect,
//     IndirectX,
//     IndirectY,
// }

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct AddressingModeFlag: u16 {
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
        const INDIRECT    = 0b0000_0100_0000_0000;
        const INDIRECT_X  = 0b0000_1000_0000_0000;
        const INDIRECT_Y  = 0b0001_0000_0000_0000;
    }
}

pub struct Operation {
    pub name: &'static str,
    pub valid_modes: AddressingModeFlag,
    pub micro: &'static [MicroOp],
}

pub struct AddressingMode {
    pub name: &'static str,
    pub flag: AddressingModeFlag,
    pub micro: &'static [MicroOp],
}

pub struct Instruction {
    pub name: String,
    addressing: &'static [MicroOp],
    operation: &'static [MicroOp],
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            name: String::default(),
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

pub static NOP: Operation = Operation {
    name: "NOP",
    valid_modes: AddressingModeFlag::IMPLIED,
    micro: &[],
};

/* --- ADDRESSING MODES --- */
pub static NONE: AddressingMode = AddressingMode {
    name: "NONE",
    flag: AddressingModeFlag::NONE,
    micro: &[],
};

pub static IMPLIED: AddressingMode = AddressingMode {
    name: "IMPLIED",
    flag: AddressingModeFlag::IMPLIED,
    micro: &[|_cpu, _bus| StepCtl::End],
};

// Adapted from https://www.pagetable.com/?p=410
pub static RESET: Operation = Operation {
    name: "RESET",
    valid_modes: AddressingModeFlag::NONE,
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
            cpu.set_flag_bit(IRQ_DISABLE_FLAG_POS);
            cpu.clear_flag_bit(DECIMAL_FLAG_POS);

            let lo = bus.read(0xFFFC);
            cpu.tmp8 = lo;
            StepCtl::Next
        },
        // Cycle 7: fetch high byte of RESET vector at $FFFD
        |cpu, bus| {
            cpu.set_flag_bit(BREAK_FLAG_POS);
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
