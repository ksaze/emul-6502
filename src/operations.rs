use std::iter::Chain;
use std::slice::Iter;

use crate::bus::Bus;
use crate::cpu::{CPUCore, StackPointer};
use crate::shared::{BREAK_FLAG_POS, DECIMAL_FLAG_POS, IRQ_DISABLE_FLAG_POS, Word};

pub type MicroOp = fn(cpu: &mut CPUCore, bus: &mut dyn Bus) -> StepCtl;

#[derive(Copy, Clone)]
pub enum StepCtl {
    Next,
    End,
    Skip,
}

#[derive(Clone, Copy)]
pub struct Operation {
    pub name: &'static str,
    pub micro: &'static [MicroOp],
}

pub struct Instruction {
    pub name: String,
    pub pipeline: Chain<Iter<'static, MicroOp>, Iter<'static, MicroOp>>,
}

pub const EMPTY_MICROOP: &[MicroOp] = &[];
impl Instruction {
    pub fn new(addr_mode: &Operation, operation: &Operation) -> Self {
        Instruction {
            name: format!("{}_{}", addr_mode.name, operation.name),
            pipeline: addr_mode.micro.iter().chain(operation.micro.iter()),
        }
    }

    pub fn prepend_with_null(operation: &Operation) -> Self {
        Instruction {
            name: operation.name.to_owned(),
            pipeline: EMPTY_MICROOP.iter().chain(operation.micro.iter()),
        }
    }

    pub fn empty() -> Self {
        Instruction {
            name: "Empty".to_owned(),
            pipeline: EMPTY_MICROOP.iter().chain(EMPTY_MICROOP.iter()),
        }
    }
}

static DUMMY_READ: MicroOp = |_cpu, bus| {
    bus.read(0x00FF);
    StepCtl::Next
};

pub static NOP_INSTR: Operation = Operation {
    name: "NOP",
    micro: &[|_cpu, _bus| StepCtl::End],
};

// Adapted from https://www.pagetable.com/?p=410
pub static RESET: Operation = Operation {
    name: "RESET",
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
