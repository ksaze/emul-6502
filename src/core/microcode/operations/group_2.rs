use super::prelude::*;
use super::shared_macros::{load, store};

const G2_MODES: AddressingModeFlag = combine!(
    AddressingModeFlag::IMMEDIATE,
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ACCUMULATOR,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE_X,
);

macro_rules! alu_rmw {
    // RMW operations with accumulator mode support
    ($name:literal, G2_with_acc, $modify:expr) => {
        alu_rmw!(@impl $name,
             G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE]),
             true,
             $modify)
    };

    // RMW operations without accumulator mode (INC/DEC)
    ($name:literal, G2_no_acc, $modify:expr) => {
        alu_rmw!(@impl $name,
             G2_MODES.clear(&[AddressingModeFlag::IMMEDIATE, AddressingModeFlag::ACCUMULATOR]),
             false,
             $modify)
    };

    // Undocumented operation combining RMW operations
    ($name:literal, G1, $modify:expr) => {
        alu_rmw!(@impl $name,
             G1_MODES.clear(&[AddressingModeFlag::IMMEDIATE]),
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
                micro_op!(
                    (READ eff_addr)
                    |cpu| {
                        cpu.tmp8 = cpu.data_bus;
                        if $has_acc && cpu.instr.addressing.flag.contains(AddressingModeFlag::ACCUMULATOR) {
                            cpu.a = { cpu.tmp8 = cpu.a; ($modify)(cpu); cpu.tmp8 };
                            StepCtl::End
                        } else {
                            StepCtl::Next
                        }
                    }
                ),

                micro_op!(
                    (WRITE tmp8 -> eff_addr)
                    |cpu| {
                        ($modify)(cpu);
                        StepCtl::Next
                    }
                ),

                micro_op!(
                    (WRITE tmp8 -> eff_addr)
                    |_cpu| {
                        StepCtl::End
                    }
                )
            ],
        }
    };
}

pub(super) use alu_rmw;

pub static ASL: Operation = alu_rmw!("ASL", G2_with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shl(cpu.tmp8)
});

pub static LSR: Operation = alu_rmw!("LSR", G2_with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shr(cpu.tmp8)
});

pub static ROL: Operation = alu_rmw!("ROL", G2_with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_rol(cpu.tmp8)
});

pub static ROR: Operation = alu_rmw!("ROR", G2_with_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_ror(cpu.tmp8)
});

pub static STX: Operation = store!(
    "STX",
    x,
    combine!(
        AddressingModeFlag::ZERO_PAGE,
        AddressingModeFlag::ABSOLUTE,
        AddressingModeFlag::ZERO_PAGE_Y
    )
);

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
pub static INC: Operation = alu_rmw!("INC", G2_no_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_add(1);
    cpu.flags.set_nz(cpu.tmp8);
});

pub static DEC: Operation = alu_rmw!("DEC", G2_no_acc, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_sub(1);
    cpu.flags.set_nz(cpu.tmp8);
});
