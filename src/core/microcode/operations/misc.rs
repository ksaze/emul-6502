use crate::core::cpu::CPUState;

use super::prelude::*;

pub static NOP: Operation = Operation {
    name: "NOP",
    valid_modes: combine!(
        AddressingModeFlag::IMPLIED,
        AddressingModeFlag::IMMEDIATE,
        AddressingModeFlag::ZERO_PAGE,
        AddressingModeFlag::ZERO_PAGE_X,
        AddressingModeFlag::ABSOLUTE,
        AddressingModeFlag::ABSOLUTE_X
    ),
    typ: OperationType::Timing,
    micro: &[micro_op!(
        (READ eff_addr)
        |_cpu| {StepCtl::End}
    )],
};

pub static JAM: Operation = Operation {
    name: "JAM",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Timing,
    micro: &[micro_op!(
        (READ tmp16) // pc + 1 stored in tmp16
        |cpu| {
            cpu.state = CPUState::Jammed;
            StepCtl::Next
        }
    )],
};
