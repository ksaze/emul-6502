use super::prelude::*;
use super::shared_macros::*;

const G3_MODES: AddressingModeFlag = combine!(
    AddressingModeFlag::IMMEDIATE,
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE_X,
);

pub static BIT: Operation = Operation {
    name: "BIT",
    valid_modes: combine!(AddressingModeFlag::ZERO_PAGE, AddressingModeFlag::ABSOLUTE),
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.tmp8 = cpu.data_bus;
            cpu.flags.set(Status::ZERO, cpu.tmp8 & cpu.a == 0);
            cpu.flags.set(Status::NEGATIVE, cpu.tmp8 & 0x80 != 0);
            // V Flag => Copy bit 6 from memory
            cpu.flags.set(Status::OVERFLOW, cpu.tmp8 & 0x40 != 0);
            StepCtl::End
        }
    )],
};

pub static JMP: Operation = Operation {
    name: "JMP",
    valid_modes: combine!(AddressingModeFlag::ABSOLUTE, AddressingModeFlag::ABS_IND),
    typ: OperationType::Control,
    micro: &[micro_op!(
        (INTERNAL) // Already latched from addressing mode
            |cpu| {
                cpu.pc = cpu.eff_addr;
                StepCtl::End
            }
    )],
};

pub static STY: Operation = store!(
    "STY",
    y,
    G3_MODES.clear(&[
        AddressingModeFlag::IMMEDIATE,
        AddressingModeFlag::ABSOLUTE_X
    ])
);

pub static LDY: Operation = load!("LDY", y, G3_MODES);

pub static CPY: Operation = compare!(
    "CPY",
    y,
    G3_MODES.clear(&[
        AddressingModeFlag::ZERO_PAGE_X,
        AddressingModeFlag::ZERO_PAGE_Y
    ])
);
pub static CPX: Operation = compare!(
    "CPX",
    x,
    G3_MODES.clear(&[
        AddressingModeFlag::ZERO_PAGE_X,
        AddressingModeFlag::ZERO_PAGE_Y
    ])
);
