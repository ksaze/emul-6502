use crate::core::variants::ALUOuput;

use super::prelude::*;
use super::shared_macros::*;

const G1_MODES: AddressingModeFlag = combine!(
    AddressingModeFlag::IMMEDIATE,
    AddressingModeFlag::ZERO_PAGE,
    AddressingModeFlag::ZERO_PAGE_X,
    AddressingModeFlag::ABSOLUTE,
    AddressingModeFlag::ABSOLUTE_X,
    AddressingModeFlag::ABSOLUTE_Y,
    AddressingModeFlag::IDX_IND,
    AddressingModeFlag::IND_IDX,
);

macro_rules! alu {
    ($name:literal, $op:tt) => {
        Operation {
            name: $name,
            valid_modes: G1_MODES,
            typ: OperationType::Read,
            micro: &[micro_op!(
                (READ eff_addr)
                |cpu| {
                    cpu.a = cpu.a $op cpu.data_bus;
                    cpu.flags.set_nz(cpu.a);
                    StepCtl::End
                }
            )],
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
        micro_op!(
            (READ eff_addr)
            |cpu| {
                match cpu.adc(cpu.data_bus) {
                    ALUOuput::Done(value) => {
                        cpu.a = value;
                        StepCtl::End
                    }
                    ALUOuput::Penalty(value) => {
                        cpu.tmp8 = value;
                        StepCtl::Next
                    }
                }
            }
        ),
        micro_op!(
            (READ pc)
            |_cpu| {
                todo!()
            }
        ),
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
        micro_op!(
            (READ eff_addr)
            |cpu| {
                match cpu.sbc(cpu.data_bus) {
                    ALUOuput::Done(value) => {
                        cpu.a = value;
                        StepCtl::End
                    }
                    ALUOuput::Penalty(value) => {
                        cpu.tmp8 = value;
                        StepCtl::Next
                    }
                }
            }
        ),
        micro_op!(
            (READ pc)
            |_cpu| {
                todo!()
            }
        ),
    ],
};
