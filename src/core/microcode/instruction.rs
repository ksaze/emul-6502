use super::addressing_modes::NONE;
use super::micro_op::MicroOp;
use super::operations::NOP;
use super::types::*;

pub struct Operation {
    pub name: &'static str,
    pub valid_modes: AddressingModeFlag,
    pub typ: OperationType,
    pub micro: &'static [MicroOp],
}

// Addressing mode through their micro must produce cpu.eff_addr
// Usage of eff_addr for read, write, or any other purpose is done by the Operation
pub struct AddressingMode {
    pub name: &'static str,
    pub flag: AddressingModeFlag,
    pub micro: &'static [MicroOp],
}

pub struct Instruction {
    pub name: String,
    pub(super) addressing: &'static AddressingMode,
    pub(super) operation: &'static Operation,
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

        let name = if combine!(
            AddressingModeFlag::NONE,
            AddressingModeFlag::IMPLIED,
            AddressingModeFlag::RELATIVE
        )
        .contains(addressing.flag)
        {
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
        self.addressing
            .micro
            .iter()
            .chain(self.operation.micro.iter())
    }
}
