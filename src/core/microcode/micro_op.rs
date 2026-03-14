use crate::core::cpu::CPUCore;

use super::types::{BusOpSpec, StepCtl};

#[derive(Copy, Clone, Debug)]
pub struct MicroOp {
    pub(super) external: BusOpSpec,
    pub(super) internal: for<'a> fn(&'a mut CPUCore) -> StepCtl,
}

impl MicroOp {
    pub(in crate::core) fn execute(&self, cpu: &mut CPUCore) -> StepCtl {
        (self.internal)(cpu)
    }

    pub(in crate::core) fn bus_spec(&self) -> BusOpSpec {
        self.external
    }
}

macro_rules! micro_op {
    (@reg_addr sp) => { |cpu| cpu.sp.to_word() };
    (@reg_addr tmp8) => { |cpu| cpu.tmp8 as Word };
    (@reg_addr $reg:ident) => { |cpu| cpu.$reg };

    (@reg_data sp) => { |cpu| cpu.sp.value };
    (@reg_data pc_h) => { |cpu| (cpu.pc >> 8) as Byte };
    (@reg_data pc_l) => { |cpu| (cpu.pc & 0xFF) as Byte };
    (@reg_data p) => { |cpu| cpu.flags.bits() | 0x20 | ((cpu.signals.D1x1 as u8) << 4) };
    (@reg_data $reg:ident) => {|cpu| cpu.$reg };

    ((READ $target:ident) $action:expr) => {
        micro_op!(@impl
            (READ micro_op!(@reg_addr $target))
            $action
        )
    };

    ((WRITE $data:ident -> $target:ident) $action:expr) => {
        micro_op!(@impl
            (WRITE micro_op!(@reg_addr $target), micro_op!(@reg_data $data))
            $action
        )
    };

    (@impl (READ $addr:expr) $action:expr) => {
        MicroOp {
            external: BusOpSpec::Read {
                addr: $addr,
            },
            internal: $action,
        }
    };

    (@impl (WRITE $ea:expr, $data:expr) $action:expr) => {
        MicroOp {
            external: BusOpSpec::Write {
                addr:$ea,
                data:$data
            },
            internal: $action,
        }
    };

    ((INTERNAL) $action:expr) => {
        MicroOp {
            external: BusOpSpec::Internal,
            internal: $action,
        }
    };
}

pub(super) use micro_op;
