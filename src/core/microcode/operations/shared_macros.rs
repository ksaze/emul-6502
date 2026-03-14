macro_rules! store {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Store,
            micro: &[micro_op!(
                (WRITE $register -> eff_addr)
                |_cpu| {
                    StepCtl::End
                }
            )]
        }
    };
}

macro_rules! load {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Read,
            micro: &[micro_op!(
                (READ eff_addr)
                |cpu| {
                    cpu.$register = cpu.data_bus;
                    cpu.flags.set_nz(cpu.$register);
                    StepCtl::End
                }
            )],
        }
    };
}

macro_rules! compare {
    ($name: literal, $register: ident, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Read,
            micro: &[micro_op!(
                (READ eff_addr)
                |cpu| {
                    cpu.tmp8 = cpu.data_bus;
                    cpu.flags.set(Status::CARRY, cpu.$register >= cpu.tmp8);
                    cpu.flags.set_nz(cpu.$register.wrapping_sub(cpu.tmp8));
                    StepCtl::End
                }
            )],
        }
    };
}

pub(super) use compare;
pub(super) use load;
pub(super) use store;
