use super::prelude::*;

pub static JSR: Operation = Operation {
    name: "JSR abs",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Control,
    micro: &[
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                cpu.pc = cpu.pc.wrapping_add(1);
                StepCtl::Next
            }
        ),
        micro_op!(
            (READ sp)
            |_cpu| {
                // For return address, the address of next instruction - 1 is pushed
                // Buffer ADL
                StepCtl::Next
            }
        ),
        micro_op!(
            (WRITE pc_h -> sp)
            |cpu| {
                cpu.sp.decrement();
                StepCtl::Next
            }
        ),
        micro_op!(
            (WRITE pc_l -> sp)
            |cpu| {
                cpu.sp.decrement();
                StepCtl::Next
            }
        ),
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.pc = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                StepCtl::End
            }
        ),
    ],
};

pub static RTS: Operation = Operation {
    name: "RTS",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Control,
    micro: &[
        // cycle 2: dummy read
        micro_op!(
            (READ eff_addr)
            |_cpu| StepCtl::Next
        ),
        // cycle 3: increment sp
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.sp.increment();
                StepCtl::Next
            }
        ),
        // cycle 4: pull PCL
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                cpu.sp.increment();
                StepCtl::Next
            }
        ),
        // cycle 5: pull PCH
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.tmp16 = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                StepCtl::Next
            }
        ),
        // cycle 6: increment PC to next instruction
        micro_op!(
            (READ tmp16)
            |cpu| {
                cpu.pc = cpu.tmp16.wrapping_add(1);
                StepCtl::End
            }
        ),
    ],
};
