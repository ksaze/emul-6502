use super::prelude::*;

// External operations will be declared as write.
// CPU will convert them into read when RESG is set.
pub static BRK: Operation = Operation {
    name: "BRK",
    valid_modes: AddressingModeFlag::NONE,
    typ: OperationType::Interrupt,

    micro: &[
        micro_op!(
            (READ pc)
            |cpu| {
                // Skip padding byte if software BRK
                if cpu.signals.D1x1 {
                    cpu.pc = cpu.pc.wrapping_add(1);
                }
                StepCtl::Next
            }
        ),
        // push PCH
        micro_op!(
            (WRITE pc_h -> sp)
            |cpu| {
                cpu.sp.decrement();
                StepCtl::Next
            }
        ),
        // push PCL
        micro_op!(
            (WRITE pc_l -> sp)
            |cpu| {
                cpu.sp.decrement();
                cpu.signals.VEC_next_cycle = true;
                StepCtl::Next
            }
        ),
        // push P
        micro_op!(
            (WRITE p -> sp)
            |cpu| {
                cpu.sp.decrement();

                // Vector selector
                // RESP check for hijack case (temp fix before phase seperation implemented correctly)
                if cpu.signals.RESG | cpu.signals.RESP {
                    cpu.tmp16 = 0xFFFC;
                    cpu.signals.res_hijack = !cpu.signals.in_reset;
                } else if cpu.signals.NMIG {
                    // NMI Hijack
                    cpu.tmp16 = 0xFFFA;
                } else {
                    cpu.tmp16 = 0xFFFE;
                }

                StepCtl::Next
            }
        ),
        // vector low
        micro_op!(
            (READ tmp16)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                cpu.tmp16 += 1;

                cpu.flags.insert(Status::IRQ_DISABLE);
                cpu.signals.INTG = false;
                cpu.signals.D1x1 = true;
                cpu.signals.doIRQ = false;
                cpu.signals.brk_done = true;

                cpu.signals.VEC_next_cycle = false;
                if cpu.signals.res_hijack && cpu.signals.RESP {
                    cpu.pc = Word::from_le_bytes([0xFD, cpu.tmp8]);
                    StepCtl::Skip(1)
                } else if cpu.signals.res_hijack && !cpu.signals.RESP {
                    cpu.pc = Word::from_le_bytes([0xFD, cpu.tmp8]);
                    StepCtl::End
                } else {
                    // RES half-hijack
                    if cpu.signals.RESP {
                        cpu.signals.res_hijack = true;
                        cpu.tmp16 = 0xFFFD;
                    }
                    StepCtl::Next
                }
            }
        ),
        // vector high
        micro_op!(
            (READ tmp16)
            |cpu| {
                cpu.pc = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);

                // It's impossible for the cpu to have in_reset signal while servicing some other form of interrupt
                // Hence no check if whether current BRK was a RESET is required
                cpu.signals.in_reset = false;

                // phase 2
                // RESG & NMIG are cleared using brk_done in phase 1
                cpu.signals.brk_done = false;

                if cpu.signals.res_hijack && cpu.signals.RESP {
                    StepCtl::Next
                } else {
                    cpu.signals.RESG = false;
                    StepCtl::End
                }
            }
        ),
        // RES Hijack extra cycles for RES pin held low
        micro_op!(
            (READ pc)
            |cpu| {
                // For full hijack pin held down case, brk_done isn't cleared because regular T0 microp is never triggered
                cpu.signals.brk_done = false;

                cpu.tmp8 = cpu.data_bus;

                let pcl = cpu.pc.to_le_bytes()[1].wrapping_sub(1);
                cpu.pc = Word::from_le_bytes([pcl, cpu.tmp8]);

                if cpu.signals.res_hijack && cpu.signals.RESP {
                    StepCtl::Next
                } else {
                    cpu.signals.res_hijack = false;
                    cpu.signals.in_reset = true;
                    StepCtl::End
                }
            }
        ),
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                let pcl = cpu.pc.to_le_bytes()[1].wrapping_sub(1);

                cpu.pc = Word::from_le_bytes([pcl, cpu.tmp8]);

                if cpu.signals.res_hijack && cpu.signals.RESP {
                    StepCtl::Next
                } else {
                    cpu.signals.res_hijack = false;
                    cpu.signals.in_reset = true;
                    StepCtl::End
                }
            }
        ),
        micro_op!(
            (READ pc)
            |cpu| {
                cpu.pc = 0x00FF;
                cpu.signals.res_hijack = false;
                cpu.signals.in_reset = true;
                StepCtl::End
            }
        ),
    ],
};

pub static RTI: Operation = Operation {
    name: "RTI",
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
        // cycle 4: pull P
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.flags = Status::from_bits_truncate(cpu.data_bus);
                cpu.flags.insert(Status::UNUSED);
                cpu.sp.increment();
                StepCtl::Next
            }
        ),
        // cycle 5: pull PCL
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.tmp8 = cpu.data_bus;
                cpu.sp.increment();
                StepCtl::Next
            }
        ),
        // cycle 6: pull PCH
        micro_op!(
            (READ sp)
            |cpu| {
                cpu.pc = Word::from_le_bytes([cpu.tmp8, cpu.data_bus]);
                StepCtl::End
            }
        ),
    ],
};
