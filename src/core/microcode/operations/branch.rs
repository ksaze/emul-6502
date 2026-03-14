use super::prelude::*;

macro_rules! branch {
    ($name:literal, $flag:path, $flag_value:literal) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::RELATIVE,
            typ: OperationType::Control,
            micro: &[
                micro_op!(
                    (INTERNAL) // Latched offest to tmp8
                    |cpu| {
                        cpu.signals.poll_int = true;
                        if cpu.flags.contains($flag) == $flag_value {
                            StepCtl::Next
                        } else {
                            StepCtl::End
                        }
                    }
                ),

                micro_op!(
                    (READ pc) // DUMMY
                    |cpu| {
                        let old_pcl = cpu.pc as u8;
                        let new_pcl = old_pcl.wrapping_add(cpu.tmp8);
                        cpu.pc = (cpu.pc & 0xFF00) | new_pcl as Word;

                        let offset_is_negative = cpu.tmp8 & 0x80 != 0;
                        let page_wrap = if offset_is_negative {
                            cpu.tmp8 = 0xFF;
                            new_pcl > old_pcl
                        } else {
                            cpu.tmp8 = 0x1;
                            old_pcl > new_pcl
                        };

                        if page_wrap {
                            StepCtl::Next
                        } else {
                            cpu.signals.branch_T3 = true;
                            StepCtl::End
                        }
                    }
                ),

                micro_op!(
                    (READ pc) // DUMMY
                    // page correction cycle
                    // cpu.tmp8 holds carry value
                    |cpu| {
                        cpu.pc = cpu.pc.wrapping_add((cpu.tmp8 as Word) << 8);
                        StepCtl::End
                    }
                )
            ]
        }
    };
}

pub static BEQ: Operation = branch!("BEQ", Status::ZERO, true);
pub static BNE: Operation = branch!("BNE", Status::ZERO, false);

pub static BCS: Operation = branch!("BCS", Status::CARRY, true);
pub static BCC: Operation = branch!("BCC", Status::CARRY, false);

pub static BMI: Operation = branch!("BMI", Status::NEGATIVE, true);
pub static BPL: Operation = branch!("BPL", Status::NEGATIVE, false);

pub static BVS: Operation = branch!("BVS", Status::OVERFLOW, true);
pub static BVC: Operation = branch!("BVC", Status::OVERFLOW, false);
