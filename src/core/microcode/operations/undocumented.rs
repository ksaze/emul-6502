use crate::core::variants::ALUOuput;

use super::group_2::alu_rmw;
use super::prelude::*;

// cc = 11 instructions combine their cc = 01 (Group 1) & cc = 10 (Group 2) counterparts.
// Addressing modes of group 1 are used
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

// ─── Combined Read ───────────────────────────────────────────────────────────

//ALR (ASR) AND + LSR
pub static ALR: Operation = Operation {
    name: "ALR",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a &= cpu.data_bus;
            cpu.a = cpu.alu_shr(cpu.a);
            StepCtl::End
        }
    )],
};

// AND (AAC): AND + set C as ASL/ROR
pub static ANC: Operation = Operation {
    name: "ANC",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a &= cpu.data_bus;
            cpu.flags.set_nz(cpu.a);
            cpu.flags.set(Status::CARRY, cpu.a & 0x80 != 0);
            StepCtl::End
        }
    )],
};

// ANE (XAA): AND X + AND oper
// Highly unstable. Involves use of magic constant dependent on temperature, chip, etc.
// Emulator uses 0xEE for the constant
pub static ANE: Operation = Operation {
    name: "ANE",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a = (cpu.a | 0xEE) & cpu.x & cpu.data_bus;
            cpu.flags.set_nz(cpu.a);
            StepCtl::End
        }
    )],
};

// ARR: AND + ROR
pub static ARR_RICOH: Operation = Operation {
    name: "ARR",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a &= cpu.data_bus;
            // NZ set from alu_ror
            cpu.a = cpu.alu_ror(cpu.a);

            let bit_6 = (cpu.a & 0x40) >> 6;
            let bit_5 = (cpu.a & 0x20) >> 5;
            cpu.flags.set(Status::CARRY, bit_6 != 0);
            cpu.flags.set(Status::OVERFLOW, (bit_5 ^ bit_6) != 0);
            StepCtl::End
        }
    )],
};

// ARR: AND + ROR
pub static ARR_WITH_DECIMAL: Operation = Operation {
    name: "ARR",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            let tmp = cpu.a & cpu.data_bus;
            cpu.a = cpu.alu_ror(tmp);  // alu_ror sets NZ from ror result here
            let ror = cpu.a;

            let bit_6 = (ror & 0x40) >> 6;
            let bit_5 = (ror & 0x20) >> 5;
            cpu.flags.set(Status::OVERFLOW, (bit_5 ^ bit_6) != 0);

            if cpu.flags.contains(Status::DECIMAL) {
                if (tmp & 0x0F) >= 0x05 {
                    cpu.a = (ror & 0xF0) | ((ror.wrapping_add(0x06)) & 0x0F);
                }
                if (tmp & 0xF0) >= 0x50 {
                    cpu.flags.set(Status::CARRY, true);
                    cpu.a = cpu.a.wrapping_add(0x60);
                } else {
                    cpu.flags.set(Status::CARRY, false);
                }
                // NZ set from ror, NOT from adjusted result — already done by alu_ror
            } else {
                cpu.flags.set(Status::CARRY, bit_6 != 0);
                // NZ already set by alu_ror
            }
            StepCtl::End
        }
    )],
};

// LAS (LAR, LAE): LDA/TSX
pub static LAS: Operation = Operation {
    name: "LAS",
    valid_modes: AddressingModeFlag::ABSOLUTE_Y,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a = cpu.data_bus & cpu.sp.value;
            cpu.x = cpu.a;
            cpu.sp.value = cpu.a;
            cpu.flags.set_nz(cpu.a);
            StepCtl::End
        }
    )],
};

// LAX: LDA + LDX
pub static LAX: Operation = Operation {
    name: "LAX",
    valid_modes: combine!(G1_MODES, AddressingModeFlag::ZERO_PAGE_Y).clear(&[
        AddressingModeFlag::IMMEDIATE,
        AddressingModeFlag::ZERO_PAGE_X,
        AddressingModeFlag::ABSOLUTE_X,
        AddressingModeFlag::IMMEDIATE,
    ]),
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a = cpu.data_bus;
            cpu.x = cpu.a;
            cpu.flags.set_nz(cpu.a);
            StepCtl::End
        }
    )],
};

// LXA (ATX, OAL) LDA + LDX (Unstable variant of LAX)
// Highly unstable. Involves use of magic constant dependent on temperature, chip, etc.
// Emulator uses 0xEE for the constant
pub static LXA: Operation = Operation {
    name: "LXA",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.a = (cpu.a | 0xEE) & cpu.data_bus;
            cpu.x = cpu.a;
            cpu.flags.set_nz(cpu.a);
            StepCtl::End
        }
    )],
};

// SBX (AXS, SAX) CMP + DEX
// Mnemonic uses "S", implying store, but by emulator's OperationType's semantics, SBX is Read because no data is written to effective address
pub static SBX: Operation = Operation {
    name: "SBX",
    valid_modes: AddressingModeFlag::IMMEDIATE,
    typ: OperationType::Read,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.tmp8 = cpu.a & cpu.x;
            cpu.x = cpu.tmp8.wrapping_sub(cpu.data_bus);

            cpu.flags.set(Status::CARRY, cpu.tmp8 >= cpu.data_bus);
            cpu.flags.set_nz(cpu.x);
            StepCtl::End
        }
    )],
};

// Undocumented SBC: Same as documented SBC
pub static USBC: Operation = Operation {
    name: "USBC",
    valid_modes: AddressingModeFlag::IMMEDIATE,
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

// ─── Combined Store ───────────────────────────────────────────────────────────

// SAX (AAX, AXS): STA + STX
// Open drain behaviour. Operands (and a constant) are combined using bitwise AND and and written to effective address
// Constant is assumed to be 0 for the emulator
pub static SAX: Operation = Operation {
    name: "SAX",
    valid_modes: combine!(
        AddressingModeFlag::ZERO_PAGE,
        AddressingModeFlag::ZERO_PAGE_Y,
        AddressingModeFlag::ABSOLUTE,
        AddressingModeFlag::IDX_IND
    ),
    typ: OperationType::Store,
    micro: &[micro_op!(
        (WRITE |cpu| cpu.x & cpu.a => eff_addr)
        |_cpu| {
            StepCtl::End
        }
    )],
};

// Store with & (high byte of effective address + 1)
// Page crossing bug: base address before page correction is used for H
// So effectively in case of page cross, operand & H is stored
macro_rules! store_with_hmask {
    ($name: literal, $operand: expr, copy_to_sp: $copy: expr, $modes: expr) => {
        Operation {
            name: $name,
            valid_modes: $modes,
            typ: OperationType::Store,
            micro: &[
                micro_op!(
                    // Prepare data before bus operation
                    (INTERNAL)
                    |cpu| {
                        let high = if cpu.crossed {
                            // In case of page cross, high byte of base address without page correction is used.
                            cpu.eff_addr.to_le_bytes()[1]
                        } else {
                            cpu.eff_addr.to_le_bytes()[1].wrapping_add(1)
                        };
                        cpu.tmp8 = ($operand)(cpu) & high;

                        // corrupt effective address's high byte with computed value in case of page cross
                        cpu.tmp16 = if cpu.crossed {
                            ((cpu.tmp8 as u16) << 8) | (cpu.eff_addr & 0xFF)
                        } else {
                            cpu.eff_addr
                        };
                        StepCtl::Merge
                    }
                ),

                micro_op!(
                    (WRITE tmp8 -> tmp16)
                    |cpu| {
                        if $copy {
                            cpu.sp.value = ($operand)(cpu);
                        }
                        StepCtl::End
                    }
                )
            ]
        }
    };
}

// SHA (AXA)
pub static SHA: Operation = store_with_hmask!(
    "SHA",
    |cpu: &mut CPUCore| cpu.x & cpu.a,
    copy_to_sp: false,
    combine!(AddressingModeFlag::ABSOLUTE_Y, AddressingModeFlag::IND_IDX)
);

// SHX (SXA, XAS)
pub static SHX: Operation = store_with_hmask!(
    "SHX",
    |cpu: &mut CPUCore| cpu.x,
    copy_to_sp: false,
    AddressingModeFlag::ABSOLUTE_Y
);

// SHY (SYA, SAY)
pub static SHY: Operation = store_with_hmask!(
    "SHY",
    |cpu: &mut CPUCore| cpu.y,
    copy_to_sp: false,
    AddressingModeFlag::ABSOLUTE_X
);

// TAS (SHS, XAS): SHA + transfer X & A -> SP
pub static TAS: Operation = store_with_hmask!(
    "TAS",
    |cpu: &mut CPUCore| cpu.x & cpu.a,
    copy_to_sp: true,
    AddressingModeFlag::ABSOLUTE_Y
);

// ─── Combined RMW ────────────────────────────────────────────────────────────

// DCP (DCM): DEC + CMP (RMW)
pub static DCP: Operation = alu_rmw!("DCP", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_sub(1);
    cpu.flags.set(Status::CARRY, cpu.a >= cpu.tmp8);
    cpu.flags.set_nz(cpu.a.wrapping_sub(cpu.tmp8));
    cpu.tmp8
});

// ISC (ISB, INS): INC + SBC (RMW)
pub static ISC: Operation = alu_rmw!("ISC", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.tmp8.wrapping_add(1);
    cpu.a = match cpu.sbc(cpu.tmp8) {
        ALUOuput::Done(value) | ALUOuput::Penalty(value) => value,
    };
    cpu.tmp8
});

// RLA: ROL + AND (RMW)
pub static RLA: Operation = alu_rmw!("RLA", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_rol(cpu.tmp8);
    cpu.a &= cpu.tmp8;
    cpu.flags.set_nz(cpu.a);
    cpu.tmp8
});

// RRA: ROR + ADC (RMW)
pub static RRA: Operation = alu_rmw!("RRA", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_ror(cpu.tmp8);
    cpu.a = match cpu.adc(cpu.tmp8) {
        ALUOuput::Done(value) | ALUOuput::Penalty(value) => value,
    };
    cpu.tmp8
});

// SLO: ASL + ORA (RMW)
pub static SLO: Operation = alu_rmw!("SLO", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shl(cpu.tmp8);
    cpu.a |= cpu.tmp8;
    cpu.flags.set_nz(cpu.a);
    cpu.tmp8
});

// SRE: LSR + EOR (RMW)
pub static SRE: Operation = alu_rmw!("SRE", G1, |cpu: &mut CPUCore| {
    cpu.tmp8 = cpu.alu_shr(cpu.tmp8);
    cpu.a ^= cpu.tmp8;
    cpu.flags.set_nz(cpu.a);
    cpu.tmp8
});
