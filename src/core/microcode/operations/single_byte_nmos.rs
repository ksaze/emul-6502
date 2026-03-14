use super::prelude::*;

macro_rules! reg_set {
    // PUBLIC INTERFACES
    ($name:literal, $to:ident <- sp) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.sp.value)
    };

    ($name:literal, $to:ident <- $from:ident) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$from)
    };


    ($name:literal, $to:ident <- $r:ident + 1) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$r.wrapping_add(1))
    };

    ($name:literal, $to:ident <- $r:ident - 1) => {
        reg_set!(@impl $name, $to, |cpu: &mut CPUCore| cpu.$r.wrapping_sub(1))
    };

    // INTERNAL IMPLEMENTATION
    (@impl $name:literal, $to:ident, $rhs:expr) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                micro_op!(
                    (READ eff_addr)
                    |cpu| {
                        cpu.$to = ($rhs)(cpu);
                        cpu.flags.set_nz(cpu.$to);
                        StepCtl::End
                    }
                )
            ]
        }
    };
}

macro_rules! flag {
    // INTERNAL IMPLEMENTATION
    (@impl $name:literal, $op:ident, $flag:ident) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                micro_op!(
                    (READ eff_addr)
                    |cpu| {
                        cpu.flags.$op(Status::$flag);
                        StepCtl::End
                    }
                )
            ]
        }
    };

    // PUBLIC INTERFACES
    ($name:literal, set $flag:ident) => {
        flag!(@impl $name, insert, $flag)
    };

    ($name:literal, clear $flag:ident) => {
        flag!(@impl $name, remove, $flag)
    };
}

macro_rules! stack {
    // PUSH INTERFACE
    ($name:literal, push a) => {
        stack!(@push $name, a)
    };

    ($name:literal, push p) => {
        // Set UNUSED & BREAK in pushed status byte
        stack!(@push $name, p)
    };

    // PULL INTERFACE
    ($name:literal, pull a) => {
        stack!(@pull $name, a, |cpu: &mut CPUCore, v: Byte| {
            cpu.a = v;
            cpu.flags.set_nz(cpu.a);
        })
    };

    ($name:literal, pull p) => {
        stack!(@pull $name, p, |cpu: &mut CPUCore, v: Byte| {
            cpu.flags = Status::from_bits_truncate(v);
            cpu.flags.insert(Status::UNUSED);
        })
    };

    // INTERNAL PUSH IMPLEMENTATION
    (@push $name:literal, $reg:ident) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                micro_op!(
                    (READ eff_addr)
                    |_cpu| StepCtl::Next
                ),

                micro_op!(
                    // cycle 2: write to stack
                    (WRITE $reg -> sp)
                    |cpu| {
                        cpu.sp.decrement();
                        StepCtl::End
                    }
                )
            ]
        }
    };

    // INTERNAL PULL IMPLEMENTATION
    (@pull $name:literal, $reg:ident, $assign:expr) => {
        Operation {
            name: $name,
            valid_modes: AddressingModeFlag::IMPLIED,
            typ: OperationType::Register,
            micro: &[
                // cycle 1: dummy read
                micro_op!(
                    (READ eff_addr)
                    |_cpu| StepCtl::Next
                ),

                // cycle 2: increment SP
                micro_op!(
                    (READ sp)
                    |cpu| {
                        cpu.sp.increment();
                        StepCtl::Next
                    }
                ),

                // cycle 3: read from stack
                micro_op!(
                    (READ sp)
                    |cpu| {
                        let v = cpu.data_bus;
                        ($assign)(cpu, v);
                        StepCtl::End
                    }
                )
            ]
        }
    };
}

pub static PHA: Operation = stack!("PHA", push a);
pub static PHP: Operation = stack!("PHP", push p);
pub static PLA: Operation = stack!("PLA", pull a);
pub static PLP: Operation = stack!("PLP", pull p);

pub static CLC: Operation = flag!("CLC", clear CARRY);
pub static SEC: Operation = flag!("SEC", set CARRY);
pub static CLI: Operation = flag!("CLI", clear IRQ_DISABLE);
pub static SEI: Operation = flag!("SEI", set IRQ_DISABLE);
pub static CLV: Operation = flag!("CLV", clear OVERFLOW);
pub static CLD: Operation = flag!("CLD", clear DECIMAL);
pub static SED: Operation = flag!("SED", set DECIMAL);

pub static DEY: Operation = reg_set!("DEY", y <- y-1);
pub static INY: Operation = reg_set!("INY", y <- y+1);
pub static INX: Operation = reg_set!("INX", x <- x+1);
pub static DEX: Operation = reg_set!("DEX", x <- x-1);
pub static TAY: Operation = reg_set!("TAY", y <- a);
pub static TYA: Operation = reg_set!("TYA", a <- y);
pub static TXA: Operation = reg_set!("TXA", a <- x);
pub static TAX: Operation = reg_set!("TAX", x <- a);
pub static TSX: Operation = reg_set!("TSX", x <- sp);

// reg_set! macro isn't used because TXS doesn't set NZ flags like others
pub static TXS: Operation = Operation {
    name: "TXS",
    valid_modes: AddressingModeFlag::IMPLIED,
    typ: OperationType::Register,
    micro: &[micro_op!(
        (READ eff_addr)
        |cpu| {
            cpu.sp.value = cpu.x;
            StepCtl::End
        }
    )],
};
