use crate::bus::Bus;
use crate::operations::{BusOpSpec, Instruction, MicroOp, StepCtl};
use crate::shared::{Byte, Word};
use crate::variants::{ALUOuput, Decoder, Quirks, VariantQuirks};

use bitflags::bitflags;

pub struct StackPointer {
    pub value: Byte,
}

impl StackPointer {
    #[must_use]
    pub const fn to_word(&self) -> Word {
        Word::from_le_bytes([self.value, 0x01])
    }

    pub const fn decrement(&mut self) {
        self.value = self.value.wrapping_sub(1);
    }

    pub const fn increment(&mut self) {
        self.value = self.value.wrapping_add(1);
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Status: u8 {
        const CARRY     = 0b0000_0001; // C
        const ZERO      = 0b0000_0010; // Z
        const IRQ_DISABLE = 0b0000_0100; // I
        const DECIMAL   = 0b0000_1000; // D
        // bit 4: B flag is phantom
        const UNUSED   = 0b0010_0000; // always set on 6502
        const OVERFLOW = 0b0100_0000; // V
        const NEGATIVE = 0b1000_0000; // N
    }
}

impl Status {
    #[inline]
    pub fn set_nz(&mut self, value: Byte) {
        self.set(Status::ZERO, value == 0);
        self.set(Status::NEGATIVE, value & 0x80 != 0);
    }
}

#[allow(non_snake_case)]
#[allow(dead_code)]
pub struct Signals {
    pub RES_sync: bool,
    pub IRQ_sync: bool,
    pub NMIP: bool,

    pub RESP: bool,
    pub IRQP: bool,

    pub NMIL: bool,
    pub sig_1368: bool,

    pub doIRQ: bool,
    pub NMIG: bool,
    pub RESG: bool,

    pub INTG: bool,
    pub D1x1: bool,

    pub VEC: bool,
    pub brk_done: bool,

    // Internal emulator signals
    pub poll_int: bool,
    pub in_reset: bool,
    pub res_hijack: bool,
    pub NMIP_ph1: bool,
    pub branch_T3: bool,
    pub VEC_next_cycle: bool,
}

fn bool_str(val: bool) -> &'static str {
    if val { "1" } else { "0" }
}

impl Signals {
    pub fn new() -> Self {
        Self {
            RES_sync: true,
            IRQ_sync: false,
            RESP: true,
            NMIP: false,
            IRQP: false,
            NMIL: false,
            sig_1368: false,
            RESG: true, // RESET on power-on
            NMIG: false,
            INTG: false,
            doIRQ: false,
            D1x1: true,
            poll_int: false,
            VEC: false,
            brk_done: false,
            in_reset: true, // RESET on power-on (skip pin down cycles)
            NMIP_ph1: false,
            branch_T3: false,
            res_hijack: false,
            VEC_next_cycle: false,
        }
    }

    pub fn ph1(&mut self, irq_disable: bool) {
        self.RESP = !self.RES_sync;
        // Ignore RES clear during res_hijack
        // res_hijack cleared in fetch following full hijack before this fn is called
        if !self.res_hijack {
            self.RESG &= !self.brk_done;
        }
        self.D1x1 = !self.RESG & !self.INTG;

        self.NMIG = self.sig_1368 | (self.NMIG & !self.brk_done);
        self.NMIL = self.NMIP_ph1;
        self.sig_1368 = self.NMIP & !self.NMIL & !self.VEC;
        self.NMIP_ph1 = self.NMIP && !self.VEC;

        self.IRQP = self.IRQ_sync;

        self.doIRQ = self.NMIG | (self.IRQP & !irq_disable);

        self.VEC = self.VEC_next_cycle;
    }

    pub fn ph2(&mut self) {
        // valid for vec_hi.ph2, but not vec_lo.ph2--which is the same as brk-done
        // post operation completion
        // self.sig_1368 = self.NMIP & !self.NMIL & !self.brk_done;
        self.sig_1368 = self.NMIP & !self.NMIL & !self.VEC;

        // before operation microp
        self.RESG |= self.RESP;
        self.INTG = (!self.poll_int && self.INTG) || (self.poll_int && self.doIRQ);

        self.D1x1 = !self.RESG & !self.INTG;

        self.poll_int = false;
    }

    pub fn trace(&self) -> String {
        format!(
            "1368={} NMIG={} INTG={} RESG={} D1x1={} VEC={} brk_done={} IRQP={} NMIP={}",
            bool_str(self.sig_1368),
            bool_str(self.NMIG),
            bool_str(self.INTG),
            bool_str(self.RESG),
            bool_str(self.D1x1),
            bool_str(self.VEC),
            bool_str(self.brk_done),
            bool_str(self.IRQP),
            bool_str(self.NMIP),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RdyResumeState {
    Fetch,
    Exec,
    Jammed,
    Reset,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CPUState {
    Fetch,
    Exec,
    Blocked(RdyResumeState),
    Jammed,
    Reset,
}

impl CPUState {
    pub fn block(self) -> CPUState {
        let resume = match self {
            CPUState::Fetch => RdyResumeState::Fetch,
            CPUState::Exec => RdyResumeState::Exec,
            CPUState::Jammed => RdyResumeState::Jammed,
            CPUState::Reset => RdyResumeState::Reset,
            CPUState::Blocked(_) => panic!("Tried to block an already blocked CPU"),
        };
        CPUState::Blocked(resume)
    }

    pub fn unblock(self) -> CPUState {
        match self {
            CPUState::Blocked(resume) => match resume {
                RdyResumeState::Fetch => CPUState::Fetch,
                RdyResumeState::Exec => CPUState::Exec,
                RdyResumeState::Jammed => CPUState::Jammed,
                RdyResumeState::Reset => CPUState::Reset,
            },
            _ => panic!("Tried to unblock a CPU that wasn't blocked"),
        }
    }
}

pub struct CPUCore {
    pub pc: Word,
    pub sp: StackPointer,

    pub a: Byte,
    pub x: Byte,
    pub y: Byte,
    pub flags: Status,

    pub data_bus: Byte,
    pub addr_bus: Word,
    pub rw: bool,

    pub quirks: &'static VariantQuirks,

    pub ir: Byte,
    pub tmp8: Byte,
    pub tmp16: Word,
    pub eff_addr: Word,
    pub crossed: bool,

    pub signals: Signals,

    pub instr: Instruction,
    pub micro_iter: Option<
        std::iter::Chain<std::slice::Iter<'static, MicroOp>, std::slice::Iter<'static, MicroOp>>,
    >,
    pub micro_op: Option<&'static MicroOp>,
    pub state: CPUState,
}

impl CPUCore {
    #[inline]
    pub fn adc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.quirks.adc)(self, value)
    }

    #[inline]
    pub fn sbc(&mut self, value: Byte) -> ALUOuput<Byte> {
        (self.quirks.sbc)(self, value)
    }

    #[inline]
    pub fn ind_addr_inc(&self, addr: Word) -> ALUOuput<Word> {
        (self.quirks.ind_addr_inc)(addr)
    }

    #[inline]
    pub fn alu_shl(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, value & 0x80 != 0);

        let result = value << 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_shr(&mut self, value: Byte) -> Byte {
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = value >> 1;

        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_rol(&mut self, value: u8) -> u8 {
        let carry_in = self.flags.contains(Status::CARRY) as Byte;
        let carry_out = (value & 0x80) != 0;

        let result = (value << 1) | carry_in;

        self.flags.set(Status::CARRY, carry_out);
        self.flags.set_nz(result);

        result
    }

    #[inline]
    pub fn alu_ror(&mut self, value: u8) -> u8 {
        let carry_in = (self.flags.contains(Status::CARRY) as Byte) << 7;

        // Carry gets bit 0
        self.flags.set(Status::CARRY, (value & 0x01) != 0);

        let result = (value >> 1) | carry_in;

        self.flags.set_nz(result);

        result
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU<V: Decoder + Quirks> {
    pub core: CPUCore,
    pub decoder: V,
}

impl<V: Decoder + Quirks> CPU<V> {
    pub fn new(variant: V) -> Self {
        CPU {
            core: CPUCore {
                pc: 0xFF,
                sp: StackPointer { value: 0 },
                a: 0,
                x: 0,
                y: 0,
                flags: Status::UNUSED,

                addr_bus: 0x00FF,
                data_bus: 0x0,
                rw: true,

                quirks: variant.quirks(),

                signals: Signals::new(),

                ir: 0,
                tmp8: 0,
                tmp16: 0,
                eff_addr: 0,
                crossed: false,

                instr: Instruction::default(),
                micro_iter: None,
                micro_op: None,
                state: CPUState::Fetch,
            },

            decoder: variant,
        }
    }

    fn get_external_operation(&mut self) {
        let mut internal_count = 0;
        loop {
            #[rustfmt::skip]
            let micro = self.core.micro_iter
                .as_mut()
                .expect("No iterator found while CPU state is Execute")
                .next()
                .expect("Iterator ended without StepCtl::End");

            self.core.micro_op = Some(micro);
            match micro.bus_spec() {
                // Execute internal cycle right now
                BusOpSpec::Internal => {
                    internal_count += 1;
                    assert!(
                        internal_count <= 1,
                        "Detected multiple consecutive internal cycles."
                    );
                    let ctrl = self
                        .execute_internal()
                        .expect("Non Execute CPU state while trying to fetch micro_op");

                    match ctrl {
                        StepCtl::Merge => continue,
                        StepCtl::SkipMerge => {
                            self.core.micro_iter.as_mut().unwrap().next();
                            continue;
                        }
                        _ => {
                            panic!(
                                "Non merge micro_op control state returned while trying to execute internal operation."
                            )
                        }
                    }
                }

                BusOpSpec::Read { addr } => {
                    self.core.addr_bus = (addr)(&mut self.core);
                    self.core.rw = true;
                    break;
                }

                BusOpSpec::Write { addr, data } => {
                    self.core.addr_bus = (addr)(&mut self.core);
                    self.core.data_bus = (data)(&mut self.core);
                    self.core.rw = false;
                    break;
                }
            }
        }
    }

    pub fn phi1(&mut self) {
        let irq_disable = self.core.flags.contains(Status::IRQ_DISABLE);
        if self.core.state == CPUState::Fetch {
            // This needs to be called before signals.ph1
            // res_hijack blocks clear of RESG through brk-done
            self.core.signals.res_hijack = false;
        }

        self.core.signals.ph1(irq_disable);

        // In RDY block--cycle elongated.
        if matches!(self.core.state, CPUState::Blocked(_)) {
            return;
        }

        if self.core.signals.RESG && !self.core.signals.res_hijack {
            match (self.core.signals.in_reset, self.core.signals.RESP) {
                // Fresh reset — begin servicing
                (false, _) => {
                    self.core.state = CPUState::Reset;
                    self.core.signals.in_reset = true;
                }
                // Reset re-asserted while already servicing — restart
                (true, true) => {
                    self.core.state = CPUState::Reset;
                }
                // Reset released — start T0 of reset/brk sequence
                (true, false) if self.core.state == CPUState::Reset => {
                    self.core.state = CPUState::Fetch;
                }
                _ => {}
            }
        }

        match self.core.state {
            CPUState::Fetch | CPUState::Reset => {
                self.core.addr_bus = self.core.pc;
                self.core.rw = true;
            }
            CPUState::Exec => self.get_external_operation(),
            CPUState::Jammed => {
                self.core.addr_bus = self.core.pc.wrapping_sub(1);
                self.core.rw = true;
            }
            CPUState::Blocked(_) => {
                panic!("Tried to set external operation while CPU was blocked.");
            }
        }

        // RESG forces r/w pin to read (high)
        self.core.rw |= self.core.signals.RESG;
    }

    fn execute_internal(&mut self) -> Option<StepCtl> {
        // --- Fetch & Decode Opcode Phase
        match self.core.state {
            CPUState::Fetch => {
                // phase two of fetch
                // In case of res full hijack, brk_done isn't cleared by microp
                self.core.signals.brk_done = false;
                // Service Interrupts
                if !self.core.signals.D1x1 {
                    self.core.ir = 0;
                } else {
                    self.core.ir = self.core.data_bus;
                    self.core.pc = self.core.pc.wrapping_add(1);
                }

                let instr = self.decoder.decode(self.core.ir).unwrap_or_else(|| {
                    panic!(
                        "Decode failed for opcode: ${:02X} at PC=${:04X}",
                        self.core.ir,
                        self.core.pc.wrapping_sub(1)
                    )
                });

                self.core.instr = instr;
                if self.core.instr.name.eq("JAM") {
                    self.core.pc = self.core.pc.wrapping_sub(1);
                }

                self.core.micro_iter = Some(self.core.instr.pipeline());
                self.core.state = CPUState::Exec;
                None
            }

            CPUState::Exec => {
                // micro-op already loaded into field while fetching external operation
                // execute directly from field
                Some(
                    self.core
                        .micro_op
                        .expect("No micro_op to execute")
                        .execute(&mut self.core),
                )
            }

            CPUState::Jammed | CPUState::Blocked(_) | CPUState::Reset => {
                // No internal operations performed
                None
            }
        }
    }

    pub fn drive_bus(&mut self, bus: &mut Bus) {
        if self.core.rw {
            self.core.data_bus = bus.read(self.core.addr_bus);
        } else {
            bus.write(self.core.addr_bus, self.core.data_bus);
        }
    }

    pub fn phi2(&mut self, bus: &mut Bus) {
        // synchronise external lines
        self.core.signals.RES_sync = bus.res;
        self.core.signals.NMIP = !bus.nmi;
        self.core.signals.IRQ_sync = !bus.irq;

        match (self.core.rw, bus.rdy, &self.core.state) {
            // Read cycle with RDY low — block and elongate
            (true, false, state) if !matches!(state, CPUState::Blocked(_)) => {
                self.core.state = self.core.state.block();
                self.core.signals.ph2();
                return;
            }
            // Still blocked, RDY still low — keep waiting
            (_, false, CPUState::Blocked(_)) => {
                self.core.signals.ph2();
                return;
            }
            // RDY released — resume
            (_, true, CPUState::Blocked(_)) => {
                self.core.state = self.core.state.unblock();
            }
            // Normal cycle
            _ => {}
        }

        // Drive bus
        if self.core.rw {
            self.core.data_bus = bus.read(self.core.addr_bus);
        } else {
            bus.write(self.core.addr_bus, self.core.data_bus);
        }

        // --- Fetch & Decode Opcode Phase
        loop {
            let Some(ctrl) = self.execute_internal() else {
                break;
            };

            match ctrl {
                StepCtl::Next => {
                    break;
                }

                StepCtl::End => {
                    self.core.micro_iter = None;
                    self.core.signals.poll_int = true;

                    // Don't poll interrupts at instruction boundary for branch taken without page cross case
                    // Already polled at T2
                    if self.core.signals.branch_T3 {
                        self.core.signals.poll_int = false;
                        self.core.signals.branch_T3 = false;
                    }
                    self.core.state = CPUState::Fetch;
                    break;
                }

                StepCtl::Skip(n) => {
                    for _ in 0..n {
                        self.core.micro_iter.as_mut().unwrap().next(); // skip fake stall micro-op
                    }
                    break;
                }

                StepCtl::Merge => {
                    #[rustfmt::skip]
                    let micro = self.core.micro_iter
                        .as_mut()
                        .unwrap()
                        .next()
                        .expect("Iterator ended on Merge without StepCtl::End");

                    assert!(
                        matches!(micro.bus_spec(), BusOpSpec::Internal),
                        "Micro-op following a clocked Merge must be Internal. IR=${:02X} PC=${:04X}",
                        self.core.ir,
                        self.core.pc
                    );
                    self.core.micro_op = Some(micro);
                    continue;
                }

                StepCtl::SkipMerge => {
                    self.core.micro_iter.as_mut().unwrap().next(); // skip fake stall micro-op
                    #[rustfmt::skip]
                    let micro = self.core.micro_iter
                        .as_mut()
                        .unwrap()
                        .next()
                        .expect("Iterator ended on Merge without StepCtl::End");

                    assert!(
                        matches!(micro.bus_spec(), BusOpSpec::Internal),
                        "Micro-op following a clocked Merge must be Internal. IR=${:02X} PC=${:04X}",
                        self.core.ir,
                        self.core.pc
                    );
                    self.core.micro_op = Some(micro);
                    continue;
                }
            }
        }

        self.core.signals.ph2();
    }
}
