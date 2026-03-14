use crate::core::bus::Bus;
use crate::core::microcode::{BusOpSpec, Instruction, StepCtl};
use crate::core::variants::{Decoder, Quirks};

use super::{CPU, CPUCore, CPUState, Signals, StackPointer, Status};

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
