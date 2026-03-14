use mos65x::core::bus::{BusOp, Device, EmulatorControl};
use mos65x::core::variants::{Decoder, NMOS_6502, Quirks};
use mos65x::emulator::Emulator;
use mos65x::shared::SharedDevice;

fn bus_op_debug(op: BusOp) {
    match op {
        BusOp::Read(addr, data) => {
            println!("read {:#06X} {:#04X}", addr, data);
        }
        BusOp::Write(addr, data) => {
            println!("write {:#06X} {:#04X}", addr, data);
        }
        BusOp::Internal => {
            println!("Internal");
        }
    }
}

fn setup_emulator<V>(
    variant: V,
    bin: &[u8],
    load_addr: u16,
) -> (Emulator<V>, SharedDevice<EmulatorControl>)
where
    V: Decoder + Quirks,
{
    let mut emul = Emulator::new(variant);
    emul.attach_ram(0x0000, 0x10000);

    for (i, b) in bin.iter().enumerate() {
        emul.bus.write_raw(load_addr + i as u16, *b);
    }

    emul.bus.write_raw(0xFFFC, (load_addr & 0xFF) as u8);
    emul.bus.write_raw(0xFFFD, (load_addr >> 8) as u8);

    for _ in 0..7 {
        emul.tick();
    }

    let ctrl = EmulatorControl::new().into_shared();
    emul.bus.attach_shared_device(&ctrl, 0xFFFF, 0x0);

    (emul, ctrl)
}

fn run_cycles<V, Fa, Fb>(
    emul: &mut Emulator<V>,
    ctrl: &SharedDevice<EmulatorControl>,
    cycles: usize,
    mut before_tick: Fa,
    mut after_tick: Fb,
) where
    V: Decoder + Quirks,
    Fa: FnMut(usize, &mut Emulator<V>, &SharedDevice<EmulatorControl>),
    Fb: FnMut(usize, &mut Emulator<V>, &SharedDevice<EmulatorControl>),
{
    for cycle in 0..cycles {
        before_tick(cycle, emul, ctrl);

        let op = emul.tick();

        println!("Cycle {}", cycle);
        println!(
            "pc: {:#4X}, Exec {}",
            emul.cpu.core.pc, emul.cpu.core.instr.name
        );
        bus_op_debug(op);
        println!("{}", emul.cpu.core.signals.trace());
        println!("----------------------");

        // Capture current cycle's debug output before call in case some assertion fails.
        after_tick(cycle, emul, ctrl);
    }
}

// With instructions which modify the IRQ_DISABLE flag in their last cycle (CLI, SET, PLP),
// The effect of clearing/setting the flag is delayed.
// This is due to doIRQ (the pending IRQ/NMI flag) being set in the first phase with
// doIRQ.phi1 = NMIG.phi1 | (IRQP.phi1 &  ~IRQ_DISABLE.phi2);
// but the IRQ_DISABLE is set/cleared by these instructions in phase 2, thus their response is delayed
#[test]
fn delayed_irq_response() {
    let program = [
        0xE8, // INX
        0x58, // CLI
        0xE6, 0x0F, // INC zp
        0x78, // SEI
        0x69, // ADC #
    ];

    let load_addr = 0x0010;
    let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

    const TOTAL_CYCLES: usize = 10;
    const IRQ_ASSERT_START: usize = 1;

    run_cycles(
        &mut emul,
        &ctrl,
        TOTAL_CYCLES,
        |cycle, _emul, ctrl| {
            ctrl.borrow_mut().irq_line = cycle < IRQ_ASSERT_START;
        },
        |cycle, emul, _ctrl| {
            if cycle == IRQ_ASSERT_START + 1 {
                assert!(
                    emul.cpu.core.signals.IRQP,
                    "IRQ should be latched in the cycle after IRQ pin is pulled low",
                );

                assert!(
                    !emul.cpu.core.signals.doIRQ,
                    "IRQP must not trigger next phase (doIRQ)",
                );
            }
        },
    );

    assert_eq!(
        emul.cpu.core.ir, 0x0,
        "CPU should be servicing IRQ one instruction later"
    );
    assert!(
        !emul.cpu.core.signals.D1x1,
        "CPU should be servicing IRQ one instruction later"
    );
}

// IRQ delayed by branch from
// https://www.nesdev.org/wiki/Visual6502wiki/6502_Timing_of_Interrupt_Handling
#[test]
fn branch_delay() {
    let program = [
        0x58, // CLI
        0xE8, // INX
        0xD0, 0xFE, // BEQ offset (0 offset)
    ];

    let load_addr: u16 = 0x0010;
    let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

    const TOTAL_CYCLES: usize = 11;
    const IRQ_ASSERT_START: usize = 5;

    // Branch instruction offset is 0
    const FIRST_BRANCH_T3: usize = 6;

    run_cycles(
        &mut emul,
        &ctrl,
        TOTAL_CYCLES,
        |cycle, _emul, ctrl| {
            ctrl.borrow_mut().irq_line = cycle < IRQ_ASSERT_START;
        },
        |cycle, emul, _ctrl| {
            if cycle == FIRST_BRANCH_T3 {
                // IRQ is not serviced at instruction boundary even though doIRQ is set for branch taken (no page cross)
                // For branch without page cross, interrupts are serviced at the end of T2.
                // So in this case when IRQ is pulled low during T2, doIRQ is set in the following phi1.
                // At the instruction boundary, interrupts aren't set hence INTG is never set. Hence IRQ will be delayed by one instruction.
                assert!(
                    emul.cpu.core.signals.doIRQ,
                    "doIRQ must be set at branch instruction boundary"
                );

                assert!(
                    emul.cpu.core.signals.D1x1,
                    "Interrupt should not be serviced during T3"
                );
            }
        },
    );

    // IRQ serviced one instruction later
    assert_eq!(
        emul.cpu.core.ir, 0x0000,
        "IRQ must be serviced one instruction later"
    );
}

// After interrupt vectors are fetched, one instruction is always executed.
// Even if a nmi pulse is sent again, it will be serviced one instruction later.
#[test]
fn consecutive_nmi() {
    let program = [
        0xE8, // INX
        0x58, // CLI
        0xE6, 0x0F, // INC zp
        0x78, // SEI
        0x69, // ADC #
    ];

    let load_addr: u16 = 0x0010;
    let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

    // NMI vector → $FFFA
    emul.bus.write_raw(0xFFFA, 0x10);
    emul.bus.write_raw(0xFFFB, 0x00);

    const TOTAL_CYCLES: usize = 12;
    const FIRST_NMI_ASSERT: usize = 1;
    const BRK_T6: usize = 9;

    run_cycles(
        &mut emul,
        &ctrl,
        TOTAL_CYCLES,
        |cycle, _emul, ctrl| {
            ctrl.borrow_mut().nmi_line = cycle != FIRST_NMI_ASSERT && cycle < BRK_T6;
        },
        |_cycle, _emul, _ctrl| {},
    );

    // NMI pin was pulled low during T6 of BRK
    // NMIG & NMIP set, but not in BRK
    assert!(emul.cpu.core.signals.NMIP);
    assert!(emul.cpu.core.signals.NMIG);
    assert_ne!(
        emul.cpu.core.ir, 0x0,
        "Atleast one instruction must be executed before servicing another interrupt"
    );
}

// Late NMI and Later NMI adapated from
// https://www.nesdev.org/wiki/Visual6502wiki/6502_Timing_of_Interrupt_Handling
#[test]
fn nmi_hijack() {
    fn run_case(nmi_assert_cycle: usize) -> u16 {
        let program = [
            0xE8, // INX
            0x58, // CLI
            0xE6, 0x0F, // INC zp
            0x78, // SEI
            0x69, // ADC #
        ];

        let load_addr: u16 = 0x0010;
        let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

        // NMI vector → $FFFA
        emul.bus.write_raw(0xFFFA, 0xFA);
        emul.bus.write_raw(0xFFFB, 0xFF);

        // IRQ vector → $FFFE
        emul.bus.write_raw(0xFFFE, 0xFE);
        emul.bus.write_raw(0xFFFF, 0xFF);

        const TOTAL_CYCLES: usize = 16;
        const IRQ_ASSERT_START: usize = 4;

        run_cycles(
            &mut emul,
            &ctrl,
            TOTAL_CYCLES,
            // PRE-TICK: drive lines
            |cycle, _emul, ctrl| {
                ctrl.borrow_mut().irq_line = cycle < IRQ_ASSERT_START;
                ctrl.borrow_mut().nmi_line = cycle < nmi_assert_cycle;
            },
            // POST-TICK: optional mid asserts
            |cycle, emul, _ctrl| {
                // IRQ must latch before arbitration
                if cycle == IRQ_ASSERT_START + 1 {
                    assert!(
                        emul.cpu.core.signals.IRQP,
                        "IRQ should latch before arbitration window"
                    );
                }
            },
        );

        emul.cpu.core.pc
    }

    // Hijack possible if asserted before or during T4
    const BRK_T4: usize = 12;

    // Case 1: Early enough → NMI hijacks IRQ
    let hijack_pc = run_case(BRK_T4);
    assert_eq!(
        hijack_pc, 0xFFFA,
        "NMI before IRQ T4 should hijack and pull NMI vector"
    );

    println!("------x---CASE COMPLETION---x--------");

    // Case 2: Too late (After T4) → IRQ wins
    let fail_pc = run_case(BRK_T4 + 1);
    assert_eq!(fail_pc, 0xFFFE, "NMI after IRQ T4 should NOT hijack");
}

// Lost NMI: Adapted from
// https://www.nesdev.org/wiki/Visual6502wiki/6502_Timing_of_Interrupt_Handling
#[test]
fn lost_nmi() {
    let program = [
        0xE8, // INX
        0x58, // CLI
        0xE6, 0x0F, // INC zp
        0x78, // SEI
        0x69, // ADC #
    ];

    let load_addr: u16 = 0x0010;
    let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

    // NMI vector → $FFFA
    emul.bus.write_raw(0xFFFA, 0xFA);
    emul.bus.write_raw(0xFFFB, 0xFF);

    // IRQ vector → $0020
    emul.bus.write_raw(0xFFFE, 0x20);
    emul.bus.write_raw(0xFFFF, 0x00);

    emul.bus.write_raw(0x20, 0xE8); // INX
    emul.bus.write_raw(0x21, 0x40); // RTI

    const TOTAL_CYCLES: usize = 24;
    const IRQ_ASSERT_START: usize = 4;
    const BRK_T5: usize = 13;
    const IRQ_END: usize = 15;

    run_cycles(
        &mut emul,
        &ctrl,
        TOTAL_CYCLES,
        // PRE-TICK: drive lines
        |cycle, _emul, ctrl| {
            ctrl.borrow_mut().irq_line = cycle < IRQ_ASSERT_START;
            ctrl.borrow_mut().nmi_line = cycle != BRK_T5 && cycle != (BRK_T5 + 1);
        },
        // POST-TICK: optional mid asserts
        |cycle, emul, _ctrl| {
            // IRQ must latch before arbitration
            if cycle == IRQ_ASSERT_START + 1 {
                assert!(
                    emul.cpu.core.signals.IRQP,
                    "IRQ should latch before arbitration window"
                );
            }

            if cycle == IRQ_END {
                // NMI not triggered
                assert!(
                    !emul.cpu.core.signals.NMIG,
                    "NMI pulse during VEC pull is not latched."
                );
                assert_eq!(
                    emul.cpu.core.pc, 0x0020,
                    "IRQ vector must be used. No NMI hijack.",
                );
            }
        },
    );

    // Vector from RTI, not NMI
    assert_eq!(emul.cpu.core.pc, 0x0014);
    assert!(
        !emul.cpu.core.signals.NMIG,
        "NMI pulse during VEC pull phases is lost"
    );
}

// Thwarted full-hijack and succesful half-hijack of an IRQ by RES. Adapted from:
// https://www.nesdev.org/wiki/Visual6502wiki/6502_Interrupt_Hijacking#NMI_Hijacking_IRQ/BRK
#[test]
fn res_hijack() {
    fn run_case(res_assert_cycle: usize) -> u8 {
        let program = [
            0x58, // CLI
            0xE8, // INX
            0xEA, // NOP
            0xEA, // NOP
        ];

        let load_addr: u16 = 0x0020;
        let (mut emul, ctrl) = setup_emulator(NMOS_6502, &program, load_addr);

        // IRQ vector → $FFFE
        emul.bus.write_raw(0xFFFE, 0xFE);
        emul.bus.write_raw(0xFFFF, 0xFF);

        // Target of full hijack
        emul.bus.write_raw(0x20FD, 0x4C);

        // Target of half hijack
        emul.bus.write_raw(0x00FE, 0x4D);

        const TOTAL_CYCLES: usize = 14;
        const IRQ_ASSERT: usize = 4;

        run_cycles(
            &mut emul,
            &ctrl,
            TOTAL_CYCLES,
            // PRE-TICK
            |cycle, _emul, ctrl| {
                ctrl.borrow_mut().irq_line = cycle != IRQ_ASSERT;
                ctrl.borrow_mut().res_line = cycle != res_assert_cycle;
            },
            // POST-TICK
            |cycle, emul, _ctrl| {
                if cycle == IRQ_ASSERT + 1 {
                    assert!(
                        emul.cpu.core.signals.IRQP,
                        "IRQ must latch before RESET arbitration"
                    );
                }

                if cycle == res_assert_cycle + 1 {
                    assert!(emul.cpu.core.signals.RESG, "RESET must be synchronised");
                }
            },
        );

        emul.cpu.core.ir
    }

    // Full hijack before or at T4
    const BRK_T4: usize = 9;

    // RESET early enough (before T5) → Vec hi pull replaced by fetch of bus.read(sp) << 4 || 0xFD
    let full_ir = run_case(BRK_T4);

    // Vec high read replaced with fetch cycle
    assert_eq!(
        full_ir, 0x4C,
        "Full RESET hijack should replace IRQ vector high fetch"
    );

    println!("------x---CASE COMPLETION---x--------");

    // RESET after T4 → Vec low from IRQ (0xFFFE), Vec high from RES (0xFFFD)
    let half_ir = run_case(BRK_T4 + 1);

    assert_eq!(
        half_ir, 0x4D,
        "Half RESET hijack should produce mixed vector read"
    );
}
