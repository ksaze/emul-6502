// Uses https://github.com/Klaus2m5/6502_65C02_functional_tests

#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use mos65x::cpu::CPUState;
use mos65x::emulator::Emulator;
use mos65x::shared::Word;
use mos65x::variants::NMOS_6502;

use std::fs::read;

#[test]
fn klaus_6502_functional_test() {
    let bin = read("tests/roms/6502_functional_test.bin")
        .expect("failed to load Klaus functional test binary");

    let mut emul = Emulator::new(NMOS_6502);

    // Full 64K RAM
    emul.attach_ram(0x0000, 0x10000);

    let load_addr: u16 = 0x0000;

    for (i, b) in bin.iter().enumerate() {
        emul.bus.write_raw(load_addr + (i as u16), *b);
    }

    // Reset vector → $0400
    emul.bus.write_raw(0xFFFC, 0x00);
    emul.bus.write_raw(0xFFFD, 0x04);

    let mut is_fetch_cycle = true;
    let mut prev_instr_pc = 0x400;
    let mut same_pc_counter = 0;
    for _ in 0..1_000_000_000 {
        emul.tick();

        if is_fetch_cycle {
            if emul.cpu.core.pc == prev_instr_pc {
                same_pc_counter += 1;

                if same_pc_counter > 100 {
                    break;
                }
            }

            prev_instr_pc = emul.cpu.core.pc;
            is_fetch_cycle = false;
        }

        if emul.cpu.core.state == CPUState::Fetch {
            is_fetch_cycle = true;
        }
    }

    // PC is incremented after fetch. Change back to address of last instruction.
    let final_pc = emul.cpu.core.pc - 1;
    assert!(
        final_pc == 0x3469,
        "Emulator didn't stop at success address. ❌ "
    );

    println!("Klaus Functional Test passed. ✅ ");
}

#[test]
fn bruce_clark_decimal_mode_test() {
    let bin = read("tests/roms/6502_decimal_test.bin")
        .expect("failed to load Klaus functional test binary");

    let mut emul = Emulator::new(NMOS_6502);

    // Full 64K RAM
    emul.attach_ram(0x0000, 0x10000);

    let load_addr: u16 = 0x0000;

    for (i, b) in bin.iter().enumerate() {
        emul.bus.write_raw(load_addr + (i as u16), *b);
    }

    // Reset vector → $0400
    emul.bus.write_raw(0xFFFC, 0x00);
    emul.bus.write_raw(0xFFFD, 0x04);

    const ERROR_ADDR: Word = 0x0B;

    let mut is_fetch_cycle = true;
    let mut prev_instr_pc = 0x400;
    let mut same_pc_counter = 0;
    for _ in 0..1_000_000_000 {
        emul.tick();

        if is_fetch_cycle {
            if emul.cpu.core.pc == prev_instr_pc {
                same_pc_counter += 1;

                if same_pc_counter > 100 {
                    break;
                }
            }

            prev_instr_pc = emul.cpu.core.pc;
            is_fetch_cycle = false;
        }

        if emul.cpu.core.state == CPUState::Fetch {
            is_fetch_cycle = true;
        }
    }

    assert!(
        emul.bus.read_raw(ERROR_ADDR) == 0,
        "Failed some decimal mode test"
    );

    println!("Bruce Clark Decimal Mode test passed. ✅ ");
}
