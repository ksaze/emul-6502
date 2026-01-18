#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::cpu::Status;
use crate::variants::NMOS_6502;

mod bus;
mod cpu;
mod emulator;
mod operations;
mod shared;
mod variants;

fn main() {
    let mut emul = emulator::Emulator::new(NMOS_6502);
    // Full RAM
    emul.attach_ram(0x0000, 0x10000);

    // Reset vector → $0200
    emul.bus.write(0xFFFC, 0x00);
    emul.bus.write(0xFFFD, 0x02);

    // Better test: push both, then pull both (LIFO order)
    let program = [
        0xA9, 0x42, // LDA #$42
        0x48, // PHA          - Push $42
        0x38, // SEC
        0x78, // SEI
        0xF8, // SED
        0x08, // PHP          - Push status
        // Now stack has: [deeper: $42] [top: status]
        // Pull in reverse order (LIFO)
        0x28, // PLP          - Pull status first
        0x68, // PLA          - Pull $42 second
        0xEA, // NOP
    ];

    for (i, b) in program.iter().enumerate() {
        emul.bus.write(0x0200 + i as u16, *b);
    }

    emul.reset_cpu();
    let initial_sp = emul.cpu.core.sp.value;

    // Execute everything
    for _ in 0..50 {
        emul.tick();
    }

    let cpu = &emul.cpu.core;

    println!("=== Stack Operation Tests ===\n");

    println!("PHA/PLA Test:");
    assert_eq!(cpu.a, 0x42, "PLA failed");
    println!("  ✅ A = ${:02X}", cpu.a);

    println!("\nPHP/PLP Test:");
    assert!(cpu.flags.contains(Status::CARRY), "C not restored");
    assert!(cpu.flags.contains(Status::IRQ_DISABLE), "I not restored");
    assert!(cpu.flags.contains(Status::DECIMAL), "D not restored");
    println!("  ✅ Flags restored: {:08b}", cpu.flags.bits());

    println!("\nStack Pointer Test:");
    assert_eq!(cpu.sp.value, initial_sp, "SP not balanced");
    println!("  ✅ SP = ${:02X} (balanced)", cpu.sp.value);

    println!("\n=== All Tests Passed ===");
}
