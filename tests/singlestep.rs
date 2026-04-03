// Uses https://github.com/SingleStepTests/ProcessorTests
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

#[derive(Debug, Deserialize, Serialize)]
struct TestCase {
    name: String,
    initial: CpuState,
    #[serde(rename = "final")]
    final_state: CpuState,
    cycles: Vec<(u16, u8, String)>,
}

#[derive(Debug, Deserialize, Serialize)]
struct CpuState {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    ram: Vec<(u16, u8)>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    use mos65x::core::bus::BusOp;
    use mos65x::core::cpu::test_utils::{CPUState, Status};
    use mos65x::core::variants::{Decoder, NMOS_6502, NMOS_6502_FULL, Quirks, RICOH_2A03};
    use mos65x::generic_system::BoxSystem;

    // ── Variant selection ────────────────────────────────────────────────────

    #[allow(dead_code)]
    pub enum Variant {
        Nmos,
        NmosFull,
        Ricoh,
    }

    // Default variant for the main test suites
    #[allow(dead_code)]
    const DEFAULT_VARIANT: Variant = Variant::Ricoh;

    // ── Test entry points ────────────────────────────────────────────────────
    #[test]
    fn test_opcode() {
        let raw = std::env::args()
            .last() // safest with test harness
            .expect("Provide a hex number");

        let raw = raw.strip_prefix("0x").unwrap_or(&raw);

        let case_num = u32::from_str_radix(raw, 16).expect("Invalid hex number");

        let file_path = format!("tests/6502/v1/{:02x}.json", case_num);

        run_test_suite_from_file(&file_path, &Variant::NmosFull);
    }

    #[test]
    fn test_nmos_full() {
        single_step_suite(Variant::NmosFull);
    }

    #[test]
    fn test_nmos_base() {
        single_step_suite(Variant::Nmos);
    }

    #[test]
    fn test_ricoh() {
        single_step_suite(Variant::Ricoh);
    }

    fn single_step_suite(variant: Variant) {
        let exclude_illegal: HashSet<u8> = [
            0x02, 0x03, 0x04, 0x07, 0x0B, 0x0C, 0x0F, 0x12, 0x13, 0x14, 0x17, 0x1A, 0x1B, 0x1C,
            0x1F, 0x22, 0x23, 0x27, 0x2B, 0x2F, 0x32, 0x33, 0x34, 0x37, 0x3A, 0x3B, 0x3C, 0x3F,
            0x42, 0x43, 0x44, 0x47, 0x4A, 0x4B, 0x4C, 0x4F, 0x52, 0x53, 0x54, 0x57, 0x5A, 0x5B,
            0x5C, 0x5F, 0x62, 0x63, 0x64, 0x67, 0x6B, 0x6F, 0x72, 0x73, 0x74, 0x77, 0x7A, 0x7B,
            0x7C, 0x7F, 0x81, 0x82, 0x83, 0x87, 0x89, 0x8B, 0x8F, 0x92, 0x93, 0x97, 0x9B, 0x9C,
            0x9E, 0x9F, 0xA3, 0xA7, 0xAB, 0xAF, 0xB2, 0xB3, 0xB7, 0xBB, 0xBF, 0xC2, 0xC3, 0xC7,
            0xCB, 0xCF, 0xD2, 0xD3, 0xD4, 0xD7, 0xDA, 0xDB, 0xDC, 0xDF, 0xE2, 0xE3, 0xE7, 0xEB,
            0xEF, 0xF2, 0xF3, 0xF4, 0xF7, 0xFA, 0xFB, 0xFC, 0xFF,
        ]
        .into_iter()
        .collect();

        let (dir, exclude): (&str, Option<&HashSet<u8>>) = match variant {
            Variant::Nmos => ("tests/6502/v1", Some(&exclude_illegal)),
            Variant::NmosFull => ("tests/6502/v1", None),
            Variant::Ricoh => ("tests/nes6502/v1", None),
        };

        for opcode in 0x00..=0xFF {
            if let Some(ex) = exclude {
                if ex.contains(&opcode) {
                    continue;
                }
            }

            let filename = format!("{}/{:02x}.json", dir, opcode);

            if !std::path::Path::new(&filename).exists() {
                // Optional: skip silently or log
                continue;
            }

            run_test_suite_from_file(&filename, &variant);
        }
    }

    // ── Setup ────────────────────────────────────────────────────────────────

    fn load_test_cases<P: AsRef<Path>>(path: P) -> Vec<TestCase> {
        let contents = fs::read_to_string(path).expect("Failed to read test file");
        serde_json::from_str(&contents).expect("Failed to parse JSON")
    }

    fn setup_emulator<V: Decoder + Quirks>(state: &CpuState, variant: V) -> BoxSystem<V> {
        let mut system = BoxSystem::new(variant);
        system.attach_ram(0x0000, 0x10000, 1);

        // Reset sequence
        for _ in 0..7 {
            system.tick();
        }

        // Set initial CPU state directly
        system.cpu.core.pc = state.pc;
        system.cpu.core.sp.value = state.s;
        system.cpu.core.a = state.a;
        system.cpu.core.x = state.x;
        system.cpu.core.y = state.y;
        system.cpu.core.flags = Status::from_bits_retain(state.p);
        system.cpu.core.state = CPUState::Fetch;

        for &(addr, value) in &state.ram {
            system.bus.write_raw(addr, value);
        }

        system
    }

    // ── Failure report ───────────────────────────────────────────────────────

    fn print_failure<V: Decoder + Quirks>(
        test: &TestCase,
        system: &mut BoxSystem<V>,
        actual_ops: &[BusOp],
        error: &str,
    ) {
        println!("╔═══════════════════════════════════════════════════════════════╗");
        println!("║                    TEST FAILURE REPORT                        ║");
        println!("╚═══════════════════════════════════════════════════════════════╝");
        println!("\n📋 Test: {}", test.name);
        println!("❌ Error: {}\n", error);

        // CPU state — use snapshot so we never re-run with a different variant
        let cpu = &system.cpu.core;
        let exp = &test.final_state;

        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                       CPU STATE                             │");
        println!("├─────────────────────────────────────────────────────────────┤");
        cmp_reg(
            "PC",
            format!("${:04X}", exp.pc),
            format!("${:04X}", cpu.pc),
            exp.pc == cpu.pc,
        );
        cmp_reg(
            "SP",
            format!("${:02X}", exp.s),
            format!("${:02X}", cpu.sp.value),
            exp.s == cpu.sp.value,
        );
        cmp_reg(
            "A",
            format!("${:02X}", exp.a),
            format!("${:02X}", cpu.a),
            exp.a == cpu.a,
        );
        cmp_reg(
            "X",
            format!("${:02X}", exp.x),
            format!("${:02X}", cpu.x),
            exp.x == cpu.x,
        );
        cmp_reg(
            "Y",
            format!("${:02X}", exp.y),
            format!("${:02X}", cpu.y),
            exp.y == cpu.y,
        );
        cmp_reg(
            "P",
            format!("${:02X} ({:08b})", exp.p, exp.p),
            format!("${:02X} ({:08b})", cpu.flags.bits(), cpu.flags.bits()),
            exp.p == cpu.flags.bits(),
        );
        println!("└─────────────────────────────────────────────────────────────┘\n");

        // RAM state
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                       RAM STATE                             │");
        println!("├─────────────────────────────────────────────────────────────┤");
        for &(addr, expected_val) in &exp.ram {
            let actual_val = system.bus.read_raw(addr);
            cmp_reg(
                &format!("[${:04X}]", addr),
                format!("${:02X}", expected_val),
                format!("${:02X}", actual_val),
                expected_val == actual_val,
            );
        }
        println!("└─────────────────────────────────────────────────────────────┘\n");

        // Bus cycles
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                      BUS CYCLES                             │");
        println!("├─────────────────────────────────────────────────────────────┤");
        let max = std::cmp::max(test.cycles.len(), actual_ops.len());
        for i in 0..max {
            match (test.cycles.get(i), actual_ops.get(i)) {
                (Some((ea, ev, eo)), Some(act)) => {
                    let (aa, av, ao) = bus_op_parts(act);
                    let ok = ea == &aa && ev == &av && eo == ao;
                    println!(
                        "│ {} Cycle {:2} │ Expected: {:5} ${:04X} = ${:02X}  │  Actual: {:5} ${:04X} = ${:02X} │",
                        if ok { "✓" } else { "✗" },
                        i,
                        eo,
                        ea,
                        ev,
                        ao,
                        aa,
                        av
                    );
                }
                (Some((ea, ev, eo)), None) => {
                    println!(
                        "│ ✗ Cycle {:2} │ Expected: {:5} ${:04X} = ${:02X}  │  Actual: MISSING              │",
                        i, eo, ea, ev
                    );
                }
                (None, Some(act)) => {
                    let (aa, av, ao) = bus_op_parts(act);
                    println!(
                        "│ ✗ Cycle {:2} │ Expected: NONE                     │  Actual: {:5} ${:04X} = ${:02X} │",
                        i, ao, aa, av
                    );
                }
                (None, None) => break,
            }
        }
        println!("└─────────────────────────────────────────────────────────────┘\n");
    }

    fn bus_op_parts(op: &BusOp) -> (u16, u8, &'static str) {
        match op {
            BusOp::Read(a, v) => (*a, *v, "read"),
            BusOp::Write(a, v) => (*a, *v, "write"),
            BusOp::Internal => (0, 0, "internal"),
        }
    }

    fn cmp_reg(name: &str, expected: String, actual: String, ok: bool) {
        println!(
            "│ {} {:8} │ Expected: {:20} │ Actual: {:20} │",
            if ok { "✓" } else { "✗" },
            name,
            expected,
            actual
        );
    }

    // ── Core test runner ─────────────────────────────────────────────────────
    enum TestOutcome<V: Decoder + Quirks> {
        Pass,
        Fail {
            error: String,
            system: BoxSystem<V>,
            bus_ops: Vec<BusOp>,
        },
    }

    fn run_test_with<V: Decoder + Quirks>(test: &TestCase, variant: V) -> TestOutcome<V> {
        let mut system = setup_emulator(&test.initial, variant);
        let mut bus_ops = Vec::with_capacity(test.cycles.len());

        for _ in 0..test.cycles.len() {
            bus_ops.push(system.tick());
        }

        let cpu = &system.cpu.core;
        let exp = &test.final_state;

        let error = 'check: {
            if cpu.pc != exp.pc {
                break 'check Some(format!(
                    "PC mismatch - expected ${:04X}, got ${:04X}",
                    exp.pc, cpu.pc
                ));
            }
            if cpu.sp.value != exp.s {
                break 'check Some(format!(
                    "SP mismatch - expected ${:02X}, got ${:02X}",
                    exp.s, cpu.sp.value
                ));
            }
            if cpu.a != exp.a {
                break 'check Some(format!(
                    "A mismatch - expected ${:02X}, got ${:02X}",
                    exp.a, cpu.a
                ));
            }
            if cpu.x != exp.x {
                break 'check Some(format!(
                    "X mismatch - expected ${:02X}, got ${:02X}",
                    exp.x, cpu.x
                ));
            }
            if cpu.y != exp.y {
                break 'check Some(format!(
                    "Y mismatch - expected ${:02X}, got ${:02X}",
                    exp.y, cpu.y
                ));
            }
            if cpu.flags.bits() != exp.p {
                break 'check Some(format!(
                    "P mismatch - expected ${:02X}, got ${:02X}",
                    exp.p,
                    cpu.flags.bits()
                ));
            }

            for &(addr, expected_val) in &exp.ram {
                let actual_val = system.bus.read_raw(addr);
                if actual_val != expected_val {
                    break 'check Some(format!(
                        "RAM[${:04X}] mismatch - expected ${:02X}, got ${:02X}",
                        addr, expected_val, actual_val
                    ));
                }
            }

            if bus_ops.len() != test.cycles.len() {
                break 'check Some(format!(
                    "Cycle count mismatch - expected {}, got {}",
                    test.cycles.len(),
                    bus_ops.len()
                ));
            }

            for (i, (actual, (exp_addr, exp_val, exp_op))) in
                bus_ops.iter().zip(test.cycles.iter()).enumerate()
            {
                let (act_addr, act_val, act_op) = bus_op_parts(actual);
                if exp_op != act_op {
                    break 'check Some(format!(
                        "Cycle {} op mismatch - expected '{}', got '{}'",
                        i, exp_op, act_op
                    ));
                }
                if exp_addr != &act_addr {
                    break 'check Some(format!(
                        "Cycle {} addr mismatch - expected ${:04X}, got ${:04X}",
                        i, exp_addr, act_addr
                    ));
                }
                if exp_val != &act_val {
                    break 'check Some(format!(
                        "Cycle {} value mismatch - expected ${:02X}, got ${:02X}",
                        i, exp_val, act_val
                    ));
                }
            }

            None
        };

        match error {
            None => TestOutcome::Pass,
            Some(error) => TestOutcome::Fail {
                error,
                system,
                bus_ops,
            },
        }
    }

    // ── Public test suite runner ──────────────────────────────────────────────

    pub fn run_test_suite_from_file(file_path: &str, variant: &Variant) {
        let test_cases = load_test_cases(file_path);
        println!("Running {} tests from {}", test_cases.len(), file_path);

        for (i, test) in test_cases.iter().enumerate() {
            print!("  [{}/{}] {} ... ", i + 1, test_cases.len(), test.name);

            match match variant {
                Variant::Nmos => run_test_with(test, NMOS_6502),
                Variant::Ricoh => run_test_with(test, RICOH_2A03),
                Variant::NmosFull => run_test_with(test, NMOS_6502_FULL),
            } {
                TestOutcome::Pass => println!("✅"),
                TestOutcome::Fail {
                    error,
                    mut system,
                    bus_ops,
                } => {
                    println!("❌\n");
                    print_failure(test, &mut system, &bus_ops, &error);
                    panic!("Test suite stopped at first failure");
                }
            }
        }
        println!("\n🎉 All {} tests passed!", test_cases.len());
    }
}
