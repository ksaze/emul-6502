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
    use mos65x::core::bus::BusOp;
    use mos65x::core::cpu::test_utils::{CPUState, Status};
    use mos65x::core::variants::{Decoder, NMOS_6502, Quirks};
    use mos65x::generic_system::GenericSystem;

    fn load_test_cases<P: AsRef<Path>>(path: P) -> Vec<TestCase> {
        let contents = fs::read_to_string(path).expect("Failed to read test file");
        serde_json::from_str(&contents).expect("Failed to parse JSON")
    }

    fn setup_emulator<V: Decoder + Quirks>(state: &CpuState, variant: V) -> GenericSystem<V> {
        let mut emul = GenericSystem::new(variant);

        // Attach full RAM
        emul.attach_ram(0x0000, 0x10000);

        // Complete reset sequence
        for _ in 0..7 {
            emul.tick();
        }

        // Set initial CPU state
        emul.cpu.core.pc = state.pc;
        emul.cpu.core.sp.value = state.s;
        emul.cpu.core.a = state.a;
        emul.cpu.core.x = state.x;
        emul.cpu.core.y = state.y;
        emul.cpu.core.flags = Status::from_bits_truncate(state.p);

        // Load initial RAM state
        for &(addr, value) in &state.ram {
            emul.bus.write_raw(addr, value);
        }

        emul
    }

    fn verify_cpu_state<V: Decoder + Quirks>(
        emul: &GenericSystem<V>,
        expected: &CpuState,
        _test_name: &str,
    ) -> Result<(), String> {
        let cpu = &emul.cpu.core;

        if cpu.pc != expected.pc {
            return Err(format!(
                "PC mismatch - expected ${:04X}, got ${:04X}",
                expected.pc, cpu.pc
            ));
        }

        if cpu.sp.value != expected.s {
            return Err(format!(
                "SP mismatch - expected ${:02X}, got ${:02X}",
                expected.s, cpu.sp.value
            ));
        }

        if cpu.a != expected.a {
            return Err(format!(
                "A mismatch - expected ${:02X}, got ${:02X}",
                expected.a, cpu.a
            ));
        }

        if cpu.x != expected.x {
            return Err(format!(
                "X mismatch - expected ${:02X}, got ${:02X}",
                expected.x, cpu.x
            ));
        }

        if cpu.y != expected.y {
            return Err(format!(
                "Y mismatch - expected ${:02X}, got ${:02X}",
                expected.y, cpu.y
            ));
        }

        if cpu.flags.bits() != expected.p {
            return Err(format!(
                "P mismatch - expected ${:02X} ({:08b}), got ${:02X} ({:08b})",
                expected.p,
                expected.p,
                cpu.flags.bits(),
                cpu.flags.bits()
            ));
        }

        Ok(())
    }

    fn verify_ram_state<V: Decoder + Quirks>(
        emul: &mut GenericSystem<V>,
        expected: &CpuState,
        _test_name: &str,
    ) -> Result<(), String> {
        for &(addr, expected_value) in &expected.ram {
            let actual_value = emul.bus.read_raw(addr);
            if actual_value != expected_value {
                return Err(format!(
                    "RAM[${:04X}] mismatch - expected ${:02X}, got ${:02X}",
                    addr, expected_value, actual_value
                ));
            }
        }
        Ok(())
    }

    fn verify_cycles(
        actual_ops: &[BusOp],
        expected_cycles: &[(u16, u8, String)],
        _test_name: &str,
    ) -> Result<(), String> {
        if actual_ops.len() != expected_cycles.len() {
            return Err(format!(
                "Cycle count mismatch - expected {} cycles, got {} cycles",
                expected_cycles.len(),
                actual_ops.len()
            ));
        }

        for (i, (actual, expected)) in actual_ops.iter().zip(expected_cycles.iter()).enumerate() {
            let (exp_addr, exp_value, exp_op) = expected;

            match actual {
                BusOp::Read(addr, value) => {
                    if exp_op != "read" {
                        return Err(format!(
                            "Cycle {} operation mismatch - expected '{}', got 'read'",
                            i, exp_op
                        ));
                    }
                    if addr != exp_addr {
                        return Err(format!(
                            "Cycle {} address mismatch - expected ${:04X}, got ${:04X}",
                            i, exp_addr, addr
                        ));
                    }
                    if value != exp_value {
                        return Err(format!(
                            "Cycle {} value mismatch - expected ${:02X}, got ${:02X}",
                            i, exp_value, value
                        ));
                    }
                }
                BusOp::Write(addr, value) => {
                    if exp_op != "write" {
                        return Err(format!(
                            "Cycle {} operation mismatch - expected '{}', got 'write'",
                            i, exp_op
                        ));
                    }
                    if addr != exp_addr {
                        return Err(format!(
                            "Cycle {} address mismatch - expected ${:04X}, got ${:04X}",
                            i, exp_addr, addr
                        ));
                    }
                    if value != exp_value {
                        return Err(format!(
                            "Cycle {} value mismatch - expected ${:02X}, got ${:02X}",
                            i, exp_value, value
                        ));
                    }
                }
                BusOp::Internal => {
                    return Err(format!("Cycle {} mismatch. No operation found on Bus.", i));
                }
            }
        }

        Ok(())
    }

    fn run_test(test: &TestCase) -> Result<(), String> {
        let mut emul = setup_emulator(&test.initial, NMOS_6502);
        let mut bus_ops = Vec::new();

        // Execute instruction and collect bus operations
        let num_cycles = test.cycles.len();
        for _ in 0..num_cycles {
            let bus_op = emul.tick();
            bus_ops.push(bus_op);
        }

        verify_cpu_state(&emul, &test.final_state, &test.name)?;

        verify_ram_state(&mut emul, &test.final_state, &test.name)?;

        verify_cycles(&bus_ops, &test.cycles, &test.name)?;

        // Verify if instruction was actually completed
        assert!(emul.cpu.core.state == CPUState::Fetch);
        Ok(())
    }

    #[test]
    fn test_opcode() {
        run_test_suite_from_file("tests/6502/v1/7c.json");
    }

    #[test]
    fn single_step_suite() {
        let dir = "tests/6502/v1";

        // Exclude illegal 6502 opcodes
        #[rustfmt::skip]
        let exclude = &[
            "02", "03", "04", "07", "0b", "0c", "0f",
            "12", "13", "14", "17", "1a", "1b", "1c", "1f",
            "22", "23", "27", "2b", "2f",
            "32", "33", "34", "37", "3a", "3b", "3c", "3f",
            "42", "43", "44", "47", "4a", "4b", "4c", "4f",
            "52", "53", "54", "57", "5a", "5b", "5c", "5f",
            "62", "63", "64", "67", "6b", "6f",
            "72", "73", "74", "77", "7a", "7b", "7c", "7f",
            "81", "82", "83", "87", "89", "8b", "8f",
            "92", "93", "97", "9b", "9c", "9e", "9f",
            "a3", "a7", "ab", "af",
            "b2", "b3", "b7", "bb", "bf",
            "c2", "c3", "c7", "cb", "cf",
            "d2", "d3", "d4", "d7", "da", "db", "dc", "df",
            "e2", "e3", "e7", "eb", "ef",
            "f2", "f3", "f4", "f7", "fa", "fb", "fc", "ff",
        ];

        // let exclude: &[&str] = &[];

        for entry in std::fs::read_dir(dir).expect("Failed to read test directory") {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();

            if path.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }

            let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");

            if exclude.iter().any(|&e| e.eq_ignore_ascii_case(stem)) {
                println!("Skipping: {}", path.display());
                continue;
            }

            println!("Running: {}", path.display());
            let result = std::panic::catch_unwind(|| {
                run_test_suite_from_file(path.to_str().unwrap());
            });

            assert!(result.is_ok(), "FAILED: {}", path.display());
        }
    }

    pub fn run_test_suite_from_file(file_path: &str) {
        println!("Loading test suite from: {}", file_path);

        let test_cases = load_test_cases(file_path);

        println!("Found {} test cases\n", test_cases.len());

        for (index, test) in test_cases.iter().enumerate() {
            print!(
                "Test {}/{}: '{}' ... ",
                index + 1,
                test_cases.len(),
                test.name
            );

            match run_test(test) {
                Ok(()) => {
                    println!("✅ PASSED");
                }
                Err(error) => {
                    println!("❌ FAILED\n");

                    // Run test again to get actual state and bus ops
                    let mut emul = setup_emulator(&test.initial, NMOS_6502);
                    let mut actual_bus_ops = Vec::new();

                    for _ in 0..test.cycles.len() {
                        let bus_op = emul.tick();
                        actual_bus_ops.push(bus_op);
                    }

                    print_full_test_failure(test, &mut emul, &actual_bus_ops, &error);

                    panic!("Test suite stopped at first failure");
                }
            }
        }

        println!("\n🎉 All {} tests passed!", test_cases.len());
    }

    fn print_full_test_failure<V: Decoder + Quirks>(
        test: &TestCase,
        emul: &mut GenericSystem<V>,
        actual_bus_ops: &[BusOp],
        error: &str,
    ) {
        println!("╔═══════════════════════════════════════════════════════════════╗");
        println!("║                      TEST FAILURE REPORT                      ║");
        println!("╚═══════════════════════════════════════════════════════════════╝");
        println!("\n📋 Test: {}", test.name);
        println!("❌ Error: {}\n", error);

        // CPU State Comparison
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                        CPU STATE                            │");
        println!("├─────────────────────────────────────────────────────────────┤");

        let cpu = &emul.cpu.core;
        let exp = &test.final_state;

        print_register_comparison(
            "PC",
            format!("${:04X}", exp.pc),
            format!("${:04X}", cpu.pc),
            exp.pc == cpu.pc,
        );
        print_register_comparison(
            "SP",
            format!("${:02X}", exp.s),
            format!("${:02X}", cpu.sp.value),
            exp.s == cpu.sp.value,
        );
        print_register_comparison(
            "A ",
            format!("${:02X}", exp.a),
            format!("${:02X}", cpu.a),
            exp.a == cpu.a,
        );
        print_register_comparison(
            "X ",
            format!("${:02X}", exp.x),
            format!("${:02X}", cpu.x),
            exp.x == cpu.x,
        );
        print_register_comparison(
            "Y ",
            format!("${:02X}", exp.y),
            format!("${:02X}", cpu.y),
            exp.y == cpu.y,
        );
        print_register_comparison(
            "P ",
            format!("${:02X} ({:08b})", exp.p, exp.p),
            format!("${:02X} ({:08b})", cpu.flags.bits(), cpu.flags.bits()),
            exp.p == cpu.flags.bits(),
        );
        println!("└─────────────────────────────────────────────────────────────┘\n");

        // RAM State Comparison
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                        RAM STATE                            │");
        println!("├─────────────────────────────────────────────────────────────┤");

        for &(addr, expected_value) in &exp.ram {
            let actual_value = emul.bus.read_raw(addr);
            print_register_comparison(
                &format!("[${:04X}]", addr),
                format!("${:02X}", expected_value),
                format!("${:02X}", actual_value),
                expected_value == actual_value,
            );
        }
        println!("└─────────────────────────────────────────────────────────────┘\n");

        // Bus Cycles Comparison
        println!("┌─────────────────────────────────────────────────────────────┐");
        println!("│                       BUS CYCLES                            │");
        println!("├─────────────────────────────────────────────────────────────┤");

        let max_cycles = std::cmp::max(test.cycles.len(), actual_bus_ops.len());

        for i in 0..max_cycles {
            let exp_cycle = test.cycles.get(i);
            let act_cycle = actual_bus_ops.get(i);

            match (exp_cycle, act_cycle) {
                (Some((exp_addr, exp_val, exp_op)), Some(act_op)) => {
                    let (act_addr, act_val, act_op_str) = match act_op {
                        BusOp::Read(addr, val) => (*addr, *val, "read"),
                        BusOp::Write(addr, val) => (*addr, *val, "write"),
                        BusOp::Internal => (0, 0, "internal"),
                    };

                    let matches =
                        exp_addr == &act_addr && exp_val == &act_val && exp_op == act_op_str;
                    let status = if matches { "✓" } else { "✗" };

                    println!(
                        "│ {} Cycle {:2} │ Expected: {:5} ${:04X} = ${:02X}  │  Actual: {:5} ${:04X} = ${:02X} │",
                        status, i, exp_op, exp_addr, exp_val, act_op_str, act_addr, act_val
                    );
                }
                (Some((exp_addr, exp_val, exp_op)), None) => {
                    println!(
                        "│ ✗ Cycle {:2} │ Expected: {:5} ${:04X} = ${:02X}  │  Actual: MISSING        │",
                        i, exp_op, exp_addr, exp_val
                    );
                }
                (None, Some(act_op)) => {
                    let (act_addr, act_val, act_op_str) = match act_op {
                        BusOp::Read(addr, val) => (*addr, *val, "read"),
                        BusOp::Write(addr, val) => (*addr, *val, "write"),
                        BusOp::Internal => (0, 0, "internal"),
                    };
                    println!(
                        "│ ✗ Cycle {:2} │ Expected: NONE                     │  Actual: {:5} ${:04X} = ${:02X} │",
                        i, act_op_str, act_addr, act_val
                    );
                }
                (None, None) => break,
            }
        }

        println!("└─────────────────────────────────────────────────────────────┘\n");
    }

    fn print_register_comparison(name: &str, expected: String, actual: String, matches: bool) {
        let status = if matches { "✓" } else { "✗" };
        println!(
            "│ {} {:3} │ Expected: {:20} │ Actual: {:20} │",
            status, name, expected, actual
        );
    }
}
