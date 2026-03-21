use std::thread::sleep;
use std::time::{Duration, Instant};

use crate::core::cpu::CPUState;

use super::interface::SystemInterface;

pub struct BasicDriver<S: SystemInterface> {
    system: S,
}

impl<S: SystemInterface> BasicDriver<S> {
    pub fn new(system: S) -> Self {
        Self { system }
    }

    pub fn run_cycles(&mut self, n: usize) {
        for _ in 0..n {
            self.system.tick();
        }
    }

    pub fn execute(&mut self) {
        let mut state = CPUState::Exec;
        let start = Instant::now();
        let mut cycles: u64 = 0;

        while state != CPUState::Jammed {
            self.system.tick();
            state = self.system.snapshot().state;
            cycles += 1;
        }

        let total_time = start.elapsed();
        println!(
            "Raw execution finished:\n  cycles: {}\n  time: {:.3?}\n  effective speed: {:.3} MHz",
            cycles,
            total_time,
            (cycles as f64 / total_time.as_secs_f64()) / 1_000_000.0
        );
    }

    pub fn timed_execute(&mut self, speed: u8) {
        let mut state = CPUState::Exec;

        let clock_hz = speed as u64 * 1_000_000;
        let batch_size: u64 = 10_000;

        let start = Instant::now();
        let mut cycles: u64 = 0;

        while state != CPUState::Jammed {
            let _batch_start_cycles = cycles;

            // --- Run a batch ---
            for _ in 0..batch_size {
                if state == CPUState::Jammed {
                    break;
                }

                self.system.tick();
                cycles += 1;
                state = self.system.snapshot().state;
            }

            // --- Timing sync ---
            let expected_secs = cycles as f64 / clock_hz as f64;
            let expected = Duration::from_secs_f64(expected_secs);

            let actual = start.elapsed();

            if expected > actual {
                sleep(expected - actual);
            }
        }

        let total_time = start.elapsed();

        println!(
            "Timed execution finished:\n  cycles: {}\n  time: {:.3?}\n  effective speed: {:.3} MHz",
            cycles,
            total_time,
            (cycles as f64 / total_time.as_secs_f64()) / 1_000_000.0
        );
    }
}
