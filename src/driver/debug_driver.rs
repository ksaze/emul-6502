use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    Frame, Terminal,
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Cell, Paragraph, Row, Table, TableState},
};

use std::io;

use crate::core::bus::{BusOp, Device};
use crate::core::cpu::CPUState;
use crate::devices::EmulatorControl;
use crate::shared::SharedDevice;

use super::interface::{SystemInterface, SystemSnapshot};

// ─── Palette ─────────────────────────────────────────────────────────────────

const C_BG: Color = Color::Rgb(18, 18, 24);
const C_BORDER: Color = Color::Rgb(50, 50, 70);
const C_BORDER_NAV: Color = Color::Rgb(180, 130, 50);
const C_TITLE: Color = Color::Rgb(130, 180, 255);
const C_LABEL: Color = Color::Rgb(100, 110, 140);
const C_VALUE: Color = Color::White;
const C_DIM: Color = Color::Rgb(60, 65, 80);
const C_SIG_ON: Color = Color::Rgb(80, 220, 140);
const C_SIG_OFF: Color = Color::Rgb(60, 60, 75);
const C_READ: Color = Color::Rgb(80, 180, 255);
const C_WRITE: Color = Color::Rgb(255, 120, 80);
const C_INTERN: Color = Color::Rgb(80, 80, 100);
const C_KEY: Color = Color::Rgb(255, 210, 80);
const C_PIN_HELD: Color = Color::Rgb(255, 80, 80);
const C_DIVIDER: Color = Color::Rgb(35, 35, 48);
const C_STATUS_RUN: Color = Color::Rgb(80, 220, 140);
const C_STATUS_STEP: Color = Color::Rgb(255, 210, 80);
const C_STATUS_PAUSE: Color = Color::Rgb(255, 120, 80);
const C_STATUS_NAV: Color = Color::Rgb(180, 130, 50);

// ─── Cycle Entry ─────────────────────────────────────────────────────────────

pub struct CycleEntry {
    pub index: u64,
    pub phi1: SystemSnapshot,
    pub phi2: SystemSnapshot,
}

// ─── Navigate position ───────────────────────────────────────────────────────

#[derive(Clone, Copy, PartialEq)]
enum Phase {
    Phi1,
    Phi2,
}

/// Which half-cycle is highlighted in navigate mode
struct NavPos {
    cycle: usize, // index into history (0 = most recent)
    phase: Phase,
}

impl NavPos {
    fn row_index(&self) -> usize {
        // Each CycleEntry renders 3 rows: phi2, phi1, divider
        // Most recent cycle is at top (index 0 in reversed iter)
        self.cycle * 3 + if self.phase == Phase::Phi2 { 0 } else { 1 }
    }
}

// ─── Driver Mode ─────────────────────────────────────────────────────────────

#[derive(PartialEq)]
enum DriverMode {
    Step,
    Run,
    Navigate,
}

// ─── Pin State ───────────────────────────────────────────────────────────────

struct PinState {
    res: bool,
    irq: bool,
    nmi: bool,
    rdy: bool,
}

impl PinState {
    fn all_released() -> Self {
        Self {
            res: false,
            irq: false,
            nmi: false,
            rdy: false,
        }
    }
}

// ─── Driver ──────────────────────────────────────────────────────────────────

pub struct DebugDriver<S: SystemInterface> {
    pub system: S,
    ctrl: SharedDevice<EmulatorControl>,
    history: Vec<CycleEntry>,
    mode: DriverMode,
    paused: bool,
    table_state: TableState,
    nav: NavPos,
    run_speed: u64,
    pins: PinState,
    cycle_counter: u64,
}

impl<S: SystemInterface> DebugDriver<S> {
    pub fn new(mut system: S) -> Self {
        let mut table_state = TableState::default();
        table_state.select(Some(0));

        let ctrl = EmulatorControl::new().into_shared();
        system.bus_as_mut().attach_shared_device(&ctrl, 0xFFFF, 0x0);

        Self {
            system,
            ctrl,
            history: Vec::new(),
            mode: DriverMode::Step,
            paused: true,
            table_state,
            nav: NavPos {
                cycle: 0,
                phase: Phase::Phi2,
            },
            run_speed: 1,
            pins: PinState::all_released(),
            cycle_counter: 0,
        }
    }

    pub fn run(&mut self) -> io::Result<()> {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen)?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;

        loop {
            // Determine which snapshot to show in detail panels
            let live = self.system.snapshot();
            let displayed = if self.mode == DriverMode::Navigate {
                self.history
                    .iter()
                    .rev()
                    .nth(self.nav.cycle)
                    .map(|e| match self.nav.phase {
                        Phase::Phi1 => e.phi1.clone(),
                        Phase::Phi2 => e.phi2.clone(),
                    })
                    .unwrap_or_else(|| live.clone())
            } else {
                live.clone()
            };

            terminal.draw(|f| self.draw(f, &displayed))?;

            if event::poll(std::time::Duration::from_millis(16))? {
                if let Event::Key(key) = event::read()? {
                    match (key.code, key.modifiers) {
                        (KeyCode::Char('q'), _) | (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                            break;
                        }

                        // ── Mode switching ───────────────────────────────
                        (KeyCode::Char('s'), _) => {
                            self.mode = DriverMode::Step;
                            self.paused = true;
                        }
                        (KeyCode::Char('r'), _) => {
                            self.mode = DriverMode::Run;
                            self.paused = false;
                        }
                        (KeyCode::Char('n'), _) => {
                            self.mode = DriverMode::Navigate;
                            self.nav = NavPos {
                                cycle: 0,
                                phase: Phase::Phi2,
                            };
                            self.table_state.select(Some(0));
                        }
                        (KeyCode::Esc, _) if self.mode == DriverMode::Navigate => {
                            self.mode = DriverMode::Step;
                            self.paused = true;
                        }

                        // ── Step / Pause ─────────────────────────────────
                        (KeyCode::Char(' '), _) => match self.mode {
                            DriverMode::Step => self.do_step(),
                            DriverMode::Run => self.paused = !self.paused,
                            DriverMode::Navigate => {}
                        },

                        // ── Navigate scroll ──────────────────────────────
                        (KeyCode::Up, _) => self.nav_up(),
                        (KeyCode::Down, _) => self.nav_down(),

                        // ── Run speed ────────────────────────────────────
                        (KeyCode::Char('+'), _) => {
                            self.run_speed = (self.run_speed * 2).min(1024);
                        }
                        (KeyCode::Char('-'), _) => {
                            self.run_speed = (self.run_speed / 2).max(1);
                        }

                        // ── Pin toggles ──────────────────────────────────
                        (KeyCode::Char('1'), _) => self.toggle_pin_res(),
                        (KeyCode::Char('2'), _) => self.toggle_pin_irq(),
                        (KeyCode::Char('3'), _) => self.toggle_pin_nmi(),
                        (KeyCode::Char('4'), _) => self.toggle_pin_rdy(),

                        _ => {}
                    }
                }
            }

            if self.mode == DriverMode::Run && !self.paused {
                for _ in 0..self.run_speed {
                    self.do_step();
                }
            }
        }

        disable_raw_mode()?;
        execute!(io::stdout(), LeaveAlternateScreen)?;
        Ok(())
    }

    // ── Navigation ───────────────────────────────────────────────────────────

    fn nav_up(&mut self) {
        if self.mode == DriverMode::Navigate {
            // phi2 -> phi1 -> older cycle phi2 -> ...
            match self.nav.phase {
                Phase::Phi2 => {
                    self.nav.phase = Phase::Phi1;
                }
                Phase::Phi1 => {
                    let max = self.history.len().saturating_sub(1);
                    if self.nav.cycle < max {
                        self.nav.cycle += 1;
                        self.nav.phase = Phase::Phi2;
                    }
                }
            }
        } else {
            let i = self.table_state.selected().unwrap_or(0);
            let max_row = self.history.len() * 3;
            if i + 1 < max_row {
                self.table_state.select(Some(i + 1));
            }
        }
        self.sync_table_to_nav();
    }

    fn nav_down(&mut self) {
        if self.mode == DriverMode::Navigate {
            match self.nav.phase {
                Phase::Phi1 => {
                    self.nav.phase = Phase::Phi2;
                }
                Phase::Phi2 => {
                    if self.nav.cycle > 0 {
                        self.nav.cycle -= 1;
                        self.nav.phase = Phase::Phi1;
                    }
                }
            }
        } else {
            let i = self.table_state.selected().unwrap_or(0);
            if i > 0 {
                self.table_state.select(Some(i - 1));
            }
        }
        self.sync_table_to_nav();
    }

    fn sync_table_to_nav(&mut self) {
        self.table_state.select(Some(self.nav.row_index()));
    }

    // ── Step ─────────────────────────────────────────────────────────────────

    fn do_step(&mut self) {
        self.system.half_tick(); // phi1 — sets addr_bus, rw
        let phi1_snap = self.system.snapshot();
        self.system.half_tick(); // phi2 — drives bus, executes action
        let phi2_snap = self.system.snapshot();

        self.history.push(CycleEntry {
            index: self.cycle_counter,
            phi1: phi1_snap,
            phi2: phi2_snap,
        });
        self.cycle_counter += 1;

        if self.mode != DriverMode::Navigate {
            self.nav = NavPos {
                cycle: 0,
                phase: Phase::Phi2,
            };
            self.table_state.select(Some(0));
        }
    }

    // ── Pin toggles ──────────────────────────────────────────────────────────

    fn toggle_pin_res(&mut self) {
        self.pins.res = !self.pins.res;
        self.ctrl.borrow_mut().res_line = !self.pins.res;
    }
    fn toggle_pin_irq(&mut self) {
        self.pins.irq = !self.pins.irq;
        self.ctrl.borrow_mut().irq_line = !self.pins.irq;
    }
    fn toggle_pin_nmi(&mut self) {
        self.pins.nmi = !self.pins.nmi;
        self.ctrl.borrow_mut().nmi_line = !self.pins.nmi;
    }
    fn toggle_pin_rdy(&mut self) {
        self.pins.rdy = !self.pins.rdy;
        self.ctrl.borrow_mut().rdy_line = !self.pins.rdy;
    }

    // ── Draw ─────────────────────────────────────────────────────────────────

    fn draw(&mut self, f: &mut Frame, snapshot: &SystemSnapshot) {
        let area = f.area();
        f.render_widget(Block::default().style(Style::default().bg(C_BG)), area);

        let root = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(0), Constraint::Length(3)])
            .split(area);

        let main = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Length(44), Constraint::Min(0)])
            .split(root[0]);

        let left = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(8), // registers
                Constraint::Length(5), // bus
                Constraint::Length(6), // signals
                Constraint::Length(5), // pins
                Constraint::Min(0),
            ])
            .split(main[0]);

        let nav = self.mode == DriverMode::Navigate;

        self.draw_registers(f, snapshot, left[0], nav);
        self.draw_bus(f, snapshot, left[1], nav);
        self.draw_signals(f, snapshot, left[2], nav);
        self.draw_pins(f, left[3]);
        self.draw_history(f, main[1]);
        self.draw_controls(f, root[1]);
    }

    fn draw_registers(&self, f: &mut Frame, s: &SystemSnapshot, area: Rect, nav: bool) {
        let fl = s.flags.bits();
        let flag_char = |bit: u8, ch: &'static str| {
            if fl & bit != 0 {
                Span::styled(
                    ch,
                    Style::default().fg(C_SIG_ON).add_modifier(Modifier::BOLD),
                )
            } else {
                Span::styled(ch, Style::default().fg(C_SIG_OFF))
            }
        };
        let lbl = |s: &'static str| Span::styled(s, Style::default().fg(C_LABEL));
        let val =
            |s: String| Span::styled(s, Style::default().fg(C_VALUE).add_modifier(Modifier::BOLD));
        let sp = || Span::raw("  ");

        let state_color = match s.state {
            CPUState::Exec => C_SIG_ON,
            CPUState::Fetch => C_READ,
            CPUState::Jammed => C_WRITE,
            CPUState::Reset => C_KEY,
            CPUState::Blocked(_) => C_STATUS_PAUSE,
        };

        let lines = vec![
            Line::from(vec![
                lbl("PC "),
                val(format!("${:04X}", s.pc)),
                sp(),
                lbl("SP "),
                val(format!("${:02X}", s.sp)),
                sp(),
                lbl("IR "),
                val(format!("${:02X}", s.ir)),
            ]),
            Line::from(vec![
                lbl(" A "),
                val(format!("${:02X}", s.a)),
                sp(),
                lbl(" X "),
                val(format!("${:02X}", s.x)),
                sp(),
                lbl(" Y "),
                val(format!("${:02X}", s.y)),
            ]),
            Line::from(vec![
                lbl("db "),
                val(format!("${:04X}", s.data_bus)),
                sp(),
                lbl("ab "),
                val(format!("${:02X}", s.addr_bus)),
                sp(),
                lbl("rw "),
                val(if s.rw {
                    "high".to_string()
                } else {
                    "low".to_string()
                }),
            ]),
            Line::from(vec![lbl("OP "), val(s.instr_name.clone())]),
            Line::from(vec![
                lbl("ST "),
                Span::styled(
                    format!("{:?}", s.state),
                    Style::default()
                        .fg(state_color)
                        .add_modifier(Modifier::BOLD),
                ),
            ]),
            Line::from(vec![
                lbl("FL "),
                flag_char(0x80, "N"),
                Span::raw(" "),
                flag_char(0x40, "V"),
                Span::raw(" "),
                flag_char(0x20, "U"),
                Span::raw(" "),
                val("_".to_string()),
                Span::raw(" "),
                flag_char(0x08, "D"),
                Span::raw(" "),
                flag_char(0x04, "I"),
                Span::raw(" "),
                flag_char(0x02, "Z"),
                Span::raw(" "),
                flag_char(0x01, "C"),
            ]),
        ];

        f.render_widget(
            Paragraph::new(lines).block(styled_block("Registers", nav)),
            area,
        );
    }

    fn draw_bus(&self, f: &mut Frame, s: &SystemSnapshot, area: Rect, nav: bool) {
        let (label, addr, data, color) = match s.last_op {
            BusOp::Read(addr, data) => ("READ ", addr, Some(data), C_READ),
            BusOp::Write(addr, data) => ("WRITE", addr, Some(data), C_WRITE),
            BusOp::Internal => ("INTRL", 0, None, C_INTERN),
        };

        let line = if let Some(data) = data {
            Line::from(vec![
                Span::styled(
                    label,
                    Style::default().fg(color).add_modifier(Modifier::BOLD),
                ),
                Span::raw("  "),
                Span::styled(format!("${:04X}", addr), Style::default().fg(C_VALUE)),
                Span::styled(" = ", Style::default().fg(C_LABEL)),
                Span::styled(
                    format!("${:02X}", data),
                    Style::default().fg(color).add_modifier(Modifier::BOLD),
                ),
            ])
        } else {
            Line::from(vec![Span::styled(label, Style::default().fg(color))])
        };

        f.render_widget(
            Paragraph::new(vec![line]).block(styled_block("Bus", nav)),
            area,
        );
    }

    fn draw_signals(&self, f: &mut Frame, s: &SystemSnapshot, area: Rect, nav: bool) {
        let sig = &s.signals;
        let on = Style::default().fg(C_SIG_ON).add_modifier(Modifier::BOLD);
        let off = Style::default().fg(C_SIG_OFF);
        let sp = || Span::raw(" ");
        let sig_span =
            |name: &'static str, val: bool| Span::styled(name, if val { on } else { off });

        let lines = vec![
            Line::from(vec![
                sig_span("RES", sig.RES_sync),
                sp(),
                sig_span("IRQ", sig.IRQ_sync),
                sp(),
                sig_span("NMI", sig.NMIP),
                sp(),
                sig_span("RESP", sig.RESP),
                sp(),
                sig_span("IRQP", sig.IRQP),
                sp(),
                sig_span("NMIL", sig.NMIL),
            ]),
            Line::from(vec![
                sig_span("1368", sig.sig_1368),
                sp(),
                sig_span("doIRQ", sig.doIRQ),
                sp(),
                sig_span("NMIG", sig.NMIG),
                sp(),
                sig_span("RESG", sig.RESG),
                sp(),
                sig_span("INTG", sig.INTG),
                sp(),
                sig_span("D1x1", sig.D1x1),
                sp(),
                sig_span("VEC", sig.VEC),
                sp(),
                sig_span("BRK", sig.brk_done),
            ]),
        ];

        f.render_widget(
            Paragraph::new(lines).block(styled_block("Signals", nav)),
            area,
        );
    }

    fn draw_pins(&self, f: &mut Frame, area: Rect) {
        let pin_span = |name: &'static str, held: bool, key: &'static str| {
            let (label, style) = if held {
                (
                    format!("[{}] {} HELD", key, name),
                    Style::default().fg(C_PIN_HELD).add_modifier(Modifier::BOLD),
                )
            } else {
                (
                    format!("[{}] {}     ", key, name),
                    Style::default().fg(C_SIG_OFF),
                )
            };
            Span::styled(label, style)
        };

        let lines = vec![
            Line::from(vec![
                pin_span("RES", self.pins.res, "1"),
                Span::raw("  "),
                pin_span("IRQ", self.pins.irq, "2"),
            ]),
            Line::from(vec![
                pin_span("NMI", self.pins.nmi, "3"),
                Span::raw("  "),
                pin_span("RDY", self.pins.rdy, "4"),
            ]),
        ];

        f.render_widget(
            Paragraph::new(lines).block(styled_block("Pins  [1][2][3][4]", false)),
            area,
        );
    }

    fn draw_history(&mut self, f: &mut Frame, area: Rect) {
        let nav = self.mode == DriverMode::Navigate;

        // Each CycleEntry produces 3 rows: phi2, phi1, divider
        let mut rows: Vec<Row> = Vec::with_capacity(self.history.len() * 3);

        for entry in self.history.iter().rev() {
            // ── phi2 row ─────────────────────────────────────────────────────
            let (bus_str, bus_color) = match entry.phi2.last_op {
                BusOp::Read(a, d) => (format!("R ${:04X}=${:02X}", a, d), C_READ),
                BusOp::Write(a, d) => (format!("W ${:04X}=${:02X}", a, d), C_WRITE),
                BusOp::Internal => ("internal".to_string(), C_INTERN),
            };
            let rw_str = if entry.phi1.rw { "1" } else { "0" };
            let rw_color = if entry.phi1.rw { C_READ } else { C_WRITE };

            rows.push(Row::new(vec![
                Cell::from(format!("  {}", entry.index)).style(Style::default().fg(C_LABEL)),
                Cell::from("φ2").style(Style::default().fg(C_LABEL)),
                Cell::from(format!("${:04X}", entry.phi2.pc)).style(Style::default().fg(C_VALUE)),
                Cell::from(format!("{}", entry.phi2.instr_name.clone()))
                    .style(Style::default().fg(C_TITLE)),
                Cell::from(format!("${:04X}", entry.phi2.addr_bus))
                    .style(Style::default().fg(C_VALUE)),
                Cell::from(format!("${:02X}", entry.phi2.data_bus))
                    .style(Style::default().fg(C_VALUE)),
                Cell::from(format!(" {}", rw_str)).style(Style::default().fg(rw_color)),
                Cell::from(format!(
                    "A={:02X} X={:02X} Y={:02X}",
                    entry.phi2.a, entry.phi2.x, entry.phi2.y
                ))
                .style(Style::default().fg(C_LABEL)),
                Cell::from(format_flags(entry.phi2.flags.bits()))
                    .style(Style::default().fg(C_VALUE)),
                Cell::from(bus_str).style(Style::default().fg(bus_color)),
            ]));

            // ── phi1 row ─────────────────────────────────────────────────────
            rows.push(Row::new(vec![
                Cell::from("") // same cycle index, don't repeat
                    .style(Style::default().fg(C_DIM)),
                Cell::from("φ1").style(Style::default().fg(C_DIM)),
                Cell::from(format!("${:04X}", entry.phi1.pc)).style(Style::default().fg(C_DIM)),
                Cell::from(format!("{}", entry.phi1.instr_name.clone()))
                    .style(Style::default().fg(C_TITLE)),
                Cell::from(format!("${:04X}", entry.phi1.addr_bus))
                    .style(Style::default().fg(C_VALUE)),
                Cell::from(format!("${:02X}", entry.phi1.data_bus))
                    .style(Style::default().fg(C_VALUE)),
                Cell::from(format!(" {}", rw_str)).style(Style::default().fg(rw_color)),
                Cell::from(format!(
                    "A={:02X} X={:02X} Y={:02X}",
                    entry.phi1.a, entry.phi1.x, entry.phi1.y
                ))
                .style(Style::default().fg(C_DIM)),
                Cell::from(format_flags(entry.phi1.flags.bits())).style(Style::default().fg(C_DIM)),
                Cell::from("—").style(Style::default().fg(C_DIM)),
            ]));

            // ── cycle divider ─────────────────────────────────────────────
            rows.push(
                Row::new(vec![
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                    Cell::from(""),
                ])
                .style(Style::default().bg(C_DIVIDER))
                .height(1),
            );
        }

        let title = if nav {
            format!(
                "History  NAV [C{} {}]",
                self.history
                    .len()
                    .saturating_sub(self.nav.cycle)
                    .saturating_sub(1),
                if self.nav.phase == Phase::Phi1 {
                    "φ1"
                } else {
                    "φ2"
                }
            )
        } else {
            "History".to_string()
        };

        let table = Table::new(
            rows,
            [
                Constraint::Length(6),  // cycle index
                Constraint::Length(3),  // phase φ1/φ2
                Constraint::Length(7),  // pc
                Constraint::Length(13), // instr
                Constraint::Length(7),  // addr_bus
                Constraint::Length(5),  // data_bus
                Constraint::Length(5),  // rw
                Constraint::Length(15), // regs
                Constraint::Length(9),  // flags
                Constraint::Min(0),     // bus op
            ],
        )
        .header(
            Row::new(vec![
                "Cycle",
                "Ph",
                "  PC",
                "Instr",
                " AB",
                "DB",
                "R/W",
                "  Registers",
                " Flags",
                "   Bus",
            ])
            .style(
                Style::default()
                    .fg(C_LABEL)
                    .add_modifier(Modifier::UNDERLINED),
            ),
        )
        .row_highlight_style(
            Style::default()
                .bg(Color::Rgb(38, 38, 58))
                .add_modifier(Modifier::BOLD),
        )
        .block(styled_block(&title, nav));

        f.render_stateful_widget(table, area, &mut self.table_state);
    }

    fn draw_controls(&self, f: &mut Frame, area: Rect) {
        let mode_str = match self.mode {
            DriverMode::Step => Span::styled(" STEP ", Style::default().fg(C_BG).bg(C_STATUS_STEP)),
            DriverMode::Run if self.paused => {
                Span::styled(" PAUSED ", Style::default().fg(C_BG).bg(C_STATUS_PAUSE))
            }
            DriverMode::Run => Span::styled(
                format!(" RUN x{} ", self.run_speed),
                Style::default().fg(C_BG).bg(C_STATUS_RUN),
            ),
            DriverMode::Navigate => Span::styled(
                format!(
                    " NAV [C{} {}] ",
                    self.cycle_counter.saturating_sub(self.nav.cycle as u64 + 1),
                    if self.nav.phase == Phase::Phi1 {
                        "φ1"
                    } else {
                        "φ2"
                    }
                ),
                Style::default().fg(C_BG).bg(C_STATUS_NAV),
            ),
        };

        let key = |k: &'static str| Span::styled(k, Style::default().fg(C_KEY));
        let txt = |t: &'static str| Span::styled(t, Style::default().fg(C_LABEL));
        let sp = || Span::raw("  ");

        let line = Line::from(vec![
            mode_str,
            sp(),
            key("[SPC]"),
            txt(" step "),
            key("[S]"),
            txt(" step-mode "),
            key("[R]"),
            txt(" run "),
            key("[N]"),
            txt(" navigate "),
            key("[ESC]"),
            txt(" exit-nav "),
            key("[+/-]"),
            txt(" speed "),
            key("[↑↓]"),
            txt(" scroll "),
            key("[1-4]"),
            txt(" pins "),
            key("[Q]"),
            txt(" quit "),
        ]);

        f.render_widget(
            Paragraph::new(line).block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(C_BORDER))
                    .style(Style::default().bg(C_BG)),
            ),
            area,
        );
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn format_flags(fl: u8) -> String {
    format!(
        "{}{}{}-{}{}{}{}",
        if fl & 0x80 != 0 { "N" } else { "n" },
        if fl & 0x40 != 0 { "V" } else { "v" },
        if fl & 0x20 != 0 { "U" } else { "u" },
        if fl & 0x08 != 0 { "D" } else { "d" },
        if fl & 0x04 != 0 { "I" } else { "i" },
        if fl & 0x02 != 0 { "Z" } else { "z" },
        if fl & 0x01 != 0 { "C" } else { "c" },
    )
}

fn styled_block(title: &str, nav: bool) -> Block<'_> {
    let border_color = if nav { C_BORDER_NAV } else { C_BORDER };
    let title_color = if nav { C_STATUS_NAV } else { C_TITLE };
    Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(border_color))
        .title(Span::styled(
            format!(" {} ", title),
            Style::default()
                .fg(title_color)
                .add_modifier(Modifier::BOLD),
        ))
        .style(Style::default().bg(C_BG))
}
