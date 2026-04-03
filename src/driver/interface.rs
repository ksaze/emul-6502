use crate::core::{Byte, Word};
use crate::core::{
    bus::{BusOp, DeviceHandle},
    cpu::{CPUState, Signals, Status},
};
use crate::devices::EmulatorControl;

#[derive(Clone, Debug)]
pub struct SystemSnapshot {
    pub pc: Word,
    pub sp: Byte,
    pub a: Byte,
    pub x: Byte,
    pub y: Byte,
    pub addr_bus: Word,
    pub data_bus: Byte,
    pub rw: bool,
    pub flags: Status,
    pub state: CPUState,
    pub instr_name: String,
    pub ir: Byte,
    pub signals: Signals,
    pub last_op: BusOp,
}

pub trait SystemInterface {
    type ControlHandle: DeviceHandle<EmulatorControl>;

    fn tick(&mut self);
    fn half_tick(&mut self);
    fn attach_emulator_control(&mut self) -> Self::ControlHandle;
    fn snapshot(&self) -> SystemSnapshot;
}
