use crate::core::{
    bus::{Bus, BusOp},
    cpu::{CPUState, Signals, Status},
};
use crate::shared::{Byte, Word};

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
    fn tick(&mut self);
    fn half_tick(&mut self);
    fn bus_as_mut(&mut self) -> &mut Bus;
    fn snapshot(&self) -> SystemSnapshot;
}
