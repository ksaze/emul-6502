use crate::core::bus::{Bus, BusOp, Device, DeviceHandle};
use crate::core::cpu::CPU;
use crate::core::variants::{Decoder, Quirks};
use crate::core::{Byte, Word};
#[cfg(feature = "driver")]
use crate::devices::EmulatorControl;
use crate::devices::{DMAController, MemoryDevice};
#[cfg(feature = "driver")]
use crate::driver::{SystemInterface, SystemSnapshot};
use crate::handles::{IntoDMAHandle, IntoHandle, OwnedDeviceDyn, SharedDeviceDyn, SyncDeviceDyn};

#[cfg(feature = "driver")]
enum Phase {
    Phi1,
    Phi2,
}

pub type BoxSystem<V> = GenericSystem<V, OwnedDeviceDyn>;
pub type RcSystem<V> = GenericSystem<V, SharedDeviceDyn>;
pub type ArcSystem<V> = GenericSystem<V, SyncDeviceDyn>;

pub struct GenericSystem<V, H>
where
    V: Decoder + Quirks,
    H: DeviceHandle<dyn Device> + 'static,
{
    pub cpu: CPU<V>,
    pub bus: Bus<H>,
    dmas: Vec<Box<dyn DMAController>>,
    #[cfg(feature = "driver")]
    phase: Phase,
}

impl<V, H> GenericSystem<V, H>
where
    V: Decoder + Quirks,
    H: DeviceHandle<dyn Device> + 'static,
{
    pub fn new(variant: V) -> GenericSystem<V, H> {
        Self {
            cpu: CPU::new(variant),
            bus: Bus::new(),
            dmas: Vec::new(),
            #[cfg(feature = "driver")]
            phase: Phase::Phi1,
        }
    }

    pub fn attach_device<D>(
        &mut self,
        device: D,
        base_addr: Word,
        size: usize,
        mirrors: u8,
    ) -> Option<<D as IntoHandle<H>>::ConcreteHandle>
    where
        H: DeviceHandle<dyn Device> + 'static,
        D: IntoHandle<H> + 'static,
        <D as IntoHandle<H>>::ConcreteHandle: DeviceHandle<D>,
    {
        let (map_mask, base_addr) = if size == 0 {
            (0x0, 0xFFFF)
        } else {
            assert!(
                size * mirrors as usize <= 0xFFFF + 1,
                "size * mirrors exceeds address space"
            );
            assert!(size.is_power_of_two());
            assert!(mirrors >= 1, "mirrors must be at least 1");
            assert!(mirrors.is_power_of_two(), "mirrors must be a power of two");
            assert!(
                (base_addr as usize & (size * mirrors as usize - 1)) == 0,
                "base address must be aligned to size * mirrors"
            );

            let map_mask = !((size * mirrors as usize - 1) as Word);
            (map_mask, base_addr)
        };

        let addr_mask = map_mask >> (mirrors as u32).trailing_zeros();

        let (dev, handle) = device.into_handle();
        self.bus
            .attach_device_handle(dev, base_addr, map_mask, addr_mask);
        handle
    }

    pub fn attach_dma<D: Device + DMAController + 'static>(
        &mut self,
        device: D,
        base_addr: Word,
        size: usize,
    ) where
        D: IntoDMAHandle<H>,
    {
        let (mask, base_addr) = if size == 0 {
            (0x0, 0xFFFF)
        } else {
            assert!(size <= 0xFFFF + 1, "Size exceeds address space.");
            assert!(
                size.is_power_of_two(),
                "Size must be a power of two for the device to have a valid mask."
            );
            assert!(
                (base_addr as usize & (size - 1)) == 0,
                "base address must be aligned to size"
            );

            let mask = !((size - 1) as Word);
            (mask, base_addr)
        };

        let (handle, dma) = device.into_dma_handles();
        if let Some(dma) = dma {
            self.dmas.push(dma);
        }
        self.bus.attach_device_handle(handle, base_addr, mask, 1);
    }

    pub fn attach_rom(&mut self, mut rom_data: Vec<Byte>, base_addr: Word, mirrors: u8)
    where
        MemoryDevice: IntoHandle<H>,
    {
        let size = rom_data.len().next_power_of_two();
        assert!(size <= (0xFFFF + 1), "ROM size exceeds address space.");

        rom_data.resize(size, 0xFF);
        self.attach_device(MemoryDevice::rom(rom_data), base_addr, size, mirrors);
    }

    pub fn attach_ram(&mut self, base_addr: Word, size: usize, mirrors: u8)
    where
        MemoryDevice: IntoHandle<H>,
    {
        self.attach_device(MemoryDevice::ram(size), base_addr, size, mirrors);
    }

    pub fn tick(&mut self) -> BusOp {
        self.phi1();
        self.phi2();
        self.bus.last_op
    }

    fn phi1(&mut self) {
        self.bus.tick();
        self.cpu.phi1();
    }

    fn phi2(&mut self) {
        self.cpu.phi2(&mut self.bus);

        if !self.bus.rdy {
            for dma in &mut self.dmas {
                if dma.wants_bus() {
                    dma.dma_tick(&mut self.bus);
                }
            }
        }
    }
}

#[cfg(feature = "driver")]
impl<V, H> SystemInterface for GenericSystem<V, H>
where
    V: Decoder + Quirks,
    H: DeviceHandle<dyn Device> + 'static,
    EmulatorControl: IntoHandle<H>,
{
    type ControlHandle = <EmulatorControl as IntoHandle<H>>::ConcreteHandle;
    fn tick(&mut self) {
        match self.phase {
            Phase::Phi1 => {
                self.phi1();
                self.phi2();
            }

            Phase::Phi2 => {
                self.phi2();
            }
        }

        self.phase = Phase::Phi1;
    }

    fn half_tick(&mut self) {
        match self.phase {
            Phase::Phi1 => {
                self.phi1();
                self.phase = Phase::Phi2;
            }

            Phase::Phi2 => {
                self.phi2();
                self.phase = Phase::Phi1;
            }
        }
    }

    fn attach_emulator_control(&mut self) -> Self::ControlHandle {
        self.attach_device(EmulatorControl::new(), 0x0, 0x0, 1)
            .expect("System should support an external concrete handle.")
    }

    fn snapshot(&self) -> SystemSnapshot {
        let cpu = &self.cpu.core;
        SystemSnapshot {
            pc: cpu.pc,
            sp: cpu.sp.value,
            a: cpu.a,
            x: cpu.x,
            y: cpu.y,
            addr_bus: cpu.addr_bus,
            data_bus: cpu.data_bus,
            rw: cpu.rw,
            flags: cpu.flags,
            state: cpu.state,
            instr_name: cpu.instr.name.clone(),
            ir: cpu.ir,
            signals: cpu.signals,
            last_op: self.bus.last_op,
        }
    }
}
