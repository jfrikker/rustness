use std::fs::File;
use std::io::{self, Read};
use std::collections::BTreeMap;
use std::path::Path;
use std::iter;

pub trait HardwareComponent {
    fn read(&self, addr: u16) -> Option<u8>;
    fn write(&mut self, addr: u16, val: u8) -> bool;
    fn len(&self) -> u16;
    fn maps(&self, addr: u16) -> bool {
        addr < self.len()
    }
}

pub fn repeat<C: HardwareComponent>(component: C, count: u16) -> Repeated<C> {
    Repeated {
        component,
        count
    }
}

pub struct Repeated<C> {
    component: C,
    count: u16
}

impl <C: HardwareComponent> HardwareComponent for Repeated<C> {
    fn read(&self, addr: u16) -> Option<u8> {
        if !self.maps(addr) {
            None
        } else {
            self.component.read(addr % self.component.len())
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> bool {
        if !self.maps(addr) {
            false
        } else {
            let fixed_addr = addr % self.component.len();
            self.component.write(fixed_addr, val)
        }
    }

    fn len(&self) -> u16 {
        self.component.len() * self.count
    }
}

pub struct ROM {
    bytes: Vec<u8>
}

impl ROM {
    pub fn from_bytes(bytes: Vec<u8>) -> ROM {
        ROM {
            bytes
        }
    }

    pub fn from_file<S: AsRef<Path>>(name: S) -> io::Result<ROM> {
        let mut f = File::open(name)?;
        let mut rom = Vec::new();
        f.read_to_end(&mut rom)?;
        Ok(ROM::from_bytes(rom))
    }
}

impl HardwareComponent for ROM {
    fn read(&self, addr: u16) -> Option<u8> {
        self.bytes.get(addr as usize).cloned()
    }

    fn write(&mut self, addr: u16, _val: u8) -> bool {
        self.maps(addr)
    }

    fn len(&self) -> u16 {
        self.bytes.len() as u16
    }
}

pub struct RAM {
    bytes: Vec<u8>
}

impl RAM {
    pub fn new(len: u16) -> RAM {
        RAM {
            bytes: iter::repeat(0).take(len as usize).collect()
        }
    }
}

impl HardwareComponent for RAM {
    fn read(&self, addr: u16) -> Option<u8> {
        self.bytes.get(addr as usize).cloned()
    }

    fn write(&mut self, addr: u16, val: u8) -> bool {
        let maps = self.maps(addr);
        if maps {
            self.bytes[addr as usize] = val;
        }
        maps
    }

    fn len(&self) -> u16 {
        self.bytes.len() as u16
    }
}

pub struct PartialHardware {
    components: BTreeMap<u16, Box<dyn HardwareComponent>>
}

impl PartialHardware {
    pub fn read(&self, addr: u16) -> Option<u8> {
        self.components.range(..addr + 1)
            .next_back()
            .and_then(|(offset, c)| c.read(addr - offset))
    }

    pub fn write(&mut self, addr: u16, val: u8) -> bool {
        self.components.range_mut(..addr + 1)
            .next_back()
            .map(|(offset, c)| c.write(addr - offset, val))
            .unwrap_or(false)
    }
}

pub struct HardwareBuilder {
    components: BTreeMap<u16, Box<dyn HardwareComponent>>
}

impl HardwareBuilder {
    pub fn new() -> HardwareBuilder {
        HardwareBuilder {
            components: BTreeMap::new()
        }
    }

    pub fn add_component<C: 'static + HardwareComponent>(mut self, offset: u16, component: C) -> HardwareBuilder {
        self.components.insert(offset, Box::new(component));
        self
    }

    pub fn build_partial(self) -> PartialHardware {
        PartialHardware {
            components: self.components
        }
    }
}