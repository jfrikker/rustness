extern crate rustman_6500;

use std::io::{self, Read};
use std::fs::File;
use rustman_6500::cpu::IORequest;

pub fn main() -> io::Result<()> {
    let mut f = File::open("smbrom")?;
    let mut rom = Vec::new();
    f.read_to_end(&mut rom)?;

    let mut ram: [u8; 0x800] = [0; 0x800];

    let mut cpu = rustman_6500::cpu::CPU6500::new();
    let mut io_req = cpu.reset();
    println!("{:x?}", io_req);
    loop {
        io_req = match io_req {
            IORequest::Read(addr) => if addr >= 0x8000 {
                cpu.read_result(*rom.get((addr - 0x8000) as usize).unwrap())
            } else if addr < 0x2000 {
                cpu.read_result(ram[(addr % 0x800) as usize])
            } else if addr == 0x2002 {
                cpu.read_result(0xFF)
            } else {
                cpu.read_result(0)
            },
            IORequest::Write(addr, val) => {
                if addr < 0x2000 {
                    ram[(addr % 0x800) as usize] = val;
                }
                cpu.write_result()
            },
            IORequest::Fault(op) => {
                println!("Unrecognized opcode {:x?}", op);
                return Ok(())
            }
        };
        println!("{}: {:x?}", cpu.cycle_count(), io_req);
        if cpu.cycle_count() == 100000 {
            println!("nmi");
            io_req = cpu.nmi();
            println!("{}: {:x?}", cpu.cycle_count(), io_req);
        } else if cpu.cycle_count() > 100500 {
            return Ok(())
        }
    }
}