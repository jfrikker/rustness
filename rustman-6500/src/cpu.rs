use std::boxed::FnBox;
use std::mem::swap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IORequest {
    Read(u16),
    Write(u16, u8),
    Fault(u8)
}

struct Continuation {
    continuation: Box<FnBox(&mut CPU6500, Option<u8>) -> IORequest>
}

impl Continuation {
    pub fn new<T: 'static + FnOnce(&mut CPU6500, Option<u8>) -> IORequest>(continuation: T) -> Continuation {
        Continuation {
            continuation: Box::new(continuation)
        }
    }

    pub fn apply(self, cpu: &mut CPU6500, val: Option<u8>) -> IORequest {
        self.continuation.call_box((cpu, val))
    }
}

pub struct CPU6500 {
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    reg_sp: u8,
    reg_pc: u16,
    flag_n: bool,
    flag_v: bool,
    flag_z: bool,
    flag_c: bool,
    flag_i: bool,
    flag_d: bool,
    cycle_count: u128,
    continuation: Continuation
}

impl CPU6500 {
    pub fn new() -> CPU6500 {
        CPU6500 {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            reg_sp: 0xff,
            reg_pc: 0,
            flag_n: false,
            flag_v: false,
            flag_z: false,
            flag_c: false,
            flag_i: false,
            flag_d: false,
            cycle_count: 0,
            continuation: Continuation::new(|_, _| panic!("WHAT??!?"))
        }
    }

    pub fn reset(&mut self) -> IORequest {
        self.read_u16(0xFFFC, |cpu, val| {
            cpu.reg_pc = val;
            cpu.execute()
        })
    }

    pub fn run_from(&mut self, addr: u16) -> IORequest {
        self.reg_pc = addr;
        self.execute()
    }

    pub fn nmi(&mut self) -> IORequest {
        self.read_u16(0xFFFA, |cpu, val| {
            cpu.reg_pc = val;
            cpu.execute()
        })
    }

    fn read_u16<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, addr: u16, continuation: T) -> IORequest {
        self.read(addr, move |cpu, val| {
            cpu.read(addr + 1, move |cpu2, val2| {
                continuation(cpu2, ((val2 as u16) << 8) + (val as u16))
            })
        })
    }

    /*fn write_u16<T: 'static + FnOnce(&mut CPU6500) -> IORequest>(&mut self, addr: u16, val: u16, continuation: T) -> IORequest {
        self.write(addr + 1, (val >> 8) as u8, move |cpu| {
            cpu.write(addr, (val & 0xFF) as u8, move |cpu2| {
                continuation(cpu2)
            })
        })
    }*/

    fn push<T: 'static + FnOnce(&mut CPU6500) -> IORequest>(&mut self, val: u8, continuation: T) -> IORequest {
        let reg_sp = self.reg_sp;
        let write_addr = 0x100 + (reg_sp as u16);
        self.reg_sp = reg_sp.wrapping_sub(1);
        self.write(write_addr, val, continuation)
    }

    fn push_u16<T: 'static + FnOnce(&mut CPU6500) -> IORequest>(&mut self, val: u16, continuation: T) -> IORequest {
        self.push((val >> 8) as u8, move |cpu| {
            cpu.push((val & 0xFF) as u8, move |cpu2| {
                continuation(cpu2)
            })
        })
    }

    fn pop<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, continuation: T) -> IORequest {
        let reg_sp = self.reg_sp.wrapping_add(1);
        let read_addr = 0x100 + (reg_sp as u16);
        self.reg_sp = reg_sp;
        self.read(read_addr, continuation)
    }

    fn pop_u16<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, continuation: T) -> IORequest {
        self.pop(move |cpu, val| {
            cpu.pop(move |cpu2, val2| {
                continuation(cpu2, ((val2 as u16) << 8) + (val as u16))
            })
        })
    }

    pub fn read_result(&mut self, result: u8) -> IORequest {
        let mut continuation = Continuation::new(|_, _| panic!("WHAT??!?"));
        swap(&mut continuation, &mut self.continuation);
        continuation.apply(self, Some(result))
    }

    pub fn write_result(&mut self) -> IORequest {
        let mut continuation = Continuation::new(|_, _| panic!("WHAT??!?"));
        swap(&mut continuation, &mut self.continuation);
        continuation.apply(self, None)
    }

    pub fn cycle_count(&self) -> u128 {
        self.cycle_count
    }

    fn execute(&mut self) -> IORequest {
        let pc = self.reg_pc;
        self.read(pc, CPU6500::execute_op)
    }

    fn execute_op(&mut self, opcode: u8) -> IORequest {
        println!("running {:x?}", opcode);
        match opcode {
            0x05 => self.zpg(CPU6500::ora),
            0x09 => self.imm(CPU6500::ora),
            0x10 => self.rel_addr(CPU6500::bpl),
            0x20 => self.abs_addr(CPU6500::jsr),
            0x21 => self.ind_x(CPU6500::and),
            0x24 => self.zpg(CPU6500::bit),
            0x25 => self.zpg(CPU6500::and),
            0x29 => self.imm(CPU6500::and),
            0x2a => self.implied(CPU6500::rol_a),
            0x2c => self.abs(CPU6500::bit),
            0x2d => self.abs(CPU6500::and),
            0x31 => self.ind_y(CPU6500::and),
            0x35 => self.zpg_x(CPU6500::and),
            0x39 => self.abs_y(CPU6500::and),
            0x3d => self.abs_x(CPU6500::and),
            0x48 => self.implied(CPU6500::pha),
            0x4a => self.implied(CPU6500::lsr_a),
            0x4c => self.abs_addr(CPU6500::jmp),
            0x60 => self.implied(CPU6500::rts),
            0x68 => self.implied(CPU6500::pla),
            0x78 => self.implied(CPU6500::sei),
            0x85 => self.zpg_addr(CPU6500::sta),
            0x86 => self.zpg_addr(CPU6500::stx),
            0x88 => self.implied(CPU6500::dey),
            0x8a => self.implied(CPU6500::txa),
            0x8d => self.abs_addr(CPU6500::sta),
            0x91 => self.ind_y_addr(CPU6500::sta),
            0x99 => self.abs_y_addr(CPU6500::sta),
            0x9a => self.implied(CPU6500::txs),
            0x9d => self.abs_x_addr(CPU6500::sta),
            0xd8 => self.implied(CPU6500::cld),
            0xa0 => self.imm(CPU6500::ldy),
            0xa2 => self.imm(CPU6500::ldx),
            0xa9 => self.imm(CPU6500::lda),
            0xaa => self.implied(CPU6500::tax),
            0xac => self.abs(CPU6500::ldy),
            0xad => self.abs(CPU6500::lda),
            0xae => self.abs(CPU6500::ldx),
            0xb0 => self.rel_addr(CPU6500::bcs),
            0xb1 => self.ind_y(CPU6500::lda),
            0xbd => self.abs_x(CPU6500::lda),
            0xbe => self.abs_y(CPU6500::ldx),
            0xc0 => self.imm(CPU6500::cpy),
            0xc1 => self.ind_x(CPU6500::cmp),
            0xc4 => self.zpg(CPU6500::cpy),
            0xc5 => self.zpg(CPU6500::cmp),
            0xc8 => self.implied(CPU6500::iny),
            0xc9 => self.imm(CPU6500::cmp),
            0xca => self.implied(CPU6500::dex),
            0xcc => self.abs(CPU6500::cpy),
            0xcd => self.abs(CPU6500::cmp),
            0xd0 => self.rel_addr(CPU6500::bne),
            0xd1 => self.ind_y(CPU6500::cmp),
            0xd5 => self.zpg_x(CPU6500::cmp),
            0xd9 => self.abs_y(CPU6500::cmp),
            0xdd => self.abs_x(CPU6500::cmp),
            0xe0 => self.imm(CPU6500::cpx),
            0xe4 => self.zpg(CPU6500::cpx),
            0xe6 => self.zpg_addr(CPU6500::inc),
            0xec => self.abs(CPU6500::cpx),
            0xee => self.abs_addr(CPU6500::inc),
            0xf6 => self.zpg_x_addr(CPU6500::inc),
            0xfe => self.abs_x_addr(CPU6500::inc),
            op => return IORequest::Fault(op)
        }
    }

    fn abs<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.abs_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn abs_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 3;
        self.read_u16(addr, f)
    }

    fn abs_x<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.abs_x_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn abs_x_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 3;
        self.read_u16(addr, |cpu, val| {
            let addr = val + cpu.reg_x as u16;
            f(cpu, addr)
        })
    }

    fn abs_y<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.abs_y_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn abs_y_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 3;
        self.read_u16(addr, |cpu, val| {
            let addr = val + (cpu.reg_y as u16);
            f(cpu, addr)
        })
    }

    fn imm<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, f)
    }

    fn implied<T: 'static + FnOnce(&mut CPU6500) -> IORequest>(&mut self, f: T) -> IORequest {
        self.reg_pc += 1;
        let pc = self.reg_pc;
        self.read(pc, |cpu, _| f(cpu))
    }

    fn ind_x<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.ind_x_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn ind_x_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, move |cpu, val| {
            cpu.read_u16(val as u16, move |cpu, val| {
                let hi = addr & 0xFF00;
                let mut real_addr = cpu.reg_x as u16 + val;
                real_addr = hi | (real_addr & 0xFF);
                f(cpu, addr)
            })
        })
    }

    fn ind_y<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.ind_y_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn ind_y_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, move |cpu, val| {
            cpu.read_u16(val as u16, move |cpu, val| {
                let hi = addr & 0xFF00;
                let real_addr = cpu.reg_y as u16 + val;
                let new_hi = real_addr & 0xFF00;
                if hi != new_hi {
                    cpu.read(hi + (real_addr & 0xFF), move |cpu, _| {
                        f(cpu, real_addr)
                    })
                } else {
                    f(cpu, addr)
                }
            })
        })
    }

    fn rel_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, |cpu, val| {
            let addr = if val < 0x80 {
                cpu.reg_pc + (val as u16)
            } else {
                cpu.reg_pc - (0xFF - (val as u16)) - 1
            };
            f(cpu, addr)
        })
    }

    fn zpg<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.zpg_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn zpg_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, |cpu, val| {
            f(cpu, val as u16)
        })
    }

    fn zpg_x<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.zpg_x_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn zpg_x_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, |cpu, val| {
            let reg_x = cpu.reg_x;
            f(cpu, val.wrapping_add(reg_x) as u16)
        })
    }

    fn zpg_y<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, f: T) -> IORequest {
        self.zpg_y_addr(|cpu, addr| {
            cpu.read(addr, f)
        })
    }

    fn zpg_y_addr<T: 'static + FnOnce(&mut CPU6500, u16) -> IORequest>(&mut self, f: T) -> IORequest {
        let addr = self.reg_pc + 1;
        self.reg_pc += 2;
        self.read(addr, |cpu, val| {
            let reg_y = cpu.reg_y;
            f(cpu, val.wrapping_add(reg_y) as u16)
        })
    }

    fn and(&mut self, val: u8) -> IORequest {
        self.reg_a = self.reg_a & val;
        let reg_a = self.reg_a;
        self.set_nz(reg_a);
        self.execute()
    }

    fn bcs(&mut self, addr: u16) -> IORequest {
        if self.flag_c {
            self.reg_pc = addr;
        }
        self.execute()
    }

    fn bit(&mut self, val: u8) -> IORequest {
        self.flag_n = 0x80 & val != 0;
        self.flag_v = 0x40 & val != 0;
        let result = self.reg_a & val;
        self.flag_z = result == 0;
        self.execute()
    }

    fn bne(&mut self, addr: u16) -> IORequest {
        if !self.flag_z {
            self.reg_pc = addr;
        }
        self.execute()
    }

    fn bpl(&mut self, addr: u16) -> IORequest {
        if !self.flag_n {
            self.reg_pc = addr;
        }
        self.execute()
    }

    fn cld(&mut self) -> IORequest {
        self.flag_d = false;
        self.execute()
    }

    fn cmp(&mut self, val: u8) -> IORequest {
        let res = (self.reg_a as u16).wrapping_sub(val as u16);
        self.flag_z = res == 0;
        self.flag_n = res & 0x80 != 0;
        self.flag_c = res & 0x8000 == 0;
        self.execute()
    }

    fn cpx(&mut self, val: u8) -> IORequest {
        let res = (self.reg_x as u16).wrapping_sub(val as u16);
        self.flag_z = res == 0;
        self.flag_n = res & 0x80 != 0;
        self.flag_c = res & 0x8000 == 0;
        self.execute()
    }

    fn cpy(&mut self, val: u8) -> IORequest {
        let res = (self.reg_y as u16).wrapping_sub(val as u16);
        self.flag_z = res == 0;
        self.flag_n = res & 0x80 != 0;
        self.flag_c = res & 0x8000 == 0;
        self.execute()
    }

    fn dex(&mut self) -> IORequest {
        self.reg_x = self.reg_x.wrapping_sub(1);
        let reg_x = self.reg_x;
        self.set_nz(reg_x);
        self.execute()
    }

    fn dey(&mut self) -> IORequest {
        self.reg_y = self.reg_y.wrapping_sub(1);
        let reg_y = self.reg_y;
        self.set_nz(reg_y);
        self.execute()
    }

    fn inc(&mut self, addr: u16) -> IORequest {
        self.read(addr, move |cpu, val| {
            let val = val.wrapping_add(1);
            cpu.set_nz(val);
            cpu.write(addr, val, |cpu| cpu.execute())
        })
    }

    fn iny(&mut self) -> IORequest {
        self.reg_y = self.reg_y.wrapping_add(1);
        let reg_y = self.reg_y;
        self.set_nz(reg_y);
        self.execute()
    }

    fn jmp(&mut self, addr: u16) -> IORequest {
        self.reg_pc = addr;
        self.execute()
    }

    fn jsr(&mut self, addr: u16) -> IORequest {
        let ret_addr = self.reg_pc - 1;
        self.push_u16(ret_addr, move |cpu| {
            cpu.reg_pc = addr;
            cpu.execute()
        })
    }

    fn lda(&mut self, val: u8) -> IORequest {
        self.reg_a = val;
        self.set_nz(val);
        self.execute()
    }

    fn ldx(&mut self, val: u8) -> IORequest {
        self.reg_x = val;
        self.set_nz(val);
        self.execute()
    }

    fn ldy(&mut self, val: u8) -> IORequest {
        self.reg_y = val;
        self.set_nz(val);
        self.execute()
    }

    fn lsr_a(&mut self) -> IORequest {
        let reg_a = self.reg_a;
        self.flag_c = reg_a & 0x01 != 0;
        self.flag_z = reg_a == 0;
        self.reg_a = reg_a >> 1;
        self.execute()
    }

    fn ora(&mut self, val: u8) -> IORequest {
        self.reg_a = self.reg_a | val;
        let reg_a = self.reg_a;
        self.set_nz(reg_a);
        self.execute()
    }

    fn pha(&mut self) -> IORequest {
        let reg_a = self.reg_a;
        self.push(reg_a, CPU6500::execute)
    }

    fn pla(&mut self) -> IORequest {
        self.pop(|cpu, val| {
            cpu.reg_a = val;
            cpu.set_nz(val);
            cpu.execute()
        })
    }

    fn rol_a(&mut self) -> IORequest {
        let mut reg_a = self.reg_a;
        let carry = reg_a & 0x80 != 0;
        reg_a = reg_a << 1;
        if self.flag_c {
            reg_a += 1;
        }
        self.flag_c = carry;
        self.flag_z = reg_a == 0;
        self.reg_a = reg_a;
        self.execute()
    }

    fn rts(&mut self,) -> IORequest {
        self.pop_u16(|cpu, val| {
            cpu.reg_pc = val + 1;
            cpu.execute()
        })
    }

    fn sei(&mut self) -> IORequest {
        self.flag_i = true;
        self.execute()
    }

    fn sta(&mut self, addr: u16) -> IORequest {
        let val = self.reg_a;
        self.write(addr, val, |cpu| cpu.execute())
    }

    fn stx(&mut self, addr: u16) -> IORequest {
        let val = self.reg_x;
        self.write(addr, val, |cpu| cpu.execute())
    }

    fn tax(&mut self) -> IORequest {
        self.reg_x = self.reg_a;
        let reg_x = self.reg_x;
        self.set_nz(reg_x);
        self.execute()
    }

    fn txa(&mut self) -> IORequest {
        self.reg_a = self.reg_x;
        let reg_a = self.reg_a;
        self.set_nz(reg_a);
        self.execute()
    }

    fn txs(&mut self) -> IORequest {
        self.reg_sp = self.reg_x;
        self.execute()
    }

    fn set_nz(&mut self, val: u8) {
        self.flag_z = val == 0;
        self.flag_n = (val & 0x80) != 0;
    }

    fn read<T: 'static + FnOnce(&mut CPU6500, u8) -> IORequest>(&mut self, addr: u16, continuation: T) -> IORequest {
        self.continuation = Continuation::new(
            move |cpu, val| continuation(cpu, val.unwrap())
        );
        self.cycle_count += 1;
        IORequest::Read(addr)
    }

    fn write<T: 'static + FnOnce(&mut CPU6500) -> IORequest>(&mut self, addr: u16, val: u8, continuation: T) -> IORequest {
        self.continuation = Continuation::new(
            move |cpu, _| continuation(cpu)
        );
        self.cycle_count += 1;
        IORequest::Write(addr, val)
    }
}

#[cfg(test)]
mod tests {
    use cpu::*;
    use cpu::IORequest::*;

    #[test]
    fn test_lda() {
        let mut mem = [0xa9, 0x66, 0x85, 0x04, 0x00];
        let mut cpu = CPU6500::new();
        let actual = run_test(&mut cpu, 5, &mut mem);
        assert_eq!(vec!(
            Read(0x00),
            Read(0x01),
            Read(0x02),
            Read(0x03),
            Write(0x0004, 0x66)
        ), actual);
        assert_eq!(false, cpu.flag_n);
        assert_eq!(false, cpu.flag_z);
    }

    #[test]
    fn test_subroutine() {
        let mut mem = [0x20, 0x04, 0x00, 0x02, 0x20, 0x09, 0x00, 0x60, 0x02, 0x60];
        let mut cpu = CPU6500::new();
        let actual = run_test(&mut cpu, 18, &mut mem);
        assert_eq!(vec!(
            Read(0x00),
            Read(0x01),
            Read(0x02),
            Write(0x01ff, 0x00),
            Write(0x01fe, 0x02),
            Read(0x04),
            Read(0x05),
            Read(0x06),
            Write(0x01fd, 0x00),
            Write(0x01fc, 0x06),
            Read(0x09),
            Read(0x0a),
            Read(0x01fc),
            Read(0x01fd),
            Read(0x07),
            Read(0x08),
            Read(0x01fe),
            Read(0x01ff)
        ), actual);
    }

    fn run_test(cpu: &mut CPU6500, cycle_count: u128, mem: &mut [u8]) -> Vec<IORequest> {
        let mut ram = [0; 0x10000];
        let mut result = Vec::new();
        let mut io_req = cpu.run_from(0);
        result.push(io_req.clone());
        while cpu.cycle_count() < cycle_count {
            io_req = match io_req {
                Read(addr) => {
                    if (addr as usize) < mem.len() {
                        cpu.read_result(mem[addr as usize])
                    } else {
                        cpu.read_result(ram[addr as usize])
                    }
                },
                Write(addr, val) => {
                    if (addr as usize) < mem.len() {
                        mem[addr as usize] = val;
                    } else {
                        ram[addr as usize] = val;
                    }
                    cpu.write_result()
                },
                Fault(_) => return result
            };
            result.push(io_req.clone());
        }
        result
    }
}