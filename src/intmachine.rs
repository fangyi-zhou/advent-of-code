use std::collections::HashMap;
use std::collections::VecDeque;

fn b2i(input: bool) -> i64 {
    if input {
        1
    } else {
        0
    }
}

pub struct IntMachine {
    pub memory: HashMap<usize, i64>,
    pub inputs: VecDeque<i64>,
    pub outputs: VecDeque<i64>,
    pub pc: usize,
    pub finished: bool,
    pub base: i64,
}

impl IntMachine {
    pub fn new(input: &[i32]) -> IntMachine {
        let mut memory = HashMap::new();
        for (addr, val) in input.iter().enumerate() {
            memory.insert(addr, *val as i64);
        }
        IntMachine {
            memory,
            inputs: VecDeque::new(),
            outputs: VecDeque::new(),
            pc: 0,
            finished: false,
            base: 0,
        }
    }

    pub fn new_i64(input: &[i64]) -> IntMachine {
        let mut memory = HashMap::new();
        for (addr, val) in input.iter().enumerate() {
            memory.insert(addr, *val);
        }
        IntMachine {
            memory,
            inputs: VecDeque::new(),
            outputs: VecDeque::new(),
            pc: 0,
            finished: false,
            base: 0,
        }
    }

    pub fn input(&mut self, val: i64) {
        self.inputs.push_back(val)
    }

    pub fn output(&mut self) -> Option<i64> {
        self.outputs.pop_front()
    }

    fn mem_get(&mut self, addr: usize) -> i64 {
        match self.memory.get(&addr) {
            Some(val) => *val,
            None => {
                self.memory.insert(addr, 0);
                0
            }
        }
    }

    pub fn run(&mut self) {
        if self.finished {
            return;
        }
        loop {
            let instr = self.memory[&self.pc];
            // println!("pc:{}, instr:{}, base:{}", self.pc, instr, self.base);
            let opcode = instr % 100;
            match opcode {
                1 | 2 | 7 | 8 => {
                    let mode1 = instr / 100 % 10;
                    let mem1 = self.mem_get(self.pc + 1);
                    let operand1 = match mode1 {
                        0 => self.mem_get(mem1 as usize),
                        1 => mem1,
                        2 => self.mem_get((self.base + mem1) as usize),
                        _ => unreachable!(),
                    };
                    let mode2 = instr / 1000 % 10;
                    let mem2 = self.mem_get(self.pc + 2);
                    let operand2 = match mode2 {
                        0 => self.mem_get(mem2 as usize),
                        1 => mem2,
                        2 => self.mem_get((self.base + mem2) as usize),
                        _ => unreachable!(),
                    };
                    let mode3 = instr / 10000 % 10;
                    let dest = match mode3 {
                        0 => self.mem_get(self.pc + 3) as usize,
                        2 => (self.base + self.mem_get(self.pc + 3)) as usize,
                        _ => unreachable!(),
                    };
                    let result = match opcode {
                        1 => operand1 + operand2,
                        2 => operand1 * operand2,
                        7 => b2i(operand1 < operand2),
                        8 => b2i(operand1 == operand2),
                        _ => unreachable!(),
                    };
                    self.memory.insert(dest, result);
                    self.pc += 4;
                }
                3 => {
                    let mode1 = instr / 100 % 10;
                    let dest = match mode1 {
                        0 => self.mem_get(self.pc + 1) as usize,
                        2 => (self.base + self.mem_get(self.pc + 1)) as usize,
                        _ => unreachable!(),
                    };
                    match self.inputs.pop_front() {
                        Some(input) => {
                            self.memory.insert(dest, input);
                            self.pc += 2
                        }
                        None => {
                            break;
                        }
                    }
                }
                4 => {
                    let mode1 = instr / 100 % 10;
                    let mem1 = self.mem_get(self.pc + 1);
                    let operand1 = match mode1 {
                        0 => self.mem_get(mem1 as usize),
                        1 => mem1,
                        2 => self.mem_get((self.base + mem1) as usize),
                        _ => unreachable!(),
                    };
                    self.outputs.push_back(operand1);
                    self.pc += 2;
                }
                5 | 6 => {
                    let mode1 = instr / 100 % 10;
                    let mem1 = self.mem_get(self.pc + 1);
                    let operand1 = match mode1 {
                        0 => self.mem_get(mem1 as usize),
                        1 => mem1,
                        2 => self.mem_get((self.base + mem1) as usize),
                        _ => unreachable!(),
                    };
                    let mode2 = instr / 1000 % 10;
                    let mem2 = self.mem_get(self.pc + 2);
                    let operand2 = match mode2 {
                        0 => self.mem_get(mem2 as usize),
                        1 => mem2,
                        2 => self.mem_get((self.base + mem2) as usize),
                        _ => unreachable!(),
                    };
                    self.pc = if opcode == 5 && operand1 != 0 || opcode == 6 && operand1 == 0 {
                        operand2 as usize
                    } else {
                        self.pc + 3
                    }
                }
                9 => {
                    let mode1 = instr / 100 % 10;
                    let mem1 = self.mem_get(self.pc + 1);
                    let operand1 = match mode1 {
                        0 => self.mem_get(mem1 as usize),
                        1 => mem1,
                        2 => self.mem_get((self.base + mem1) as usize),
                        _ => unreachable!(),
                    };
                    self.base += operand1;
                    self.pc += 2;
                }
                99 => {
                    self.finished = true;
                    break;
                }
                _ => {
                    unreachable!();
                }
            }
        }
    }
}
