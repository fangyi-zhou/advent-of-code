use std::collections::VecDeque;

fn b2i(input: bool) -> i32 {
    if input {
        1
    } else {
        0
    }
}

pub struct IntMachine {
    pub memory: Vec<i32>,
    pub inputs: VecDeque<i32>,
    pub outputs: VecDeque<i32>,
    pub pc: usize,
    pub finished: bool,
}

impl IntMachine {
    pub fn new(memory: &[i32]) -> IntMachine {
        IntMachine {
            memory: Vec::from(memory),
            inputs: VecDeque::new(),
            outputs: VecDeque::new(),
            pc: 0,
            finished: false,
        }
    }

    pub fn input(&mut self, val: i32) {
        self.inputs.push_back(val)
    }

    pub fn output(&mut self) -> Option<i32> {
        self.outputs.pop_front()
    }

    pub fn run(&mut self) {
        if self.finished {
            return;
        }
        loop {
            let instr = self.memory[self.pc];
            let opcode = instr % 100;
            match opcode {
                1 | 2 | 7 | 8 => {
                    let operand1 = if (instr / 100 % 10) == 0 {
                        self.memory[self.memory[self.pc + 1] as usize]
                    } else {
                        self.memory[self.pc + 1]
                    };
                    let operand2 = if (instr / 1000 % 10) == 0 {
                        self.memory[self.memory[self.pc + 2] as usize]
                    } else {
                        self.memory[self.pc + 2]
                    };
                    let dest = self.memory[self.pc + 3] as usize;
                    let result = match opcode {
                        1 => operand1 + operand2,
                        2 => operand1 * operand2,
                        7 => b2i(operand1 < operand2),
                        8 => b2i(operand1 == operand2),
                        _ => unreachable!(),
                    };
                    self.memory[dest] = result;
                    self.pc += 4;
                }
                3 => {
                    let dest = self.memory[self.pc + 1] as usize;
                    match self.inputs.pop_front() {
                        Some(input) => {
                            self.memory[dest] = input;
                            self.pc += 2
                        }
                        None => {
                            break;
                        }
                    }
                }
                4 => {
                    let output_src = self.memory[self.pc + 1] as usize;
                    self.outputs.push_back(self.memory[output_src]);
                    self.pc += 2;
                }
                5 | 6 => {
                    let operand1 = if (instr / 100 % 10) == 0 {
                        self.memory[self.memory[self.pc + 1] as usize]
                    } else {
                        self.memory[self.pc + 1]
                    };
                    let operand2 = if (instr / 1000 % 10) == 0 {
                        self.memory[self.memory[self.pc + 2] as usize]
                    } else {
                        self.memory[self.pc + 2]
                    };
                    self.pc = if opcode == 5 && operand1 != 0 || opcode == 6 && operand1 == 0 {
                        operand2 as usize
                    } else {
                        self.pc + 3
                    }
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
