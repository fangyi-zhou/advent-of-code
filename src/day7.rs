use std::collections::VecDeque;

#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

fn b2i(input: bool) -> i32 {
    if input {
        1
    } else {
        0
    }
}

struct IntMachine {
    memory: Vec<i32>,
    inputs: VecDeque<i32>,
    outputs: VecDeque<i32>,
    pc: usize,
    finished: bool,
}

impl IntMachine {
    fn new(memory: &mut [i32]) -> IntMachine {
        IntMachine {
            memory: Vec::from(memory),
            inputs: VecDeque::new(),
            outputs: VecDeque::new(),
            pc: 0,
            finished: false,
        }
    }

    fn input(&mut self, val: i32) {
        self.inputs.push_back(val)
    }

    fn output(&mut self) -> Option<i32> {
        self.outputs.pop_front()
    }

    fn run(&mut self) {
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

fn run_with_config(program: &[i32], config: &[i32]) -> i32 {
    let mut input_val = 0;
    for i in 0..5 {
        let mut program_copy = program.to_owned();
        let mut machine = IntMachine::new(program_copy.as_mut());
        machine.input(config[i]);
        machine.input(input_val);
        machine.run();
        input_val = machine.output().unwrap();
    }
    input_val
}

fn run_with_config_forever(program: &[i32], config: &[i32]) -> i32 {
    let mut machines = Vec::new();
    for i in 0..5 {
        let mut program_copy = program.to_owned().clone();
        let mut machine = IntMachine::new(program_copy.as_mut());
        machine.input(config[i]);
        machines.push(machine);
    }
    machines[0].input(0);
    loop {
        let mut all_finished = true;
        for i in 0..5 {
            machines[i].run();
            if let Some(output) = machines[i].output() {
                machines[(i + 1) % 5].input(output);
            }
            all_finished = all_finished && machines[i].finished;
        }
        if all_finished {
            break;
        }
    }
    machines[0].inputs.pop_front().unwrap()
}

fn generate_config(start: i32, end: i32) -> Vec<Vec<i32>> {
    let mut configs = Vec::new();
    for i1 in start..end {
        for i2 in start..end {
            if i2 == i1 {
                continue;
            }
            for i3 in start..end {
                if i3 == i2 || i3 == i1 {
                    continue;
                }
                for i4 in start..end {
                    if i4 == i3 || i4 == i2 || i4 == i1 {
                        continue;
                    }
                    for i5 in start..end {
                        if i5 == i4 || i5 == i3 || i5 == i2 || i5 == i1 {
                            continue;
                        }
                        configs.push(vec![i1, i2, i3, i4, i5]);
                    }
                }
            }
        }
    }
    configs
}

#[aoc(day7, part1)]
pub fn solve_part1(input: &[i32]) -> i32 {
    let input = input.to_owned();
    generate_config(0, 5).iter().fold(0, |acc, config| {
        i32::max(acc, run_with_config(&input, config))
    })
}

#[aoc(day7, part2)]
pub fn solve_part2(input: &[i32]) -> i32 {
    let input = input.to_owned();
    generate_config(5, 10).iter().fold(0, |acc, config| {
        i32::max(acc, run_with_config_forever(&input, config))
    })
}
