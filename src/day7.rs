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

fn run(input: &mut [i32], mut input_val: VecDeque<i32>) -> i32 {
    let mut pc: usize = 0;
    let mut output: Option<i32> = None;
    loop {
        let instr = input[pc];
        let opcode = instr % 100;
        match opcode {
            1 | 2 | 7 | 8 => {
                let operand1 = if (instr / 100 % 10) == 0 {
                    input[input[pc + 1] as usize]
                } else {
                    input[pc + 1]
                };
                let operand2 = if (instr / 1000 % 10) == 0 {
                    input[input[pc + 2] as usize]
                } else {
                    input[pc + 2]
                };
                let dest = input[pc + 3] as usize;
                let result = match opcode {
                    1 => operand1 + operand2,
                    2 => operand1 * operand2,
                    7 => b2i(operand1 < operand2),
                    8 => b2i(operand1 == operand2),
                    _ => unreachable!(),
                };
                input[dest] = result;
                pc += 4;
            }
            3 => {
                let dest = input[pc + 1] as usize;
                input[dest] = input_val.pop_front().unwrap();
                pc += 2;
            }
            4 => {
                let output_src = input[pc + 1] as usize;
                output = Some(input[output_src]);
                pc += 2;
            }
            5 | 6 => {
                let operand1 = if (instr / 100 % 10) == 0 {
                    input[input[pc + 1] as usize]
                } else {
                    input[pc + 1]
                };
                let operand2 = if (instr / 1000 % 10) == 0 {
                    input[input[pc + 2] as usize]
                } else {
                    input[pc + 2]
                };
                pc = if opcode == 5 && operand1 != 0 || opcode == 6 && operand1 == 0 {
                    operand2 as usize
                } else {
                    pc + 3
                }
            }
            99 => {
                break;
            }
            _ => {
                unreachable!();
            }
        }
    }
    output.unwrap()
}

fn run_with_config(program: &[i32], config: &[i32]) -> i32 {
    let mut input_val = 0;
    for i in 0..5 {
        let mut program_copy = program.to_owned();
        let mut input = VecDeque::new();
        input.push_back(config[i]);
        input.push_back(input_val);
        input_val = run(program_copy.as_mut(), input);
    }
    input_val
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
