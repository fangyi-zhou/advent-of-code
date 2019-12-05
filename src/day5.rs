#[aoc_generator(day5)]
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

fn run(input: &mut [i32], input_val: i32) -> i32 {
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
                input[dest] = input_val;
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

#[aoc(day5, part1)]
pub fn solve_part1(input: &[i32]) -> i32 {
    let mut input = input.to_owned();
    run(input.as_mut(), 1)
}

#[aoc(day5, part2)]
pub fn solve_part2(input: &[i32]) -> i32 {
    let mut input = input.to_owned();
    run(input.as_mut(), 5)
}
