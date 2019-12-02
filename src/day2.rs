#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

fn set_mode(mut input: Vec<i32>, noun: i32, verb: i32) -> Vec<i32> {
    input[1] = noun;
    input[2] = verb;
    input
}

fn set_program_alarm(input: Vec<i32>) -> Vec<i32> {
    set_mode(input, 12, 2)
}

fn run(mut input: Vec<i32>) -> i32 {
    let mut pc: usize = 0;
    loop {
        let opcode = input[pc];
        match opcode {
            1 | 2 => {
                let operand1 = input[input[pc + 1] as usize];
                let operand2 = input[input[pc + 2] as usize];
                let dest = input[pc + 3] as usize;
                let result = if opcode == 1 {
                    operand1 + operand2
                } else {
                    operand1 * operand2
                };
                input[dest] = result;
                pc += 4;
            }
            99 => {
                break;
            }
            _ => {
                unreachable!();
            }
        }
    }
    input[0]
}

#[aoc(day2, part1)]
pub fn solve_part1(input: &Vec<i32>) -> i32 {
    let input = input.clone();
    let input = set_program_alarm(input);
    run(input)
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &Vec<i32>) -> i32 {
    for noun in 0..99 {
        for verb in 0..99 {
            let working_input = input.clone();
            let working_input = set_mode(working_input, noun, verb);
            let result = run(working_input);
            if (result == 19690720) {
                return 100 * noun + verb;
            }
        }
    }
    unreachable!()
}
