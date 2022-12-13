#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

fn set_mode(input: &mut [i32], noun: i32, verb: i32) {
    input[1] = noun;
    input[2] = verb;
}

fn set_program_alarm(input: &mut [i32]) {
    set_mode(input, 12, 2)
}

fn run(input: &mut [i32]) -> i32 {
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
pub fn solve_part1(input: &[i32]) -> i32 {
    let mut input = input.to_owned();
    set_program_alarm(input.as_mut());
    run(input.as_mut())
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &[i32]) -> i32 {
    for noun in 0..99 {
        for verb in 0..99 {
            let mut working_input = input.to_owned();
            set_mode(working_input.as_mut(), noun, verb);
            let result = run(working_input.as_mut());
            if result == 19_690_720 {
                return 100 * noun + verb;
            }
        }
    }
    unreachable!()
}
