use crate::intmachine::IntMachine;

#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

fn run_with_config(program: &[i32], config: &[i32]) -> i32 {
    let mut input_val = 0;
    for config in config.iter().take(5) {
        let mut program_copy = program.to_owned();
        let mut machine = IntMachine::new(program_copy.as_mut());
        machine.input((*config).into());
        machine.input(input_val);
        machine.run();
        input_val = machine.output().unwrap();
    }
    input_val as i32
}

fn run_with_config_forever(program: &[i32], config: &[i32]) -> i32 {
    let mut machines = Vec::new();
    for config in config.iter().take(5) {
        let mut program_copy = program.to_owned().clone();
        let mut machine = IntMachine::new(program_copy.as_mut());
        machine.input((*config).into());
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
    machines[0].inputs.pop_front().unwrap() as i32
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
