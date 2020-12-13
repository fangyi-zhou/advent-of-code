use crate::intmachine::IntMachine;
#[aoc_generator(day5)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

#[aoc(day5, part1)]
pub fn solve_part1(input: &[i32]) -> i32 {
    let input = input.to_owned();
    let mut machine = IntMachine::new(input.clone().as_ref());
    machine.input(1);
    machine.run();
    machine.outputs.pop_back().unwrap() as i32
}

#[aoc(day5, part2)]
pub fn solve_part2(input: &[i32]) -> i32 {
    let input = input.to_owned();
    let mut machine = IntMachine::new(input.clone().as_ref());
    machine.input(5);
    machine.run();
    machine.outputs.pop_back().unwrap() as i32
}
