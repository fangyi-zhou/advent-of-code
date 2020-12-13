use crate::intmachine::IntMachine;
#[aoc_generator(day9)]
pub fn input_generator(input: &str) -> Vec<i64> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

#[aoc(day9, part1)]
pub fn solve_part1(input: &[i64]) -> i64 {
    let input = input.to_owned();
    let mut machine = IntMachine::new_i64(input.clone().as_ref());
    machine.input(1);
    machine.run();
    machine.outputs.pop_back().unwrap()
}

#[aoc(day9, part2)]
pub fn solve_part2(input: &[i64]) -> i64 {
    let input = input.to_owned();
    let mut machine = IntMachine::new_i64(input.clone().as_ref());
    machine.input(2);
    machine.run();
    machine.outputs.pop_back().unwrap()
}
