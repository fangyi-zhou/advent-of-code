use crate::intmachine::IntMachine;
use std::collections::HashMap;
#[aoc_generator(day13)]
pub fn input_generator(input: &str) -> Vec<i64> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

#[aoc(day13, part1)]
pub fn solve_part1(input: &[i64]) -> i32 {
    let input = input.to_owned();
    let mut machine = IntMachine::new_i64(input.as_ref());
    let mut panel = HashMap::new();
    machine.run();
    while !machine.outputs.is_empty() {
        let x = machine.output().unwrap();
        let y = machine.output().unwrap();
        let tile = machine.output().unwrap();
        panel.insert((x, y), tile);
    }
    panel
        .values()
        .into_iter()
        .filter(|tile| **tile == 2)
        .count() as i32
}
