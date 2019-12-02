#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> Vec<i32> {
    input.lines().map(|x| x.parse().unwrap()).collect()
}

#[aoc(day1, part1)]
pub fn solve_part1(input: &Vec<i32>) -> i32 {
    input.into_iter().map(|x| x / 3 - 2).sum()
}

fn get_cost(x: &i32) -> i32 {
    let cost = x / 3 - 2;
    if cost <= 0 {
        0
    } else {
        cost + get_cost(&cost)
    }
}

#[aoc(day1, part2)]
pub fn solve_part2(input: &Vec<i32>) -> i32 {
    input.into_iter().map(|x| get_cost(&x)).sum()
}
