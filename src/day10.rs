use std::collections::HashSet;
#[aoc_generator(day10)]
pub fn input_generator(input: &str) -> HashSet<(i32, i32)> {
    let mut points = HashSet::new();
    for (x, line) in input.lines().enumerate() {
        for (y, ch) in line.chars().enumerate() {
            match ch {
                '.' => {}
                '#' => {
                    points.insert((x as i32, y as i32));
                }
                _ => unreachable!(),
            }
        }
    }
    points
}

fn gcd(x: i32, y: i32) -> i32 {
    let x = i32::abs(x);
    let y = i32::abs(y);
    if x == 0 && y == 0 {
        1
    } else if x < y {
        gcd(y, x)
    } else if x == y {
        x
    } else if y == 0 {
        x
    } else {
        gcd(x % y, y)
    }
}

fn diff(this: &(i32, i32), that: &(i32, i32)) -> (i32, i32) {
    let (x1, y1) = this;
    let (x2, y2) = that;
    match (x2 - x1, y2 - y1) {
        (0, 0) => (0, 0),
        (x, y) => {
            let gcd = gcd(x, y);
            (x / gcd, y / gcd)
        }
    }
}

#[aoc(day10, part1)]
pub fn solve_part1(input: &HashSet<(i32, i32)>) -> i32 {
    input.iter().fold(0, |acc, point| {
        let mut visible_points = HashSet::new();
        for other_point in input.iter() {
            visible_points.insert(diff(&point, &other_point));
        }
        i32::max(acc, visible_points.len() as i32)
    }) - 1
}
