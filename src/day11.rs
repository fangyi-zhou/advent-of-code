use crate::intmachine::IntMachine;
use std::collections::HashMap;
#[aoc_generator(day11)]
pub fn input_generator(input: &str) -> Vec<i64> {
    input.split(',').map(|x| x.parse().unwrap()).collect()
}

type Point = (i32, i32);

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

enum Rotation {
    Left,
    Right,
}

fn rotation_of_int(i: i64) -> Rotation {
    match i {
        0 => Rotation::Left,
        1 => Rotation::Right,
        _ => unreachable!(),
    }
}

fn delta(dir: &Dir) -> Point {
    match dir {
        Dir::Up => (0, -1),
        Dir::Down => (0, 1),
        Dir::Left => (-1, 0),
        Dir::Right => (1, 0),
    }
}

fn rotate(dir: &Dir, rot: &Rotation) -> Dir {
    match rot {
        Rotation::Right => match dir {
            Dir::Up => Dir::Right,
            Dir::Right => Dir::Down,
            Dir::Down => Dir::Left,
            Dir::Left => Dir::Up,
        },
        Rotation::Left => rotate(
            &rotate(&rotate(dir, &Rotation::Right), &Rotation::Right),
            &Rotation::Right,
        ),
    }
}

fn forward(point: &Point, dir: &Dir) -> Point {
    let (x, y) = point;
    let (dx, dy) = delta(dir);
    (x + dx, y + dy)
}

#[aoc(day11, part1)]
pub fn solve_part1(input: &[i64]) -> i32 {
    let input = input.to_owned();
    let mut machine = IntMachine::new_i64(input.as_ref());
    let mut panel = HashMap::new();
    let mut dir = Dir::Up;
    let mut pos = (0, 0);
    while !machine.finished {
        machine.input(*panel.get(&pos).unwrap_or(&0));
        machine.run();
        let new_color = machine.output().unwrap();
        panel.insert(pos, new_color);
        let rotation = rotation_of_int(machine.output().unwrap());
        dir = rotate(&dir, &rotation);
        pos = forward(&pos, &dir);
    }
    panel.keys().len() as i32
}

fn panel_to_string(panel: HashMap<Point, i64>) -> String {
    let (minx, _) = panel.keys().min_by_key(|(x, _y)| x).unwrap();
    let (maxx, _) = panel.keys().max_by_key(|(x, _y)| x).unwrap();
    let (_, miny) = panel.keys().min_by_key(|(_x, y)| y).unwrap();
    let (_, maxy) = panel.keys().max_by_key(|(_x, y)| y).unwrap();
    let mut output = String::new();
    output.push('\n');
    for y in *miny..(*maxy + 1) {
        for x in *minx..(*maxx + 1) {
            let value = *panel.get(&(x, y)).unwrap_or(&0);
            if value == 0 {
                output.push('.');
            } else {
                output.push('#');
            }
        }
        output.push('\n');
    }
    output
}

#[aoc(day11, part2)]
pub fn solve_part2(input: &[i64]) -> String {
    let input = input.to_owned();
    let mut machine = IntMachine::new_i64(input.as_ref());
    let mut panel = HashMap::new();
    let mut dir = Dir::Up;
    let mut pos = (0, 0);
    panel.insert(pos, 1);
    while !machine.finished {
        machine.input(*panel.get(&pos).unwrap_or(&0));
        machine.run();
        let new_color = machine.output().unwrap();
        panel.insert(pos, new_color);
        let rotation = rotation_of_int(machine.output().unwrap());
        dir = rotate(&dir, &rotation);
        pos = forward(&pos, &dir);
    }
    panel_to_string(panel)
}
