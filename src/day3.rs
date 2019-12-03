use std::collections::{HashMap, HashSet};
use std::i32;
use std::ops::Add;
use std::str::FromStr;

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }

    fn man_dist(&self) -> i32 {
        i32::abs(self.x) + i32::abs(self.y)
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Clone, Copy)]
enum Dir {
    U,
    D,
    L,
    R,
}

impl Dir {
    fn step(&self) -> Point {
        match &self {
            Dir::U => Point::new(1, 0),
            Dir::D => Point::new(-1, 0),
            Dir::L => Point::new(0, -1),
            Dir::R => Point::new(0, 1),
        }
    }

    fn move_in_dir(&self, point: Point) -> Point {
        self.step() + point
    }
}

#[derive(Clone)]
pub struct Waypoint {
    dir: Dir,
    steps: i32,
}

impl Waypoint {
    fn new(dir: Dir, steps: i32) -> Waypoint {
        Waypoint { dir, steps }
    }
}

impl FromStr for Waypoint {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let dir_char = s.chars().nth(0).unwrap();
        let dir = match dir_char {
            'U' => Ok(Dir::U),
            'D' => Ok(Dir::D),
            'L' => Ok(Dir::L),
            'R' => Ok(Dir::R),
            _ => Err(()),
        };
        let steps = s[1..].parse();
        match (dir, steps) {
            (Ok(dir), Ok(steps)) => Ok(Waypoint::new(dir, steps)),
            (_, _) => Err(()),
        }
    }
}

#[aoc_generator(day3)]
pub fn input_generator(input: &str) -> Vec<Vec<Waypoint>> {
    input
        .lines()
        .map(|l| {
            l.split(',')
                .map(|x| Waypoint::from_str(x).unwrap())
                .collect()
        })
        .collect()
}

struct CircuitIterator<'a> {
    waypoints: &'a Vec<Waypoint>,
    idx: usize,
    curr_steps: i32,
}

impl CircuitIterator<'_> {
    fn new(waypoints: &Vec<Waypoint>) -> CircuitIterator {
        CircuitIterator {
            waypoints,
            idx: 0,
            curr_steps: 0,
        }
    }

    fn next_dir<'a>(&'a mut self) -> Option<Dir> {
        if self.idx >= self.waypoints.len() {
            None
        } else {
            let first = self.waypoints.get(self.idx).unwrap();
            if self.curr_steps < first.steps {
                self.curr_steps += 1;
            }
            if self.curr_steps == first.steps {
                self.idx += 1;
                self.curr_steps = 0;
            }
            Some(first.dir)
        }
    }
}

fn make_trace(mut it: CircuitIterator) -> HashSet<Point> {
    let mut points = HashSet::new();
    let mut pos = Point::new(0, 0);
    loop {
        if let Some(dir) = it.next_dir() {
            pos = dir.move_in_dir(pos);
            points.insert(pos);
        } else {
            break;
        }
    }
    points
}

fn make_trace_with_steps(mut it: CircuitIterator) -> (HashSet<Point>, HashMap<Point, i32>) {
    let mut points = HashSet::new();
    let mut steps = HashMap::new();
    let mut pos = Point::new(0, 0);
    let mut step = 0;
    loop {
        if let Some(dir) = it.next_dir() {
            pos = dir.move_in_dir(pos);
            step += 1;
            points.insert(pos);
            if !steps.contains_key(&pos) {
                steps.insert(pos, step);
            }
        } else {
            break;
        }
    }
    (points, steps)
}

#[aoc(day3, part1)]
pub fn solve_part1(input: &Vec<Vec<Waypoint>>) -> i32 {
    let circuit1 = CircuitIterator::new(&input[0]);
    let circuit2 = CircuitIterator::new(&input[1]);
    let trace1 = make_trace(circuit1);
    let trace2 = make_trace(circuit2);
    trace1
        .intersection(&trace2)
        .fold(i32::max_value(), |mini, pt| i32::min(mini, pt.man_dist()))
}

#[aoc(day3, part2)]
pub fn solve_part2(input: &Vec<Vec<Waypoint>>) -> i32 {
    let circuit1 = CircuitIterator::new(&input[0]);
    let circuit2 = CircuitIterator::new(&input[1]);
    let (trace1, steps1) = make_trace_with_steps(circuit1);
    let (trace2, steps2) = make_trace_with_steps(circuit2);
    trace1
        .intersection(&trace2)
        .fold(i32::max_value(), |mini, pt| {
            i32::min(mini, steps1.get(&pt).unwrap() + steps2.get(&pt).unwrap())
        })
}
