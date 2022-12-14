use std::collections::hash_map::Keys;
use std::collections::HashMap;

type Point = (i32, i32);

#[aoc_generator(day10)]
pub fn input_generator(input: &str) -> Vec<Point> {
    let mut points = Vec::new();
    for (x, line) in input.lines().enumerate() {
        for (y, ch) in line.chars().enumerate() {
            match ch {
                '.' => {}
                '#' => {
                    points.push((x as i32, y as i32));
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
    } else if x == y || y == 0 {
        x
    } else {
        gcd(x % y, y)
    }
}

fn diff(this: &Point, that: &Point) -> Point {
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

fn max_elem(input: &[Point]) -> (&Point, HashMap<Point, Vec<&Point>>, i32) {
    input
        .iter()
        .fold((&(0, 0), HashMap::new(), 0), |(pt, pts, maxi), point| {
            let mut visible_points: HashMap<(i32, i32), Vec<_>> = HashMap::new();
            for other_point in input.iter() {
                visible_points
                    .entry(diff(point, other_point))
                    .or_default()
                    .push(other_point);
            }
            let new_len = visible_points.keys().len() as i32;
            if new_len > maxi {
                (point, visible_points, new_len)
            } else {
                (pt, pts, maxi)
            }
        })
}

fn distance(this: &Point, that: &Point) -> i32 {
    let (x1, y1) = this;
    let (x2, y2) = that;
    (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
}

#[aoc(day10, part1)]
pub fn solve_part1(input: &[Point]) -> i32 {
    let (_, _, ans) = max_elem(input);
    ans - 1
}

fn sort_entries<'a>(pts: Keys<'a, Point, Vec<&Point>>) -> Vec<&'a Point> {
    let mut indices = Vec::new();
    for index in pts {
        indices.push(index);
    }
    indices.sort_by(|(x1, y1), (x2, y2)| {
        f64::partial_cmp(
            &(f64::atan2((*y2).into(), (*x2).into())),
            &(f64::atan2((*y1).into(), (*x1).into())),
        )
        .unwrap()
    });
    indices
}

#[aoc(day10, part2)]
pub fn solve_part2(input: &[Point]) -> i32 {
    let (pt, mut pts, _) = max_elem(input);
    pts.remove(&(0, 0));
    for (_, points) in pts.iter_mut() {
        points.sort_by(|pt1, pt2| i32::cmp(&distance(pt, pt1), &distance(pt, pt2)))
    }
    let mut vaporised = Vec::new();
    while !pts.is_empty() {
        let pts_copy = pts.clone();
        let indices = sort_entries(pts_copy.keys());
        for index in indices {
            vaporised.push(pts.get_mut(index).unwrap().remove(0));
            if pts.get(index).unwrap().is_empty() {
                pts.remove(index);
            }
        }
    }
    // Question askes 200th, but we index from 0
    let (x, y) = vaporised[199];
    // Our x and y are inverted
    100 * y + x
}
