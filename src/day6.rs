use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub struct Edge {
    src: String,
    dst: String,
}

impl Edge {
    fn new(src: &str, dst: &str) -> Edge {
        let src = src.to_string();
        let dst = dst.to_string();
        Edge { src, dst }
    }
}

#[aoc_generator(day6)]
pub fn input_generator(input: &str) -> Vec<Edge> {
    input
        .lines()
        .map(|ln| {
            let mut splitted = ln.split(')');
            let src = splitted.next().unwrap();
            let dst = splitted.next().unwrap();
            Edge::new(src, dst)
        })
        .collect()
}

fn make_directed(input: &[Edge]) -> HashMap<&String, Vec<&String>> {
    let mut nodes: HashMap<&String, Vec<&String>> = HashMap::new();
    for edge in input {
        let src = nodes.entry(&edge.src).or_insert_with(Vec::new);
        src.push(&edge.dst);
    }
    nodes
}

fn make_bidirected(input: &[Edge]) -> HashMap<&String, Vec<&String>> {
    let mut nodes: HashMap<&String, Vec<&String>> = HashMap::new();
    for edge in input {
        let src = nodes.entry(&edge.src).or_insert_with(Vec::new);
        src.push(&edge.dst);
        let dst = nodes.entry(&edge.dst).or_insert_with(Vec::new);
        dst.push(&edge.src);
    }
    nodes
}

#[aoc(day6, part1)]
pub fn solve_part1(input: &[Edge]) -> i32 {
    let nodes = make_directed(input);
    let mut queue = VecDeque::new();
    let com = "COM".to_string();
    queue.push_back((&com, 0));
    let mut acc = 0;
    while !queue.is_empty() {
        let (node, depth) = queue.pop_front().unwrap();
        acc += depth;
        if let Some(children) = nodes.get(node) {
            for c in children.iter() {
                queue.push_back((c, depth + 1));
            }
        }
    }
    acc
}

#[aoc(day6, part2)]
pub fn solve_part2(input: &[Edge]) -> i32 {
    let nodes = make_bidirected(input);
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    let start = "YOU".to_string();
    seen.insert(&start);
    let end = "SAN".to_string();
    queue.push_back((&start, 0));
    while !queue.is_empty() {
        let (node, distance) = queue.pop_front().unwrap();
        if let Some(children) = nodes.get(node) {
            for c in children.iter() {
                if **c == end {
                    return distance - 1;
                }
                if !seen.contains(c) {
                    queue.push_back((c, distance + 1));
                    seen.insert(c);
                }
            }
        }
    }
    0
}
