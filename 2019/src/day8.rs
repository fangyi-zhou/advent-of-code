const WIDTH: usize = 25;
const HEIGHT: usize = 6;
#[aoc_generator(day8)]
pub fn input_generator(input: &str) -> Vec<String> {
    let mut layers = Vec::new();
    let mut idx = 0;
    while idx * WIDTH * HEIGHT < input.len() {
        layers.push(input[(idx * WIDTH * HEIGHT)..((idx + 1) * WIDTH * HEIGHT)].to_string());
        idx += 1;
    }
    layers
}

struct Count {
    zero: i32,
    one: i32,
    two: i32,
}

impl Count {
    fn new(input: &str) -> Count {
        let mut zero = 0;
        let mut one = 0;
        let mut two = 0;
        for c in input.chars() {
            match c {
                '0' => zero += 1,
                '1' => one += 1,
                '2' => two += 1,
                _ => {}
            }
        }
        Count { zero, one, two }
    }
}

fn overlay_ch(ch1: char, ch2: char) -> char {
    if ch1 == '2' {
        ch2
    } else {
        ch1
    }
}

fn overlay(layer1: &str, layer2: &str) -> String {
    let s = layer1
        .chars()
        .zip(layer2.chars())
        .map(|(x, y)| overlay_ch(x, y))
        .collect();
    s
}

#[aoc(day8, part1)]
pub fn solve_part1(input: &[String]) -> i32 {
    let min_layer = input
        .iter()
        .map(|s| Count::new(s))
        .min_by(|x, y| x.zero.cmp(&y.zero))
        .unwrap();
    min_layer.one * min_layer.two
}

fn render_char(ch: char) -> char {
    match ch {
        '0' => '#',
        '1' => '.',
        '2' => ' ',
        _ => unreachable!(),
    }
}

#[aoc(day8, part2)]
pub fn solve_part2(input: &[String]) -> i32 {
    let mut iter = input.iter();
    let fst = iter.next().unwrap();
    let final_layer = iter.fold((fst).clone(), |acc, layer| overlay(&acc, layer));
    for i in 0..HEIGHT {
        let output: String = final_layer[i * WIDTH..(i + 1) * WIDTH]
            .to_string()
            .chars()
            .map(render_char)
            .collect();
        println!("{}", output);
    }
    0
}
