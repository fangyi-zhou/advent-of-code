#[aoc_generator(day4)]
pub fn input_generator(input: &str) -> (i32, i32) {
    let numbers: Vec<i32> = input.split('-').map(|x| x.parse().unwrap()).collect();
    (numbers[0], numbers[1])
}

fn is_valid_pw_part1(num: &i32) -> bool {
    let mut has_adj_identical = false;
    let mut prev = num % 10;
    let mut num_ = num / 10;
    while num_ > 0 {
        let last = num_ % 10;
        if last == prev {
            has_adj_identical = true;
        }
        if last > prev {
            return false;
        }
        prev = last;
        num_ /= 10;
    }
    has_adj_identical
}

fn is_valid_pw_part2(num: &i32) -> bool {
    let mut pass_adj_rule = false;
    let mut prev = num % 10;
    let mut prev_count = 1;
    let mut num_ = num / 10;
    while num_ > 0 {
        let last = num_ % 10;
        if last == prev {
            prev_count += 1;
        }
        if last > prev {
            return false;
        }
        if last < prev {
            if prev_count == 2 {
                pass_adj_rule = true;
            }
            prev_count = 1;
        }
        prev = last;
        num_ /= 10;
    }
    if prev_count == 2 {
        pass_adj_rule = true;
    }
    pass_adj_rule
}

#[aoc(day4, part1)]
pub fn solve_part1(range: &(i32, i32)) -> i32 {
    let (start, finish) = range;
    let mut count = 0;
    for i in *start..*finish {
        if is_valid_pw_part1(&i) {
            count += 1
        }
    }
    count
}

#[aoc(day4, part2)]
pub fn solve_part2(range: &(i32, i32)) -> i32 {
    let (start, finish) = range;
    let mut count = 0;
    for i in *start..*finish {
        if is_valid_pw_part2(&i) {
            count += 1;
        }
    }
    count
}
