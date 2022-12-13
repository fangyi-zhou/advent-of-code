type Vec3 = (i32, i32, i32);

fn parse_vec3(input: &str) -> Vec3 {
    let input = input.strip_prefix("<").unwrap();
    let input = input.strip_suffix(">").unwrap();
    let mut splitted = input.split(", ");
    let x = splitted
        .next()
        .unwrap()
        .strip_prefix("x=")
        .unwrap()
        .parse()
        .unwrap();
    let y = splitted
        .next()
        .unwrap()
        .strip_prefix("y=")
        .unwrap()
        .parse()
        .unwrap();
    let z = splitted
        .next()
        .unwrap()
        .strip_prefix("z=")
        .unwrap()
        .parse()
        .unwrap();
    (x, y, z)
}

fn gcd(x: i64, y: i64) -> i64 {
    let x = i64::abs(x);
    let y = i64::abs(y);
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

fn lcm2(x: i64, y: i64) -> i64 {
    x * y / gcd(x, y)
}

fn lcm3(x: i64, y: i64, z: i64) -> i64 {
    lcm2(lcm2(x, y), z)
}

struct Moon {
    pos: Vec3,
    vel: Vec3,
}

impl Moon {
    fn new(pos: Vec3) -> Moon {
        Moon {
            pos,
            vel: (0, 0, 0),
        }
    }

    fn make_move(&mut self) {
        let (x, y, z) = self.pos;
        let (dx, dy, dz) = self.vel;
        self.pos = (x + dx, y + dy, z + dz);
    }

    fn interact(&mut self, other_pos: Vec3) {
        let (x1, y1, z1) = self.pos;
        let (x2, y2, z2) = other_pos;
        let (dx, dy, dz) = self.vel;
        self.vel = (
            dx + i32::min(1, i32::max(x2 - x1, -1)),
            dy + i32::min(1, i32::max(y2 - y1, -1)),
            dz + i32::min(1, i32::max(z2 - z1, -1)),
        )
    }

    fn energy(&self) -> i32 {
        let (x, y, z) = self.pos;
        let potential = i32::abs(x) + i32::abs(y) + i32::abs(z);
        let (dx, dy, dz) = self.vel;
        let kinetic = i32::abs(dx) + i32::abs(dy) + i32::abs(dz);
        kinetic * potential
    }
}

struct Moons {
    moons: Vec<Moon>,
    steps: i64,
    cycle_x: Option<i64>,
    cycle_y: Option<i64>,
    cycle_z: Option<i64>,
    init_x: Vec<(i32, i32)>,
    init_y: Vec<(i32, i32)>,
    init_z: Vec<(i32, i32)>,
}

impl Moons {
    fn new(moons: Vec<Moon>) -> Moons {
        let init_x = moons.iter().map(|moon| (moon.pos.0, moon.vel.0)).collect();
        let init_y = moons.iter().map(|moon| (moon.pos.1, moon.vel.1)).collect();
        let init_z = moons.iter().map(|moon| (moon.pos.2, moon.vel.2)).collect();
        Moons {
            steps: 0,
            moons,
            cycle_x: None,
            cycle_y: None,
            cycle_z: None,
            init_x,
            init_y,
            init_z,
        }
    }

    fn step(&mut self) {
        self.steps += 1;
        for i in 0..self.moons.len() {
            for j in 0..self.moons.len() {
                let other_pos = self.moons[j].pos;
                self.moons[i].interact(other_pos);
            }
        }
        for moon in &mut self.moons {
            moon.make_move();
        }
        let x_entry: Vec<(i32, i32)> = self
            .moons
            .iter()
            .map(|moon| (moon.pos.0, moon.vel.0))
            .collect();
        if x_entry == self.init_x {
            match self.cycle_x {
                Some(_) => {}
                None => {
                    self.cycle_x = Some(self.steps);
                }
            }
        }
        let y_entry: Vec<(i32, i32)> = self
            .moons
            .iter()
            .map(|moon| (moon.pos.1, moon.vel.1))
            .collect();
        if y_entry == self.init_y {
            match self.cycle_y {
                Some(_) => {}
                None => {
                    self.cycle_y = Some(self.steps);
                }
            }
        }
        let z_entry: Vec<(i32, i32)> = self
            .moons
            .iter()
            .map(|moon| (moon.pos.2, moon.vel.2))
            .collect();
        if z_entry == self.init_z {
            match self.cycle_z {
                Some(_) => {}
                None => {
                    self.cycle_z = Some(self.steps);
                }
            }
        }
    }

    fn energy(&self) -> i32 {
        self.moons.iter().map(|moon| moon.energy()).sum()
    }

    fn steps_to_origin(&mut self) -> i64 {
        loop {
            self.step();
            if self.cycle_x.is_some() && self.cycle_y.is_some() && self.cycle_z.is_some() {
                break;
            }
        }
        lcm3(
            self.cycle_x.unwrap(),
            self.cycle_y.unwrap(),
            self.cycle_z.unwrap(),
        )
    }
}

#[aoc_generator(day12)]
pub fn input_generator(input: &str) -> Vec<Vec3> {
    input.lines().map(parse_vec3).collect()
}

#[aoc(day12, part1)]
pub fn solve_part1(input: &[Vec3]) -> i32 {
    let moons_vec: Vec<Moon> = input.iter().map(|pos| Moon::new(*pos)).collect();
    let mut moons = Moons::new(moons_vec);
    let steps = 1000;
    for _ in 0..steps {
        moons.step();
    }
    moons.energy()
}

#[aoc(day12, part2)]
pub fn solve_part2(input: &[Vec3]) -> i64 {
    let moons_vec: Vec<Moon> = input.iter().map(|pos| Moon::new(*pos)).collect();
    let mut moons = Moons::new(moons_vec);
    moons.steps_to_origin()
}
