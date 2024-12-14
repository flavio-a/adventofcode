use aoc_parse::{parser, prelude::*};
use crustofcode::{up2p, visualize_as, Point, UPoint};

// const W: usize = 11;
// const H: usize = 7;
const W: usize = 101;
const H: usize = 103;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Robot {
    pos: UPoint,
    vel: Point,
}

impl Robot {
    fn check(&self) {
        assert!(self.vel.0 < W.try_into().unwrap());
        assert!(self.vel.1 < H.try_into().unwrap());
    }

    fn move_n(&mut self, n: usize) {
        let ni: isize = n.try_into().unwrap();
        self.pos.0 = (self.pos.0 + W * n).saturating_add_signed(self.vel.0 * ni) % W;
        self.pos.1 = (self.pos.1 + H * n).saturating_add_signed(self.vel.1 * ni) % H;
    }

    fn get_quadrant(&self) -> Option<usize> {
        let w2 = W / 2;
        let h2 = H / 2;
        if self.pos.0 == w2 || self.pos.1 == h2 {
            return None;
        }
        let r = if self.pos.0 < w2 { 1 } else { 0 };
        let c = if self.pos.1 < h2 { 1 } else { 0 };
        return Some(r * 2 + c);
    }
}

fn main() {
    println!("Day 14");

    let p = parser!(lines(
        "p=" px:usize "," py:usize " v=" vx:isize "," vy:isize
        => Robot { pos: (px, py), vel: (vx, vy) }
    ));
    let robots: Vec<Robot> = p.parse(&crustofcode::read_input()).unwrap();
    for robot in &robots {
        robot.check();
    }

    // Part 1
    let mut robots1 = robots.clone();
    let mut quadrants = [0; 4];
    for robot in &mut robots1 {
        robot.move_n(100);
        let q = robot.get_quadrant();
        if q.is_some() {
            quadrants[q.unwrap()] += 1;
        }
    }
    let res1: usize = quadrants.into_iter().product();
    println!("{res1}");

    // Part 2
    let mut robots2 = robots.clone();
    // let mut n = 95;
    let j = 101;
    let mut n = 7569 - j;
    for robot in &mut robots2 {
        robot.move_n(n);
    }
    loop {
        for robot in &mut robots2 {
            robot.move_n(j);
        }
        visualize_as(robots2.iter().map(|r| (up2p(r.pos), '█')));
        n += j;
        println!("\n{n}");
        println!("---------------------------------------------------------------------------------------");
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
}
