use crustofcode::*;
use std::collections::HashMap;
use std::collections::HashSet;

const DISPLACE: i64 = 4;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Dir {
    N,
    S,
    W,
    E,
}

impl Dir {
    fn to_check(&self, &(x, y): &Point) -> [Point; 3] {
        match self {
            Dir::N => [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)],
            Dir::S => [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)],
            Dir::W => [(x + 1, y - 1), (x, y - 1), (x - 1, y - 1)],
            Dir::E => [(x + 1, y + 1), (x, y + 1), (x - 1, y + 1)],
        }
    }

    fn shift(&self, p: &Point) -> Point {
        self.to_check(p)[1]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Elf {
    pos: Point,
}

type Elves = Vec<Elf>;
type Board = HashSet<Point>;

impl Elf {
    fn get_list(round_num: usize) -> [Dir; 4] {
        match round_num % 4 {
            0 => [Dir::N, Dir::S, Dir::W, Dir::E],
            1 => [Dir::S, Dir::W, Dir::E, Dir::N],
            2 => [Dir::W, Dir::E, Dir::N, Dir::S],
            3 => [Dir::E, Dir::N, Dir::S, Dir::W],
            _ => panic!("AAAAA"),
        }
    }

    fn has_adj(&self, board: &Board) -> bool {
        let (x, y) = self.pos;
        [
            (x + 1, y),
            (x + 1, y + 1),
            (x, y + 1),
            (x - 1, y + 1),
            (x - 1, y),
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
        ]
        .into_iter()
        .any(|p| board.contains(&p))
    }

    fn propose(&self, board: &Board, round_num: usize) -> Option<Dir> {
        for d in Elf::get_list(round_num) {
            if d.to_check(&self.pos)
                .into_iter()
                .all(|p| !board.contains(&p))
            {
                return Some(d);
            }
        }
        return None;
    }
}

#[allow(dead_code)]
fn print_elves(elves: &Elves) -> () {
    let e: Vec<Point> = elves
        .iter()
        .map(|e| (e.pos.1 + DISPLACE, e.pos.0 + DISPLACE))
        .collect();
    visualize(e.iter());
    println!("\n----------------------");
}

fn make_round(elves: Elves, round_num: usize) -> Elves {
    let board: Board = HashSet::from_iter(elves.iter().map(|e| e.pos));
    let elves: Vec<(Elf, Option<Point>)> = elves
        .into_iter()
        .map(|e| {
            let target = if e.has_adj(&board) {
                e.propose(&board, round_num).map(|dir| dir.shift(&e.pos))
            } else {
                None
            };
            (e, target)
        })
        .collect();
    let mut targets: HashMap<Point, i64> = HashMap::with_capacity(elves.len());
    for (_, pt) in &elves {
        if pt.is_some() {
            let target = pt.clone().unwrap();
            *targets.entry(target).or_insert(0) += 1;
        }
    }
    let targets: HashSet<Point> =
        HashSet::from_iter(targets.into_iter().filter(|(_, v)| *v >= 2).map(|(p, _)| p));
    return elves
        .into_iter()
        .map(|(e, pt)| {
            if pt.is_none() {
                e
            } else {
                let target = pt.unwrap();
                if targets.contains(&target) {
                    e
                } else {
                    Elf { pos: target }
                }
            }
        })
        .collect();
}

fn main() {
    println!("Day 23");

    let mut elves: Elves = vec![];
    for (x, line) in crustofcode::read_input_lines().into_iter().enumerate() {
        let x: i64 = x.try_into().unwrap();
        for (y, c) in line.chars().enumerate() {
            let y: i64 = y.try_into().unwrap();
            if c == '#' {
                let elf = Elf { pos: (x, y) };
                elves.push(elf);
            }
        }
    }

    // Part 1
    for i in 0..10 {
        elves = make_round(elves, i);
    }
    let minx = elves.iter().map(|e| e.pos.0).min().unwrap();
    let maxx = elves.iter().map(|e| e.pos.0).max().unwrap();
    let miny = elves.iter().map(|e| e.pos.1).min().unwrap();
    let maxy = elves.iter().map(|e| e.pos.1).max().unwrap();
    println!(
        "{}",
        (maxx - minx + 1) * (maxy - miny + 1) - i64::try_from(elves.len()).unwrap()
    );

    // Part 2
    let mut i = 10;
    let mut oldelves = elves.clone();
    elves = make_round(elves, i);
    i += 1;
    while elves != oldelves {
        oldelves = elves.clone();
        elves = make_round(elves, i);
        i += 1;
    }
    println!("{}", i);
}
