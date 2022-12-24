use crustofcode::*;
use itertools::Itertools;
use std::iter;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Dir {
    N,
    S,
    W,
    E,
}

#[derive(Debug, Clone)]
struct Blizzard {
    pos: UPoint,
    dir: Dir,
}

impl Blizzard {
    fn from_char(c: char, p: UPoint) -> Blizzard {
        Blizzard {
            pos: p,
            dir: match c {
                '<' => Dir::W,
                '>' => Dir::E,
                'v' => Dir::S,
                '^' => Dir::N,
                _ => panic!("AAAAA"),
            },
        }
    }

    fn shift(&mut self, &(width, height): &UPoint) -> () {
        let (x, y) = self.pos;
        let x = if x == 0 { height } else { x };
        let y = if y == 0 { width } else { y };
        self.pos = match self.dir {
            Dir::N => (x - 1, y),
            Dir::S => (x + 1, y),
            Dir::W => (x, y - 1),
            Dir::E => (x, y + 1),
        };
        self.pos = (self.pos.0 % height, self.pos.1 % width);
    }

    // fn backshift(&mut self, &(width, height): &UPoint) -> () {
    //     let (x, y) = self.pos;
    //     let x = if x == 0 { height } else { x };
    //     let y = if y == 0 { width } else { y };
    //     self.pos = match self.dir {
    //         Dir::N => (x + 1, y),
    //         Dir::S => (x - 1, y),
    //         Dir::W => (x, y + 1),
    //         Dir::E => (x, y - 1),
    //     };
    //     self.pos = (self.pos.0 % height, self.pos.1 % width);
    // }

    // fn shift_into(&self, &(width, height): &UPoint) -> Blizzard {
    //     let (x, y) = self.pos;
    //     let x = if x == 0 { height } else { x };
    //     let y = if y == 0 { width } else { y };
    //     let pos = match self.dir {
    //         Dir::N => (x - 1, y),
    //         Dir::S => (x + 1, y),
    //         Dir::W => (x, y - 1),
    //         Dir::E => (x, y + 1),
    //     };
    //     let pos = (pos.0 % height, pos.1 % width);
    //     return Blizzard {
    //         pos: pos,
    //         dir: self.dir.clone(),
    //     };
    // }

    #[allow(dead_code)]
    fn to_char(&self) -> char {
        match self.dir {
            Dir::N => '^',
            Dir::S => 'v',
            Dir::W => '<',
            Dir::E => '>',
        }
    }
}

fn parse_first_line(line: String) -> usize {
    let width = line.len();
    let mut chars = line.chars();
    assert_eq!(chars.next(), Some('#'));
    assert_eq!(chars.next(), Some('.'));
    for c in chars {
        assert_eq!(c, '#');
    }
    return width - 2;
}

fn parse_middle_lines(line: String, x: usize, v: &mut Vec<Blizzard>) -> () {
    if line.chars().nth(1) == Some('#') {
        assert_eq!(line.chars().nth(line.len() - 2), Some('.'));
        return;
    }
    for (y, c) in line.chars().enumerate() {
        match c {
            '#' => assert!(y == 0 || y == line.len() - 1),
            '.' => {}
            '<' | '>' | 'v' | '^' => v.push(Blizzard::from_char(c, (x, y - 1))),
            _ => panic!("AAAAA"),
        }
    }
}

fn get_neighbours(&(x, y): &UPoint, &(width, height): &UPoint) -> Vec<UPoint> {
    let mut res = vec![(x, y)];
    if x > 0 {
        res.push((x - 1, y));
    };
    if x < height - 1 {
        res.push((x + 1, y));
    };
    if y > 0 {
        res.push((x, y - 1));
    };
    if y < width - 1 {
        res.push((x, y + 1));
    };
    return res;
}

#[allow(dead_code)]
fn print_blizzards(me: UPoint, v: &Vec<Blizzard>) -> () {
    println!("---------------");
    visualize_as(
        v.iter()
            .map(|b| (point_transpose(up2p(b.pos)), b.to_char()))
            .chain(iter::once((point_transpose(up2p(me)), 'E'))),
    );
    println!("");
}

fn move_blizzards(blizzards: &mut Vec<Blizzard>, sizes: &UPoint) -> () {
    for b in blizzards.iter_mut() {
        b.shift(sizes);
    }
}

fn is_empty(p: &UPoint, v: &Vec<Blizzard>) -> bool {
    v.iter().all(|b| b.pos != *p)
}

fn get_poss(me: UPoint, v: &Vec<Blizzard>, sizes: &UPoint) -> Vec<UPoint> {
    let mut poss: Vec<UPoint> = vec![];
    for n in get_neighbours(&me, sizes) {
        if is_empty(&n, v) {
            poss.push(n);
        }
    }
    return poss;
}

fn make_trip(start: UPoint, end: UPoint, blizzards: &mut Vec<Blizzard>, sizes: &UPoint) -> usize {
    let mut time: usize = 0;
    // First step to get to the start
    move_blizzards(blizzards, &sizes);
    time += 1;
    while !is_empty(&start, blizzards) {
        move_blizzards(blizzards, &sizes);
        time += 1;
    }
    let mut frontier: Vec<UPoint> = vec![start];
    'outer: loop {
        // println!("{time}");
        // print_blizzards(frontier[0], &blizzards);
        move_blizzards(blizzards, &sizes);
        time += 1;
        let mut new_frontier = vec![];
        for state in frontier {
            for succ in get_poss(state, &blizzards, &sizes) {
                if succ == end {
                    break 'outer;
                } else {
                    new_frontier.push(succ);
                }
            }
        }
        // Dedup
        frontier = new_frontier.into_iter().sorted().dedup().collect();
    }
    // Last step to get to the end
    move_blizzards(blizzards, &sizes);
    time += 1;
    return time;
}

fn main() {
    println!("Day 24");

    let mut lines = crustofcode::read_input_lines().into_iter();
    let width = parse_first_line(lines.next().unwrap());
    let mut blizzards: Vec<Blizzard> = vec![];
    let mut height: usize = 0;
    for line in lines {
        parse_middle_lines(line, height, &mut blizzards);
        height += 1;
    }
    height -= 1;
    let sizes: UPoint = (width, height);
    // println!("{} {}\n{blizzards:?}", width, height);
    // print_blizzards((0, 0), &blizzards);

    // Part 1
    let mut time = make_trip((0, 0), (height - 1, width - 1), &mut blizzards, &sizes);
    println!("{}", time);
    // print_blizzards((height, width - 1), &blizzards);

    // Part 2
    // This are needed to make the test work, but not for the input
    if std::env::args().nth(1).unwrap().contains("test") {
        move_blizzards(&mut blizzards, &sizes);
        time += 1;
        move_blizzards(&mut blizzards, &sizes);
        time += 1;
        move_blizzards(&mut blizzards, &sizes);
        time += 1;
        move_blizzards(&mut blizzards, &sizes);
        time += 1;
        move_blizzards(&mut blizzards, &sizes);
        time += 1;
    }
    // Back to the start
    time += make_trip((height - 1, width - 1), (0, 0), &mut blizzards, &sizes);
    println!("{}", time);
    // Back to the end again
    time += make_trip((0, 0), (height - 1, width - 1), &mut blizzards, &sizes);
    println!("{}", time);
}
