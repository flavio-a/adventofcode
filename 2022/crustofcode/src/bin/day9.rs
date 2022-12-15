use crustofcode::*;
use std::collections::HashSet;

type Move = (char, i64);

fn parse_line(s: String) -> Move {
    let splits: Vec<&str> = s.split(' ').collect();
    assert_eq!(splits.len(), 2);
    return (splits[0].chars().nth(0).unwrap(), str2int(splits[1]));
}

fn dir_to_point(d: char) -> Point {
    match d {
        'R' => (0, 1),
        'L' => (0, -1),
        'U' => (1, 0),
        'D' => (-1, 0),
        _ => panic!("AAAAAA"),
    }
}

fn muovi((px, py): Point, (dx, dy): Point) -> Point {
    (px + dx, py + dy)
}

fn follow((tx, ty): Point, (hx, hy): Point) -> Point {
    let dx = hx - tx;
    let dy = hy - ty;
    let mut ax = 0;
    let mut ay = 0;
    if dx > 1 {
        ax = 1;
        ay = if dy > 0 {
            1
        } else if dy < 0 {
            -1
        } else {
            0
        };
    }
    if dx < -1 {
        ax = -1;
        ay = if dy > 0 {
            1
        } else if dy < 0 {
            -1
        } else {
            0
        };
    }
    if dy > 1 {
        ay = 1;
        ax = if dx > 0 {
            1
        } else if dx < 0 {
            -1
        } else {
            0
        };
    }
    if dy < -1 {
        ay = -1;
        ax = if dx > 0 {
            1
        } else if dx < 0 {
            -1
        } else {
            0
        };
    }
    return muovi((tx, ty), (ax, ay));
}

fn main() {
    println!("Day 9");

    let moves: Vec<Move> = read_input_lines().into_iter().map(parse_line).collect();

    // Part 1
    let mut visited = HashSet::new();
    let mut posh: Point = (0, 0);
    let mut post: Point = (0, 0);
    visited.insert(post);
    for &(dir, l) in &moves {
        for _ in 0..l {
            posh = muovi(posh, dir_to_point(dir));
            post = follow(post, posh);
            visited.insert(post);
        }
    }
    println!("{}", visited.len());

    // Part 2
    let mut visited = HashSet::new();
    let mut poss: [Point; 10] = [(0, 0); 10];
    visited.insert(poss[9]);
    for &(dir, l) in &moves {
        for _ in 0..l {
            poss[0] = muovi(poss[0], dir_to_point(dir));
            for i in 1..10 {
                poss[i] = follow(poss[i], poss[i - 1]);
            }
            visited.insert(poss[9]);
        }
    }
    println!("{}", visited.len());
}
