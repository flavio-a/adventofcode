use crustofcode::*;
use std::collections::HashSet;

type Move = (char, i64);
type Pos = (i64, i64);

fn parse_line(s: String) -> Move {
    let splits: Vec<&str> = s.split(' ').collect();
    assert_eq!(splits.len(), 2);
    return (splits[0].chars().nth(0).unwrap(), str2int(splits[1]));
}

fn dir_to_pos(d: char) -> Pos {
    match d {
        'R' => (0, 1),
        'L' => (0, -1),
        'U' => (1, 0),
        'D' => (-1, 0),
        _ => panic!("AAAAAA"),
    }
}

fn muovi((px, py): Pos, (dx, dy): Pos) -> Pos {
    (px + dx, py + dy)
}

fn follow((tx, ty): Pos, (hx, hy): Pos) -> Pos {
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
    let mut posh: Pos = (0, 0);
    let mut post: Pos = (0, 0);
    visited.insert(post);
    for &(dir, l) in &moves {
        for _ in 0..l {
            posh = muovi(posh, dir_to_pos(dir));
            post = follow(post, posh);
            visited.insert(post);
        }
    }
    println!("{}", visited.len());

    // Part 2
    let mut visited = HashSet::new();
    let mut poss: [Pos; 10] = [(0, 0); 10];
    visited.insert(poss[9]);
    for &(dir, l) in &moves {
        for _ in 0..l {
            poss[0] = muovi(poss[0], dir_to_pos(dir));
            for i in 1..10 {
                poss[i] = follow(poss[i], poss[i - 1]);
            }
            visited.insert(poss[9]);
        }
    }
    println!("{}", visited.len());
}
