use aoc_parse::{parser, prelude::*};
use crustofcode::{Dir, UPoint};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    MirrBs,
    MirrS,
    SplitV,
    SplitH,
}

impl Tile {
    fn from_idx(i: usize) -> Tile {
        match i {
            0 => Tile::Empty,
            1 => Tile::MirrBs,
            2 => Tile::MirrS,
            3 => Tile::SplitV,
            4 => Tile::SplitH,
            _ => panic!("Unexpected plot twist"),
        }
    }

    #[allow(dead_code)]
    fn to_char(&self) -> char {
        match self {
            Tile::Empty => '.',
            Tile::MirrBs => '\\',
            Tile::MirrS => '/',
            Tile::SplitH => '-',
            Tile::SplitV => '|',
        }
    }
}

fn dir_to_bit(d: &Dir) -> u8 {
    match d {
        Dir::D => 1,
        Dir::U => 2,
        Dir::L => 4,
        Dir::R => 8,
    }
}

fn has_dir(v: u8, d: &Dir) -> bool {
    let p: u8 = dir_to_bit(d);
    v & p == p
}

fn add_dir(v: u8, d: &Dir) -> u8 {
    v | dir_to_bit(d)
}

fn add_stack(stack: &mut Vec<(UPoint, Dir)>, h: usize, w: usize, i: usize, j: usize, d: Dir) {
    match d {
        Dir::R => {
            if j + 1 < w {
                stack.push(((i, j + 1), d))
            }
        }
        Dir::L => {
            if j > 0 {
                stack.push(((i, j - 1), d))
            }
        }
        Dir::U => {
            if i > 0 {
                stack.push(((i - 1, j), d))
            }
        }
        Dir::D => {
            if i + 1 < h {
                stack.push(((i + 1, j), d))
            }
        }
    }
}

fn energize_first_aid(board: &Vec<Vec<Tile>>, start: (UPoint, Dir)) -> usize {
    let mut lights = vec![vec![0_u8; board[0].len()]; board.len()];
    let mut stack: Vec<(UPoint, Dir)> = vec![start];
    while !stack.is_empty() {
        let ((i, j), dir) = stack.pop().unwrap();
        if !has_dir(lights[i][j], &dir) {
            lights[i][j] = add_dir(lights[i][j], &dir);
            match board[i][j] {
                Tile::Empty => add_stack(&mut stack, board.len(), board[0].len(), i, j, dir),
                Tile::MirrBs => match dir {
                    Dir::D => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::R),
                    Dir::U => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::L),
                    Dir::L => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::U),
                    Dir::R => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::D),
                },
                Tile::MirrS => match dir {
                    Dir::D => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::L),
                    Dir::U => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::R),
                    Dir::L => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::D),
                    Dir::R => add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::U),
                },
                Tile::SplitH => match dir {
                    Dir::D | Dir::U => {
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::L);
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::R)
                    }
                    Dir::L | Dir::R => {
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, dir)
                    }
                },
                Tile::SplitV => match dir {
                    Dir::L | Dir::R => {
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::U);
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, Dir::D)
                    }
                    Dir::U | Dir::D => {
                        add_stack(&mut stack, board.len(), board[0].len(), i, j, dir)
                    }
                },
            }
        }
    }
    // visualize_grid_t(&lights, |v| if *v > 0 { '#' } else { '.' });
    return lights
        .into_iter()
        .map(|r| {
            r.into_iter()
                .map(|v| if v > 0 { 1 } else { 0 })
                .sum::<usize>()
        })
        .sum();
}

fn energize_revitalize(board: &Vec<Vec<Tile>>) -> usize {
    let mut all_starts = vec![];
    for i in 0..board.len() {
        all_starts.push(((i, 0), Dir::R));
        all_starts.push(((i, board[0].len() - 1), Dir::L));
    }
    for j in 0..board[0].len() {
        all_starts.push(((0, j), Dir::D));
        all_starts.push(((board.len() - 1, j), Dir::U));
    }
    all_starts
        .into_iter()
        .map(|start| energize_first_aid(&board, start))
        .max()
        .unwrap()
}

fn main() {
    println!("Day 16");

    let tile_p = parser!(i:char_of(".\\/|-") => Tile::from_idx(i));
    let p = parser!(lines(tile_p+));
    let board: Vec<Vec<Tile>> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let r1 = energize_first_aid(&board, ((0, 0), Dir::R));
    println!("{r1}");

    // Part 2
    let r2 = energize_revitalize(&board);
    println!("{r2}");
}
