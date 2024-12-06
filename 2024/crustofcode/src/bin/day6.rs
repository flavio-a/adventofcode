use aoc_parse::{parser, prelude::*};
use crustofcode::{count_true, Dir4, UPoint};

fn move_to_end(
    board: &Vec<Vec<bool>>,
    initial_pos: &UPoint,
    initial_dir: Dir4,
) -> Option<Vec<Vec<bool>>> {
    let h = board.len();
    let w = board[0].len();

    let mut visited: Vec<Vec<bool>> = vec![vec![false; w]; h];
    let mut visited_plus: Vec<Vec<[bool; 4]>> = vec![vec![[false; 4]; w]; h];
    let mut pos = initial_pos.clone();
    let mut dir = initial_dir.clone();

    visited[pos.0][pos.1] = true;
    visited_plus[pos.0][pos.1][dir.to_index()] = true;
    let mut new_pos = dir.move_point(pos, h, w);
    while new_pos.is_some() {
        let mut new_pos1 = new_pos.unwrap();
        while board[new_pos1.0][new_pos1.1] {
            // Obstacle, turn right
            dir = dir.turn_right();
            new_pos = dir.move_point(pos, h, w);
            new_pos1 = new_pos.unwrap();
        }
        pos = new_pos1;
        if visited_plus[pos.0][pos.1][dir.to_index()] {
            return None;
        }
        visited[pos.0][pos.1] = true;
        visited_plus[pos.0][pos.1][dir.to_index()] = true;
        new_pos = dir.move_point(pos, h, w);
    }

    return Some(visited);
}

fn main() {
    println!("Day 6");

    let p = parser!(lines(char_of(".#^")+));
    let board_in: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();
    let h = board_in.len();
    let w = board_in[0].len();
    let mut initial_pos: UPoint = (0, 0);
    let board: Vec<Vec<bool>> = board_in
        .into_iter()
        .enumerate()
        .map(|(i, row)| {
            row.into_iter()
                .enumerate()
                .map(|(j, c)| match c {
                    0 => false,
                    1 => true,
                    2 => {
                        initial_pos = (i, j);
                        false
                    }
                    _ => panic!("E"),
                })
                .collect()
        })
        .collect();
    let initial_pos: UPoint = initial_pos;

    // visualize_grid(&board);

    // Part 1
    let visisted1 = move_to_end(&board, &initial_pos, Dir4::U).unwrap();
    let res1 = count_true(&visisted1);
    println!("{res1}");

    // Part 2
    let mut obstacles: Vec<UPoint> = vec![];
    let mut board = board;
    for i in 0..h {
        for j in 0..w {
            if (i, j) != initial_pos && visisted1[i][j] {
                // Try to put an obstacle here
                assert!(!board[i][j]);
                board[i][j] = true;
                if move_to_end(&board, &initial_pos, Dir4::U).is_none() {
                    obstacles.push((i, j));
                }
                board[i][j] = false;
            }
        }
    }
    let res2 = obstacles.len();
    println!("{res2}");
}
