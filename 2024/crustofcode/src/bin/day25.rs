use aoc_parse::{parser, prelude::*};
use crustofcode::get_dimensions;
use itertools::{iproduct, Itertools};

fn is_lock(board: &Vec<Vec<bool>>) -> bool {
    assert_eq!(get_dimensions(board), (5, 7));
    assert!(board[0].iter().all_equal());
    assert!(board[6].iter().all_equal());
    board[0][0]
}

fn compact(board: &Vec<Vec<bool>>, downwards: bool) -> [usize; 5] {
    assert_eq!(get_dimensions(board), (5, 7));
    let rows = if downwards {
        (1..=6).collect_vec()
    } else {
        (1..=6).rev().collect_vec()
    };
    (0..5)
        .map(|j| {
            rows.iter()
                .position(|&i| board[i][j] != board[i - 1][j])
                .unwrap()
        })
        .collect_vec()
        .try_into()
        .unwrap()
}

fn kl_match((keys, lock): &(&[usize; 5], &[usize; 5])) -> bool {
    keys.iter().zip_eq(lock.iter()).all(|(ki, li)| ki + li <= 5)
}

fn main() {
    println!("Day 25");

    let board_p = parser!(lines({a:char_of(".#") => a == 1}+));
    let p = parser!(sections(board_p));
    let boards: Vec<Vec<Vec<bool>>> = p.parse(&crustofcode::read_input()).unwrap();

    let (locks, keys): (Vec<_>, Vec<_>) = boards.into_iter().partition(is_lock);
    let locks = locks.into_iter().map(|b| compact(&b, true)).collect_vec();
    let keys = keys.into_iter().map(|b| compact(&b, false)).collect_vec();

    // Part 1
    let res1 = iproduct!(keys.iter(), locks.iter())
        .filter(kl_match)
        .count();
    println!("{res1}");
}
