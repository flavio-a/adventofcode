use aoc_parse::{parser, prelude::*};
use crustofcode::{option_assert, Dir8, UPoint};

fn main() {
    println!("Day 4");

    let p = parser!(lines(char_of("XMAS")+));
    let board: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();
    let h = board.len();
    let w = board[0].len();

    let is_at = |&(i, j): &UPoint, v: usize| board[i][j] == v;

    // Part 1
    let res1: usize = itertools::iproduct!((0..w), (0..h))
        .map(|p: UPoint| {
            Dir8::ALL_DIRS
                .iter()
                .filter_map(|&d| {
                    let p0 = p;
                    option_assert(is_at(&p0, 0))?;
                    let p1 = d.move_point(p0, h, w)?;
                    option_assert(is_at(&p1, 1))?;
                    let p2 = d.move_point(p1, h, w)?;
                    option_assert(is_at(&p2, 2))?;
                    let p3 = d.move_point(p2, h, w)?;
                    option_assert(is_at(&p3, 3))?;
                    Some(p)
                })
                .count()
        })
        .sum();
    println!("{res1}");

    // Part 2
    let res2 = itertools::iproduct!((0..w), (0..h))
        .filter(|p| is_at(p, 2))
        .filter_map(|p| {
            let d = Dir8::LU;
            let pa = d.move_point(p, h, w)?;
            let pb = d.opposite().move_point(p, h, w)?;
            option_assert(is_at(&pa, 1) && is_at(&pb, 3) || (is_at(&pa, 3) && is_at(&pb, 1)))?;
            Some(p)
        })
        .filter_map(|p| {
            let d = Dir8::LD;
            let pa = d.move_point(p, h, w)?;
            let pb = d.opposite().move_point(p, h, w)?;
            option_assert(is_at(&pa, 1) && is_at(&pb, 3) || (is_at(&pa, 3) && is_at(&pb, 1)))?;
            Some(p)
        })
        .count();
    println!("{res2}");
}
