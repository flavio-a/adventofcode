use aoc_parse::{parser, prelude::*};
use crustofcode::{get_dimensions, into_fst, Dir4, UPoint};
use itertools::Itertools;
use pathfinding::directed::astar;

fn neighbours(grid: &Vec<Vec<bool>>, (pos, dir): &(UPoint, Dir4)) -> Vec<((UPoint, Dir4), usize)> {
    let (w, h) = get_dimensions(grid);
    let mut res = vec![
        ((*pos, dir.turn_left()), 1000),
        ((*pos, dir.turn_right()), 1000),
    ];
    let moved = dir.move_point(*pos, h, w).filter(|&(i, j)| !grid[i][j]);
    if moved.is_some() {
        res.push(((moved.unwrap(), *dir), 1));
    }
    return res;
}

fn heuristic((pos, _): &(UPoint, Dir4), end: &UPoint) -> usize {
    let turn = pos.0.abs_diff(end.0) != 0 && pos.1.abs_diff(end.1) != 0;
    pos.0.abs_diff(end.0) + pos.1.abs_diff(end.1) + if turn { 1000 } else { 0 }
}

fn main() {
    println!("Day 16");

    let p = parser!(lines({c:char_of("#.SE")}+));
    let input_grid: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();
    let mut start: UPoint = (0, 0);
    let mut end: UPoint = (0, 0);
    let (w, h) = get_dimensions(&input_grid);
    let mut grid = vec![vec![false; w]; h];
    for i in 0..h {
        for j in 0..w {
            match input_grid[i][j] {
                0 => grid[i][j] = true,
                1 => grid[i][j] = false,
                2 => start = (i, j),
                3 => end = (i, j),
                _ => panic!("E"),
            }
        }
    }
    let grid = grid;
    let start = start;
    let end = end;

    // Part 1
    let (paths, res1) = astar::astar_bag(
        &(start, Dir4::R),
        |n| neighbours(&grid, n),
        |n| heuristic(n, &end),
        |(p, _)| *p == end,
    )
    .unwrap();
    println!("{res1}");

    // Part 2
    let res2 = paths.flatten().map(into_fst).unique().count();
    println!("{res2}");
}
