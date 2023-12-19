use aoc_parse::{parser, prelude::*};
use crustofcode::UPoint;
use itertools::Itertools;

fn expand_row_indices(grid: &Vec<Vec<bool>>, expansion: usize) -> Vec<usize> {
    assert!(expansion > 0);
    let mut i1 = 0;
    let mut res = vec![];
    for i in 0..grid.len() {
        res.push(i1);
        if grid[i].iter().all(|b| !b) {
            i1 += expansion - 1;
        }
        i1 += 1;
    }
    return res;
}

fn expand_col_indices(grid: &Vec<Vec<bool>>, expansion: usize) -> Vec<usize> {
    assert!(expansion > 0);
    let mut j1 = 0;
    let mut res = vec![];
    for j in 0..grid[0].len() {
        res.push(j1);
        if grid.iter().all(|r| !r[j]) {
            j1 += expansion - 1;
        }
        j1 += 1;
    }
    return res;
}

fn get_galaxies(grid: &Vec<Vec<bool>>, expansion: usize) -> Vec<UPoint> {
    // Given the row idx in the original grid, gives the row idx in the expanded grid
    let expanded_row_idx: Vec<usize> = expand_row_indices(&grid, expansion);
    // Given the col idx in the original grid, gives the col idx in the expanded grid
    let expanded_col_idx: Vec<usize> = expand_col_indices(&grid, expansion);

    let tmp_fun = |i1: usize, j: usize, v: bool| v.then_some((i1, expanded_col_idx[j]));

    grid.iter()
        .enumerate()
        .map(|(i, row)| {
            let i1 = expanded_row_idx[i];
            row.iter()
                .enumerate()
                .filter_map(move |(j, v)| tmp_fun(i1, j, *v))
        })
        .flatten()
        .collect_vec()
}

fn distance_u(x1: usize, x2: usize) -> usize {
    if x1 > x2 {
        x1 - x2
    } else {
        x2 - x1
    }
}

fn distance((&(x1, y1), &(x2, y2)): (&UPoint, &UPoint)) -> usize {
    distance_u(x1, x2) + distance_u(y1, y2)
}

fn total_distance(galaxies: &Vec<UPoint>) -> usize {
    galaxies
        .iter()
        .cartesian_product(galaxies.iter())
        .map(distance)
        .sum::<usize>()
        / 2
}

fn main() {
    println!("Day 11");

    let p = parser!(lines((t:char_of(".#") => t == 1)+));
    let grid: Vec<Vec<bool>> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let galaxies: Vec<UPoint> = get_galaxies(&grid, 2);
    let r1 = total_distance(&galaxies);
    println!("{r1}");

    // Part 2
    let galaxies: Vec<UPoint> = get_galaxies(&grid, 1000000);
    let r2: usize = total_distance(&galaxies);
    println!("{r2}");
}
