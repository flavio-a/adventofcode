use aoc_parse::{parser, prelude::*};
use crustofcode::{direction, get_dimensions, option_assert, UPoint};
use itertools::{iproduct, Itertools};
use pathfinding::utils::move_in_direction;

fn antinodes1(grid: &Vec<Vec<char>>, freq: &char) -> Vec<UPoint> {
    let (w, h) = get_dimensions(&grid);
    let antennae: Vec<UPoint> = iproduct!((0..w), (0..h))
        .filter(|&(i, j)| grid[i][j] == *freq)
        .collect();

    let res: Vec<UPoint> = antennae
        .iter()
        .flat_map(|&(x1, y1)| {
            antennae.iter().filter_map(move |&(x2, y2)| {
                if (x1, y1) == (x2, y2) {
                    return None;
                }
                if (x1 + x1 >= x2) && (x1 + x1 - x2 < w) && (y1 + y1 >= y2) && (y1 + y1 - y2 < h) {
                    return Some((x1 + x1 - x2, y1 + y1 - y2));
                }
                return None;
            })
        })
        .collect();
    return res;
}

fn antinodes2(grid: &Vec<Vec<char>>, freq: &char) -> Vec<UPoint> {
    let (w, h) = get_dimensions(&grid);
    let antennae: Vec<UPoint> = iproduct!((0..w), (0..h))
        .filter(|&(i, j)| grid[i][j] == *freq)
        .collect();

    let res: Vec<UPoint> = antennae
        .iter()
        .flat_map(|&start_p| {
            antennae.iter().flat_map(move |&next_p| {
                let dir = direction(&start_p, &next_p);
                return itertools::iterate(Some(start_p), move |p| {
                    option_assert(dir != (0, 0))?;
                    move_in_direction((*p)?, dir, (w, h))
                })
                .take_while(|p| p.is_some())
                .map(|p| p.unwrap());
            })
        })
        .collect();
    return res;
}

fn main() {
    println!("Day 8");

    let p = parser!(lines({"." => '.', a:alnum => a}+));
    let grid: Vec<Vec<char>> = p.parse(&crustofcode::read_input()).unwrap();
    let frequencies: Vec<char> = grid
        .iter()
        .flat_map(|row| row.iter().filter(|&&c| c != '.'))
        .cloned()
        .unique()
        .collect();

    // Part 1
    let res1 = frequencies
        .iter()
        .flat_map(|freq| antinodes1(&grid, freq))
        // .inspect(|p| println!("{:?}", p))
        .unique()
        .count();
    println!("{res1}");

    // Part 2
    let res2 = frequencies
        .iter()
        .flat_map(|freq| antinodes2(&grid, freq))
        // .inspect(|p| println!("{:?}", p))
        .unique()
        .count();
    println!("{res2}");
}
