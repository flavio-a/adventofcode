use std::collections::HashMap;

use aoc_parse::{parser, prelude::*};
use crustofcode::{get_dimensions, grid_neighbours, manhattan_distance, UPoint};
use itertools::iproduct;
use pathfinding::prelude::bfs;

/// Return the list of positions reachable in at most n steps of cheating
fn cheat_n_steps((x, y): UPoint, n: usize, grid: &Vec<Vec<bool>>) -> Vec<(UPoint, usize)> {
    let (w, h) = get_dimensions(grid);
    let minx = x.saturating_sub(n);
    let miny = y.saturating_sub(n);
    let maxx = std::cmp::min(x + n, w - 1);
    let maxy = std::cmp::min(y + n, h - 1);
    iproduct!(minx..=maxx, miny..=maxy)
        .map(|(i, j)| ((i, j), manhattan_distance(&(x, y), &(i, j))))
        .filter(|(_, d)| *d <= n)
        .filter(|((i, j), _)| !grid[*i][*j])
        .collect()
}

fn count_cheats(cheats: Vec<(UPoint, UPoint, usize)>, cost_map: &HashMap<UPoint, usize>) -> usize {
    let cheats_save = cheats.iter().filter_map(|(p, q, s)| {
        let p_cost = *cost_map.get(p).unwrap();
        let q_cost = *cost_map.get(q).unwrap();
        p_cost.checked_sub(q_cost + s)
    });
    // println!("{:?}", cheats_save.clone().collect_vec());
    cheats_save.filter(|s| *s >= 100).count()
}

fn main() {
    println!("Day 20");

    let p = parser!(lines(char_of("#.SE")+));
    let grid_in: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();
    let (w, h) = get_dimensions(&grid_in);
    let mut grid = vec![vec![false; w]; h];
    let mut start: UPoint = (0, 0);
    let mut end: UPoint = (0, 0);
    for (i, j) in iproduct!(0..h, 0..w) {
        match grid_in[i][j] {
            0 => grid[i][j] = true,
            1 => {}
            2 => start = (i, j),
            3 => end = (i, j),
            _ => panic!("Unexpected plot twist!"),
        }
    }
    let grid = grid;
    let start = start;
    let end = end;

    let baseline = bfs(&start, |p| grid_neighbours(p, &grid), |p| *p == end).unwrap();
    let mut cost_map: HashMap<UPoint, usize, _> = HashMap::with_capacity(baseline.len());
    for (i, p) in baseline.iter().enumerate() {
        cost_map.insert(p.clone(), baseline.len() - i);
    }
    // Check assumptions: this means the shortest path from any point to the
    // end (without cheats) is given by its position in baseline
    for (i, j) in iproduct!(0..h, 0..w) {
        assert!(grid[i][j] || cost_map.contains_key(&(i, j)));
    }
    // println!("{}", baseline.len() - 1);

    // Part 1
    let cheats1: Vec<(UPoint, UPoint, usize)> = baseline
        .iter()
        .flat_map(|p| {
            cheat_n_steps(*p, 2, &grid)
                .into_iter()
                .map(|(q, s)| (*p, q, s))
        })
        .collect();
    let res1 = count_cheats(cheats1, &cost_map);
    println!("{res1}");

    // Part 2
    let cheats2: Vec<(UPoint, UPoint, usize)> = baseline
        .iter()
        .flat_map(|p| {
            cheat_n_steps(*p, 20, &grid)
                .into_iter()
                .map(|(q, s)| (*p, q, s))
        })
        .collect();
    let res2 = count_cheats(cheats2, &cost_map);
    println!("{res2}");
}
