use aoc_parse::{parser, prelude::*};
use crustofcode::{neighbours, UPoint};
use pathfinding::prelude::bfs;

const DIM: usize = 71;
const FALLEN: usize = 1024;
// const DIM: usize = 7;
// const FALLEN: usize = 12;

fn main() {
    println!("Day 18");

    let p = parser!(lines(usize "," usize));
    let rocks: Vec<UPoint> = p.parse(&crustofcode::read_input()).unwrap();
    let mut grid: Vec<Vec<bool>> = vec![vec![false; DIM]; DIM];

    // Part 1
    for i in 0..FALLEN {
        grid[rocks[i].0][rocks[i].1] = true;
    }
    let mut path = bfs(
        &(0, 0),
        |p| {
            neighbours(p, DIM, DIM)
                .into_iter()
                .filter(|&(x, y)| !grid[x][y])
        },
        |&(i, j)| i == DIM - 1 && j == DIM - 1,
    );
    let res1 = path.as_ref().unwrap().len() - 1;
    println!("{res1}");

    // Part 2
    let mut i = FALLEN;
    while path.is_some() {
        let path_u = path.unwrap();
        while !path_u.contains(&rocks[i]) {
            grid[rocks[i].0][rocks[i].1] = true;
            i += 1;
        }
        grid[rocks[i].0][rocks[i].1] = true;
        path = bfs(
            &(0, 0),
            |p| {
                neighbours(p, DIM, DIM)
                    .into_iter()
                    .filter(|&(x, y)| !grid[x][y])
            },
            |&(i, j)| i == DIM - 1 && j == DIM - 1,
        );
    }
    println!("{},{}", rocks[i].0, rocks[i].1);
}
