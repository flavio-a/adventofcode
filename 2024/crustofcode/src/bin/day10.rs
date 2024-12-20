use aoc_parse::{parser, prelude::*};
use crustofcode::{filtered_neighbours, get_dimensions, neighbours_iter, UPoint};
use itertools::iproduct;
use pathfinding::prelude::bfs_reach;

fn compute_score(topomap: &Vec<Vec<u8>>, pos: UPoint) -> usize {
    let (w, h) = get_dimensions(topomap);
    let successors = |p: &UPoint| {
        let (x, y) = *p;
        // println!("{:?} / {}", p, topomap[x][y]);
        filtered_neighbours(p, h, w, move |&(i, j)| topomap[i][j] == topomap[x][y] + 1)
    };
    let reachables = bfs_reach(pos, successors);
    return reachables.filter(|&(i, j)| topomap[i][j] == 9).count();
}

fn compute_rating(
    topomap: &Vec<Vec<u8>>,
    memoize: &mut Vec<Vec<Option<usize>>>,
    pos: &UPoint,
) -> usize {
    if topomap[pos.0][pos.1] == 9 {
        memoize[pos.0][pos.1] = Some(1);
        return 1;
    }
    if memoize[pos.0][pos.1].is_some() {
        return memoize[pos.0][pos.1].unwrap();
    }

    let (w, h) = get_dimensions(topomap);
    let (x, y) = *pos;
    let res = neighbours_iter(&pos, h, w)
        .filter(move |&(i, j)| topomap[i][j] == topomap[x][y] + 1)
        .map(|p| compute_rating(topomap, memoize, &p))
        .sum();
    memoize[pos.0][pos.1] = Some(res);
    return res;
}

fn main() {
    println!("Day 10");

    let p = parser!(lines((d:digit => u8::try_from(d).unwrap())+));
    let topomap: Vec<Vec<u8>> = p.parse(&crustofcode::read_input()).unwrap();
    let (w, h) = get_dimensions(&topomap);

    // Part 1
    // println!("{}", compute_score(&topomap, (0, 2)));
    let res1: usize = iproduct!((0..w), (0..h))
        .filter(|&(i, j)| topomap[i][j] == 0)
        // .inspect(|p| println!("{:?}", p))
        .map(|p| compute_score(&topomap, p))
        .sum();
    println!("{res1}");

    // Part 2
    let mut memoize: Vec<Vec<Option<usize>>> = vec![vec![None; w]; h];
    let res2: usize = iproduct!((0..w), (0..h))
        .filter(|&(i, j)| topomap[i][j] == 0)
        .map(|p| compute_rating(&topomap, &mut memoize, &p))
        .sum();
    println!("{res2}");
}
