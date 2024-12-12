use std::collections::HashSet;

use aoc_parse::{parser, prelude::*};
use crustofcode::{get_dimensions, neighbours, Dir4, UPoint};
use itertools::iproduct;
use pathfinding::prelude::connected_components;

fn get_price2(cc: &HashSet<UPoint>, w: usize, h: usize) -> usize {
    let mut area: usize = 0;
    let mut sides: usize = 0;

    for &p in cc {
        area += 1;
        for dir in Dir4::ALL_DIRS {
            // s | r
            // -   -
            // q | p
            let q = dir.move_point(p, h, w);
            let dir1 = dir.turn_right();
            let r = dir1.move_point(p, h, w);
            let s = r.and_then(|r| dir.move_point(r, h, w));

            let pq_same_color = q.is_some() && cc.contains(&q.unwrap());
            if !pq_same_color {
                let pr_same_color = r.is_some() && cc.contains(&r.unwrap());
                let ps_same_color = s.is_some() && cc.contains(&s.unwrap());
                if !pr_same_color {
                    sides += 1;
                } else if ps_same_color {
                    sides += 1;
                }
            }
        }
    }

    // println!("{area} * {sides}");
    return area * sides;
}

fn main() {
    println!("Day 12");

    let p = parser!(lines(alpha+));
    let garden: Vec<Vec<char>> = p.parse(&crustofcode::read_input()).unwrap();
    let (w, h) = get_dimensions(&garden);
    let ccs = connected_components::<UPoint, _, _>(
        &iproduct!((0..h), (0..w)).collect::<Vec<UPoint>>(),
        |&(i, j)| {
            let kind = &garden[i][j];
            neighbours(&(i, j), h, w)
                .into_iter()
                .filter(|&(x, y)| garden[x][y] == *kind)
        },
    );

    // Part 1
    let mut cell_perimeter = vec![vec![0; w]; h];
    for (i, j) in iproduct!((0..h), (0..w)) {
        cell_perimeter[i][j] = 4 - neighbours(&(i, j), h, w)
            .into_iter()
            .filter(|&(x, y)| garden[x][y] == garden[i][j])
            .count();
    }
    let cell_perimeter = cell_perimeter;
    let res1: usize = ccs
        .iter()
        .map(|cc| {
            let mut area: usize = 0;
            let perimeter: usize = cc
                .iter()
                .inspect(|_| area += 1)
                .map(|&(i, j)| cell_perimeter[i][j])
                .sum();
            return area * perimeter;
        })
        .sum();
    println!("{res1}");

    // Part 2
    let res2: usize = ccs.iter().map(|cc| get_price2(cc, w, h)).sum();
    println!("{res2}");
}
