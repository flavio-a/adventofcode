use aoc_parse::{parser, prelude::*};
use crustofcode::{snd, UPoint};
use itertools::Itertools;
use pathfinding::{directed::dijkstra::dijkstra_all, prelude::Grid};

const STEPS: usize = 26501365;

fn walk_n_steps(grid: &Grid, currents: &[UPoint], steps: usize) -> usize {
    if steps == 0 {
        return currents.len();
    }
    let mut new_currents = vec![];
    for p in currents {
        new_currents.append(&mut grid.neighbours(*p));
    }
    new_currents.sort();
    new_currents.dedup();
    return walk_n_steps(grid, &new_currents, steps - 1);
}

fn count_color(grid: &Grid, start: &UPoint, color: usize) -> usize {
    grid.bfs_reachable(*start, |_| true)
        .into_iter()
        .filter(|v| grid.distance(*v, *start) % 2 == color)
        .count()
}

fn start_to_edges(grid: &Grid, start: &UPoint) -> Vec<(UPoint, usize)> {
    let width = grid.width;
    let distances = dijkstra_all(start, |node| {
        grid.neighbours(*node).into_iter().map(|n| (n, 1))
    });
    // Checks that all the points are reachable within (width + height) steps
    assert!(distances.iter().all(|(_, (_, d))| *d < width * 2));
    return [
        (0, 0),
        (0, width - 1),
        (width - 1, 0),
        (width - 1, width - 1),
    ]
    .into_iter()
    .map(|(w, h)| {
        (
            (width - 1 - w, width - 1 - h),
            distances.get(&(w, h)).unwrap().1,
        )
    })
    .collect();
}

fn count_2_starts(grid: &Grid, remaining_steps: &[(UPoint, usize)]) -> usize {
    let width = grid.width;
    let starts: Vec<usize> = remaining_steps.iter().map(|(_, s)| s - 1).collect_vec();
    // println!("Starts: {:?}", starts);

    let make_reachs = |(p, s): &(UPoint, usize)| -> Vec<UPoint> {
        dijkstra_all(p, |n| grid.neighbours(*n).into_iter().map(|n| (n, 1)))
            .into_iter()
            .filter(|(_, (_, d))| d <= s && d % 2 == s % 2)
            .map(|(n, (_, _))| n)
            .chain(std::iter::once(p.clone()))
            .collect_vec()
    };
    let count_pair = |p1: &(UPoint, usize), p2: &(UPoint, usize)| {
        let mut a = make_reachs(p1);
        a.append(&mut make_reachs(p2));
        a.sort();
        a.dedup();
        return a.len();
    };
    let w1 = width - 1;
    // [(0, 0), (0, width - 1), (width - 1, 0), (width - 1, width - 1)]
    // println!(
    //     "Vertices: {} {} {} {}",
    //     count_pair(&((w1, 0), starts[0]), &((w1, w1), starts[1])),
    //     count_pair(&((0, w1), starts[0]), &((w1, w1), starts[2])),
    //     count_pair(&((0, 0), starts[1]), &((w1, 0), starts[3])),
    //     count_pair(&((0, 0), starts[2]), &((0, w1), starts[3]))
    // );
    return count_pair(&((w1, 0), starts[0]), &((w1, w1), starts[1]))
        + count_pair(&((0, w1), starts[0]), &((w1, w1), starts[2]))
        + count_pair(&((0, 0), starts[1]), &((w1, 0), starts[3]))
        + count_pair(&((0, 0), starts[2]), &((0, w1), starts[3]));
}

fn main() {
    println!("Day 21");

    let p = parser!(lines((char_of("#.S"))+));
    let grid: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();
    let start: UPoint = grid
        .iter()
        .enumerate()
        .filter_map(|(i, row)| Some((i, row.iter().position(|v| *v == 2)?)))
        .next()
        .unwrap();
    let grid: Grid = grid
        .into_iter()
        .enumerate()
        .map(|(i, row)| {
            row.into_iter()
                .enumerate()
                .filter_map(move |(j, v)| (v != 0).then_some((j, i)))
        })
        .flatten()
        .collect();

    // Part 1
    let r1 = walk_n_steps(&grid, &[start], 64);
    println!("{r1}");

    // Part 2
    assert_eq!(grid.width, grid.height);
    assert!(grid.width % 2 == 1);
    let width = grid.width;
    // println!("{}", grid.width);
    let edges_dists = start_to_edges(&grid, &start);
    // This means all the edges are the same color as the start
    assert!(edges_dists.iter().all(|(_, d)| d % 2 == 0));
    println!("Edges dists: {edges_dists:?}");

    // Grids completely traversed
    let big_steps: usize = (STEPS - edges_dists.iter().map(snd).max().unwrap()) / width;
    println!("Big steps: {big_steps}");
    // 4 * (1, 0) (1, 2) (1 + 3, 2) (1 + 3, 2 + 4) (1 + 3 + 5, 2 + 4)
    let num_full_odd: usize = 4 * ((big_steps + 1) / 2) * ((big_steps + 1) / 2);
    let num_full_even: usize = 4 * (big_steps / 2) * (big_steps / 2 + 1) + 1;
    let segments_full: usize = num_full_even * count_color(&grid, &start, STEPS % 2)
        + num_full_odd * count_color(&grid, &start, (STEPS + 1) % 2);
    println!(
        "odd: {}, even: {}",
        count_color(&grid, &start, (STEPS + 1) % 2),
        count_color(&grid, &start, STEPS % 2)
    );
    println!("num_full_odd: {num_full_odd}, num_full_even: {num_full_even}, segments_full: {segments_full}");

    // Borders
    let remaining_steps = edges_dists
        .into_iter()
        .map(|(p, d)| (p, STEPS - d - big_steps * width))
        .collect_vec();
    assert!(remaining_steps.iter().all(|(_, d)| *d <= width));
    println!("Remaining steps: {remaining_steps:?}");
    // Edges: I have "big_steps" grids with (remaining_steps + width) and "big_steps + 1" with (remaining_steps)
    let inner: usize = big_steps
        * remaining_steps
            .iter()
            .map(|&(p, d)| walk_n_steps(&grid, &[p], d + width - 2))
            .inspect(|v| print!("{v} "))
            .sum::<usize>();
    println!("");
    let outer: usize = (big_steps + 1)
        * remaining_steps
            .iter()
            .map(|&(p, d)| walk_n_steps(&grid, &[p], d - 2))
            .inspect(|v| print!("{v} "))
            .sum::<usize>();
    println!("");
    // Vertices
    let vertices = count_2_starts(&grid, &remaining_steps);
    // println!("Vertices: {vertices:?}");

    // Final sum
    let r2: usize = segments_full + inner + outer + vertices;
    println!("{r2}");

    if remaining_steps
        .iter()
        .all(|(_, d)| *d == remaining_steps[0].1)
    {
        let w1 = width - 1;
        let vertices2: usize = [
            ((0, 0), (0, w1)),
            ((0, w1), (w1, w1)),
            ((w1, w1), (w1, 0)),
            ((w1, 0), (0, 0)),
        ]
        .iter()
        .map(|&(p1, p2)| walk_n_steps(&grid, &[p1, p2], remaining_steps[0].1 - 1))
        .sum();
        let r2: usize = segments_full + inner + outer + vertices2;
        println!("{r2}");
    }

    // Prove
    // let edges = [(0, 0), (w1, 0), (0, w1), (w1, w1)];
    // let t1 = edges.iter().map(|p| walk_n_steps(&grid, &[*p], 64)).collect_vec();
    // println!("{t1:?}");
    // let t2 = edges.iter().map(|p| walk_n_steps(&grid, &[*p], 195)).collect_vec();
    // println!("{t2:?}");
}
