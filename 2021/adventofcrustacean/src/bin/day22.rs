use adventofcrustacean;
use itertools::Itertools;
use std::cmp;

type Bounds = [i32; 2];
#[derive(Debug,Clone)]
struct Step {
    on: bool,
    x: Bounds,
    y: Bounds,
    z: Bounds,
}
type Board = Vec<Vec<Vec<bool>>>;

fn parse_bound(s: &str) -> Bounds {
    // x=10..12
    let mut tmp = s.split("=");
    assert_eq!(tmp.next().unwrap().len(), 1);
    let mut tmp = tmp.next().unwrap().split("..").map(adventofcrustacean::str2int).map(|v| v.try_into().unwrap());
    return [tmp.next().unwrap(), tmp.next().unwrap()];
}

fn parse_line(s: &str) -> Step {
    // on x=10..12,y=10..12,z=10..12
    let mut tmp = s.split(" ");
    let on = match tmp.next() {
        Some("on") => true,
        Some("off") => false,
        _ => panic!("Error while parsing line \"{}\"", s),
    };
    let rest = tmp.next().unwrap();
    assert_eq!(tmp.next(), None);

    let tmp = rest.split(",");
    let mut bounds = tmp.map(parse_bound);

    return Step { on: on, x: bounds.next().unwrap(), y: bounds.next().unwrap(), z: bounds.next().unwrap() };
}

const INIT_ZONE_BOUNDS: [i32; 2] = [-50, 50];

fn apply_step(mut board: Board, step: &Step) -> Board {
    let offset = -INIT_ZONE_BOUNDS[0];
    let starts = |v| if v > INIT_ZONE_BOUNDS[1] { INIT_ZONE_BOUNDS[1] + 1 } else { cmp::max(v, INIT_ZONE_BOUNDS[0]) } + offset;
    let ends = |v| if v < INIT_ZONE_BOUNDS[0] { INIT_ZONE_BOUNDS[0] - 1 } else { cmp::min(v, INIT_ZONE_BOUNDS[1]) } + offset;
    for x in starts(step.x[0])..=ends(step.x[1]) {
        for y in starts(step.y[0])..=ends(step.y[1]) {
            for z in starts(step.z[0])..=ends(step.z[1]) {
                let x: usize = x.try_into().unwrap();
                let y: usize = y.try_into().unwrap();
                let z: usize = z.try_into().unwrap();
                board[x][y][z] = step.on;
            }
        }
    }
    return board;
}

// Change indexes in steps to match those of the given set of bounds
fn reindex_step(step: Step, xbounds: &Vec<i32>, ybounds: &Vec<i32>, zbounds: &Vec<i32>) -> Step {
    // New bounds are index(step.w[0]) and index(step.w[1] + 1) - 1
    let getlow = |bounds: &Vec<i32>, val| bounds.binary_search(&val).map(|v| i32::try_from(v).unwrap()).unwrap();
    let gethigh = |bounds: &Vec<i32>, val| getlow(bounds, val + 1) - 1;
    let newx: Bounds = [getlow(xbounds, step.x[0]), gethigh(xbounds, step.x[1])];
    let newy: Bounds = [getlow(ybounds, step.y[0]), gethigh(ybounds, step.y[1])];
    let newz: Bounds = [getlow(zbounds, step.z[0]), gethigh(zbounds, step.z[1])];
    return Step { on: step.on, x: newx, y: newy, z: newz };
}

fn apply_reindexed_step(mut board: Board, step: Step) -> Board {
    for x in step.x[0]..=step.x[1] {
        for y in step.y[0]..=step.y[1] {
            for z in step.z[0]..=step.z[1] {
                let x: usize = x.try_into().unwrap();
                let y: usize = y.try_into().unwrap();
                let z: usize = z.try_into().unwrap();
                board[x][y][z] = step.on;
            }
        }
    }
    return board;
}

fn get_region_size(x: usize, y: usize, z: usize, xbounds: &Vec<i32>, ybounds: &Vec<i32>, zbounds: &Vec<i32>) -> i64 {
    i64::from(xbounds[x + 1] - xbounds[x]) * i64::from(ybounds[y + 1] - ybounds[y]) * i64::from(zbounds[z + 1] - zbounds[z])
}

fn main() {
    let content = adventofcrustacean::read_input();
    let steps = content.lines().map(parse_line).collect::<Vec<Step>>();

    // Part 1
    // Wrong approach, part 2 won't work like this
    let mut board: Board = vec![];
    for x in -50..=50 {
        board.push(vec![]);
        for _y in -50..=50 {
            board[usize::try_from(x - INIT_ZONE_BOUNDS[0]).unwrap()].push(vec![false; 101]);
        }
    }
    let board: Board = steps.iter().fold(board, apply_step);
    println!("{}", board.into_iter().flatten().flatten().map(|b| if b { 1 } else { 0 }).sum::<u64>());

    // Part 2

    // Idea: separate space in regions, where bounds are all possible values
    // found in any step. This means that each reagon is completely within
    // every step
    let xbounds: Vec<i32> = steps.iter().map(|s| [s.x[0], s.x[1] + 1]).flatten().sorted().dedup().collect();
    let ybounds: Vec<i32> = steps.iter().map(|s| [s.y[0], s.y[1] + 1]).flatten().sorted().dedup().collect();
    let zbounds: Vec<i32> = steps.iter().map(|s| [s.z[0], s.z[1] + 1]).flatten().sorted().dedup().collect();
    // println!("{} {} {}", xbounds.len(), ybounds.len(), zbounds.len());
    #[cfg(debug_assertions)]
    println!("Number of regions: {}", xbounds.len() * ybounds.len() * zbounds.len());

    let mut board: Board = vec![];
    for x in 0..xbounds.len() {
        board.push(vec![]);
        for _y in 0..ybounds.len() {
            board[x].push(vec![false; zbounds.len()]);
        }
    }
    let reindexed_steps: Vec<Step> = steps.iter().map(|s| reindex_step(s.clone(), &xbounds, &ybounds, &zbounds)).collect();
    let board: Board = reindexed_steps.into_iter().fold(board, apply_reindexed_step);
    let res: i64 = board.into_iter().enumerate().map(
        |(x, plane)| plane.into_iter().enumerate().map(
            |(y, line)| line.into_iter().enumerate().map(
                |(z, point)| if point { get_region_size(x, y, z, &xbounds, &ybounds, &zbounds) } else { 0 }
            ).sum::<i64>()
        ).sum::<i64>()
    ).sum::<i64>();

    println!("{}", res);
}
