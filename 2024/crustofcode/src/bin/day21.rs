use std::fmt::Debug;

use aoc_parse::{parser, prelude::*};
use crustofcode::{get_dimensions, Dir4, UPoint};
use either::Either;
use itertools::{chain, iproduct, Itertools};

type Code = [u8; 3];
type Action = Either<Dir4, ()>;

fn search_in_grid<T: Eq>(grid: &Vec<Vec<T>>, val: &T) -> UPoint {
    let (w, h) = get_dimensions(grid);
    for (i, j) in iproduct!(0..h, 0..w) {
        if grid[i][j] == *val {
            return (i, j);
        }
    }
    panic!("Not found in grid!")
}

/// Get the sequence of movements to perform to get from start to end, in some
/// order
fn movements_sequence(start: UPoint, end: UPoint) -> [usize; 4] {
    let mut curr: UPoint = start;
    let mut dirs = [0; 4];
    while curr != end {
        let (x, y) = curr;
        if y < end.1 {
            curr.1 += 1;
            dirs[Dir4::R.to_index()] += 1;
        }
        if y > end.1 {
            curr.1 -= 1;
            dirs[Dir4::L.to_index()] += 1;
        }
        if x < end.0 {
            curr.0 += 1;
            dirs[Dir4::D.to_index()] += 1;
        }
        if x > end.0 {
            curr.0 -= 1;
            dirs[Dir4::U.to_index()] += 1;
        }
    }
    return dirs;
}

fn test_sequence<T>(grid: &Vec<Vec<Option<T>>>, seq: &[Dir4], start: UPoint, end: UPoint) -> bool {
    let mut pos = start;
    let (w, h) = get_dimensions(grid);
    for d in seq {
        pos = d.move_point(pos, h, w).unwrap();
        if grid[pos.0][pos.1].is_none() {
            return false;
        }
    }
    assert_eq!(pos, end);
    return true;
}

fn actionify_seq(seq: Vec<Dir4>) -> Vec<Action> {
    chain!(
        seq.into_iter().map(|d| Either::Left(d)),
        std::iter::once(Either::Right(()))
    )
    .collect()
}

/// Get the list of all sequences of actions with equal values consecutive from
/// start to end (terminated with the activate action)
fn all_valid_movements<T>(
    grid: &Vec<Vec<Option<T>>>,
    start: UPoint,
    end: UPoint,
) -> Vec<Vec<Action>> {
    let mut res = vec![];
    let actions = movements_sequence(start, end);
    let dirs = Dir4::ALL_DIRS.iter().filter(|d| actions[d.to_index()] > 0);
    let l = actions.iter().filter(|v| **v > 0).count();
    for perm in dirs.into_iter().permutations(l) {
        let seq = perm
            .into_iter()
            .flat_map(|d| std::iter::repeat(*d).take(actions[d.to_index()]))
            .collect_vec();
        if test_sequence(grid, &seq, start, end) {
            res.push(actionify_seq(seq));
        }
    }
    return res;
}

fn keep_shortest<T>(l: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let minl = l.iter().map(|s| s.len()).min();
    if minl.is_none() {
        return vec![];
    }
    l.into_iter().filter(|s| s.len() == minl.unwrap()).collect()
}

/// Compute a set of all the sequences of actions that minimize length of
/// encoding the code
fn indirection<T: Eq + Clone + Debug>(
    code: &[T],
    grid: &Vec<Vec<Option<T>>>,
    start: T,
) -> Vec<Vec<Action>> {
    let mut res: Vec<Vec<Action>> = vec![vec![]];
    let mut start_pos: UPoint = search_in_grid(grid, &Some(start));

    for v in code {
        let target_pos: UPoint = search_in_grid(grid, &Some(v.clone()));
        let seqs = all_valid_movements(grid, start_pos, target_pos);
        // Extends all sequences to start_pos with all the ways to get to target
        res = seqs
            .into_iter()
            .flat_map(|tail| {
                res.iter().map(move |head| {
                    let mut head1: Vec<Action> = head.clone();
                    head1.extend(&tail);
                    head1
                })
            })
            .collect();
        start_pos = target_pos;
    }
    keep_shortest(res)
}

fn numpad_indirection(code: &Code) -> Vec<Vec<Action>> {
    let numpad_grid: Vec<Vec<Option<u8>>> = vec![
        vec![Some(7), Some(8), Some(9)],
        vec![Some(4), Some(5), Some(6)],
        vec![Some(1), Some(2), Some(3)],
        vec![None, Some(0), Some(100)],
    ];
    let mut code = code.to_vec();
    code.push(100);
    indirection(&code, &numpad_grid, 100)
}

fn keypad_indirection(code: &[Action]) -> Vec<Vec<Action>> {
    let keypad_grid: Vec<Vec<Option<Action>>> = vec![
        vec![None, Some(Either::Left(Dir4::U)), Some(Either::Right(()))],
        vec![
            Some(Either::Left(Dir4::L)),
            Some(Either::Left(Dir4::D)),
            Some(Either::Left(Dir4::R)),
        ],
    ];
    indirection(code, &keypad_grid, Either::Right(()))
}

fn all_indirections1(code: &Code) -> Vec<Action> {
    let firsts = numpad_indirection(code);
    let seconds = keep_shortest(
        firsts
            .into_iter()
            .flat_map(|s| keypad_indirection(&s))
            .collect_vec(),
    );
    let thirds = keep_shortest(
        seconds
            .into_iter()
            .flat_map(|s| keypad_indirection(&s))
            .collect_vec(),
    );
    thirds.into_iter().next().unwrap()
}

#[allow(unused)]
fn action2char(a: &Action) -> char {
    match *a {
        Either::Left(d) => d.into(),
        Either::Right(()) => 'A',
    }
}

fn code_number(code: &Code) -> usize {
    usize::from(code[0]) * 100 + usize::from(code[1]) * 10 + usize::from(code[2])
}

fn all_indirections2(code: &Code, n: usize) -> Vec<Action> {
    let firsts = numpad_indirection(code);
    let mut seconds = firsts;
    for _ in 0..n {
        seconds = keep_shortest(
            seconds
                .into_iter()
                .flat_map(|s| keypad_indirection(&s))
                .collect_vec(),
        );
    }
    seconds.into_iter().next().unwrap()
}

fn main() {
    println!("Day 21");

    let code_p = parser!({
        a:digit b:digit c:digit "A"
        => [a.try_into().unwrap(), b.try_into().unwrap(), c.try_into().unwrap()]
    });
    let p = parser!(lines(code_p));
    let codes: Vec<Code> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let res1: usize = codes
        .iter()
        .map(|c| (c, all_indirections2(&c, 2)))
        .map(|(c, actions)| actions.len() * code_number(c))
        .sum();
    println!("{res1}");

    // Part 2
    let res2: usize = codes
        .iter()
        .map(|c| (c, all_indirections2(&c, 25)))
        .map(|(c, actions)| actions.len() * code_number(c))
        .sum();
    println!("{res2}");
}
