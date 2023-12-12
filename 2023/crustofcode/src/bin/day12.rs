use aoc_parse::{parser, prelude::*};
use memoize::memoize;

type Row = (Vec<usize>, Vec<u32>);

#[allow(non_upper_case_globals)]
static mut rows: Vec<Row> = vec![];
#[allow(non_upper_case_globals)]
static mut rows2: Vec<Row> = vec![];

#[allow(dead_code)]
fn check_row(row: &[usize], nums: &[u32]) -> bool {
    // https://www.reddit.com/r/rust/comments/hgcpds/how_to_split_a_vector_by_an_entry_and_collect_all/
    let elems = row.iter().enumerate();
    let (k, mut r) = elems.fold((-1_i32, vec![]), |(i, mut r), (j, x)| {
        if *x == 0 {
            if j > ((i + 1) as usize) {
                r.push(&row[(i + 1) as usize..j]);
            }
            return (j as i32, r);
        }
        (i, r)
    });
    if ((k + 1) as usize) < row.len() {
        r.push(&row[(k + 1) as usize..]);
    }
    // println!("{:?}", row);
    // println!("{:?}", r);
    if r.len() != nums.len() {
        return false;
    }
    return r
        .iter()
        .map(|l| l.len())
        .filter(|v| *v > 0)
        .zip(nums)
        .all(|(a1, a2)| a1 as u32 == *a2);
}

#[allow(dead_code)]
fn all_tries1(row: &mut [usize], nums: &[u32], idx: usize) -> u64 {
    if idx >= row.len() {
        return if check_row(row, nums) { 1 } else { 0 };
    }
    if row[idx] == 2 {
        // ?
        row[idx] = 0;
        let n1 = all_tries1(row, nums, idx + 1);
        row[idx] = 1;
        let n2 = all_tries1(row, nums, idx + 1);
        row[idx] = 2;
        return n1 + n2;
    } else {
        all_tries1(row, nums, idx + 1)
    }
}

fn times5<T: Clone>(v: &Vec<T>, sep: Option<T>) -> Vec<T> {
    let base_iter = v.clone().into_iter();
    if sep.is_some() {
        base_iter
            .chain(std::iter::once(sep.unwrap()))
            .cycle()
            .take(v.len() * 5 + 4)
            .collect()
    } else {
        base_iter.cycle().take(v.len() * 5).collect()
    }
}

// Factoring the code for when the first char of row is 0
fn assign_row_0(
    row: &'static [usize],
    nums: &'static [u32],
    in_run: bool,
    curr_run_len: u32,
) -> u64 {
    if in_run {
        if curr_run_len > 0 {
            // There are still # missing from the current run -> impossible assignment
            #[cfg(debug_assertions)]
            println!("> Impossible: run not finished");
            return 0;
        } else {
            return assign_row_inner(&row[1..], nums, false, 0);
        }
    } else {
        return assign_row_inner(&row[1..], nums, false, 0);
    }
}

// Factoring the code for when the first char of row is 1
fn assign_row_1(
    row: &'static [usize],
    nums: &'static [u32],
    in_run: bool,
    curr_run_len: u32,
) -> u64 {
    if in_run {
        if curr_run_len == 0 {
            // There are no # missing from the current run -> impossible assignment
            #[cfg(debug_assertions)]
            println!("> Impossible: run finished");
            return 0;
        } else {
            return assign_row_inner(&row[1..], nums, true, curr_run_len - 1);
        }
    } else {
        if nums.len() == 0 {
            // Can't start a new run -> impossible assignment
            #[cfg(debug_assertions)]
            println!("> Impossible: no more runs");
            return 0;
        } else {
            // Start a new run
            let new_run_len = nums[0] - 1;
            let n = assign_row_inner(&row[1..], &nums[1..], true, new_run_len);
            return n;
        }
    }
}

#[memoize]
fn assign_row_inner(
    row: &'static [usize],
    nums: &'static [u32],
    in_run: bool,
    curr_run_len: u32,
) -> u64 {
    if row.len() == 0 {
        if nums.len() == 0 && curr_run_len == 0 {
            #[cfg(debug_assertions)]
            println!("> Solution found!");
            return 1;
        } else {
            return 0;
        }
    }

    #[cfg(debug_assertions)]
    if !in_run {
        assert_eq!(curr_run_len, 0);
    }
    match row[0] {
        // .
        0 => {
            #[cfg(debug_assertions)]
            println!("-- {:?} {:?} {in_run} {curr_run_len}\nCase .", row, nums);
            assign_row_0(row, nums, in_run, curr_run_len)
        }
        // #
        1 => {
            #[cfg(debug_assertions)]
            println!("-- {:?} {:?} {in_run} {curr_run_len}\nCase #", row, nums);
            assign_row_1(row, nums, in_run, curr_run_len)
        }
        // ?
        2 => {
            // . (0)
            #[cfg(debug_assertions)]
            println!(
                "-- {:?} {:?} {in_run} {curr_run_len}\nCase ?, subcase .",
                row, nums
            );
            let n0 = assign_row_0(row, nums, in_run, curr_run_len);
            // # (1)
            #[cfg(debug_assertions)]
            println!(
                "-- {:?} {:?} {in_run} {curr_run_len}\nCase ?, subcase #",
                row, nums
            );
            let n1 = assign_row_1(row, nums, in_run, curr_run_len);
            // Final sum
            n0 + n1
        }
        _ => panic!("Unexpected plot twist"),
    }
}

fn assign_row(row: &'static [usize], nums: &'static Vec<u32>) -> u64 {
    assign_row_inner(row, nums, false, 0)
}

fn main() {
    println!("Day 12");

    let p = parser!(lines(char_of(".#?")+ ' ' repeat_sep(u32, ',')));
    unsafe {
        rows = p.parse(&crustofcode::read_input()).unwrap();
    }

    // Part 1
    let r1: u64 = unsafe { rows.iter().map(|(r, n)| assign_row(r, n)).sum() };
    println!("{:?}", r1);

    // Part 2
    unsafe {
        rows2 = rows
            .iter()
            .map(|(r, n)| (times5(r, Some(2)), times5(n, None)))
            .collect();
    }
    let r2: u64 = unsafe {
        rows2
            .iter()
            .map(|(r, n)| assign_row(r, n))
            .enumerate()
            // .inspect(|(i, v)| println!("{i} - {v}"))
            .map(|(_, v)| v)
            .sum()
    };
    println!("{:?}", r2);
}
