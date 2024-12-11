use aoc_parse::{parser, prelude::*};
use num::pow;
use std::collections::HashMap;

const MAX_TIME: usize = 75;
type Memo = HashMap<u64, [u64; MAX_TIME]>;

fn digits_ten(n: u32) -> u64 {
    pow(10, n.try_into().unwrap())
}

fn num_digits(n: u64) -> u32 {
    n.checked_ilog10().unwrap_or(0) + 1
}

fn evolve_stone(stone: u64) -> (u64, Option<u64>) {
    if stone == 0 {
        return (1, None);
    }
    let nd = num_digits(stone);
    if nd % 2 == 0 {
        let th = digits_ten(nd / 2);
        let n1 = stone / th;
        let n2 = stone % th;
        return (n1, Some(n2));
    }
    return (stone * 2024, None);
}

fn evolve_stones<T: IntoIterator<Item = u64>>(stones: T) -> Vec<u64> {
    stones
        .into_iter()
        .flat_map(|s| {
            let (s1, s2) = evolve_stone(s);
            if s2.is_some() {
                vec![s1, s2.unwrap()]
            } else {
                vec![s1]
            }
        })
        .collect()
}

/// Given a stone and a time, returns the number of stones it produces after
/// that time has passed
fn count_stone_memo(stone: u64, time: usize, memoize: &mut Memo) -> u64 {
    assert!(time <= MAX_TIME, "Recompile with bigger MAX_TIME");
    // time = 0 is always 1, just the original stone
    if time == 0 {
        return 1;
    }
    if !memoize.contains_key(&stone) {
        memoize.insert(stone, [0; MAX_TIME]);
    }
    let memo_key = memoize.get(&stone).unwrap();
    if memo_key[time - 1] != 0 {
        return memo_key[time - 1];
    }
    let (s1, s2) = evolve_stone(stone);
    let r1 = count_stone_memo(s1, time - 1, memoize);
    let r2 = s2.map_or(0, |s| count_stone_memo(s, time - 1, memoize));
    let res = r1 + r2;
    let memo_key = memoize.get_mut(&stone).unwrap();
    memo_key[time - 1] = res;
    return res;
}

fn main() {
    println!("Day 11");

    let p = parser!(line(repeat_sep(u64, " ")));
    let stones: Vec<u64> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let mut stones1 = stones.clone();
    for _ in 0..25 {
        stones1 = evolve_stones(stones1);
    }
    let res1 = stones1.len();
    println!("{res1}");

    // Part 2
    let mut memoize = HashMap::new();
    let res2: u64 = stones
        .iter()
        .map(|&s| count_stone_memo(s, 75, &mut memoize))
        .sum();
    println!("{res2}");
}
