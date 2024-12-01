use aoc_parse::{parser, prelude::*};
use itertools::Itertools;

fn main() {
    println!("Day 1");

    let p = parser!(lines(u32 ' '+ u32));
    let lists: Vec<(u32, Vec<()>, u32)> = p.parse(&crustofcode::read_input()).unwrap();
    let list1: Vec<u32> = lists.iter().map(|(v, _, _)| v.clone()).collect();
    let list2: Vec<u32> = lists.into_iter().map(|(_, _, v)| v).collect();

    // Part 1
    let l1s = list1.iter().sorted();
    let l2s = list2.iter().sorted();
    let res1: u32 = itertools::zip_eq(l1s, l2s)
        .map(|(v, w)| v.abs_diff(*w))
        .sum();
    println!("{res1}");

    // Part 2
    let res2: usize = list1
        .into_iter()
        .map(|v| list2.iter().filter(|&&w| v == w).count() * usize::try_from(v).unwrap())
        .sum();
    println!("{res2}");
}
