use aoc_parse::{parser, prelude::*};

fn extrapolate(mut seq: Vec<i32>, partial: i32) -> i32 {
    if seq.iter().all(|v| *v == 0) {
        return partial;
    }
    for i in 0..seq.len() - 1 {
        seq[i] = seq[i + 1] - seq[i];
    }
    let l = seq.pop().unwrap();
    extrapolate(seq, partial + l)
}

fn main() {
    println!("Day 9");

    let p = parser!(lines(repeat_sep(i32, " ")));
    let sequences: Vec<Vec<i32>> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let r1: i32 = sequences.iter().cloned().map(|s| extrapolate(s, 0)).sum();
    println!("{r1}");

    // Part 2
    let r2: i32 = sequences
        .iter()
        .cloned()
        .map(|mut s| {
            s.reverse();
            extrapolate(s, 0)
        })
        .sum();
    println!("{r2}");
}
