use aoc_parse::{parser, prelude::*};

const COLORS: &str = "rgubw";

fn solve_pattern_inner(
    pattern: &Vec<usize>,
    towels: &Vec<Vec<usize>>,
    idx: usize,
    memoize: &mut Vec<Option<usize>>,
) -> usize {
    // println!("idx: {idx}, suffix: {:?}", &pattern[idx..]);
    if idx == pattern.len() {
        return 1;
    }
    if memoize[idx].is_some() {
        return memoize[idx].unwrap();
    }
    let res = towels
        .iter()
        .map(|towel| {
            if pattern[idx..].starts_with(towel) {
                solve_pattern_inner(pattern, towels, idx + towel.len(), memoize)
            } else {
                0
            }
        })
        .sum();
    memoize[idx] = Some(res);
    return res;
}

/// Returns whether the pattern is possible or not
fn solve_pattern(pattern: &Vec<usize>, towels: &Vec<Vec<usize>>) -> usize {
    let mut memoize = vec![None; pattern.len()];
    // println!("{:?}", towels);
    solve_pattern_inner(pattern, towels, 0, &mut memoize)
}

fn main() {
    println!("Day 19");

    let towels_p = parser!(line(repeat_sep(char_of(COLORS)+, ", ")));
    let patterns_p = parser!(lines(char_of(COLORS)+));
    let p = parser!(section(towels_p) section(patterns_p));
    let (towels, patterns) = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let mappeds: Vec<_> = patterns
        .iter()
        .map(|pat| solve_pattern(pat, &towels))
        .collect();
    let res1 = mappeds.iter().filter(|&&v| v > 0).count();
    println!("{res1}");

    // Part 2
    let res2: usize = mappeds.iter().sum();
    println!("{res2}");
}
