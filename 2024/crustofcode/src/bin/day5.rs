use aoc_parse::{parser, prelude::*};

fn rules_gt(rules: &Vec<(u8, u8)>, a: &u8, b: &u8) -> bool {
    rules.contains(&(*b, *a))
}

fn sum_middle_pages(v: Vec<Vec<u8>>) -> u32 {
    v.iter()
        .map(|u| u32::try_from(u[(u.len() - 1) / 2]).unwrap())
        .sum()
}

fn main() {
    println!("Day 5");

    let rule_p = parser!(u8 "|" u8);
    let update_p = parser!(repeat_sep(u8, ","));
    let p = parser!(section(lines(rule_p)) section(lines(update_p)));
    let (rules, updates) = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let (correct, incorrect): (Vec<_>, Vec<_>) = updates.into_iter().partition(|u| {
        u.iter()
            .enumerate()
            .all(|(i, v)| u[i..].iter().all(|w| !rules_gt(&rules, v, w)))
    });
    let res1: u32 = sum_middle_pages(correct);
    println!("{res1}");

    // Part 2
    let sorted: Vec<Vec<_>> = incorrect
        .into_iter()
        .map(|u| {
            let mut ts = topological_sort::TopologicalSort::<u8>::new();
            for (a, b) in &rules {
                if u.contains(a) && u.contains(b) {
                    ts.add_dependency(*a, *b);
                }
            }
            ts.into_iter().collect()
        })
        .collect();
    let res2 = sum_middle_pages(sorted);
    println!("{res2}");
}
