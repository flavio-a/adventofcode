use aoc_parse::{parser, prelude::*};
use crustofcode;
use crustofcode::into_snd;
use std::cmp;

#[derive(Debug, Clone)]
struct Scratchcard {
    winnings: Vec<u32>,
    got: Vec<u32>,
}

fn scratchcard_matches(s: &Scratchcard) -> usize {
    s.got.iter().filter(|v| s.winnings.contains(v)).count()
}

fn scratchcard_value(s: &Scratchcard) -> u32 {
    let l = scratchcard_matches(s);
    if l == 0 {
        return 0;
    } else {
        return (2_u32).pow((l - 1).try_into().unwrap());
    }
}

fn main() {
    println!("Day 4");

    let p = parser!(lines("Card" " "+ u32 ":" winnings:(' '* u32)+ " | " got:(' '* u32)+ => Scratchcard {
        winnings: winnings.into_iter().map(into_snd).collect(),
        got: got.into_iter().map(into_snd).collect()
    }));
    let scratchcards: Vec<Scratchcard> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    println!(
        "{}",
        scratchcards.iter().map(scratchcard_value).sum::<u32>()
    );

    // Part 2
    let mut copies: Vec<usize> = vec![1; scratchcards.len()];
    for i in 0..copies.len() {
        let n = scratchcard_matches(&scratchcards[i]);
        for j in (i + 1)..=(cmp::min(i + n, copies.len())) {
            copies[j] += copies[i];
        }
    }
    println!("{}", copies.iter().sum::<usize>());
}
