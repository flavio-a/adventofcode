use crustofcode;
use itertools::Itertools;

fn main() {
    println!("Day 1");

    let elves: Vec<i64> = crustofcode::read_input_lines()
        .as_slice()
        .split(|s| s == "")
        .map(|elf| elf.into_iter().map(crustofcode::str2int).sum())
        .collect();

    // Part 1
    println!("{}", elves.iter().max().unwrap());
    // Part 2
    println!("{}", elves.iter().sorted().rev().take(3).sum::<i64>());
}
