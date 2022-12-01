use crustofcode;

fn main() {
    println!("Day 1");

    let content = crustofcode::read_input();

    let elves = content.split("\n\n");
    let elves: Vec<i64> = elves.map(|s| s.lines().map(crustofcode::str2int).sum()).collect();

    println!("{}", elves.iter().max().unwrap());

    let mut sorted_elves = elves.clone();
    sorted_elves.sort_unstable();
    println!("{}", sorted_elves.into_iter().rev().take(3).sum::<i64>());
}
