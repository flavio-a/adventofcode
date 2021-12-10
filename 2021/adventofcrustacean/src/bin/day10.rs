use adventofcrustacean;
use itertools::Itertools;

const OPEN: [char; 4] = ['(', '[', '{', '<'];

fn closing(c: &char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => panic!("Unexpected char"),
    }
}

fn cost(c: char) -> u64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!("Unexpected char"),
    }
}

fn char_score(c: char) -> u64 {
    match c {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => panic!("Unexpected char"),
    }
}

fn score(stack: Vec<char>) -> u64 {
    let mut score = 0_u64;
    for c in stack.into_iter().rev() {
        score *= 5;
        score += char_score(c);
    }
    return score;
}

// The bool is true iff is corrupted
fn corrupted_or_repair(line: &str) -> (bool, u64) {
    let mut stack = vec![];
    for c in line.chars() {
        if OPEN.contains(&c) {
            stack.push(c);
        }
        else if c == closing(stack.last().unwrap()) {
            stack.pop();
        }
        else {
            return (true, cost(c));
        }
    }
    return (false, score(stack));
}



fn main() {
    let content = adventofcrustacean::read_input();
    let lines = content.lines().map(corrupted_or_repair);
    let (corrupted, incomplete): (Vec<(bool, u64)>, Vec<(bool, u64)>) = lines.partition(|(b, _)| *b);

    // Part 1
    let r1: u64 = corrupted.into_iter().map(|(_, v)| v).sum();
    println!("{}", r1);

    // Part 2
    let tmp = incomplete.into_iter().map(|(_, v)| v).sorted().collect::<Vec<u64>>();
    println!("{}", tmp[tmp.len() / 2]);
}