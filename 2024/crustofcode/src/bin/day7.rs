use aoc_parse::{parser, prelude::*};
use num::pow;

type Equation = (u64, Vec<u64>);

fn num_digits(n: u64) -> u32 {
    n.checked_ilog10().unwrap_or(0) + 1
}

fn digits_ten(n: u64) -> u64 {
    pow(10, num_digits(n).try_into().unwrap())
}

fn try_deeval1(res: u64, numbers: &[u64]) -> bool {
    // println!("Trying de-eval {} = {:?}", res, numbers);
    if numbers.len() == 0 {
        return res == 0;
    }
    let head = numbers[0];
    let tail = &numbers[1..];
    if head != 0 && res % head == 0 && try_deeval1(res / head, tail) {
        return true;
    }
    if res >= head && try_deeval1(res - head, tail) {
        return true;
    }
    return false;
}

fn try_deeval2(res: u64, numbers: &[u64]) -> bool {
    if numbers.len() == 0 {
        return res == 0;
    }
    let head = numbers[0];
    let tail = &numbers[1..];
    if head != 0 && res % head == 0 && try_deeval2(res / head, tail) {
        return true;
    }
    if res >= head
        && (res - head) % digits_ten(head) == 0
        && try_deeval2((res - head) / digits_ten(head), tail)
    {
        return true;
    }
    if res >= head && try_deeval2(res - head, tail) {
        return true;
    }
    return false;
}

fn main() {
    println!("Day 7");

    let p = parser!(lines(u64 ": " repeat_sep(u64, " ")));
    let mut equations: Vec<Equation> = p.parse(&crustofcode::read_input()).unwrap();
    for (_, nums) in &mut equations {
        nums.reverse();
    }

    // Part 1
    let res1: u64 = equations
        .iter()
        .filter(|(res, nums)| try_deeval1(*res, nums))
        .map(|(res, _)| res)
        .sum();
    println!("{res1}");

    // Part 2
    let res2: u64 = equations
        .iter()
        .filter(|(res, nums)| try_deeval2(*res, nums))
        .map(|(res, _)| res)
        .sum();
    println!("{res2}");
}
