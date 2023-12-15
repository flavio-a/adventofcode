use aoc_parse::{parser, prelude::*};

type Lens = (String, u32);

fn to_ascii(c: char) -> u8 {
    (c as u32).try_into().unwrap()
}

fn hash(s: &str) -> u8 {
    let mut val: u8 = 0;
    for c in s.chars() {
        val = val.wrapping_add(to_ascii(c));
        val = val.wrapping_mul(17);
    }
    return val;
}

#[derive(Debug, Clone)]
enum Op {
    Remove,
    Add(u32),
}

#[derive(Debug, Clone)]
struct Step {
    label: String,
    op: Op,
}

impl Step {
    fn box_num(&self) -> u8 {
        hash(&self.label)
    }
}

#[allow(dead_code)]
fn print_boxes(boxes: &[Vec<Lens>; 256]) {
    println!("Status: ");
    for (i, b) in boxes.iter().enumerate() {
        if b.len() > 0 {
            println!("> Box {i}: {:?}", b);
        }
    }
}

fn box_focusing_power((bnum, b): (usize, &Vec<Lens>)) -> u64 {
    let m1: u64 = (bnum + 1).try_into().unwrap();
    m1 * b
        .iter()
        .enumerate()
        .map(|(lnum, (_, v))| (lnum as u64 + 1) * (*v as u64))
        .sum::<u64>()
}

fn focusing_power(boxes: &[Vec<Lens>; 256]) -> u64 {
    boxes.iter().enumerate().map(box_focusing_power).sum()
}

fn main() {
    println!("Day 15");

    // Part 1
    let r1: u64 = crustofcode::read_input()
        .trim()
        .split(",")
        .map(hash)
        .map(|v| u64::from(v))
        .sum();
    println!("{r1}");

    // Part 2
    let step_p = parser!(code:alpha+ op:{
        "=" val:u32 => Op::Add(val),
        "-" => Op::Remove
    } => Step { label: String::from_iter(code), op: op });
    let p = parser!(line(repeat_sep(step_p, ",")));
    let steps: Vec<Step> = p.parse(&crustofcode::read_input()).unwrap();

    const EMPTY: Vec<Lens> = vec![];
    let mut boxes: [Vec<Lens>; 256] = [EMPTY; 256];
    for step in steps {
        let box_num: usize = step.box_num().into();
        match step.op {
            Op::Add(n) => {
                let idx = boxes[box_num]
                    .iter()
                    .position(|(label, _)| *label == step.label);
                match idx {
                    Some(idx) => boxes[box_num][idx].1 = n,
                    None => boxes[box_num].push((step.label.clone(), n)),
                };
            }
            Op::Remove => {
                boxes[box_num].retain(|(label, _)| *label != step.label);
            }
        }
        // print_boxes(&boxes);
    }
    println!("{}", focusing_power(&boxes));
}
