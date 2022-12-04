use crustofcode;
use std::iter::Iterator;

type Line = [[i64; 2]; 2];

trait IterExt {
    type I;

    fn take_two(&mut self) -> [Self::I; 2];
}

impl<It, T: Iterator<Item = It>> IterExt for T {
    type I = It;

    fn take_two(&mut self) -> [It; 2] {
        [self.next().unwrap(), self.next().unwrap()]
    }
}

fn parse_line(line: String) -> Line {
    line.split(",")
        .map(|s| s.split("-").map(crustofcode::str2int).take_two())
        .take_two()
}

fn fully_contains(&&[p1, p2]: &&Line) -> bool {
    (p1[0] >= p2[0] && p1[1] <= p2[1]) || (p1[0] <= p2[0] && p1[1] >= p2[1])
}

fn overlaps(&&[p1, p2]: &&Line) -> bool {
    p1[1] >= p2[0] && p1[0] <= p2[1]
}

fn main() {
    println!("Day 4");

    let assigments_pairs: Vec<Line> = crustofcode::read_input_lines()
        .into_iter()
        .map(parse_line)
        .collect();

    // Part 1
    println!("{}", assigments_pairs.iter().filter(fully_contains).count());

    // Part 2
    println!("{}", assigments_pairs.iter().filter(overlaps).count());
}
