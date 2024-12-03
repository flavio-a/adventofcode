use regex::Regex;

fn main() {
    println!("Day 3");

    let input: String = crustofcode::read_input();

    // Part 1
    let re1 = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    let res1: u32 = re1
        .captures_iter(&input)
        .map(|c| c[1].parse::<u32>().unwrap() * c[2].parse::<u32>().unwrap())
        .sum();
    println!("{res1}");

    // Part 2
    let re2 = Regex::new(r"(?:mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\))").unwrap();
    let mut active: bool = true;
    let res2: u32 = re2
        .captures_iter(&input)
        .map(|c| {
            if c[0].starts_with("mul") {
                if active {
                    c[1].parse::<u32>().unwrap() * c[2].parse::<u32>().unwrap()
                } else {
                    0
                }
            } else if c[0].starts_with("don't") {
                active = false;
                0
            } else if c[0].starts_with("do") {
                active = true;
                0
            } else {
                panic!("Bruh")
            }
        })
        .sum();
    println!("{res2}");
}
