use crustofcode::*;

fn from_snafu_digit(c: char) -> i64 {
    match c {
        '2' => 2,
        '1' => 1,
        '0' => 0,
        '-' => -1,
        '=' => -2,
        _ => panic!("AAAA"),
    }
}

fn from_snafu(s: String) -> i64 {
    let mut n = 0;
    for c in s.chars() {
        n = n * 5 + from_snafu_digit(c);
    }
    return n;
}

fn to_snafu_digit(d: i64) -> char {
    match d {
        -2 => '=',
        -1 => '-',
        0 => '0',
        1 => '1',
        2 => '2',
        _ => panic!("AAAAA"),
    }
}

fn to_snafu(mut n: i64) -> String {
    let mut s: Vec<char> = vec![];
    while n != 0 {
        let mut rem = n % 5;
        if rem > 2 {
            rem -= 5;
        }
        if rem < -2 {
            rem += 5;
        }
        n -= rem;
        s.push(to_snafu_digit(rem));
        assert_eq!(n % 5, 0);
        n = n / 5;
    }
    return s.into_iter().rev().collect();
}

fn main() {
    println!("Day 25");

    let snafus: Vec<String> = read_input_lines();

    // Part 1
    println!("{}", to_snafu(snafus.into_iter().map(from_snafu).sum()));
}
