use adventofcrustacean;
use itertools::Itertools;

const COMPS: [&str; 10] = [
    "abcefg",   // 0
    "cf",       // 1
    "acdeg",    // 2
    "acdfg",    // 3
    "bcdf",     // 4
    "abdfg",    // 5
    "abdefg",   // 6
    "acf",      // 7
    "abcdefg",  // 8
    "abcdfg",   // 9
];

fn char2idx(c: char) -> usize {
    (c as usize) - ('a' as usize)
}

fn isinstr(c: char, s: &str) -> bool {
    s.chars().any(|x| x == c)
}

fn translate_str(s: &str, transl_vec: &Vec<char>) -> usize {
    let tmp = s.chars()
               .map(|c| transl_vec[char2idx(c)])
               .sorted()
               .collect::<String>();
    let idx = COMPS.iter().position(|&r| r == tmp).unwrap();
    return idx;
}

// Takes a line and returns the four output digits
fn solve_line(line: &str) -> Vec<usize> {
    let tmp = line.split("|").collect::<Vec<&str>>();
    let patterns = tmp[0].trim().split(" ").sorted_by_key(|&s| s.len()).collect::<Vec<&str>>();
    let output_digits = tmp[1].trim().split(" ").collect::<Vec<&str>>();

    // println!("{:?}", patterns);
    // Hand-made solver
    let mut cusu = vec!['0'; 7];
    // 1
    for c in patterns[0].chars() {
        // c appears in 8 numbers, f appears in 9
        if patterns.iter().filter(|&s| isinstr(c, s)).count() == 8 {
            cusu[char2idx(c)] = 'c';
        }
        else {
            cusu[char2idx(c)] = 'f';
        }
    }
    // 7
    for c in patterns[1].chars() {
        if cusu[char2idx(c)] == '0' {
            cusu[char2idx(c)] = 'a';
        }
    }
    // 4
    for c in patterns[2].chars() {
        if cusu[char2idx(c)] == '0' {
            // b appears in 6 numbers, d appears in 7
            if patterns.iter().filter(|&s| isinstr(c, s)).count() == 6 {
                cusu[char2idx(c)] = 'b';
            }
            else {
                cusu[char2idx(c)] = 'd';
            }
        }
    }
    // Now e appears in 4 numbers, g in 7
    for c in "abcdefg".chars() {
        if cusu[char2idx(c)] == '0' {
            if patterns.iter().filter(|&s| isinstr(c, s)).count() == 4 {
                cusu[char2idx(c)] = 'e';
            }
            else {
                cusu[char2idx(c)] = 'g';
            }
        }
    }
    // println!("{:?}", cusu);

    // Translation
    output_digits.iter().map(|&s| translate_str(s, &cusu)).collect::<Vec<usize>>()
}

fn is_unique_len(v: usize) -> bool {
    v == 1_usize || v == 4_usize || v == 7_usize || v == 8_usize
}

fn baserep(digits: &Vec<usize>) -> usize {
    let mut tot = 0_usize;
    for d in digits.iter() {
        tot = tot * 10 + d;
    }
    return tot;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let outputs = content.lines().map(solve_line).collect::<Vec<Vec<usize>>>();

    // Part 1
    let a = outputs.iter().flatten().filter(|&v| is_unique_len(*v)).count();
    println!("{}", a);

    // Part 2
    let b: usize = outputs.iter().map(baserep).sum();
    println!("{}", b);
}