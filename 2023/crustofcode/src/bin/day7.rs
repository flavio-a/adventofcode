use aoc_parse::{parser, prelude::*};

fn convert_card1(c: &char) -> u8 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 11,
        'T' => 10,
        '9' => 9,
        '8' => 8,
        '7' => 7,
        '6' => 6,
        '5' => 5,
        '4' => 4,
        '3' => 3,
        '2' => 2,
        _ => panic!("Unexpected plot twist"),
    }
}

fn freqs_to_type(freq: Vec<u8>) -> u8 {
    match freq[0] {
        5 => 7,
        4 => 6,
        3 => {
            if freq[1] == 2 {
                5
            } else {
                4
            }
        }
        2 => {
            if freq[1] == 2 {
                3
            } else {
                2
            }
        }
        1 => 1,
        _ => panic!("Unexpected frequency"),
    }
}

fn get_type1(cards: &Vec<u8>) -> u8 {
    let mut reps = [0_u8; 15];
    for c in cards {
        reps[*c as usize] += 1;
    }
    let mut freq: Vec<u8> = reps.into_iter().filter(|f| *f > 0).collect();
    freq.sort_by(|a, b| b.cmp(a));
    return freqs_to_type(freq);
}

fn cmp_hands(a: &(u8, Vec<u8>, u64), b: &(u8, Vec<u8>, u64)) -> std::cmp::Ordering {
    if a.0 != b.0 {
        return a.0.cmp(&b.0);
    }
    for i in 0..5 {
        if a.1[i] != b.1[i] {
            return a.1[i].cmp(&b.1[i]);
        }
    }
    return std::cmp::Ordering::Equal;
}

fn convert_card2(c: &char) -> u8 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'T' => 10,
        '9' => 9,
        '8' => 8,
        '7' => 7,
        '6' => 6,
        '5' => 5,
        '4' => 4,
        '3' => 3,
        '2' => 2,
        'J' => 1,
        _ => panic!("Unexpected plot twist"),
    }
}

fn get_type2(cards: &Vec<u8>) -> u8 {
    let mut reps = [0_u8; 15];
    for c in cards {
        reps[*c as usize] += 1;
    }
    let mut js: u8 = reps[1];
    reps[1] = 0;
    let mut freq: Vec<u8> = reps.to_vec();
    freq.sort_by(|a, b| b.cmp(a));
    // Assign jokers
    for i in 0..freq.len() {
        freq[i] += js;
        if freq[i] > 5 {
            js = freq[i] - 5;
            freq[i] = 5;
        } else {
            js = 0;
        }
    }
    return freqs_to_type(freq);
}

fn main() {
    println!("Day 7");

    let p = parser!(lines(alnum+ " " u64));
    let hands: Vec<(Vec<char>, u64)> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let mut hands_typed: Vec<(u8, Vec<u8>, u64)> = hands
        .iter()
        .map(|h| (h.0.iter().map(convert_card1).collect::<Vec<u8>>(), h.1))
        .map(|h| (get_type1(&h.0), h.0, h.1))
        .collect();
    hands_typed.sort_by(cmp_hands);
    let res1: u64 = hands_typed
        .iter()
        .enumerate()
        // .inspect(|(i, a)| println!("{i} {:?}", a))
        .map(|(i, (_, _, b))| ((i + 1) as u64) * b)
        .sum();
    println!("{res1}");

    // Part 2
    let mut hands_typed: Vec<(u8, Vec<u8>, u64)> = hands
        .iter()
        .map(|h| (h.0.iter().map(convert_card2).collect::<Vec<u8>>(), h.1))
        .map(|h| (get_type2(&h.0), h.0, h.1))
        .collect();
    hands_typed.sort_by(cmp_hands);
    let res2: u64 = hands_typed
        .iter()
        .enumerate()
        // .inspect(|(i, a)| println!("{i} {:?}", a))
        .map(|(i, (_, _, b))| ((i + 1) as u64) * b)
        .sum();
    println!("{res2}");
}
