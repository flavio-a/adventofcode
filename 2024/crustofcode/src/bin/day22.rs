use aoc_parse::{parser, prelude::*};
use circular_buffer::CircularBuffer;

const MODULO: u64 = 16777216;

fn mix(secret: u64, value: u64) -> u64 {
    secret ^ value
}

fn prune(value: u64) -> u64 {
    value % MODULO
}

fn next(secret: u64) -> u64 {
    let secret = prune(mix(secret, secret * 64));
    let secret = prune(mix(secret, secret / 32));
    let secret = prune(mix(secret, secret * 2048));
    secret
}

fn secret_sequence(secret: u64) -> impl Iterator<Item = u64> {
    itertools::iterate(secret, |secret: &u64| next(*secret))
}

fn part1(secret: u64) -> u64 {
    secret_sequence(secret).nth(2000).unwrap()
}

// In fact, numbers here are from -9 to 9
const BASE: usize = 19;
const MAX_IDX: usize = BASE.pow(4);

fn deltas_to_idx<'a, T: Iterator<Item = &'a i64>>(deltas: T) -> usize {
    let base_i: i64 = BASE.try_into().unwrap();
    let mut res: usize = 0;
    for d in deltas {
        let v = d + 9;
        assert!(0 <= v && v < base_i);
        res = res * BASE + usize::try_from(v).unwrap();
    }
    res
}

fn get_price(secret: u64) -> i64 {
    i64::try_from(secret).unwrap() % 10
}

fn update_deltas_count(secret: u64, deltas_counts: &mut [i64; MAX_IDX]) {
    let mut seen = [false; MAX_IDX];
    let mut curr_deltas = CircularBuffer::<4, i64>::new();
    let mut secret_sequence = secret_sequence(secret);
    let mut prev_price = get_price(secret_sequence.next().unwrap());
    for sec in secret_sequence.take(2000) {
        let price = get_price(sec);
        let delta = price - prev_price;
        curr_deltas.push_back(delta);
        if curr_deltas.len() == 4 {
            let idx = deltas_to_idx(curr_deltas.iter());
            if !seen[idx] {
                seen[idx] = true;
                deltas_counts[idx] += price;
            }
        }
        prev_price = price;
    }
}

fn main() {
    println!("Day 22");

    let p = parser!(lines(u64));
    let secrets: Vec<_> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let res1: u64 = secrets.iter().map(|secret: &u64| part1(*secret)).sum();
    println!("{res1}");

    // Part 2
    let mut deltas_counts = [0; MAX_IDX];
    // For each sequence, adds the bananas you can get from that sequence with a given deltas
    for secret in &secrets {
        update_deltas_count(*secret, &mut deltas_counts);
    }
    let res2 = deltas_counts.iter().max().unwrap();
    println!("{res2}");
}
