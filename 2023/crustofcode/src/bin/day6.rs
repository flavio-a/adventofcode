use aoc_parse::{parser, prelude::*};

fn get_time(t: u64, hold: u64) -> u64 {
    hold * (t - hold)
}

fn get_min_max_win((t, d): (u64, u64)) -> (u64, u64) {
    let thalf = t / 2;
    let mid = thalf * thalf;
    assert!(mid >= d);
    let mut min_hold = thalf - num::integer::sqrt(mid - d);
    // println!("{min_hold}");
    if get_time(t, min_hold) > d {
        while get_time(t, min_hold) > d {
            // println!("Above ({}), decreasing", get_time(t, min_hold));
            min_hold -= 1;
        }
        min_hold += 1;
    } else {
        while get_time(t, min_hold) <= d {
            // println!("Below ({}), increasing", get_time(t, min_hold));
            min_hold += 1;
        }
    }

    let mut max_hold = thalf + num::integer::sqrt(mid - d);
    if get_time(t, max_hold) > d {
        while get_time(t, max_hold) > d {
            max_hold += 1;
        }
        max_hold -= 1;
    } else {
        while get_time(t, max_hold) <= d {
            max_hold -= 1;
        }
    }

    return (min_hold, max_hold);
}

fn main() {
    println!("Day 6");

    let input = crustofcode::read_input();

    // Part 1
    let time_p1 = parser!(line("Time:" (' '+ t:u64 => t)*));
    let dist_p1 = parser!(line("Distance:" (' '+ d:u64 => d)*));
    let p1 = parser!(times:time_p1 dists:dist_p1 =>
        times.into_iter().zip(dists.into_iter()).collect::<Vec<(u64, u64)>>()
    );
    let races = p1.parse(&input).unwrap();

    let res1: u64 = races
        .into_iter()
        .map(get_min_max_win)
        // .inspect(|&(l, u)| println!("{l} {u}"))
        .map(|(l, u)| u - l + 1)
        // .inspect(|&p| println!("{p}"))
        .reduce(|acc, v| acc * v)
        .unwrap();
    println!("{res1}");

    // Part 2
    let mut input2 = input.clone();
    input2.retain(|c| c != ' ');
    let time_p2 = parser!(line("Time:" u64));
    let dist_p2 = parser!(line("Distance:" u64));
    let p2 = parser!(time_p2 dist_p2);
    let race2 = p2.parse(&input2).unwrap();

    let (l2, u2) = get_min_max_win(race2);

    println!("{}", u2 + 1 - l2);
}
