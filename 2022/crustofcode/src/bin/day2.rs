use crustofcode;

fn split_pair(s: String) -> (char, char) {
    assert_eq!(s.chars().count(), 3);
    return (s.chars().nth(0).unwrap(), s.chars().nth(2).unwrap());
}

type Play = i32;
type Round = (Play, Play);

// 1 rock, 2 paper, 3 scissor
fn translate_char(c: char) -> Play {
    match c {
        'A' | 'X' => 1,
        'B' | 'Y' => 2,
        'C' | 'Z' => 3,
        _ => panic!("Unexpected char {}", c),
    }
}

fn compare_plays(opp: Play, me: Play) -> i32 {
    if opp == me {
        0
    }
    else if me - 1 == opp % 3 {
        1
    }
    else {
        -1
    }
}

fn get_score(&(opp, me): &Round) -> i32 {
    me + (3 + compare_plays(opp, me) * 3)
}

fn get_me(opp: Play, res: i32) -> i32 {
    match (opp, res) {
        (2, 1) | (1, 2) | (3, 3) => 1,
        (3, 1) | (2, 2) | (1, 3) => 2,
        (1, 1) | (3, 2) | (2, 3) => 3,
        _ => panic!("Unexpected Round ({}, outcome {})", opp, res),
    }
}

fn get_score2(&(opp, res): &Round) -> i32 {
    (res * 3 - 3) + get_me(opp, res)
}

fn main() {
    println!("Day 2");

    let rounds: Vec<Round> =
        crustofcode::read_input_lines().into_iter()
        .map(split_pair)
        .map(|(a, b)| (translate_char(a), translate_char(b)))
        .collect();

    // Part 1
    println!("{}", rounds.iter().map(get_score).sum::<i32>());
    // Part 2
    println!("{}", rounds.iter().map(get_score2).sum::<i32>());
}
