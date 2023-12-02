use crustofcode;
use std::cmp;

#[derive(Debug, Clone)]
struct Draw {
    red: u32,
    green: u32,
    blue: u32,
}

type Game = Vec<Draw>;

fn add_pair(acc: Draw, v: Vec<&str>) -> Draw {
    let n = v[0].parse::<u32>().unwrap();
    return match v[1].trim() {
        "red" => Draw {
            red: acc.red + n,
            green: acc.green,
            blue: acc.blue,
        },
        "green" => Draw {
            red: acc.red,
            green: acc.green + n,
            blue: acc.blue,
        },
        "blue" => Draw {
            red: acc.red,
            green: acc.green,
            blue: acc.blue + n,
        },
        _ => panic!("unexpected plot twist"),
    };
}

fn parse_draw(s: &str) -> Draw {
    let empty_draw = Draw {
        red: 0,
        green: 0,
        blue: 0,
    };
    return s
        .split(",")
        .map(|s| s.trim_start())
        .map(|s| s.split(" ").collect::<Vec<&str>>())
        .fold(empty_draw, add_pair);
}

fn parse_game(s: String) -> Game {
    return s
        .split(":")
        .nth(1)
        .unwrap()
        .split(";")
        .map(parse_draw)
        .collect();
}

fn valid_p1(d: &Draw) -> bool {
    d.red <= 12 && d.green <= 13 && d.blue <= 14
}

fn max_draw(d1: Draw, d2: &Draw) -> Draw {
    return Draw {
        red: cmp::max(d1.red, d2.red),
        green: cmp::max(d1.green, d2.green),
        blue: cmp::max(d1.blue, d2.blue),
    };
}

fn main() {
    println!("Day 2");

    let games: Vec<Game> = crustofcode::read_input_lines()
        .into_iter()
        .map(parse_game)
        .collect();

    // Part 1
    println!(
        "{}",
        games
            .iter()
            .enumerate()
            .filter(|(_, g)| g.iter().all(valid_p1))
            .map(|(i, _)| i + 1)
            .sum::<usize>()
    );
    // Part 2
    let empty_draw = Draw {
        red: 0,
        green: 0,
        blue: 0,
    };
    println!(
        "{}",
        games
            .iter()
            .map(|g| g.iter().fold(empty_draw.clone(), max_draw))
            .map(|d| d.red * d.green * d.blue)
            .sum::<u32>()
    );
}
