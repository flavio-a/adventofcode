use aoc_parse::{parser, prelude::*};
use crustofcode;

type Schematic = Vec<Vec<String>>;

const SYMBOLS: &str = "*#+$&%/-=@";

fn get_range(x: usize, y1: usize, y2: usize, schematic: &Schematic) -> std::ops::Range<usize> {
    if x > schematic.len() {
        return 0..0;
    }
    let mut pos: usize = 0;
    let mut i: usize = 0;
    while pos <= y1 {
        pos += schematic[x][i].len();
        i += 1;
    }
    let start = i - 1;
    while pos < y2 && i + 1 < schematic[x].len() {
        pos += schematic[x][i].len();
        i += 1;
    }
    // end is excluded
    let end = i + 1;
    return start..end;
}

fn check_neighbour(i: usize, j: usize, schematic: &Schematic) -> bool {
    // same line
    if (j > 0 && SYMBOLS.contains(&schematic[i][j - 1]))
        || (j + 1 < schematic[i].len() && SYMBOLS.contains(&schematic[i][j + 1]))
    {
        return true;
    }
    // Get physical coordinates in the schematic
    let x = i;
    let y: usize = schematic[i].iter().take(j).map(|s| s.len()).sum();
    let len = schematic[i][j].len();
    // println!("{i}, {j} -> {x}, {y}");
    // should check [y-1; y+len]
    if i > 0 {
        // println!(
        //     "{:?}",
        //     get_range(x.saturating_sub(1), y.saturating_sub(1), y + len, schematic)
        // );
        for j1 in get_range(x.saturating_sub(1), y.saturating_sub(1), y + len, schematic) {
            if SYMBOLS.contains(&schematic[i - 1][j1]) {
                return true;
            }
        }
    }
    if i + 1 < schematic.len() {
        // println!(
        //     "{:?}",
        //     get_range(x + 1, y.saturating_sub(1), y + len, schematic)
        // );
        for j1 in get_range(x + 1, y.saturating_sub(1), y + len, schematic) {
            if SYMBOLS.contains(&schematic[i + 1][j1]) {
                return true;
            }
        }
    }
    return false;
}

fn is_number(s: &String) -> bool {
    s.chars().next().unwrap().is_digit(10)
}

fn gear_ratio(i: usize, j: usize, schematic: &Schematic) -> u32 {
    if schematic[i][j] != "*" {
        return 0;
    }
    // same line
    let mut nums = 0_u8;
    let mut ratio = 1_u32;
    if j > 0 && is_number(&schematic[i][j - 1]) {
        nums += 1;
        ratio *= schematic[i][j - 1].parse::<u32>().unwrap();
    }
    if j + 1 < schematic[i].len() && is_number(&schematic[i][j + 1]) {
        nums += 1;
        ratio *= schematic[i][j + 1].parse::<u32>().unwrap();
    }

    // Get physical coordinates in the schematic
    let x = i;
    let y: usize = schematic[i].iter().take(j).map(|s| s.len()).sum();
    let len = schematic[i][j].len();
    // should check [y-1; y+len]
    if i > 0 {
        for j1 in get_range(x.saturating_sub(1), y.saturating_sub(1), y + len, schematic) {
            if is_number(&schematic[i - 1][j1]) {
                nums += 1;
                ratio *= schematic[i - 1][j1].parse::<u32>().unwrap();
            }
        }
    }
    if i + 1 < schematic.len() {
        for j1 in get_range(x + 1, y.saturating_sub(1), y + len, schematic) {
            if is_number(&schematic[i + 1][j1]) {
                nums += 1;
                ratio *= schematic[i + 1][j1].parse::<u32>().unwrap();
            }
        }
    }

    if nums == 2 {
        return ratio;
    } else {
        return 0;
    }
}

fn main() {
    println!("Day 3");

    let p = parser!(lines({string(u32), string(char_of(".*#+$&%/-=@"))}+));
    let schematic: Schematic = p.parse(&crustofcode::read_input()).unwrap();

    // check_neighbour(0, 5, &schematic);
    // Part 1
    let res1 = schematic
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter_map(|(j, s)| s.parse::<u32>().ok().map(|v| (j, v)))
                .filter(|(j, _)| check_neighbour(i, *j, &schematic))
                .map(|(_, v)| v)
                .sum::<u32>()
        })
        .sum::<u32>();
    println!("{}", res1);

    // Part 2
    let res2 = schematic
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .map(|(j, _)| gear_ratio(i, j, &schematic))
                .sum::<u32>()
        })
        .sum::<u32>();
    println!("{}", res2);
}
