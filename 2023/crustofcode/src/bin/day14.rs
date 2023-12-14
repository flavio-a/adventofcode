use aoc_parse::{parser, prelude::*};
use itertools::Itertools;

// For every position, computes the index it rolls north to
fn north_pos(col: &Vec<usize>) -> Vec<usize> {
    let mut res = vec![0_usize; col.len()];
    let mut stop = 0;
    for i in 0..col.len() {
        match col[i] {
            0 => res[i] = i,
            1 => {
                res[i] = stop;
                stop += 1;
            }
            2 => {
                stop = i + 1;
                res[i] = i
            }
            _ => panic!("Unexpected plot twist"),
        }
    }
    return res;
}

// For every position, computes the index it rolls south to
fn south_pos(col: &Vec<usize>) -> Vec<usize> {
    let mut res = vec![0_usize; col.len()];
    let mut stop = col.len() - 1;
    for i in (0..col.len()).rev() {
        match col[i] {
            0 => res[i] = i,
            1 => {
                res[i] = stop;
                stop = stop.saturating_sub(1);
            }
            2 => {
                stop = i.saturating_sub(1);
                res[i] = i
            }
            _ => panic!("Unexpected plot twist"),
        }
    }
    return res;
}

fn roll_ns<F>(mut tab: Vec<Vec<usize>>, get_pos: F) -> Vec<Vec<usize>>
where
    F: Fn(&Vec<usize>) -> Vec<usize>,
{
    for j in 0..tab[0].len() {
        let c = get_column(&tab, j);
        let pos = get_pos(&c);
        for i in 0..tab.len() {
            tab[i][j] = 0;
        }
        for (i, v) in c.iter().enumerate() {
            if *v == 1 || *v == 2 {
                tab[pos[i]][j] = *v
            }
        }
    }
    return tab;
}

// For every position, computes the index it rolls east to
fn east_pos(row: &Vec<usize>) -> Vec<usize> {
    let mut res = vec![0_usize; row.len()];
    let mut stop = row.len() - 1;
    for j in (0..row.len()).rev() {
        match row[j] {
            0 => res[j] = j,
            1 => {
                res[j] = stop;
                stop = stop.saturating_sub(1);
            }
            2 => {
                stop = j.saturating_sub(1);
                res[j] = j
            }
            _ => panic!("Unexpected plot twist"),
        }
    }
    return res;
}

// For every position, computes the index it rolls west to
fn west_pos(row: &Vec<usize>) -> Vec<usize> {
    let mut res = vec![0_usize; row.len()];
    let mut stop = 0;
    for j in 0..row.len() {
        match row[j] {
            0 => res[j] = j,
            1 => {
                res[j] = stop;
                stop += 1;
            }
            2 => {
                stop = j + 1;
                res[j] = j
            }
            _ => panic!("Unexpected plot twist"),
        }
    }
    return res;
}

fn roll_we<F>(mut tab: Vec<Vec<usize>>, get_pos: F) -> Vec<Vec<usize>>
where
    F: Fn(&Vec<usize>) -> Vec<usize>,
{
    for i in 0..tab.len() {
        let r = tab[i].clone();
        let pos = get_pos(&r);
        for j in 0..tab[0].len() {
            tab[i][j] = 0;
        }
        for (j, v) in r.iter().enumerate() {
            if *v == 1 || *v == 2 {
                tab[i][pos[j]] = *v
            }
        }
    }
    return tab;
}

fn do_cycle(mut tab: Vec<Vec<usize>>) -> Vec<Vec<usize>> {
    // Roll north
    tab = roll_ns(tab, north_pos);
    tab = roll_we(tab, west_pos);
    tab = roll_ns(tab, south_pos);
    tab = roll_we(tab, east_pos);
    return tab;
}

fn get_column(tab: &Vec<Vec<usize>>, j: usize) -> Vec<usize> {
    tab.iter().map(|r| r[j]).collect_vec()
}

fn north_load(tab: &Vec<Vec<usize>>) -> usize {
    tab.iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .filter_map(|v| (*v == 1).then_some(tab.len() - i))
                .sum::<usize>()
        })
        .sum()
}

#[allow(dead_code)]
fn print_platform(platform: &Vec<Vec<usize>>, i0: usize) {
    for i in i0..platform.len() {
        let line = &platform[i];
        for &dot in line.iter() {
            match dot {
                0 => print!("."),
                1 => print!("O"),
                2 => print!("#"),
                _ => panic!("E"),
            };
        }
        println!("");
    }
}

fn main() {
    println!("Day 14");

    let p = parser!(lines(char_of(".O#")+));
    let platform: Vec<Vec<usize>> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let mut res1 = 0;
    for j in 0..platform[0].len() {
        let c = get_column(&platform, j);
        let ranks = north_pos(&c);
        res1 += c
            .into_iter()
            .zip(ranks.into_iter())
            .filter(|(v, _)| *v == 1)
            .map(|(_, r)| platform.len() - r)
            .sum::<usize>();
    }
    println!("{res1}");

    // Part 2
    let mut platform = platform;
    // Take a configuration after some cycles to make sure we entered the cycle
    let old_i = 1000;
    for _ in 0..old_i {
        platform = do_cycle(platform);
    }
    let old_platform = platform.clone();
    // Look for cycle
    platform = do_cycle(platform);
    let mut i = old_i + 1;
    while platform != old_platform {
        platform = do_cycle(platform);
        i += 1;
    }
    // I know that I repeat every (i - old_i) cycles
    let period = i - old_i;
    let target_i = 1000000000;
    let start_i = i + period * (target_i / period);
    for _ in start_i..target_i {
        platform = do_cycle(platform);
    }
    println!("{}", north_load(&platform));
}
