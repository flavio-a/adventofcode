use aoc_parse::{parser, prelude::*};

type Pattern = Vec<Vec<usize>>;

fn eq_col(p: &Pattern, j1: usize, j2: usize) -> bool {
    for i in 0..p.len() {
        if p[i][j1] != p[i][j2] {
            return false;
        }
    }
    return true;
}

fn eq_row(p: &Pattern, i1: usize, i2: usize) -> bool {
    return p[i1] == p[i2];
}

fn search_horiz_refl(p: &Pattern, i0: usize) -> Option<usize> {
    for i in i0..p.len() {
        let mut delta = 0;
        let mut refl = true;
        while refl && delta < std::cmp::min(p.len() - i, i) {
            if !eq_row(p, i + delta, i - delta - 1) {
                refl = false;
            }
            delta += 1;
        }
        if refl {
            return Some(i);
        }
    }
    return None;
}

fn search_vert_refl(p: &Pattern, j0: usize) -> Option<usize> {
    for j in j0..p[0].len() {
        let mut delta = 0;
        let mut refl = true;
        while refl && delta < std::cmp::min(p[0].len() - j, j) {
            if !eq_col(p, j + delta, j - delta - 1) {
                refl = false;
            }
            delta += 1;
        }
        if refl {
            return Some(j);
        }
    }
    return None;
}

fn search_both(p: &Pattern) -> usize {
    search_horiz_refl(p, 1).unwrap_or(0) * 100 + search_vert_refl(p, 1).unwrap_or(0)
}

fn search_both2(p: &Pattern, old_i: Option<usize>, old_j: Option<usize>) -> usize {
    let mut i = search_horiz_refl(p, 1);
    if i.is_some() {
        if i == old_i {
            i = search_horiz_refl(p, old_i.unwrap() + 1);
        }
        if i.is_some() {
            return i.unwrap() * 100;
        }
    }
    let mut j = search_vert_refl(p, 1);
    if j.is_some() {
        if j == old_j {
            j = search_vert_refl(p, old_j.unwrap() + 1);
        }
        if j.is_some() {
            return j.unwrap();
        }
    }
    return 0;
}

// Change one value in each possible way, and get the reflection line
fn tweak(p: &Pattern) -> usize {
    let old_i = search_horiz_refl(p, 1);
    let old_j = search_vert_refl(p, 1);
    let mut new_p = p.clone();
    for i in 0..p.len() {
        for j in 0..p[0].len() {
            new_p[i][j] = 1 - new_p[i][j];
            let s = search_both2(&new_p, old_i, old_j);
            if s != 0 {
                // println!("Tweaked in position {i} {j}, result {s}");
                return s;
            }
            new_p[i][j] = 1 - new_p[i][j];
        }
    }
    crustofcode::visualize_grid(
        &p.iter()
            .map(|r| r.iter().map(|v| *v == 1).collect())
            .collect(),
    );
    panic!("Unexpected plot twist, no tweak found");
}

fn main() {
    println!("Day 13");

    let p_pattern = parser!(lines(char_of(".#")+));
    let p = parser!(sections(p_pattern));
    let patterns: Vec<Pattern> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let r1: usize = patterns.iter().map(search_both).sum();
    println!("{r1}");

    // Part 2

    // let m = patterns
    //     .iter()
    //     .map(|p| (p.len(), p[0].len()))
    //     .inspect(|(h, w)| println!("({w}, {h}) {}", w * h))
    //     .map(|(h, w)| h * w)
    //     .max()
    //     .unwrap();
    // println!("{m}");
    let r2: usize = patterns.iter().map(tweak).sum();
    println!("{r2}");
}
