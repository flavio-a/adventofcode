use adventofcrustacean;
use itertools::Itertools;

fn char2int(c: char) -> u32 {
    c.to_digit(10).expect("Char parsable to number expected")
}

fn neighbours(heights: &Vec<u32>, x: usize, pos: usize) -> Vec<usize> {
    let px = pos % x;
    let mut poss: Vec<usize> = vec![];
    if px > 0 {
        poss.push(pos - 1);
    }
    if px + 1 < x {
        poss.push(pos + 1);
    }
    if pos >= x {
        poss.push(pos - x);
    }
    if pos + x < heights.len() {
        poss.push(pos + x);
    }
    return poss;
}

fn is_low(heights: &Vec<u32>, x: usize, pos: usize) -> bool {
    let mut low = true;
    for &i in neighbours(heights, x, pos).iter() {
        low = low && heights[pos] < heights[i];
    }
    return low;
}

fn basin_size(heights: &Vec<u32>, x: usize, pos: usize, visited: &mut Vec<bool>) -> u32 {
    if heights[pos] == 9 {
        return 0;
    }
    let mut size = 1_u32;
    visited[pos] = true;
    // I would like to do this, but because of mutable borrowing in basin_size
    // I can't also borrow it in filter
    // size += neighbours(heights, x, pos).into_iter()
    //                                    .filter(|&v| !visited.get(v).unwrap() )
    //                                    .map(|v| basin_size(heights, x, v, visited))
    //                                    .sum::<u32>();
    for neigh in neighbours(heights, x, pos).into_iter() {
        if !visited[neigh] {
            size += basin_size(heights, x, neigh, visited);
        }
    }
    return size;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let lines = content.lines().collect::<Vec<&str>>();
    let x = lines[0].len();
    let heights = lines.into_iter().map(|s| s.chars().map(char2int)).flatten().collect::<Vec<u32>>();

    // Part 1
    let low_points = (0..heights.len()).filter(|&v| is_low(&heights, x, v)).collect::<Vec<usize>>();
    let r1: u32 = low_points.iter().map(|&v| heights[v] + 1).sum();
    println!("{}", r1);

    // Part 2
    let mut visited = vec![false; heights.len()];
    let tmp = low_points.iter().map(|&v| basin_size(&heights, x, v, &mut visited)).collect::<Vec<u32>>();
    let r2: u32 = tmp.into_iter().sorted().rev().take(3).product();
    println!("{}", r2);
}