use crustofcode::*;
// use itertools::iproduct;
use pathfinding::directed::dijkstra::*;

type Pos = (usize, usize);

fn find_char(grid: &Vec<Vec<char>>, tgt: char) -> Option<Pos> {
    let x = grid
        .iter()
        .position(|s| s.iter().find(|&&c| c == tgt).is_some())?;
    let y = grid[x].iter().position(|&c| c == tgt)?;
    return Some((x, y));
}

fn char_to_height(c: char) -> u8 {
    let c = match c {
        'S' => 'a',
        'E' => 'z',
        _ => c,
    };
    return c as u8;
}

fn get_neighbours(grid: &Vec<Vec<u8>>, &(x, y): &Pos) -> Vec<(Pos, u64)> {
    let mut pos = vec![];
    if x > 0 {
        pos.push((x - 1, y));
    };
    if x < grid.len() - 1 {
        pos.push((x + 1, y));
    };
    if y > 0 {
        pos.push((x, y - 1));
    };
    if y < grid[0].len() - 1 {
        pos.push((x, y + 1));
    };
    let limith = grid[x][y] + 1;
    return pos
        .into_iter()
        .filter(|&(nx, ny)| grid[nx][ny] <= limith)
        .map(|p| (p, 1))
        .collect();
}

// fn get_neighbours_rev(grid: &Vec<Vec<u8>>, &(x, y): &Pos) -> Vec<(Pos, u64)> {
//     let mut pos = vec![];
//     if x > 0 {
//         pos.push((x - 1, y));
//     };
//     if x < grid.len() - 1 {
//         pos.push((x + 1, y));
//     };
//     if y > 0 {
//         pos.push((x, y - 1));
//     };
//     if y < grid[0].len() - 1 {
//         pos.push((x, y + 1));
//     };
//     let limith = grid[x][y].saturating_sub(1);
//     return pos
//         .into_iter()
//         .filter(|&(nx, ny)| grid[nx][ny] >= limith)
//         .map(|p| (p, 1))
//         .collect();
// }

fn main() {
    println!("Day 12");

    let grid: Vec<Vec<char>> = read_input_lines()
        .into_iter()
        .map(|s| s.chars().collect())
        .collect();
    let start = find_char(&grid, 'S').unwrap();
    let end = find_char(&grid, 'E').unwrap();
    let grid: Vec<Vec<u8>> = grid
        .into_iter()
        .map(|l| l.into_iter().map(char_to_height).collect())
        .collect();

    // Part 1
    let path = dijkstra(&start, |p| get_neighbours(&grid, p), |&p| p == end);
    println!("{:?}", path.map(|(_, l)| l));

    // Part 2
    // dijkstra_all(&end, |p| get_neighbours_rev(&grid, p));
    let mut all_starts: Vec<Pos> = vec![];
    for (i, line) in grid.iter().enumerate() {
        for (j, &v) in line.iter().enumerate() {
            if v == ('a' as u8) {
                all_starts.push((i, j));
            }
        }
    }
    let res: u64 = all_starts
        .into_iter()
        .filter_map(|p| dijkstra(&p, |p| get_neighbours(&grid, p), |&p| p == end))
        .map(|(_, l)| l)
        .min()
        .unwrap();
    println!("{}", res);
}
