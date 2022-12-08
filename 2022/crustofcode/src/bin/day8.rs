use crustofcode::*;

fn is_empty_iter<T, I: Iterator<Item = T>>(it: I) -> bool {
    it.peekable().peek().is_none()
}

fn to_usize(x: i16) -> usize {
    x.try_into().unwrap()
}

fn get_scenic_score(grid: &Vec<Vec<u32>>, (i, j): (usize, usize)) -> u64 {
    let score = get_distance(grid, (i, j), (1, 0))
        * get_distance(grid, (i, j), (-1, 0))
        * get_distance(grid, (i, j), (0, 1))
        * get_distance(grid, (i, j), (0, -1));
    return score;
}

fn get_distance(grid: &Vec<Vec<u32>>, (i, j): (usize, usize), (dx, dy): (i16, i16)) -> u64 {
    let h = grid[i][j];
    let mut i: i16 = i.try_into().unwrap();
    let mut j: i16 = j.try_into().unwrap();
    i += dx;
    j += dy;
    let mut count = 1;
    while 0 < i
        && i + 1 < grid.len().try_into().unwrap()
        && 0 < j
        && j + 1 < grid[0].len().try_into().unwrap()
        && grid[to_usize(i)][to_usize(j)] < h
    {
        i = i + dx;
        j = j + dy;
        count += 1;
    }
    return count;
}

fn main() {
    println!("Day 8");

    let grid: Vec<Vec<u32>> = read_input_lines()
        .into_iter()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();

    // Part 1
    let mut count: u32 = 0;
    for (i, line) in grid.iter().enumerate() {
        for (j, &h) in line.iter().enumerate() {
            if i == 0 || i == grid.len() - 1 || j == 0 || j == line.len() - 1 {
                count += 1;
            } else if is_empty_iter(line.iter().take(j).filter(|&&v| v >= h))
                || is_empty_iter(line.iter().skip(j + 1).filter(|&&v| v >= h))
                || is_empty_iter(grid.iter().take(i).filter(|l| l[j] >= h))
                || is_empty_iter(grid.iter().skip(i + 1).filter(|l| l[j] >= h))
            {
                count += 1;
            }
        }
    }
    println!("{}", count);

    // Part 2
    let a = grid
        .iter()
        .enumerate()
        .map(|(i, line)| {
            line.iter()
                .enumerate()
                .map(|(j, _)| get_scenic_score(&grid, (i, j)))
                .max()
                .unwrap()
        })
        .max()
        .unwrap();
    println!("{}", a);
}
