use adventofcrustacean;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum SeaCucumber {
    Left,
    Down,
    None,
}

type Grid = Vec<Vec<SeaCucumber>>;

fn parse_cell(c: char) -> SeaCucumber {
    match c {
        '.' => SeaCucumber::None,
        '>' => SeaCucumber::Left,
        'v' => SeaCucumber::Down,
        _ => panic!("Unexpected character {}", c),
    }
}

#[inline(always)]
fn is_empty(c: &SeaCucumber) -> bool {
    *c == SeaCucumber::None
}

fn step(grid: &Grid) -> Grid {
    let height = grid.len();
    let width = grid[0].len();
    let mut tmp: Grid = vec![vec![SeaCucumber::None; width]; height];
    for (i, row) in grid.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            if *c == SeaCucumber::Left {
                let j1 = (j + 1) % width;
                if is_empty(&grid[i][j1]) {
                    tmp[i][j1] = *c;
                } else {
                    tmp[i][j] = *c;
                }
            } else if *c == SeaCucumber::Down {
                tmp[i][j] = *c;
            }
        }
    }
    let mut res: Grid = vec![vec![SeaCucumber::None; width]; height];
    for (i, row) in tmp.iter().enumerate() {
        for (j, c) in row.iter().enumerate() {
            if *c == SeaCucumber::Down {
                let i1 = (i + 1) % height;
                if is_empty(&tmp[i1][j]) {
                    res[i1][j] = *c;
                } else {
                    res[i][j] = *c;
                }
            } else if *c == SeaCucumber::Left {
                res[i][j] = *c;
            }
        }
    }
    return res;
}

#[allow(dead_code)]
fn print_grid(grid: &Grid) {
    for row in grid {
        for c in row {
            let display = match c {
                SeaCucumber::None => '.',
                SeaCucumber::Left => '>',
                SeaCucumber::Down => 'v',
            };
            print!("{}", display);
        }
        println!("");
    }
}

fn main() {
    let content = adventofcrustacean::read_input();
    let grid: Grid = content
        .lines()
        .map(|s| s.chars().map(parse_cell).collect())
        .collect();

    // Part 1
    // Iterate until fixpoint
    let mut idx: u32 = 0;
    let mut old = grid;
    let mut new;
    loop {
        new = step(&old);
        idx += 1;
        if old == new {
            break;
        }
        old = new;
    }
    // print_grid(&new);
    println!("{}", idx);

    // Part 2
    // There's no part 2 for the last day
}
