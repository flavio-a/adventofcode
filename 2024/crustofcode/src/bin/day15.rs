use aoc_parse::{parser, prelude::*};
use crustofcode::{get_dimensions, Dir4, UPoint};

const DIRS: [char; 4] = ['^', '>', 'v', '<'];

#[derive(Debug, Clone, PartialEq, Eq)]
enum GridCell {
    Wall,
    Empty,
    Box,
    Robot,
    BoxL,
    BoxR,
}

impl GridCell {
    #[allow(unused)]
    fn visualize(&self) -> char {
        match &self {
            GridCell::Box => 'O',
            GridCell::Wall => '█',
            GridCell::Empty => ' ',
            GridCell::Robot => '@',
            GridCell::BoxL => '[',
            GridCell::BoxR => ']',
        }
    }
}

/// Checks whether you can move the thing at position pos in direction dir,
/// recursively
fn check_move(
    grid: &Vec<Vec<GridCell>>,
    pos: UPoint,
    dir: &Dir4,
    memo: &mut Vec<Vec<Option<bool>>>,
) -> bool {
    if memo[pos.0][pos.1].is_some() {
        return memo[pos.0][pos.1].unwrap();
    }
    let (w, h) = get_dimensions(&grid);
    let what = grid[pos.0][pos.1].clone();
    if what == GridCell::Empty {
        memo[pos.0][pos.1] = Some(true);
        return true;
    }
    if what == GridCell::Wall {
        memo[pos.0][pos.1] = Some(false);
        return false;
    }
    let new_pos = dir.move_point(pos, h, w).unwrap();
    let can_move_me = check_move(grid, new_pos, dir, memo);
    if !can_move_me {
        memo[pos.0][pos.1] = Some(false);
        return false;
    }

    if what == GridCell::BoxL && dir.is_vertical() {
        // Corresponding BoxR
        let posr = Dir4::R.move_point(pos, h, w).unwrap();
        assert_eq!(grid[posr.0][posr.1], GridCell::BoxR);
        let new_posr = dir.move_point(posr, h, w).unwrap();
        let can_move_other = check_move(grid, new_posr, dir, memo);
        memo[pos.0][pos.1] = Some(can_move_other);
        return can_move_other;
    }
    if what == GridCell::BoxR && dir.is_vertical() {
        // Corresponding BoxL
        let posl = Dir4::L.move_point(pos, h, w).unwrap();
        assert_eq!(grid[posl.0][posl.1], GridCell::BoxL);
        let new_posl = dir.move_point(posl, h, w).unwrap();
        let can_move_other = check_move(grid, new_posl, dir, memo);
        memo[pos.0][pos.1] = Some(can_move_other);
        return can_move_other;
    }
    memo[pos.0][pos.1] = Some(true);
    return true;
}

/// Moves whatever is at position pos in direction dir, pushing things on its
/// way. Assumes that everything can be pushed (check with check_move before)
fn apply_move(grid: &mut Vec<Vec<GridCell>>, pos: UPoint, dir: &Dir4) {
    let (w, h) = get_dimensions(&grid);
    let what = grid[pos.0][pos.1].clone();
    if what == GridCell::Empty {
        return;
    }
    if what == GridCell::Wall {
        return;
    }
    let new_pos = dir.move_point(pos, h, w).unwrap();
    apply_move(grid, new_pos, dir);
    grid[new_pos.0][new_pos.1] = grid[pos.0][pos.1].clone();
    grid[pos.0][pos.1] = GridCell::Empty;
    if what == GridCell::BoxL && dir.is_vertical() {
        // Corresponding BoxR
        let posr = Dir4::R.move_point(pos, h, w).unwrap();
        assert_eq!(grid[posr.0][posr.1], GridCell::BoxR);
        let new_posr = dir.move_point(posr, h, w).unwrap();
        apply_move(grid, new_posr, dir);
        grid[new_posr.0][new_posr.1] = grid[posr.0][posr.1].clone();
        grid[posr.0][posr.1] = GridCell::Empty;
    }
    if what == GridCell::BoxR && dir.is_vertical() {
        // Corresponding BoxL
        let posl = Dir4::L.move_point(pos, h, w).unwrap();
        assert_eq!(grid[posl.0][posl.1], GridCell::BoxL);
        let new_posl = dir.move_point(posl, h, w).unwrap();
        apply_move(grid, new_posl, dir);
        grid[new_posl.0][new_posl.1] = grid[posl.0][posl.1].clone();
        grid[posl.0][posl.1] = GridCell::Empty;
    }
}

fn main() {
    println!("Day 15");

    let grid_p = parser!(lines(char_of("#.@O")+));
    let movements_p = parser!(lines(char_of("^>v<")+));
    let p = parser!(section(grid_p) section(movements_p));
    let (input_grid, input_movements) = p.parse(&crustofcode::read_input()).unwrap();
    let movements: Vec<Dir4> = input_movements
        .into_iter()
        .flatten()
        .map(|i| Dir4::from(DIRS[i]))
        .collect();
    let (w, h) = get_dimensions(&input_grid);
    let mut robot: UPoint = (0, 0);
    let mut grid = vec![vec![GridCell::Wall; w]; h];
    for i in 0..h {
        for j in 0..w {
            grid[i][j] = match input_grid[i][j] {
                0 => GridCell::Wall,
                1 => GridCell::Empty,
                2 => {
                    robot = (i, j);
                    GridCell::Robot
                }
                3 => GridCell::Box,
                _ => panic!("Unexpected item in grid"),
            }
        }
    }
    let grid_backup = grid.clone();

    // Part 1
    for movement in &movements {
        // let can_move = try_move_nolr(&mut grid, robot, movement);
        // if can_move {
        //     robot = movement.move_point(robot, h, w).unwrap();
        // }
        let mut memo = vec![vec![None; w]; h];
        let can_move = check_move(&grid, robot, movement, &mut memo);
        if can_move {
            apply_move(&mut grid, robot, movement);
            robot = movement.move_point(robot, h, w).unwrap();
        }
    }
    // visualize_grid_t(&grid, |c| c.visualize());
    let res1: usize = grid
        .iter()
        .enumerate()
        .map(|(i, r)| {
            r.iter()
                .enumerate()
                .filter(|(_, c)| **c == GridCell::Box)
                .map(|(j, _)| 100 * i + j)
                .sum::<usize>()
        })
        .sum();
    println!("{res1}");

    // Part 2
    let mut grid2 = vec![vec![GridCell::Empty; 2 * w]; h];
    for i in 0..h {
        for j in 0..w {
            (grid2[i][2 * j], grid2[i][2 * j + 1]) = match grid_backup[i][j] {
                GridCell::Empty => (GridCell::Empty, GridCell::Empty),
                GridCell::Wall => (GridCell::Wall, GridCell::Wall),
                GridCell::Box => (GridCell::BoxL, GridCell::BoxR),
                GridCell::Robot => {
                    robot = (i, 2 * j);
                    (GridCell::Robot, GridCell::Empty)
                }
                _ => panic!("Unexpected item in grid"),
            }
        }
    }
    for movement in &movements {
        // visualize_grid_t(&grid2, |c| c.visualize());
        // println!("Moving {:?}", movement);
        let mut memo = vec![vec![None; 2 * w]; h];
        let can_move = check_move(&grid2, robot, movement, &mut memo);
        // println!("Can move");
        if can_move {
            apply_move(&mut grid2, robot, movement);
            robot = movement.move_point(robot, h, 2 * w).unwrap();
        }
    }
    // visualize_grid_t(&grid2, |c| c.visualize());
    let res2: usize = grid2
        .iter()
        .enumerate()
        .map(|(i, r)| {
            r.iter()
                .enumerate()
                .filter(|(_, c)| **c == GridCell::BoxL)
                .map(|(j, _)| 100 * i + j)
                .sum::<usize>()
        })
        .sum();
    println!("{res2}");
}
