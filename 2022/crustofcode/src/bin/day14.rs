use crustofcode::*;
use std::cmp::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Cell {
    Rock,
    Sand,
}

impl Cell {
    fn to_char(&self) -> char {
        match self {
            // Cell::Air => ' ',
            Cell::Rock => '#',
            Cell::Sand => 'o',
        }
    }
}

impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_char())
    }
}

type Grid = HashMap<Point, Cell>;

#[allow(dead_code)]
fn show_grid(grid: &Grid) -> () {
    visualize_as(
        grid.iter()
            .map(|((x, y), c)| ((x - 400, *y), (*c).to_char())),
    );
    println!("");
}

fn parse_pair(pair: &str) -> Point {
    let mut a = pair.split(',').map(str2int);
    return (a.next().unwrap(), a.next().unwrap());
}

fn add_points(grid: &mut Grid, &(oldx, oldy): &Point, &(x, y): &Point) -> () {
    for i in min(oldx, x)..=max(oldx, x) {
        for j in min(oldy, y)..=max(oldy, y) {
            grid.insert((i, j), Cell::Rock);
        }
    }
}

// This assumes the moving sand is not in the grid yet
fn move_sand(grid: &Grid, (x, y): Point) -> Option<Point> {
    // Arbitrary cut-off, should be the biggest y in the grid
    if y > 200 {
        return None;
    };
    match grid.get(&(x, y + 1)) {
        None => move_sand(grid, (x, y + 1)),
        Some(Cell::Rock) | Some(Cell::Sand) => match grid.get(&(x - 1, y + 1)) {
            None => move_sand(grid, (x - 1, y + 1)),
            Some(Cell::Rock) | Some(Cell::Sand) => match grid.get(&(x + 1, y + 1)) {
                None => move_sand(grid, (x + 1, y + 1)),
                Some(Cell::Rock) | Some(Cell::Sand) => Some((x, y)),
            },
        },
    }
}

fn pour_sand(grid: &mut Grid) -> i64 {
    let mut new_sand = move_sand(&grid, (500, 0));
    let mut res = 0;
    while new_sand.is_some() && grid.get(&(500, 0)).is_none() {
        let pos = new_sand.unwrap();
        grid.insert(pos, Cell::Sand);
        new_sand = move_sand(&grid, (500, 0));
        res += 1;
    }
    return res;
}

fn main() {
    println!("Day 14");

    let mut grid = HashMap::new();
    for line in read_input_lines() {
        let mut pairs = line.split(" -> ");
        let mut oldp = parse_pair(pairs.next().unwrap());
        for pair in pairs {
            let p = parse_pair(pair);
            add_points(&mut grid, &oldp, &p);
            oldp = p;
        }
    }

    // show_grid(&grid);

    // Part 1
    let p1 = pour_sand(&mut grid);
    println!("{}", p1);

    // Part 2
    let floor: i64 = grid.keys().map(|(_, y)| y).max().unwrap() + 2;
    // 10 for safety
    let minx: i64 = 500 - floor - 10;
    let maxx: i64 = 500 + floor + 10;
    add_points(&mut grid, &(minx, floor), &(maxx, floor));
    println!("{}", p1 + pour_sand(&mut grid));
    show_grid(&grid);
}
