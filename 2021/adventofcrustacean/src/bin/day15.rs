extern crate pathfinding;

use adventofcrustacean;
use pathfinding::dijkstra;
use pathfinding::astar;

// Commented measurement code
// I wanted to make sure that get_weight didn't take up too much time (since
// it's called quite a few times), and I also wanted to try and do something
// like this in Rust at least once in my life.
// use std::time::Instant;
// static mut usec: u128 = 0;

type Row = Vec<u8>;
type Board = Vec<Row>;

fn char2int(c: char) -> u8 {
    c.to_digit(10).expect("Char parsable to number expected").try_into().unwrap()
}

fn neighs1(board: &Board, x: usize, y: usize) -> Vec<((usize, usize), u32)> {
    let mut res = vec![];
    for &x1 in &[x.saturating_sub(1), x + 1] {
        if x1 < board.len() {
            res.push(((x1, y), board[x1][y].into()));
        }
    }
    for &y1 in &[y.saturating_sub(1), y + 1] {
        if y1 < board[0].len() {
            res.push(((x, y1), board[x][y1].into()));
        }
    }
    return res;
}

fn get_weight(board: &Board, x: usize, y: usize) -> u32 {
    let basex = board.len();
    let basey = board[0].len();
    let incrx: u32 = (x / basex).try_into().unwrap();
    let incry: u32 = (y / basey).try_into().unwrap();
    let w = u32::from(board[x % basex][y % basey]) + incrx + incry;
    return ((w - 1) % 9) + 1;
}

fn neighs2(board: &Board, x: usize, y: usize) -> Vec<((usize, usize), u32)> {
    let mut res = vec![];
    let maxx = board.len() * 5;
    let maxy = board[0].len() * 5;
    for &x1 in &[x.saturating_sub(1), x + 1] {
        if x1 < maxx {
            // unsafe {
            // let start = Instant::now();
            // let w = get_weight(board, x1, y);
            // usec += start.elapsed().as_micros();
            // res.push(((x1, y), w));
            // }
            res.push(((x1, y), get_weight(board, x1, y)));
        }
    }
    for &y1 in &[y.saturating_sub(1), y + 1] {
        if y1 < maxy {
            // unsafe {
            // let start = Instant::now();
            // let w = get_weight(board, x, y1);
            // usec += start.elapsed().as_micros();
            // res.push(((x, y1), w));
            // }
            res.push(((x, y1), get_weight(board, x, y1)));
        }
    }
    return res;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let board = content.lines().map(|s| s.chars().map(char2int).collect::<Row>()).collect::<Board>();

    // Part 1
    let maxx = board.len() - 1;
    let maxy = board[0].len() - 1;
    let path1 = dijkstra(&(0, 0),
                         |&(x, y)| neighs1(&board, x, y),
                         |&(x, y)| x == maxx && y == maxy
                );
    println!("{}", path1.unwrap().1);

    // Part 2
    let maxx = board.len() * 5 - 1;
    let maxy = board[0].len() * 5 - 1;
    let path2 = astar(&(0, 0),
                      |&(x, y)| neighs2(&board, x, y),
                      // Manhattan distance as heuristic
                      |&(x, y)| ((maxx - x) + (maxy - y)).try_into().unwrap(),
                      |&(x, y)| x == maxx && y == maxy
                );
    println!("{}", path2.unwrap().1);
    // unsafe{
    // println!("usec: {}", usec);
    // }
}