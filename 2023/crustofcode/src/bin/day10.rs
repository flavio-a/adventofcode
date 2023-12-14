use aoc_parse::{parser, prelude::*};

type Node = (usize, usize);

fn idx2char(c: usize) -> char {
    match c {
        0 => '.',
        1 => '|',
        2 => '-',
        3 => 'L',
        4 => 'J',
        5 => '7',
        6 => 'F',
        7 => 'S',
        _ => panic!("Unexpected plot twist"),
    }
}

fn successor(curr: Node, prev: Node, grid: &Vec<Vec<char>>) -> Node {
    let (i, j) = curr;
    let c = grid[i][j];
    let (pi, pj) = prev;

    match c {
        '.' => panic!("Reached empty tile"),
        '|' => {
            assert_eq!(j, pj);
            if pi == i + 1 {
                (i - 1, j)
            } else if pi == i - 1 {
                (i + 1, j)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        '-' => {
            assert_eq!(i, pi);
            if pj == j + 1 {
                (i, j - 1)
            } else if pj == j - 1 {
                (i, j + 1)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        'L' => {
            if pi == i {
                assert_eq!(pj, j + 1);
                (i - 1, j)
            } else if pj == j {
                assert_eq!(pi, i - 1);
                (i, j + 1)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        'J' => {
            if pi == i {
                assert_eq!(pj, j - 1);
                (i - 1, j)
            } else if pj == j {
                assert_eq!(pi, i - 1);
                (i, j - 1)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        '7' => {
            if pi == i {
                assert_eq!(pj, j - 1);
                (i + 1, j)
            } else if pj == j {
                assert_eq!(pi, i + 1);
                (i, j - 1)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        'F' => {
            if pi == i {
                assert_eq!(pj, j + 1);
                (i + 1, j)
            } else if pj == j {
                assert_eq!(pi, i + 1);
                (i, j + 1)
            } else {
                panic!("Previous node in wrong position")
            }
        }
        'S' => panic!("Successor of starting tile"),
        _ => panic!("Unexpected plot twist"),
    }
}

fn connect_above(grid: &Vec<Vec<char>>, i: usize, j: usize) -> bool {
    i > 0 && ['|', 'F', '7'].contains(&grid[i - 1][j])
}
fn connect_below(grid: &Vec<Vec<char>>, i: usize, j: usize) -> bool {
    i + 1 < grid.len() && ['|', 'L', 'J'].contains(&grid[i + 1][j])
}
fn connect_left(grid: &Vec<Vec<char>>, i: usize, j: usize) -> bool {
    j > 0 && ['-', 'L', 'F'].contains(&grid[i][j - 1])
}
fn connect_right(grid: &Vec<Vec<char>>, i: usize, j: usize) -> bool {
    j + 1 < grid[0].len() && ['-', 'J', '7'].contains(&grid[i][j + 1])
}

fn get_start_replacement(grid: &Vec<Vec<char>>) -> (Node, Node, char) {
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if grid[i][j] == 'S' {
                if connect_above(grid, i, j) {
                    if connect_below(grid, i, j) {
                        return ((i, j), (i - 1, j), '|');
                    } else if connect_left(grid, i, j) {
                        return ((i, j), (i - 1, j), 'J');
                    } else if connect_right(grid, i, j) {
                        return ((i, j), (i - 1, j), 'L');
                    }
                    panic!("Only connected above");
                }
                if connect_left(grid, i, j) {
                    if connect_below(grid, i, j) {
                        return ((i, j), (i, j - 1), '7');
                    } else if connect_above(grid, i, j) {
                        return ((i, j), (i, j - 1), 'J');
                    } else if connect_right(grid, i, j) {
                        return ((i, j), (i, j - 1), '-');
                    }
                    panic!("Only connected left");
                }
                if connect_below(grid, i, j) {
                    if connect_above(grid, i, j) {
                        return ((i, j), (i + 1, j), '|');
                    } else if connect_left(grid, i, j) {
                        return ((i, j), (i + 1, j), '7');
                    } else if connect_right(grid, i, j) {
                        return ((i, j), (i + 1, j), 'F');
                    }
                    panic!("Only connected below");
                }
                if connect_right(grid, i, j) {
                    panic!("Already matched before");
                    // if connect_below(grid, i, j) {
                    //     return ((i, j), (i, j + 1), '7');
                    // } else if connect_above(grid, i, j) {
                    //     return ((i, j), (i, j + 1), 'J');
                    // } else if connect_left(grid, i, j) {
                    //     return ((i, j), (i, j + 1), '-');
                    // }
                    // panic!("Only connected right");
                }
            }
        }
    }
    panic!("Start not found");
}

fn main() {
    println!("Day 10");

    let tile_p = parser!(c:char_of(".|-LJ7FS") => idx2char(c));
    let p = parser!(lines(tile_p+));
    let mut grid: Vec<Vec<char>> = p.parse(&crustofcode::read_input()).unwrap();
    let (start, start_next, repl) = get_start_replacement(&grid);
    grid[start.0][start.1] = repl;
    let grid = grid;

    // Part 1
    {
        let (mut prev, mut curr) = (start, start_next);
        let mut l = 1;
        while curr != start {
            let tmp = successor(curr, prev, &grid);
            prev = curr;
            curr = tmp;
            l += 1;
        }
        assert_eq!(l % 2, 0);
        println!("{}", l / 2);
    }

    // Part 2
    let (mut prev, mut curr) = (start, start_next);
    let mut edges = vec![curr];
    while curr != start {
        let tmp = successor(curr, prev, &grid);
        prev = curr;
        curr = tmp;
        edges.push(curr);
    }
    edges.sort();
    let edges = edges;
    // Start counting
    let mut inner = 0;
    let mut is_in = false;
    let mut last_bend = None;
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if edges.binary_search(&(i, j)).is_ok() {
                if grid[i][j] == '|' {
                    assert!(last_bend.is_none());
                    is_in = !is_in;
                } else if grid[i][j] == '-' {
                    assert!(last_bend.is_some());
                } else if ['L', 'F'].contains(&grid[i][j]) {
                    assert!(last_bend.is_none());
                    last_bend = Some(grid[i][j]);
                } else if ['J', '7'].contains(&grid[i][j]) {
                    assert!(last_bend.is_some());
                    if (grid[i][j] == 'J' && last_bend == Some('F'))
                        || (grid[i][j] == '7' && last_bend == Some('L'))
                    {
                        is_in = !is_in;
                    }
                    last_bend = None;
                }
            } else if is_in {
                inner += 1;
            }
        }
        assert!(!is_in);
    }
    println!("{inner}");
}
