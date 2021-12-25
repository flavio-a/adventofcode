use adventofcrustacean;
// use itertools::Itertools;

const SIZE: usize = 10;
type Row = Vec<u8>;
type Board = Vec<Row>;

fn char2int(c: char) -> u8 {
    c.to_digit(10).expect("Char parsable to number expected").try_into().unwrap()
}

fn incr_neighs(board: &mut Board, x: usize, y: usize) {
    // 10 means increased but not yet increased neighbours
    if board[x][y] == 10 {
        board[x][y] += 1; // This prevents infinite recursion
        for x1 in x.saturating_sub(1)..=(x + 1) {
            // with saturating_add, values lower than 0 are saturated to 0
            if x1 < SIZE {
                for y1 in y.saturating_sub(1)..=(y + 1) {
                    if y1 < SIZE {
                        if board[x1][y1] <= 9 {
                            board[x1][y1] += 1;
                            incr_neighs(board, x1, y1);
                        }
                    }
                }
            }
        }
    }
}

// Returns a pair (new_board, number of flashes)
fn step(board: Board) -> (Board, u32) {
    let mut incr_board = board.into_iter().map(|r| r.into_iter().map(|v| v + 1).collect::<Row>()).collect::<Board>();
    for x in 0..SIZE {
        for y in 0..SIZE {
            incr_neighs(&mut incr_board, x, y);
        }
    }
    let mut flashes: u32 = 0;
    let new_board = incr_board.iter().map(|r| r.iter().map(|&v| if v > 9 { flashes += 1; 0 } else { v }).collect::<Row>()).collect::<Board>();
    return (new_board, flashes);
}

#[allow(dead_code)]
fn print_board(b: &Board) {
    for r in b.iter() {
        for &v in r.iter() {
            print!("{}", v);
        }
        println!("");
    }
}

fn all_together(b: &Board) -> bool {
    b.iter().all(|r| r.iter().all(|&v| v == 0))
}

fn main() {
    let content = adventofcrustacean::read_input();
    let init_board = content.lines().map(|s| s.chars().map(char2int).collect::<Row>()).collect::<Board>();

    // Part 1
    let mut tot_flashes = 0;
    let mut final_board = init_board;
    for _ in 1..=100 {
        let a = step(final_board);
        final_board = a.0;
        tot_flashes += a.1;
    }
    println!("{}", tot_flashes);
    // print_board(&final_board);

    // Part 2
    let mut step_num = 100;
    while !all_together(&final_board) {
        let (tmp, _) = step(final_board);
        final_board = tmp;
        step_num += 1;
    }
    println!("{}", step_num);
}