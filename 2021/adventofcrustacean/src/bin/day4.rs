use adventofcrustacean;
use itertools::Itertools;

type Board = [(i64, bool); 25];

fn parse_board<'a, I>(lines: I) -> Board
where
    I: Iterator<Item = &'a str>,
{
    let mut newboard: Board = [(0, false); 25];
    for (x, line) in lines.map(|s| s.trim()).filter(|s| !s.is_empty()).enumerate() {
        for (y, val) in line.split_whitespace().enumerate() {
            newboard[x * 5 + y] = (adventofcrustacean::str2int(val), false);
        }
    }
    newboard
}

fn update_board(board: Board, num: i64) -> Board {
    // Can't collect back into an array
    // board.iter().map(|&(val, found)| (val, if val == num { true } else { found })).collect()

    let mut newboard = [(0, false); 25];
    for (i, &(val, found)) in board.iter().enumerate() {
        newboard[i] = (val, if val == num { true } else { found });
    }
    newboard
}

// Returns the product of unchecked numbers, not the actual result
fn check_board(board: &Board) -> i64 {
    let mut found = false;
    // Check rows
    for x in 0..5 {
        let mut thisrow = true;
        for y in 0..5 {
            if !board[x * 5 + y].1 {
                thisrow = false;
            }
        }
        if thisrow {
            found = true;
        }
    }
    // Check cols
    for y in 0..5 {
        let mut thiscol = true;
        for x in 0..5 {
            if !board[x * 5 + y].1 {
                thiscol = false;
            }
        }
        if thiscol {
            found = true;
        }
    }

    if !found {
        0
    }
    else {
        let mut a = 0;
        for &(val, found1) in board.iter() {
            a += if found1 { 0 } else { val }
        }
        a
    }
}

fn main() {
    let content = adventofcrustacean::read_input();
    let mut lines = content.lines();

    let draws: Vec<i64> = lines.next().unwrap().split(",").map(adventofcrustacean::str2int).collect();
    // println!("{:?}", draws);

    // This is an iterator at any time
    let mut boards: Vec<Board> = lines.chunks(6).into_iter().map(parse_board).collect();
    // println!("{:?}", boards);

    // Part 1
    for &drawn in draws.iter() {
        boards = boards.iter().map(|&b| update_board(b, drawn)).collect();
        let winner = boards.iter().map(check_board).find(|&v| v > 0);
        if winner.is_some() {
            let res1 = winner.unwrap() * drawn;
            println!("{}", res1);
            break;
        }
    }

    // Part 2
    // Goes on from the boards already updated
    for &drawn in draws.iter() { // I should do better than restart, but anyway
        // Check for last one before because otherwise it gets filtered out
        if boards.len() == 1 {
            let cusu = check_board(&update_board(boards[0], drawn));
            if cusu > 0 {
                println!("{}", cusu * drawn);
            }
        }
        boards = boards.iter().map(|&b| update_board(b, drawn)) // Update the board
                              .filter(|b| check_board(b) == 0) // Filter out winning boards
                              .collect();
    }
}