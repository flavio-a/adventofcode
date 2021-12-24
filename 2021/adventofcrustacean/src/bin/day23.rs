use adventofcrustacean;
use std::collections::HashMap;

const HALL_LEN: usize = ("#...........#").len() - 2;

type Row = Vec<char>;
type Hall = [char; HALL_LEN];
type Energy = u32;
type Coord = usize;
type Pos = (Coord, Coord);

fn energy_factor(c: &char) -> Energy {
    match c {
        'A' => 1,
        'B' => 10,
        'C' => 100,
        'D' => 1000,
        _ => panic!("Requested energy factor of unexpected char: {}", c),
    }
}

// row 0 is the hall, others are rows[row - 1]
fn get_cell(hall: &Hall, rows: &Vec<Row>, pos: Pos) -> char {
    if pos.0 == 0 {
        hall[pos.1]
    }
    else {
        rows[pos.0 - 1][pos.1]
    }
}

// Returns None if it can't perform the move, otherwise the energy spent
fn move_amph(hall: &Hall, rows: &Vec<Row>, start: Pos, end: Pos) -> Option<Energy> {
    // If the end position is in the hall above one of the rooms returns None
    // (because an amph can't stop there)
    if end.0 == 0 && (end.1 == 2 || end.1 == 4 || end.1 == 6 || end.1 == 8) {
        return None
    }
    // To move from start to end, the amphipod has to move up, then left/right
    // and then down (any of this step is possibly missing)
    let mut steps: u32 = 0;
    let energy_factor: Energy = energy_factor(&get_cell(hall, rows, start));
    // Moving up
    // Only need to do it if pos.0 > 0
    for r in (1..start.0).rev() {
        // println!("Moving up: {:?}", (r, start.1));
        if get_cell(hall, rows, (r, start.1)) != ' ' {
            return None;
        }
        steps += 1;
    }
    // Moving horizontally
    let mut hallstartcol = if start.0 == 0 { start.1 } else { 2 + start.1 * 2 };
    let mut hallendcol = if end.0 == 0 { end.1 } else { 2 + end.1 * 2 };
    if hallstartcol <= hallendcol {
        hallstartcol = if start.0 == 0 { hallstartcol + 1 } else { hallstartcol };
    }
    else {
        let tmp = if start.0 == 0 { hallstartcol - 1 } else { hallstartcol };
        hallstartcol = hallendcol;
        hallendcol = tmp;
    }
    for c in hallstartcol..=hallendcol {
        // println!("Moving horiz: {:?}", (0, c));
        if get_cell(hall, rows, (0, c)) != ' ' {
            return None;
        }
        steps += 1;
    }
    // Moving down
    for r in 1..=end.0 {
        // println!("Moving down: {:?}", (r, end.1));
        if get_cell(hall, rows, (r, end.1)) != ' ' {
            return None;
        }
        steps += 1;
    }
    return Some(steps * energy_factor);
}

// fn print_conf(hall: &Hall, rows: &Vec<Row>) {
//     println!("#############");
//     print!("#");
//     for c in hall.iter() { print!("{}", c); }
//     println!("#");
//     for row in rows.iter() {
//         print!("  #");
//         for c in row.iter() { print!("{}#", c); }
//         println!("");
//     }
// }

fn col_amph(col: Coord) -> char {
    match col {
        0 => 'A',
        1 => 'B',
        2 => 'C',
        3 => 'D',
        _ => panic!("Unexpected column number: {}", col),
    }
}

fn amph_col(a: char) -> Coord {
    match a {
        'A' => 0,
        'B' => 1,
        'C' => 2,
        'D' => 3,
        _ => panic!("Unexpected amphipod symbol: {}", a),
    }
}

// Checks whether the target column of a already contains only as, and hence we
// can try to move a to its final destination. Return the position to move it
// to or None if the column isn't ready
// TODO: should check whether the column is already full of the right char?
fn can_move_to_target(hall: &Hall, rows: &Vec<Row>, a: char) -> Option<Pos> {
    let mut bottom: Coord = rows.len();
    let col: Coord = amph_col(a);
    // Checks the bottom
    while bottom > 0 && get_cell(hall, rows, (bottom, col)) == a { bottom -= 1 };
    let mut top = bottom;
    while top > 0 && get_cell(hall, rows, (top, col)) == ' ' { top -= 1 };
    if top > 0 {
        return None;
    }
    else {
        return Some((bottom, col));
    }
}

// Checks if the last elements of the column are already right
fn bottom_col_ok(hall: &Hall, rows: &Vec<Row>, col: Coord) -> bool {
    let mut bottom: Coord = rows.len();
    let a: char = col_amph(col);
    while bottom > 0 && get_cell(hall, rows, (bottom, col)) == a { bottom -= 1 };
    return bottom == 0 || get_cell(hall, rows, (bottom, col)) == ' ';
}

// Given a position in the rooms, move the amph in that position out, unless it
// is already in its final position
fn move_out(hall: Hall, mut rows: Vec<Row>, pos: Pos) -> Vec<(Hall, Vec<Row>, Energy)> {
    assert!(pos.0 > 0);
    let amph: char = get_cell(&hall, &rows, pos);
    let target = can_move_to_target(&hall, &rows, amph);
    if target.is_some() {
        if pos.1 == amph_col(amph) {
            // Here can move to final target and is already in the right column, hence shouldn't be moved
            return vec![];
        }
        let target: Pos = target.unwrap();
        let energy_spent = move_amph(&hall, &rows, pos, target);
        if energy_spent.is_some() {
            rows[pos.0 - 1][pos.1] = ' ';
            rows[target.0 - 1][target.1] = amph;
            return vec![(hall, rows, energy_spent.unwrap())];
        }
    }
    // Can't move to target, send to all possible positions in hallway
    let mut res = vec![];
    // This is a filter_map
    for endcol in 0..HALL_LEN {
        let energy_spent = move_amph(&hall, &rows, pos, (0, endcol));
        if energy_spent.is_some() {
            let mut rows1 = rows.clone();
            rows1[pos.0 - 1][pos.1] = ' ';
            let mut hall1 = hall.clone();
            hall1[endcol] = amph;
            res.push((hall1, rows1, energy_spent.unwrap()));
        }
    }
    return res;
}

// Empties a column in all possible ways, leaving any amphipod already in the
// right place untouched
fn empty_col(hall: Hall, rows: Vec<Row>, col: Coord) -> Vec<(Hall, Vec<Row>, Energy)> {
    let mut bottom: Coord = rows.len();
    // Leaves any amphipod already in the righ column where it is
    while bottom > 0 && get_cell(&hall, &rows, (bottom, col)) == col_amph(col) { bottom -= 1 };
    let mut top: Coord = 1;
    while top <= bottom && get_cell(&hall, &rows, (top, col)) == ' ' { top += 1 };
    if top > bottom {
        return vec![(hall, rows, 0)];
    }
    let mut res = vec![];
    for r in top..=bottom {
        // let moving: Pos = (r, col);
        let one_move = move_out(hall, rows.clone(), (r, col));
        for (h, rs, en) in one_move.into_iter() {
            for (h, rs, en1) in empty_col(h, rs, col).into_iter() {
                res.push((h, rs, en1 + en));
            }
        }
    }
    return res;
}

fn move_hallway_to_target(mut hall: Hall, mut rows: Vec<Row>) -> (Hall, Vec<Row>, Energy) {
    for c in 0..HALL_LEN {
        if get_cell(&hall, &rows, (0, c)) != ' ' {
            let pos: Pos = (0, c);
            let amph: char = get_cell(&hall, &rows, pos);
            let target = can_move_to_target(&hall, &rows, amph);
            if target.is_some() {
                let target = target.unwrap();
                let energy_spent = move_amph(&hall, &rows, pos, target);
                if energy_spent.is_some() {
                    hall[pos.1] = ' ';
                    rows[target.0 - 1][target.1] = amph;
                    let (h, rs, en) = move_hallway_to_target(hall, rows);
                    return (h, rs, en + energy_spent.unwrap());
                }
            }
        }
    }
    return (hall, rows, 0);
}

fn move_one_from_col(hall: Hall, rows: Vec<Row>) -> Vec<(Hall, Vec<Row>, Energy)> {
    // Picks a column nondeterministically
    let mut res = vec![];
    for col in 0..=3 {
        // Doesn't move guys out if they're already in the right column
        if !bottom_col_ok(&hall, &rows, col) {
            let mut top = 1;
            while top <= rows.len() && get_cell(&hall, &rows, (top, col)) == ' ' { top += 1 };
            if top <= rows.len() {
                // Probably this could be optimized checking first if it can be moved to the target
                res.extend(move_out(hall.clone(), rows.clone(), (top, col)));
            }
        }
    }
    return res;
}

fn is_finished(hall: &Hall, rows: &Vec<Row>) -> bool {
    // Hall is empty
    if !hall.iter().all(|&c| c == ' ') {
        return false;
    }
    // The first row is ok
    if rows[0].iter().enumerate().any(|(i, &c)| c != col_amph(i)) {
        return false;
    }
    // All other rows are equal to the first
    for row in rows[1..].iter() {
        if row.iter().enumerate().any(|(i, &c)| c != rows[0][i]) {
            return false;
        }
    }
    return true;
}

// Remove duplicates, keeping only the least energy required for each config
fn dedup_confs(confs: Vec<(Hall, Vec<Row>, Energy)>) -> Vec<(Hall, Vec<Row>, Energy)> {
    let mut map = HashMap::new();
    for (h, rs, en) in confs.into_iter() {
        let cusu = (h, rs);
        let tmp = map.get(&cusu);
        if tmp.is_none() || *tmp.unwrap() > en {
            map.insert(cusu, en);
        }
    }
    let mut res = vec![];
    for ((h, rs), v) in map.into_iter() {
        res.push((h, rs, v));
    }
    return res;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let mut lines = content.lines();
    assert_eq!(lines.next(), Some("#############"));
    assert_eq!(lines.next(), Some("#...........#"));
    let row1: Row = lines.next().unwrap().chars().enumerate().filter(|&(i, _)| i == 3 || i == 5 || i == 7 || i == 9).map(|(_, c)| c).collect();
    let row2: Row = lines.next().unwrap().chars().enumerate().filter(|&(i, _)| i == 3 || i == 5 || i == 7 || i == 9).map(|(_, c)| c).collect();
    assert_eq!(lines.next(), Some("  #########"));
    let empty_hall: Hall = [' '; HALL_LEN];

    // Part 1

    // First, empties the column of the Ds
    // Actually this works for the p1 but is not the solution for the p2
    let mut tmp = empty_col(empty_hall.clone(), vec![row1.clone(), row2.clone()], 3);
    tmp = tmp.into_iter().map(|(h, rs, en)| {let (h, rs, en1) = move_hallway_to_target(h, rs); (h, rs, en1 + en)}).collect();

    // Now iterate the following procedure: tries to move to target all
    // elements in the hallway, then pick nondeterministically a column and
    // moves out its top element.
    while tmp.len() > 0 {
        let mut new_tmp = vec![];
        for (h, rs, en) in tmp.into_iter() {
            new_tmp.extend(move_one_from_col(h, rs).into_iter().map(|(h, rs, en1)| { let (h, rs, en2) = move_hallway_to_target(h, rs); (h, rs, en2 + en1 + en) }));
        }
        tmp = vec![];
        for conf in dedup_confs(new_tmp).into_iter() {
            // conf == (h, rs, en)
            if is_finished(&conf.0, &conf.1) {
                println!("Part 1: {}", conf.2);
            }
            else {
                tmp.push(conf);
            }
        }
    }

    // Part 2
    let row4 = row2;
    let row2 = vec!['D', 'C', 'B', 'A'];
    let row3 = vec!['D', 'B', 'A', 'C'];

    let mut tmp = vec![(empty_hall, vec![row1, row2, row3, row4], 0)];
    while tmp.len() > 0 {
        let mut new_tmp = vec![];
        for (h, rs, en) in tmp.into_iter() {
            new_tmp.extend(move_one_from_col(h, rs).into_iter().map(|(h, rs, en1)| { let (h, rs, en2) = move_hallway_to_target(h, rs); (h, rs, en2 + en1 + en) }));
        }
        tmp = vec![];
        for conf in dedup_confs(new_tmp).into_iter() {
            // conf == (h, rs, en)
            if is_finished(&conf.0, &conf.1) {
                println!("Part 2: {}", conf.2);
            }
            else {
                tmp.push(conf);
            }
        }
    }
}
