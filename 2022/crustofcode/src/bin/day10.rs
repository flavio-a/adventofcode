use crustofcode::*;

#[derive(Debug, Clone)]
enum OpKind {
    Add,
    Noop,
}

#[derive(Debug, Clone)]
struct Op {
    kind: OpKind,
    val: i64,
}

fn parse_op(s: String) -> Op {
    match &s[0..4] {
        "addx" => Op {
            kind: OpKind::Add,
            val: str2int(&s[5..]),
        },
        "noop" => Op {
            kind: OpKind::Noop,
            val: 0,
        },
        _ => panic!("AAAAA"),
    }
}

fn maybe_draw(cycle: i64, xval: i64) -> () {
    let idx = (cycle - 1) % 40;
    if xval - 1 <= idx && idx <= xval + 1 {
        print!("█");
    } else {
        print!(" ");
    };
    if cycle % 40 == 0 {
        print!("\n");
    }
}

fn main() {
    println!("Day 10");

    let ops: Vec<Op> = read_input_lines().into_iter().map(parse_op).collect();

    // Part 1
    const CYCLES: [usize; 6] = [20, 60, 100, 140, 180, 220];
    let mut cycle: i64 = 1;
    let mut xval: i64 = 1;
    let mut res = 0;
    for op in ops.iter() {
        let usize_cycle: usize = cycle.try_into().unwrap();
        match op.kind {
            OpKind::Add => {
                if CYCLES.iter().find(|&&x| usize_cycle + 1 == x).is_some() {
                    res += (cycle + 1) * xval;
                    // println!("Xval: {}, strength: {}", xval, (cycle + 1) * xval);
                }
                cycle += 2;
                xval += op.val;
            }
            OpKind::Noop => {
                cycle += 1;
            }
        }
        if CYCLES.iter().find(|&&x| usize_cycle == x).is_some() {
            res += cycle * xval;
            // println!("{}", cycle * xval);
        }
    }
    println!("{}", res);

    // Part 2
    let mut cycle: i64 = 1;
    let mut xval: i64 = 1;
    for op in ops.iter() {
        maybe_draw(cycle, xval);
        match op.kind {
            OpKind::Add => {
                maybe_draw(cycle + 1, xval);
                cycle += 2;
                xval += op.val;
            }
            OpKind::Noop => {
                cycle += 1;
            }
        }
    }
}
