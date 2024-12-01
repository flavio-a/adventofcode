use adventofcrustacean;
use either::*;
use itertools::Itertools;
// use rand::Rng;

type Var = char;
type Operand = Either<Var, i64>;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Op {
    Inp(Var),
    Add(Var, Operand),
    Mul(Var, Operand),
    Div(Var, Operand),
    Mod(Var, Operand),
    Eql(Var, Operand),
}
type Prog = Vec<Op>;
type State = [i64; 4];

fn char2int(c: char) -> i64 {
    c.to_digit(10)
        .expect("Char parsable to number expected")
        .into()
}

fn first_char(s: &str) -> char {
    s.chars().next().unwrap()
}

fn parse_operand(s: &str) -> Operand {
    if first_char(s).is_alphabetic() {
        Left(first_char(s))
    } else {
        Right(adventofcrustacean::str2int(s))
    }
}

fn parse_op(s: &str) -> Op {
    let cusi: Vec<&str> = s.split(" ").collect();
    match cusi[0] {
        "inp" => Op::Inp(first_char(cusi[1])),
        "add" => Op::Add(first_char(cusi[1]), parse_operand(cusi[2])),
        "mul" => Op::Mul(first_char(cusi[1]), parse_operand(cusi[2])),
        "div" => Op::Div(first_char(cusi[1]), parse_operand(cusi[2])),
        "mod" => Op::Mod(first_char(cusi[1]), parse_operand(cusi[2])),
        "eql" => Op::Eql(first_char(cusi[1]), parse_operand(cusi[2])),
        _ => panic!("AAAAA"),
    }
}

fn var2idx(v: Var) -> usize {
    match v {
        'w' => 0,
        'x' => 1,
        'y' => 2,
        'z' => 3,
        _ => panic!("Unexpected variable name {}", v),
    }
}

fn eval_operand(opd: Operand, state: &State) -> i64 {
    match opd {
        Left(v) => state[var2idx(v)],
        Right(i) => i,
    }
}

// Aka eval
fn run_prog(input: Vec<i64>, prog: &Prog) -> State {
    let mut state: State = [0; 4];
    let mut input_idx = 0;
    for op in prog {
        match *op {
            Op::Inp(v) => {
                state[var2idx(v)] = input[input_idx];
                input_idx += 1;
            }
            Op::Add(v, opd) => {
                state[var2idx(v)] += eval_operand(opd, &state);
            }
            Op::Mul(v, opd) => {
                state[var2idx(v)] *= eval_operand(opd, &state);
            }
            Op::Div(v, opd) => {
                if state[var2idx(v)] * eval_operand(opd, &state) < 0 {
                    println!(
                        "Division: {} / {} = {}",
                        state[var2idx(v)],
                        eval_operand(opd, &state),
                        state[var2idx(v)] / eval_operand(opd, &state)
                    );
                }
                state[var2idx(v)] /= eval_operand(opd, &state);
            }
            Op::Mod(v, opd) => {
                state[var2idx(v)] %= eval_operand(opd, &state);
            }
            Op::Eql(v, opd) => {
                state[var2idx(v)] = if state[var2idx(v)] == eval_operand(opd, &state) {
                    1
                } else {
                    0
                };
            }
        }
    }
    // println!("Final state: {:?}", state);
    return state;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let prog: Prog = content.lines().map(parse_op).collect();

    // // Testing assertions on random inputs
    // let mut rng = rand::thread_rng();
    // loop {
    //     let input: Vec<i64> = (0..14).map(|_| rng.gen_range(1..=9)).collect();
    //     run_progplus(input, &prog);
    // }

    // Part 1
    for val in (0..99999999999999_i64).rev() {
        let input: Vec<i64> = val.to_string().chars().map(char2int).collect();
        if val % 10000000 == 0 {
            println!("{}", val);
        }
        if input.iter().any(|&c| c == 0) {
            continue;
        }
        if run_prog(input, &prog)[var2idx('z')] == 0 {
            println!("Part 1: {}", val);
            break;
        }
    }

    // Part 2

    // A general solution is probably something that stores how many times z is
    // multiplied and divided by 26, and collect constraints using that, but
    // it's too annoying and I've already have a star, so we have this thing
    // made for my input
    for ((w3, w4), w6) in (1..=9).cartesian_product(1..=9).cartesian_product(1..=9) {
        let mut input: Vec<i64> = "45000091516111".to_string().chars().map(char2int).collect();
        input[3 - 1] = w3;
        input[4 - 1] = w4;
        input[5 - 1] = w4;
        input[6 - 1] = w6;
        let val = input.iter().fold(0, |acc, &v| 10 * acc + v);
        if run_prog(input, &prog)[var2idx('z')] == 0 {
            println!("Part 2: {:?}", val);
            break;
        }
    }
}
