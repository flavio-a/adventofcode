use aoc_parse::{parser, prelude::*};

type Registers = [i64; 3];
type ProgramState = (Registers, usize);

fn eval_combo_operand(op: u8, registers: &Registers) -> i64 {
    if op <= 3 {
        return op.into();
    }
    if 4 <= op && op <= 6 {
        return registers[usize::from(op - 4)];
    }
    panic!("Unexpected combo operand {}", op);
}

fn exec_one(
    program: &Vec<u8>,
    (mut registers, mut pc): ProgramState,
) -> (Option<ProgramState>, Option<u8>) {
    if pc >= program.len() {
        return (None, None);
    }
    let mut output = None;
    match program[pc] {
        0 => {
            let combo_op = eval_combo_operand(program[pc + 1], &registers);
            registers[0] = registers[0] / 2_i64.pow(combo_op.try_into().unwrap());
            pc += 2;
        }
        1 => {
            let lit_op = program[pc + 1];
            registers[1] = registers[1] ^ i64::from(lit_op);
            pc += 2;
        }
        2 => {
            let combo_op = eval_combo_operand(program[pc + 1], &registers);
            registers[1] = combo_op % 8;
            pc += 2;
        }
        3 => {
            let lit_op = program[pc + 1];
            if registers[0] != 0 {
                pc = lit_op.into()
            } else {
                pc += 2
            }
        }
        4 => {
            registers[1] = registers[1] ^ registers[2];
            pc += 2;
        }
        5 => {
            let combo_op = eval_combo_operand(program[pc + 1], &registers);
            output = Some(u8::try_from(combo_op % 8).unwrap());
            pc += 2;
        }
        6 => {
            let combo_op = eval_combo_operand(program[pc + 1], &registers);
            registers[1] = registers[0] / 2_i64.pow(combo_op.try_into().unwrap());
            pc += 2;
        }
        7 => {
            let combo_op = eval_combo_operand(program[pc + 1], &registers);
            registers[2] = registers[0] / 2_i64.pow(combo_op.try_into().unwrap());
            pc += 2;
        }
        _ => panic!("Unexpected op {} (pc {pc})", program[pc]),
    }
    return (Some((registers, pc)), output);
}

fn exec_all(program: &Vec<u8>, initial_state: ProgramState) -> (ProgramState, Vec<u8>) {
    let mut prog_state = Some(initial_state);
    let mut old_state = prog_state.unwrap();
    let mut outputs = vec![];
    while prog_state.is_some() {
        old_state = prog_state.unwrap();
        let (new_state, output) = exec_one(&program, prog_state.unwrap());
        prog_state = new_state;
        if output.is_some() {
            outputs.push(output.unwrap());
        }
    }
    return (old_state, outputs);
}

fn print_output(output: Vec<u8>) -> String {
    itertools::Itertools::intersperse(
        output
            .into_iter()
            .map(|v| char::from_digit(v.into(), 10).unwrap()),
        ',',
    )
    .collect::<String>()
}

fn find_three_bits(
    program: &Vec<u8>,
    initial_state: ProgramState,
    a: i64,
    output: u8,
    first: u8,
) -> Option<u8> {
    // To find three bits, tries all the possible initial three bits
    let mut state = initial_state.clone();
    for tail in first..8_u8 {
        state.0[0] = a * 8 + i64::from(tail);
        // println!("  {tail} - state: {}", state.0[0]);
        let (_, outputs) = exec_all(program, state.clone());
        // println!("  output: {outputs:?}");
        assert!(outputs.len() >= 1);
        if outputs[0] == output {
            return Some(tail);
        }
    }
    return None;
}

fn from_vec(a: &Vec<u8>) -> i64 {
    a.iter().fold(0, |acc, v| acc * 8 + i64::from(*v))
}

fn search_a(program: &Vec<u8>, initial_state: ProgramState, a: Vec<u8>) -> Option<Vec<u8>> {
    if a.len() == program.len() {
        return Some(a);
    }
    let idx = a.len();
    let c = program[program.len() - 1 - idx];
    // println!("a: {a:?}, idx: {idx}, c: {c}");
    let mut first = 0;
    loop {
        let mayb = find_three_bits(program, initial_state, from_vec(&a), c, first);
        if mayb.is_none() {
            return None;
        }
        let mut aprime = a.clone();
        aprime.push(mayb.unwrap());
        let res = search_a(program, initial_state, aprime);
        if res.is_some() {
            return res;
        }
        first = mayb.unwrap() + 1;
    }
}

fn main() {
    println!("Day 17");

    let registers_p = parser!({ra:line("Register A: " i64) rb:line("Register B: " i64) rc:line("Register C: " i64) => [ra, rb, rc]});
    let program_p = parser!(line("Program: " repeat_sep(u8, ",")));
    let p = parser!(section(registers_p) section(program_p));
    let (initial_registers, program) = p.parse(&crustofcode::read_input()).unwrap();
    let initial_state = (initial_registers, 0);

    // Part 1
    let (_, outputs) = exec_all(&program, initial_state.clone());
    println!("{}", print_output(outputs));

    // Part 2

    // This is based on a bunch of assumptions that we don't check (they hold
    // in my input :))
    let a = search_a(&program, initial_state, vec![]);
    println!("{}", from_vec(&a.unwrap()));
}
