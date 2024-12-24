use std::collections::{HashMap, VecDeque};

use aoc_parse::{parser, prelude::*};
use crustofcode::option_assert;
use itertools::Itertools;

type WireLabel = [char; 3];

fn label_to_string(label: &WireLabel) -> String {
    label.iter().collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BitOp {
    AND,
    OR,
    XOR,
}

impl BitOp {
    fn eval(&self, inl: bool, inr: bool) -> bool {
        match self {
            BitOp::AND => inl && inr,
            BitOp::OR => inl || inr,
            BitOp::XOR => inl != inr,
        }
    }

    fn eval_iter<T: Iterator<Item = bool>>(&self, mut ins: T) -> bool {
        let inl = ins.next().unwrap();
        let inr = ins.next().unwrap();
        self.eval(inl, inr)
    }

    fn to_string(&self) -> String {
        match self {
            BitOp::AND => "AND",
            BitOp::OR => "OR ",
            BitOp::XOR => "XOR",
        }
        .to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Gate {
    inl: WireLabel,
    inr: WireLabel,
    op: BitOp,
    out: WireLabel,
}

impl Gate {
    fn eval_memoized(&self, state: &mut HashMap<WireLabel, bool>, gates: &Vec<Gate>) -> bool {
        let res_opt = state.get(&self.out);
        if res_opt.is_some() {
            return *res_opt.unwrap();
        }
        // print!("> Eval ");
        // self.print();
        let ins = self.inputs().into_iter().map(|inp| {
            let memopt = state.get(&inp);
            if memopt.is_some() {
                return *memopt.unwrap();
            }
            let in_gate = gates.iter().find(|g| g.out == inp).unwrap();
            let in_b = in_gate.eval_memoized(state, gates);
            return in_b;
        });
        let out_b = self.op.eval_iter(ins);
        state.insert(self.out, out_b);
        out_b
    }

    fn inputs(&self) -> [WireLabel; 2] {
        [self.inl, self.inr]
    }

    #[allow(unused)]
    fn depends_on(&self, gates: &Vec<Gate>) -> Vec<WireLabel> {
        let mut res = self.inputs().to_vec();
        let mut frontier: VecDeque<WireLabel> = VecDeque::new();
        frontier.extend(res.iter());
        while !frontier.is_empty() {
            let wire = frontier.pop_front().unwrap();
            let gate_opt = gates.iter().find(|g| g.out == wire);
            if gate_opt.is_some() {
                let gate = gate_opt.unwrap();
                res.extend(gate.inputs());
                frontier.extend(gate.inputs());
            }
        }
        res.sort();
        res.dedup();
        return res;
    }

    fn to_string(&self) -> String {
        label_to_string(&self.inl)
            + " "
            + &self.op.to_string()
            + " "
            + &label_to_string(&self.inr)
            + " -> "
            + &label_to_string(&self.out)
    }

    #[allow(unused)]
    fn println(&self) {
        println!("{}", self.to_string());
    }
}

fn label_number(l: &WireLabel) -> Option<u32> {
    option_assert(l[0] == 'x' || l[0] == 'y' || l[0] == 'z')?;
    Some(l[1].to_digit(10).unwrap() * 10 + l[2].to_digit(10).unwrap())
}

fn make_label(c: char, i: u32) -> WireLabel {
    [
        c,
        char::from_digit(i / 10, 10).unwrap(),
        char::from_digit(i % 10, 10).unwrap(),
    ]
}

fn rename_label(
    label_old: WireLabel,
    label_new: WireLabel,
    gates: &mut Vec<Gate>,
    renames: &mut HashMap<WireLabel, WireLabel>,
) {
    let none = renames.insert(label_new, label_old);
    assert_eq!(none, None);
    for gate in gates.iter_mut() {
        if gate.inl == label_old {
            gate.inl = label_new;
        }
        if gate.inr == label_old {
            gate.inr = label_new;
        }
        if gate.out == label_old {
            gate.out = label_new;
        }
    }
}

fn print_gates(gates: &Vec<Gate>) {
    for g in gates.iter().sorted_by_key(|g| g.out) {
        g.println();
    }
}

fn print_renames(renames: &HashMap<WireLabel, WireLabel>) {
    for (k, v) in renames {
        println!("{} <- {}", label_to_string(k), label_to_string(v));
    }
}

fn search_and_rename(
    c: char,
    i: u32,
    in1: WireLabel,
    in2: WireLabel,
    op: BitOp,
    gates: &mut Vec<Gate>,
    renames: &mut HashMap<WireLabel, WireLabel>,
) {
    let newl: WireLabel = make_label(c, i);
    let gate_idx_opt = gates
        .iter()
        .position(|g| g.op == op && (g.inl == in1 || g.inl == in2));
    if gate_idx_opt.is_none() {
        print_gates(gates);
        print_renames(renames);
        panic!("{} not found!", label_to_string(&newl));
    }
    let gate = gates.remove(gate_idx_opt.unwrap());
    // Sanity check
    assert!(
        (gate.inl == in1 && gate.inr == in2) || (gate.inl == in2 && gate.inr == in1),
        "{}",
        gate.to_string()
    );
    let old_label = gate.out;
    if old_label[0] == 'z' && newl[0] != 'z' {
        print_gates(gates);
        gate.println();
        panic!(
            "Renaming label {} to {} (original gate {} {} {}). Swap it with the expected op s{i} XOR c{}, ({} XOR {})",
            label_to_string(&old_label),
            label_to_string(&newl),
            label_to_string(&renames.get(&gate.inl).unwrap_or(&gate.inl)),
            gate.op.to_string(),
            label_to_string(&renames.get(&gate.inr).unwrap_or(&gate.inr)),
            i - 1,
            label_to_string(&renames.get(&make_label('s', i)).unwrap()),
            label_to_string(&renames.get(&make_label('c', i - 1)).unwrap()),
        );
    }
    rename_label(old_label, newl, gates, renames);
}

fn main() {
    println!("Day 24");

    let label_p = parser!({a:alnum b:alnum c:alnum => [a, b, c]});
    let inputs_p = parser!(label_p ": " {b:char_of("01") => b == 1});
    let bitop_p = parser!({"AND" => BitOp::AND, "OR" => BitOp::OR, "XOR" => BitOp::XOR});
    let gates_p = parser!(inl:label_p " "+ op:bitop_p " "+ inr:label_p " -> " out:label_p => Gate { inl, inr, op, out });
    let p = parser!(section(lines(inputs_p)) section(lines(gates_p)));
    let (inputs, gates): (Vec<(WireLabel, bool)>, Vec<Gate>) =
        p.parse(&crustofcode::read_input()).unwrap();
    let wires_num = inputs.len() + gates.len();

    // Part 1
    let mut state1 = HashMap::<WireLabel, bool>::with_capacity(wires_num);
    for (label, val) in &inputs {
        assert!(!state1.contains_key(label));
        state1.insert(*label, *val);
    }
    let res1 = gates
        .iter()
        .filter(|g| g.out[0] == 'z')
        .sorted_by_key(|g| g.out)
        .rev()
        .map(|g| g.eval_memoized(&mut state1, &gates))
        .fold(0_u64, |acc, b| if b { acc * 2 + 1 } else { acc * 2 });
    println!("{res1}");

    // Part 2
    let num_bits = inputs
        .iter()
        .map(|(l, _)| label_number(l).unwrap())
        .max()
        .unwrap();
    let mut renames = HashMap::<WireLabel, WireLabel>::with_capacity(wires_num);
    // First, we rename all the gates that join together two inputs with the
    // same number
    let mut gates = gates;
    // We handle 0 separately because z00 = b00 = s00 and c00 = a00)
    rename_label(
        make_label('z', 0),
        make_label('s', 0),
        &mut gates,
        &mut renames,
    );
    search_and_rename(
        'c',
        0,
        make_label('x', 0),
        make_label('y', 0),
        BitOp::AND,
        &mut gates,
        &mut renames,
    );
    for i in 1..=num_bits {
        // Rename sum (s)
        search_and_rename(
            's',
            i,
            make_label('x', i),
            make_label('y', i),
            BitOp::XOR,
            &mut gates,
            &mut renames,
        );
        // Rename internal carry (a)
        search_and_rename(
            'a',
            i,
            make_label('x', i),
            make_label('y', i),
            BitOp::AND,
            &mut gates,
            &mut renames,
        );
        // Rename internal temp (b)
        search_and_rename(
            'b',
            i,
            make_label('s', i),
            make_label('c', i - 1),
            BitOp::AND,
            &mut gates,
            &mut renames,
        );
        // Makes sure z is computed right
        search_and_rename(
            'z',
            i,
            make_label('s', i),
            make_label('c', i - 1),
            BitOp::XOR,
            &mut gates,
            &mut renames,
        );
        // Rename carry (c)
        if i < num_bits {
            search_and_rename(
                'c',
                i,
                make_label('a', i),
                make_label('b', i),
                BitOp::OR,
                &mut gates,
                &mut renames,
            );
        }
    }

    let mut swaps = vec!["djg", "z12", "sbg", "z19", "hjm", "mcq", "z37", "dsd"];
    swaps.sort();
    println!("{}", swaps.join(","));
}
