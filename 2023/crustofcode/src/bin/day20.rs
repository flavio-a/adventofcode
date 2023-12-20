use aoc_parse::{parser, prelude::*};
use itertools::Itertools;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pulse {
    H,
    L,
}

impl Pulse {
    fn invert(&self) -> Self {
        match self {
            Pulse::L => Pulse::H,
            Pulse::H => Pulse::L,
        }
    }

    fn is_low(&self) -> bool {
        *self == Pulse::L
    }

    fn is_high(&self) -> bool {
        *self == Pulse::H
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ModuleType {
    Broadcast,
    Conjunction,
    FlipFlop,
    // Button,
}

impl ModuleType {
    fn from_idx(i: usize) -> Self {
        match i {
            0 => ModuleType::FlipFlop,
            1 => ModuleType::Conjunction,
            _ => panic!("Unexpected plot twist"),
        }
    }
}

#[derive(Debug, Clone)]
struct ModuleParse {
    kind: ModuleType,
    name: String,
    dests: Vec<String>,
}

type Signal = (usize, Pulse, usize);

#[derive(Debug, Clone)]
struct Module {
    kind: ModuleType,
    name: usize,
    dests: Vec<usize>,
    state: Vec<Pulse>,
    preds: Vec<usize>,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Module {} {{ {:?}, {:?}, {:?}, {:?} }}",
            self.name,
            self.kind,
            self.dests,
            self.state,
            self.preds
                .iter()
                .enumerate()
                .filter(|(_, v)| **v <= 100)
                .map(crustofcode::into_fst)
                .collect_vec()
        )
    }
}

impl Module {
    fn from_moduleparse<F: Fn(String) -> usize>(
        idx: usize,
        get_name: F,
        all: usize,
        mp: ModuleParse,
    ) -> Self {
        Module {
            name: idx,
            state: match mp.kind {
                ModuleType::Conjunction => vec![],
                ModuleType::FlipFlop => vec![Pulse::H],
                ModuleType::Broadcast => vec![],
            },
            preds: if mp.kind == ModuleType::Conjunction {
                vec![all + 100; all]
            } else {
                vec![]
            },
            kind: mp.kind,
            dests: mp.dests.into_iter().map(get_name).collect(),
        }
    }

    fn send_to_all(&self, o: Pulse) -> VecDeque<Signal> {
        self.dests.iter().map(|d| (self.name, o, *d)).collect()
    }

    fn handle_pulse(&mut self, sender: usize, p: Pulse) -> VecDeque<Signal> {
        match self.kind {
            ModuleType::Broadcast => self.send_to_all(p),
            ModuleType::Conjunction => {
                self.state[self.preds[sender]] = p;
                let o = if self.state.iter().all(|p| p.is_high()) {
                    Pulse::L
                } else {
                    Pulse::H
                };
                self.send_to_all(o)
            }
            ModuleType::FlipFlop => {
                if p.is_low() {
                    let state = self.state[0];
                    self.state[0] = self.state[0].invert();
                    self.send_to_all(state)
                } else {
                    VecDeque::from([])
                }
            } // ModuleType::Button => vec![(self.dests[0], Pulse::L)],
        }
    }
}

#[allow(dead_code)]
fn print_message(&(sender, p, receiver): &Signal) {
    println!(
        "{sender} --{}--> {receiver}",
        if p.is_low() { "low" } else { "high" }
    );
}

fn push_button1(modules: &mut Vec<Module>, broadcaster_idx: usize) -> (usize, usize) {
    let mut signals: VecDeque<Signal> = VecDeque::from([(0, Pulse::L, broadcaster_idx)]);
    let mut res: (usize, usize) = (0, 0);
    while !signals.is_empty() {
        let (sender, p, receiver) = signals.pop_front().unwrap();
        if p.is_low() {
            res.0 += 1
        } else {
            res.1 += 1
        };
        if receiver < modules.len() {
            let mut new_signals = modules[receiver].handle_pulse(sender, p);
            signals.append(&mut new_signals);
        }
    }
    return res;
}

fn push_button2(modules: &mut Vec<Module>, broadcaster_idx: usize, rx_idx: usize) -> bool {
    let mut signals: VecDeque<Signal> = VecDeque::from([(0, Pulse::L, broadcaster_idx)]);
    while !signals.is_empty() {
        let (sender, p, receiver) = signals.pop_front().unwrap();
        if p.is_low() && receiver == rx_idx {
            return true;
        }
        if receiver < modules.len() {
            let mut new_signals = modules[receiver].handle_pulse(sender, p);
            signals.append(&mut new_signals);
        }
    }
    return false;
}

fn main() {
    println!("Day 20");

    let module_p = parser!(nk:{
        "broadcaster" => (String::from("broadcaster"), ModuleType::Broadcast),
        k:char_of("%&") name:alpha+ => (name.into_iter().collect(), ModuleType::from_idx(k))
    } " -> " dests:repeat_sep(alpha+, ", ") => ModuleParse {
        kind: nk.1,
        name: nk.0,
        dests: dests.into_iter().map(|s| s.into_iter().collect()).collect(),
    });
    let p = parser!(lines(module_p));
    let modules_parsed: Vec<ModuleParse> = p.parse(&crustofcode::read_input()).unwrap();
    let names: Vec<String> = modules_parsed.iter().map(|mp| mp.name.clone()).collect();
    let get_name =
        |name: String| -> usize { names.iter().position(|n| *n == name).unwrap_or(names.len()) };
    let broadcaster_idx = get_name(String::from("broadcaster"));
    let mut modules: Vec<Module> = modules_parsed
        .into_iter()
        .enumerate()
        .map(|(i, mp)| Module::from_moduleparse(i, get_name, names.len(), mp))
        .collect();
    for m in modules.clone() {
        for d in &m.dests {
            if *d < modules.len() && modules[*d].kind == ModuleType::Conjunction {
                modules[*d].preds[m.name] = modules[*d].state.len();
                modules[*d].state.push(Pulse::L);
            }
        }
    }
    #[cfg(debug_assertions)]
    println!(
        "{}",
        modules
            .iter()
            .map(|m| m.to_string())
            .collect_vec()
            .join("\n")
    );

    // Part 1
    let mut modules_p1 = modules.clone();
    let mut r1 = (0, 0);
    for _ in 0..1000 {
        let tmp = push_button1(&mut modules_p1, broadcaster_idx);
        r1.0 += tmp.0;
        r1.1 += tmp.1;
    }
    println!("{}", r1.0 * r1.1);

    // Part 2
    let mut modules_p2 = modules.clone();
    let rx_idx = get_name(String::from("rx"));
    let mut r2: usize = 1;
    while !push_button2(&mut modules_p2, broadcaster_idx, rx_idx) {
        r2 += 1;
    }
    println!("{r2}");
}
