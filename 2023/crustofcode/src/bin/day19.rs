use aoc_parse::{parser, prelude::*};
use itertools::Itertools;

type Part = [u32; 4];

#[derive(Debug, Clone)]
enum Target {
    A,
    R,
    Label(String),
}

impl Target {
    fn from_chars(target: Vec<char>) -> Target {
        if target.len() == 1 {
            match target[0] {
                'A' => Target::A,
                'R' => Target::R,
                _ => panic!("Target of a single char"),
            }
        } else {
            assert!(target.iter().all(|c| c.is_lowercase()));
            Target::Label(target.into_iter().collect())
        }
    }
}

#[derive(Debug, Clone)]
struct Rule {
    cat: usize,
    sign: bool,
    num: u32,
    target: Target,
}

impl Rule {
    fn apply(&self, part: &Part) -> Option<Target> {
        let guard = if self.sign {
            part[self.cat] < self.num
        } else {
            part[self.cat] > self.num
        };
        guard.then_some(self.target.clone())
    }

    fn split(&self, range: PartRange) -> (Option<PartRange>, Target, Option<PartRange>) {
        let (matchrange, passrange) = if self.sign {
            (
                (range[self.cat].0, self.num - 1),
                (self.num, range[self.cat].1),
            )
        } else {
            (
                (self.num + 1, range[self.cat].1),
                (range[self.cat].0, self.num),
            )
        };
        let matchrange = is_nonempty(&matchrange).then(|| {
            let mut tmp = range.clone();
            tmp[self.cat] = matchrange;
            tmp
        });
        let passrange = is_nonempty(&passrange).then(|| {
            let mut tmp = range;
            tmp[self.cat] = passrange;
            tmp
        });
        return (matchrange, self.target.clone(), passrange);
    }
}

#[derive(Debug, Clone)]
struct Workflow {
    label: String,
    rules: Vec<Rule>,
    default: Target,
}
impl Workflow {
    fn apply(&self, part: &Part) -> Target {
        self.rules
            .iter()
            .find_map(|r| r.apply(part))
            .unwrap_or(self.default.clone())
    }

    fn split(&self, range: PartRange) -> Vec<(PartRange, Target)> {
        let mut res = vec![];
        let mut curr_range = range;
        for rule in &self.rules {
            let (matched, t, passed) = rule.split(curr_range);
            if matched.is_some() {
                res.push((matched.unwrap(), t));
            }
            if passed.is_none() {
                return res;
            }
            curr_range = passed.unwrap();
        }
        res.push((curr_range, self.default.clone()));
        return res;
    }
}

fn sort_part(workflows: &Vec<Workflow>, part: &Part) -> bool {
    let mut res = Target::Label(String::from("in"));
    loop {
        match res {
            Target::A => return true,
            Target::R => return false,
            Target::Label(l) => {
                let wf = workflows.iter().find(|w| w.label == l).unwrap();
                res = wf.apply(part);
            }
        }
    }
}

type PartRange = [(u32, u32); 4];

fn is_nonempty(&(l, h): &(u32, u32)) -> bool {
    l < h
}

fn size(range: PartRange) -> u64 {
    range
        .into_iter()
        .map(|(l, h)| u64::from(h - l + 1))
        .product()
}

fn sort_range(workflows: &Vec<Workflow>, range: PartRange) -> u64 {
    let mut accepted = 0;
    let mut to_process = vec![(range, Target::Label(String::from("in")))];
    while !to_process.is_empty() {
        let (curr_range, t) = to_process.pop().unwrap();
        match t {
            Target::A => accepted += size(curr_range),
            Target::R => {}
            Target::Label(l) => {
                let wf = workflows.iter().find(|w| w.label == l).unwrap();
                to_process.append(&mut wf.split(curr_range));
            }
        }
    }
    return accepted;
}

fn main() {
    println!("Day 19");

    let rule_p = parser!(cat:char_of("xmas") sign:char_of("<>") num:u32 ":" target:alpha+ => Rule {
        cat: cat,
        sign: sign == 0,
        num: num,
        target: Target::from_chars(target),
    });
    let workflow_p = parser!(label:alpha+ "{" rules:repeat_sep(rule_p, ",") "," default:alpha+ "}" => Workflow {
        label: label.into_iter().collect(),
        rules: rules,
        default: Target::from_chars(default),
    });
    let part_p = parser!("{x=" x:u32 ",m=" m:u32 ",a=" a:u32 ",s=" s:u32 "}" => [x, m, a, s]);
    let p = parser!(section(lines(workflow_p)) section(lines(part_p)));
    let (workflows, parts) = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let accepted_parts = parts
        .iter()
        .filter(|p| sort_part(&workflows, p))
        .collect_vec();
    let r1: u32 = accepted_parts
        .iter()
        .map(|p| p.into_iter().sum::<u32>())
        .sum();
    println!("{r1}");

    // Part 2
    let r2 = sort_range(&workflows, [(1, 4000), (1, 4000), (1, 4000), (1, 4000)]);
    println!("{r2}");
}
