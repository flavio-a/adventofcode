use crustofcode::*;
use itertools::Itertools;
use queues::*;

const TEST_OPS: [fn(i64) -> i64; 4] = [|x| x * 19, |x| x + 6, |x| x * x, |x| x + 3];
const INPUT_OPS: [fn(i64) -> i64; 8] = [
    |x| x * 3,
    |x| x * 19,
    |x| x + 2,
    |x| x * x,
    |x| x + 8,
    |x| x + 6,
    |x| x + 7,
    |x| x + 4,
];

#[derive(Debug)]
struct Monkey {
    items: Queue<i64>,
    op: fn(i64) -> i64,
    testnum: i64,
    ift: usize,
    iff: usize,
    activity: u64,
    modulus: i64,
}

impl Monkey {
    fn test(&self, x: i64) -> bool {
        x % self.testnum == 0
    }

    fn has_item(&self) -> bool {
        self.items.peek().is_ok()
    }

    fn throw_item_p1(&mut self) -> Option<(usize, i64)> {
        let worry_level: i64 = self.items.remove().ok()?;
        let worry_level = (self.op)(worry_level);
        let worry_level = worry_level / 3;
        self.activity += 1;
        if self.test(worry_level) {
            Some((self.ift, worry_level))
        } else {
            Some((self.iff, worry_level))
        }
    }

    fn throw_item_p2(&mut self) -> Option<(usize, i64)> {
        let worry_level: i64 = self.items.remove().ok()?;
        let worry_level = (self.op)(worry_level) % self.modulus;
        self.activity += 1;
        if self.test(worry_level) {
            Some((self.ift, worry_level))
        } else {
            Some((self.iff, worry_level))
        }
    }

    // // Makes a copy of the monkey, including items it's holding
    // fn make_fresh_copy(&self) -> Monkey {
    //     let mut items = queue![];
    //     return Monkey {
    //         items: items,
    //         op: self.op,
    //         testnum: self.testnum,
    //         ift: self.ift,
    //         iff: self.iff,
    //         activity: 0,
    //         modulus: 0,
    //     };
    // }
}

fn parse_monkey(m: &[String]) -> Monkey {
    let opidx: usize = str2int(&drop_prefix(&m[0], "Monkey ")[0..1])
        .try_into()
        .unwrap();
    // let items = drop_prefix(&m[1], "  Starting items: ").split(",").map(|item| str2int(item.trim())).collect();
    let mut items = queue![];
    for item in drop_prefix(&m[1], "  Starting items: ").split(",") {
        items.add(str2int(item.trim())).ok();
    }
    let testnum = str2int(drop_prefix(&m[3], "  Test: divisible by ").trim());
    let ift: usize = str2int(drop_prefix(&m[4], "    If true: throw to monkey ").trim())
        .try_into()
        .unwrap();
    let iff: usize = str2int(drop_prefix(&m[5], "    If false: throw to monkey ").trim())
        .try_into()
        .unwrap();
    return Monkey {
        items: items,
        op: INPUT_OPS[opidx],
        testnum: testnum,
        ift: ift,
        iff: iff,
        activity: 0,
        modulus: testnum,
    };
}

#[allow(dead_code)]
fn do_round_p1(monkeys: &mut Vec<Monkey>) -> &mut Vec<Monkey> {
    for midx in 0..monkeys.len() {
        while monkeys[midx].has_item() {
            let (newidx, wl) = monkeys[midx].throw_item_p1().unwrap();
            monkeys[newidx].items.add(wl).ok();
        }
    }
    return monkeys;
}

#[allow(dead_code)]
fn do_round_p2(monkeys: &mut Vec<Monkey>) -> &mut Vec<Monkey> {
    for midx in 0..monkeys.len() {
        while monkeys[midx].has_item() {
            let (newidx, wl) = monkeys[midx].throw_item_p2().unwrap();
            monkeys[newidx].items.add(wl).ok();
        }
    }
    return monkeys;
}

fn get_monkey_buisness(monkeys: &Vec<Monkey>) -> u64 {
    monkeys
        .iter()
        .map(|m| m.activity)
        .sorted()
        .rev()
        .take(2)
        .product()
}

fn main() {
    println!("Day 11");

    let mut monkeys: Vec<Monkey> = read_input_lines()
        .as_slice()
        .split(|s| s == "")
        .map(|m| parse_monkey(m))
        .collect();
    let modulus: i64 = monkeys.iter().map(|m| m.testnum).product();
    for monkey in &mut monkeys {
        monkey.modulus = modulus;
    }

    // Part 1
    // for _ in 0..20 {
    //     do_round_p1(&mut monkeys);
    // }
    // println!("{}", get_monkey_buisness(&monkeys));

    // Part 2
    for _ in 0..10000 {
        do_round_p2(&mut monkeys);
    }
    println!("{}", get_monkey_buisness(&monkeys));
}
