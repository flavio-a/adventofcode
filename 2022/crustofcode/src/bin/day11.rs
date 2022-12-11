use crustofcode::*;
use itertools::Itertools;

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

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<i64>,
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
        !self.items.is_empty()
    }

    fn throw_item(&mut self, divide: bool) -> Option<(usize, i64)> {
        let worry_level: i64 = self.items.pop()?;
        let worry_level = if divide {
            (self.op)(worry_level) / 3
        } else {
            (self.op)(worry_level) % self.modulus
        };
        self.activity += 1;
        if self.test(worry_level) {
            Some((self.ift, worry_level))
        } else {
            Some((self.iff, worry_level))
        }
    }

    #[allow(dead_code)]
    fn print_dbg(&self) -> () {
        println!("Items {:?}, activity {}", self.items, self.activity);
    }
}

fn parse_monkey(m: &[String]) -> Monkey {
    let opidx: usize = str2int(&drop_prefix(&m[0], "Monkey ")[0..1])
        .try_into()
        .unwrap();
    let items = drop_prefix(&m[1], "  Starting items: ")
        .split(",")
        .map(|item| str2int(item.trim()))
        .collect();
    let testnum = str2int(drop_prefix(&m[3], "  Test: divisible by ").trim());
    let ift: usize = str2int(drop_prefix(&m[4], "    If true: throw to monkey ").trim())
        .try_into()
        .unwrap();
    let iff: usize = str2int(drop_prefix(&m[5], "    If false: throw to monkey ").trim())
        .try_into()
        .unwrap();

    let is_test = std::env::args().nth(1).unwrap().contains("test");

    return Monkey {
        items: items,
        op: if is_test {
            TEST_OPS[opidx]
        } else {
            INPUT_OPS[opidx]
        },
        testnum: testnum,
        ift: ift,
        iff: iff,
        activity: 0,
        modulus: testnum,
    };
}

fn do_round(monkeys: &mut Vec<Monkey>, p1: bool) -> &mut Vec<Monkey> {
    for midx in 0..monkeys.len() {
        while monkeys[midx].has_item() {
            let (newidx, wl) = monkeys[midx].throw_item(p1).unwrap();
            monkeys[newidx].items.push(wl);
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
    let mut monkeys1: Vec<Monkey> = monkeys.clone();
    for _ in 0..20 {
        do_round(&mut monkeys1, true);
    }
    println!("{}", get_monkey_buisness(&monkeys1));

    // Part 2
    for _ in 0..10000 {
        do_round(&mut monkeys, false);
    }
    println!("{}", get_monkey_buisness(&monkeys));
}
