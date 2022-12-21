use crustofcode::*;
use either::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Op {
    Plus,
    Minus,
    Times,
    Div,
}

impl Op {
    fn from_char(c: &char) -> Op {
        match c {
            '+' => Op::Plus,
            '-' => Op::Minus,
            '*' => Op::Times,
            '/' => Op::Div,
            _ => panic!("AAAAA"),
        }
    }
}

type MonkeyId = [char; 4];

fn make_id<I>(it: &mut I) -> MonkeyId
where
    I: Iterator<Item = char>,
{
    [
        it.next().unwrap(),
        it.next().unwrap(),
        it.next().unwrap(),
        it.next().unwrap(),
    ]
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MonkeyOp {
    op: Op,
    left: MonkeyId,
    right: MonkeyId,
}
type Monkey = Either<i64, MonkeyOp>;

fn parse_line(line: String) -> (MonkeyId, Monkey) {
    let mut t = line.chars();
    let mid = make_id(&mut t);
    assert_eq!(t.next(), Some(':'));
    assert_eq!(t.next(), Some(' '));
    let mut t = t.peekable();
    if t.peek().unwrap().is_digit(10) {
        (mid, Left(str2int(t.collect::<String>())))
    } else {
        let m1 = make_id(&mut t);
        assert_eq!(t.next(), Some(' '));
        let op = Op::from_char(&t.next().unwrap());
        assert_eq!(t.next(), Some(' '));
        let m2 = make_id(&mut t);
        (
            mid,
            Right(MonkeyOp {
                op: op,
                left: m1,
                right: m2,
            }),
        )
    }
}

impl MonkeyOp {
    fn eval(&self, lval: i64, rval: i64) -> i64 {
        match self.op {
            Op::Plus => lval + rval,
            Op::Minus => lval - rval,
            Op::Times => lval * rval,
            Op::Div => lval / rval,
        }
    }

    // Given the result and the left val, compute the needed right val
    fn inverse_left(&self, res: i64, lval: i64) -> i64 {
        match self.op {
            Op::Plus => res - lval,
            Op::Minus => lval - res,
            Op::Times => res / lval,
            Op::Div => lval / res,
        }
    }

    // Given the result and the right val, compute the needed left val
    fn inverse_right(&self, res: i64, rval: i64) -> i64 {
        match self.op {
            Op::Plus => res - rval,
            Op::Minus => res + rval,
            Op::Times => res / rval,
            Op::Div => res * rval,
        }
    }
}

// Eval
fn eval(id: MonkeyId, monkeys: &HashMap<MonkeyId, Monkey>) -> i64 {
    let me = monkeys.get(&id).unwrap().clone();
    if me.is_left() {
        return me.unwrap_left();
    }
    let m = me.unwrap_right();
    let leftv = eval(m.left, monkeys);
    let rightv = eval(m.right, monkeys);
    let res = m.eval(leftv, rightv);
    return res;
}

fn contains(id: MonkeyId, target: &MonkeyId, monkeys: &HashMap<MonkeyId, Monkey>) -> bool {
    if id == *target {
        return true;
    }
    let me = monkeys.get(&id).unwrap();
    if me.is_left() {
        return false;
    }
    let m = me.clone().unwrap_right();
    return contains(m.left, target, monkeys) || contains(m.right, target, monkeys);
}

fn invert(
    id: MonkeyId,
    target: &MonkeyId,
    result: i64,
    monkeys: &HashMap<MonkeyId, Monkey>,
) -> i64 {
    // Assume id contains target
    let me = monkeys.get(&id).unwrap();
    if me.is_left() {
        if id == *target {
            return result;
        }
        panic!("Unexpected plot twist");
    }
    let m = me.clone().unwrap_right();
    if contains(m.left, target, monkeys) {
        let rval = eval(m.right, monkeys);
        let lval = m.inverse_right(result, rval);
        return invert(m.left, target, lval, monkeys);
    } else if contains(m.right, target, monkeys) {
        let lval = eval(m.left, monkeys);
        let rval = m.inverse_left(result, lval);
        return invert(m.right, target, rval, monkeys);
    } else {
        panic!("Inconceivable");
    }
}

fn main() {
    println!("Day 21");

    let monkeys: Vec<(MonkeyId, Monkey)> = crustofcode::read_input_lines()
        .into_iter()
        .map(parse_line)
        .collect();
    // println!("{monkeys:?}");
    let mut monkeysm: HashMap<MonkeyId, Monkey> = HashMap::with_capacity(monkeys.len());
    for (id, m) in &monkeys {
        monkeysm.insert(*id, m.clone());
    }

    // Part 1
    let root_key = ['r', 'o', 'o', 't'];
    println!("{}", eval(root_key, &monkeysm));

    // Part 2
    let humn_key = ['h', 'u', 'm', 'n'];
    let root = monkeysm.get(&root_key).unwrap().clone().unwrap_right();
    let res = if contains(root.left, &humn_key, &monkeysm) {
        let rval = eval(root.right, &monkeysm);
        invert(root.left, &humn_key, rval, &monkeysm)
    } else if contains(root.right, &humn_key, &monkeysm) {
        let lval = eval(root.left, &monkeysm);
        invert(root.right, &humn_key, lval, &monkeysm)
    } else {
        panic!("Impossible")
    };
    println!("{}", res);
}
