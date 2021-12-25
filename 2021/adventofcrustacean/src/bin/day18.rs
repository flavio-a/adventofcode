// use adventofcrustacean;
use itertools::Itertools;
use either::*;

#[derive(Debug,Clone)]
struct Tree {
    cont: Either<u8, Box<(Tree, Tree)>>,
}

fn char2int(c: char) -> u8 {
    c.to_digit(10).expect("Char parsable to number expected").try_into().unwrap()
}

fn parse_pair(s: &str) -> Tree {
    // println!("Parsing {}", s);
    if s.chars().next().unwrap().is_digit(10) {
        return Tree { cont: Left(char2int(s.chars().next().unwrap())) };
    }
    // Removing first and last char
    let mut it = s.chars();
    assert_eq!(it.next().unwrap(), '[');
    assert_eq!(it.next_back().unwrap(), ']');
    // First subpair
    let mut count = 0;
    let s1 = it.take_while_ref(|c| match c {
            '[' => { count += 1; true },
            ']' => { count -= 1; true},
            ',' => { count > 0 },
            _ => true,
        })
        .collect::<String>();
    // println!("Subpair: {}", s1);
    let sp1 = parse_pair(&s1);
    // Separator
    assert_eq!(it.next(), Some(','));
    // Second subpair
    let s2 = it.collect::<String>();
    // println!("Subpair: {}", s2);
    let sp2 = parse_pair(&s2);
    // Return
    return Tree { cont: Right(Box::new((sp1, sp2))) };

}

// Adds val to the leftmost/rightmost number of pair. Leftmost or rightmost
// depends on lr, either being 1 (left) or 2 (right)
fn addside(pair: Tree, val: u8, lr: u8) -> Tree {
    if pair.cont.is_left() {
        return Tree { cont: Left(pair.cont.unwrap_left() + val) };
    }
    let (sp1, sp2) = *(pair.cont.unwrap_right());
    let newpair = if lr == 2 { (sp1, addside(sp2, val, lr)) } else { (addside(sp1, val, lr), sp2) };
    return Tree { cont: Right(Box::new(newpair)) };
}

// Inner function looks for the first guy to explode and explodes it, reporting
// up what should be added to the left and to the right
// Returns (in order) add_left, the new snailfish_number, add_right
fn explode_(pair: Tree, depth: u8) -> (u8, Tree, u8) {
    if pair.cont.is_left() {
        return (0, pair, 0);
    }
    let (sp1, sp2) = *(pair.cont.unwrap_right());
    if depth >= 4 { // External call with depth == 0
        // Explode pair
        // print("Exploding pair", pair[1], pair[2])
        return (sp1.cont.unwrap_left(), Tree { cont: Left(0) }, sp2.cont.unwrap_left())
    }
    // Explodes left
    let (addl, new_subpl, laddr) = explode_(sp1, depth + 1);
    let sp2 = if laddr > 0 { addside(sp2, laddr, 1) } else { sp2 };
    let sp1 = new_subpl;
    // Explodes right
    let (raddl, new_subpr, addr) = explode_(sp2, depth + 1);
    let sp1 = if raddl > 0 { addside(sp1, raddl, 2) } else { sp1 };
    let sp2 = new_subpr;
    return (addl, Tree { cont: Right(Box::new((sp1, sp2))) }, addr);
}
fn explode(pair: Tree) -> Tree {
    explode_(pair, 0).1
}


fn split(pair: Tree) -> (Tree, bool) {
    if pair.cont.is_left() {
        let v = pair.cont.unwrap_left();
        if v > 9 {
            let sp1 = Tree { cont: Left(v / 2) };
            let sp2 = Tree { cont: Left(if v % 2 == 0 { v / 2 } else { v / 2 + 1 }) };
            let newpair = Tree { cont: Right(Box::new((sp1, sp2))) };
            return (newpair, true);
        }
        else {
            return (Tree { cont: Left(v) }, false);
        }
    }
    let (sp1, sp2) = *(pair.cont.unwrap_right());
    let (new_pair, done) = split(sp1);
    let sp1 = new_pair;
    let (new_pair, done) = if !done { split(sp2) } else { (sp2, done) };
    let sp2 = new_pair;
    return (Tree { cont: Right(Box::new((sp1, sp2))) }, done);
}

// I should check idempotency of explode...
fn reduce(pair: Tree) -> Tree {
    let (res, cont) = split(explode(pair));
    if cont { reduce(res) } else { res }
}

fn add(pair1: Tree, pair2: Tree) -> Tree {
    reduce(Tree { cont: Right(Box::new((pair1, pair2))) })
}

fn magnitude(pair: Tree) -> u32 {
    if pair.cont.is_left() {
        return pair.cont.unwrap_left().into();
    }
    let (sp1, sp2) = *(pair.cont.unwrap_right());
    return 3 * magnitude(sp1) + 2 * magnitude(sp2);
}

#[allow(dead_code)]
fn print_pair(pair: Tree) {
    if pair.cont.is_left() {
        print!("{}", pair.cont.unwrap_left());
    }
    else {
        let (sp1, sp2) = *(pair.cont.unwrap_right());
        print!("[");
        print_pair(sp1);
        print!(",");
        print_pair(sp2);
        print!("]");
    }
}

fn main() {
    let content = adventofcrustacean::read_input();
    let pairs = content.lines().map(parse_pair).collect::<Vec<Tree>>();

    // Part 1
    let mut it = pairs.iter();
    let fst = it.next().unwrap().clone();
    println!("{}", magnitude(it.fold(fst, |acc, elem| add(acc, elem.clone()))));

    // Part 2
    // I assume I can add an element with itself
    let maxmagn = pairs.iter()
                    .cartesian_product(pairs.iter())
                    .map(|(p1, p2)| add(p1.clone(), p2.clone()))
                    .map(magnitude)
                    .max()
                    .unwrap();
    println!("{}", maxmagn);
}
