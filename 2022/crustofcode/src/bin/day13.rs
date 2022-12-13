use crustofcode::*;
use slab_tree::*;
use std::cmp::Ord;
use std::cmp::Ordering;
use std::cmp::Ordering::*;
use std::iter;

#[allow(dead_code)]
fn print_node(node: &NodeRef<i64>) -> () {
    if *node.data() != -1 {
        print!(" {} ", node.data());
    } else {
        print!("[");
        for child in node.children() {
            print_node(&child);
        }
        print!("]");
    }
}

#[allow(dead_code)]
fn print_tree(tree: &Tree<i64>) -> () {
    print_node(&tree.root().unwrap());
    print!("\n");
}

// This expects to receive the root of the current parenthesis, and append
// children to it. Returns the string after the list
fn parse_list(s: String, mut root: NodeMut<i64>) -> String {
    let mut s = s;
    // [1],[2,3,4]]
    // 1,1,3,1,1]
    let mut c0 = s.chars().nth(0).unwrap();
    while c0 != ']' {
        if c0 == '[' {
            // A sublist begins
            root.append(-1);
            s = parse_list(s[1..].to_owned(), root.last_child().unwrap());
        }
        if c0 == ',' {
            s = s[1..].to_owned();
            c0 = s.chars().nth(0).unwrap();
        }
        if c0.is_digit(10) {
            let v: i64 = s
                .chars()
                .take_while(|c| c.is_digit(10))
                .collect::<String>()
                .parse()
                .unwrap();
            root.append(v);
            s = s.chars().skip_while(|c| c.is_digit(10)).collect::<String>();
        }
        c0 = s.chars().nth(0).unwrap();
    }
    return s[1..].to_owned();
}

fn parse_line(line: String, r: i64) -> Tree<i64> {
    let mut tree: Tree<i64> = Tree::new();
    tree.set_root(r);
    parse_list(line[1..].to_owned(), tree.root_mut().unwrap());
    return tree;
}

fn le_list<'a, T1: Iterator<Item = NodeRef<'a, i64>>, T2: Iterator<Item = NodeRef<'a, i64>>>(
    left: &mut T1,
    right: &mut T2,
) -> Ordering {
    match (left.next(), right.next()) {
        (None, None) => Equal,
        (Some(_), None) => Greater,
        (None, Some(_)) => Less,
        (Some(lc), Some(rc)) => {
            let res = le(lc, rc);
            match res {
                Equal => le_list(left, right),
                _ => res,
            }
        }
    }
}

fn le(left: NodeRef<i64>, right: NodeRef<i64>) -> Ordering {
    let res = match (left.data(), right.data()) {
        (-1, -1) | (-2, -1) | (-1, -2) | (-2, -2) => {
            le_list(&mut left.children(), &mut right.children())
        }
        (-1, _) | (-2, _) => le_list(&mut left.children(), &mut iter::once(right)),
        (_, -1) | (_, -2) => le_list(&mut iter::once(left), &mut right.children()),
        (l, r) => Ord::cmp(l, r),
    };
    return res;
}

fn le_pair((tl, tr): &(Tree<i64>, Tree<i64>)) -> Ordering {
    le(tl.root().unwrap(), tr.root().unwrap())
}

fn main() {
    println!("Day 13");

    let lines: Vec<String> = read_input_lines();
    let trees: Vec<(Tree<i64>, Tree<i64>)> = lines
        .as_slice()
        .chunks(3)
        .map(|chunk| {
            if chunk.len() > 2 {
                assert_eq!(chunk[2], "")
            };
            (
                parse_line(chunk[0].to_owned(), -1),
                parse_line(chunk[1].to_owned(), -1),
            )
        })
        .collect();

    // Part 1
    println!(
        "{}",
        trees
            .iter()
            .enumerate()
            .filter(|&(_, p)| le_pair(p).is_le())
            .map(|(i, _)| i + 1)
            .sum::<usize>()
    );

    // Part 2
    let divider_packets: Vec<Tree<i64>> = ["[[2]]", "[[6]]"]
        .into_iter()
        .map(|s| parse_line(s.to_owned(), -2))
        .collect();
    let mut new_trees: Vec<Tree<i64>> = trees
        .into_iter()
        .flat_map(|(t1, t2)| [t1, t2].into_iter())
        .chain(divider_packets.into_iter())
        .collect();
    new_trees.sort_by(|a, b| le(a.root().unwrap(), b.root().unwrap()));
    println!(
        "{}",
        new_trees
            .into_iter()
            .enumerate()
            .filter(|(_, t)| *t.root().unwrap().data() == -2)
            .map(|(i, _)| i + 1)
            .product::<usize>()
    );
}
