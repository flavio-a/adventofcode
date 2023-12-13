use aoc_parse::{parser, prelude::*};
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

type NodeId = [char; 3];

#[derive(Debug, Clone)]
struct Node {
    id: NodeId,
    nexts: [NodeId; 2],
}

fn search_node1(nodes: &Vec<Node>, id: NodeId, dir: usize) -> NodeId {
    nodes
        .iter()
        .find_map(|n| (n.id == id).then_some(n.nexts[dir]))
        .expect(&format!("Id {}{}{} not found", id[0], id[1], id[2]))
}

fn reach_steps<'a, I, F>(
    instructions: &mut I,
    nodes: &Vec<Node>,
    start_id: NodeId,
    is_end: F,
) -> (NodeId, usize)
where
    I: Iterator<Item = &'a usize>,
    F: Fn(NodeId) -> bool,
{
    instructions
        .fold_while((start_id, 0), |(curr_id, count), dir| {
            let next_id = search_node1(&nodes, curr_id, *dir);
            if is_end(next_id) {
                Done((next_id, count + 1))
            } else {
                Continue((next_id, count + 1))
            }
        })
        .into_inner()
}

// https://rosettacode.org/wiki/Least_common_multiple#Rust
fn gcd(a: usize, b: usize) -> usize {
    match ((a, b), (a & 1, b & 1)) {
        ((x, y), _) if x == y => y,
        ((0, x), _) | ((x, 0), _) => x,
        ((x, y), (0, 1)) | ((y, x), (1, 0)) => gcd(x >> 1, y),
        ((x, y), (0, 0)) => gcd(x >> 1, y >> 1) << 1,
        ((x, y), (1, 1)) => {
            let (x, y) = (std::cmp::min(x, y), std::cmp::max(x, y));
            gcd((y - x) >> 1, x)
        }
        _ => unreachable!(),
    }
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn main() {
    println!("Day 8");

    let instructions_p = parser!(char_of("LR")+);
    let nodeid_p = parser!(c1:upper c2:upper c3:upper => [c1, c2, c3]);
    let node_p = parser!(id:nodeid_p " = (" l:nodeid_p ", " r:nodeid_p ")" => Node { id: id, nexts: [l, r] });
    let p = parser!(line(instructions_p) line("") lines(node_p));
    let (instructions, _, nodes) = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let start_id = ['A', 'A', 'A'];
    let end_id = ['Z', 'Z', 'Z'];
    let (_, r1) = reach_steps(&mut instructions.iter().cycle(), &nodes, start_id, |id| {
        id == end_id
    });
    println!("{r1}");

    // Part 2
    let start_ids = nodes.iter().map(|n| n.id).filter(|id| id[2] == 'A');
    let cycles = start_ids
        .map(|start_id| {
            let mut instr = instructions.iter().cycle();
            let (first_end, prefix_len) =
                reach_steps(&mut instr, &nodes, start_id, |id| id[2] == 'Z');
            let (second_end, cycle_len) =
                reach_steps(&mut instr, &nodes, first_end, |id| id[2] == 'Z');
            // Check that all initial points reach a single final point and cycle on that
            assert_eq!(first_end, second_end);
            // Check that the cycles are the same as prefixes
            assert_eq!(prefix_len, cycle_len);
            cycle_len
        })
        .collect_vec();
    // println!("{:?}", cycles);
    let r2 = cycles.into_iter().reduce(lcm).unwrap();
    println!("{r2}");
}
