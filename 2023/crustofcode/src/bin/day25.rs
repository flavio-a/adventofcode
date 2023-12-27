use aoc_parse::{parser, prelude::*};
use crustofcode::option_assert;
use itertools::Itertools;
use pathfinding::directed::{bfs::bfs_reach, edmonds_karp::edmonds_karp};
use rand::Rng;
use std::collections::HashMap;

type NodeId = (char, char, char);

#[derive(Debug, Clone)]
struct Node {
    id: usize,
    #[allow(dead_code)]
    old_id: NodeId,
    neighs: Vec<usize>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Node {}

fn name_nodes(base_nodes: Vec<(NodeId, Vec<NodeId>)>) -> Vec<Node> {
    let mut hm = HashMap::new();
    let mut res = Vec::new();
    let mut i: usize = 0;
    for (id, _) in &base_nodes {
        hm.insert(id.clone(), i);
        res.push(Node {
            id: i,
            old_id: id.clone(),
            neighs: Vec::new(),
        });
        i += 1;
    }
    for n1 in base_nodes {
        for n2 in n1.1 {
            let i1 = *hm.get(&n1.0).unwrap();
            let i2 = if hm.contains_key(&n2) {
                *hm.get(&n2).unwrap()
            } else {
                hm.insert(n2.clone(), i);
                res.push(Node {
                    id: i,
                    old_id: n2.clone(),
                    neighs: Vec::new(),
                });
                i += 1;
                i - 1
            };
            res[i1].neighs.push(i2);
            res[i2].neighs.push(i1);
        }
    }
    return res;
}

fn find_cut(
    node_ids: &Vec<usize>,
    s: usize,
    t: usize,
    edges: &Vec<(usize, usize)>,
) -> Option<[(usize, usize); 3]> {
    let caps: Vec<((usize, usize), i32)> = edges.iter().map(|e| (*e, 1)).collect();
    let (_, max_cap, cut) =
        edmonds_karp::<usize, i32, _, pathfinding::directed::edmonds_karp::DenseCapacity<i32>>(
            &node_ids, &s, &t, caps,
        );
    option_assert(max_cap == 3)?;
    assert_eq!(cut.len(), 3);
    Some([cut[0].0, cut[1].0, cut[2].0])
}

fn find_sizes(nodes: &Vec<Node>, s: usize, t: usize, cut: [(usize, usize); 3]) -> (usize, usize) {
    let neighs = |id: &usize| {
        nodes[*id]
            .neighs
            .iter()
            .filter(|t| !cut.contains(&(*id, **t)) && !cut.contains(&(**t, *id)))
            .cloned()
            .collect_vec()
    };
    let r11 = bfs_reach(s, neighs)
        // .inspect(|n| print!("{:?} ", nodes[*n].old_id))
        .count();
    // println!("");
    let r12 = bfs_reach(t, neighs).count();
    return (r11, r12);
}

fn main() {
    println!("Day 25");

    let nodeid_p = parser!(alpha alpha alpha);
    let node_p = parser!(nodeid_p ": " repeat_sep(nodeid_p, " "));
    let p = parser!(lines(node_p));
    let base_nodes: Vec<(NodeId, Vec<NodeId>)> = p.parse(&crustofcode::read_input()).unwrap();
    let nodes = name_nodes(base_nodes);
    // println!("{:?}", nodes);

    // Part 1
    let mut rng = rand::thread_rng();
    let mut cut = None;
    let node_ids = (0..nodes.len()).into_iter().collect_vec();
    let caps = nodes
        .iter()
        .map(|n| n.neighs.iter().map(|e| (n.id, *e)))
        .flatten()
        .collect_vec();
    let mut s = 0;
    let mut t = 0;
    while cut.is_none() {
        s = rng.gen_range(0..nodes.len());
        t = rng.gen_range(0..nodes.len());
        cut = find_cut(&node_ids, s, t, &caps);
    }
    let (r11, r12) = find_sizes(&nodes, s, t, cut.unwrap());
    println!("{}", r11 * r12);
}
