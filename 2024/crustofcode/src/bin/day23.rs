use std::collections::{HashMap, HashSet};

use aoc_parse::{parser, prelude::*};
use itertools::Itertools;

type NodeLabel = [char; 2];

// Returns the maximum clique containing the given clique
fn find_maximum_clique(
    clique: &mut Vec<usize>,
    adjacencies: Vec<usize>,
    adjacency_lists: &Vec<HashSet<usize>>,
    mut current_bound: usize,
) -> Vec<usize> {
    let mut res = clique.clone();
    // Branch-and-bound-ish: if current size of clique + possible adjacencies
    // are at most the size of the best clique found yet, cut the branch
    if clique.len() + adjacencies.len() <= current_bound {
        return res;
    }
    for &new_node in &adjacencies {
        clique.push(new_node);
        let new_adjacencies = adjacencies
            .iter()
            .filter(|n| adjacency_lists[new_node].contains(n))
            // Monotonicity: only adds to the clique nodes in increasing order
            .filter(|n| **n > new_node)
            .cloned()
            .collect();
        let new_clique =
            find_maximum_clique(clique, new_adjacencies, adjacency_lists, current_bound);
        assert_eq!(clique.pop(), Some(new_node));
        if new_clique.len() > res.len() {
            current_bound = new_clique.len();
            res = new_clique;
        }
    }
    return res;
}

fn print_clique<'a, T: Iterator<Item = &'a usize>>(
    clique: T,
    id_to_labels: &Vec<NodeLabel>,
) -> String {
    let nodes = clique
        .map(|id| id_to_labels[*id].iter().collect::<String>())
        .sorted();
    itertools::Itertools::intersperse(nodes, ",".to_string()).collect()
}

fn main() {
    println!("Day 23");

    let p = parser!(lines({a:alpha b:alpha => [a, b]} "-" {a:alpha b:alpha => [a, b]}));
    let label_links: Vec<(NodeLabel, NodeLabel)> = p.parse(&crustofcode::read_input()).unwrap();
    let mut idx: usize = 0;
    let mut labels_to_id = HashMap::<NodeLabel, usize>::new();
    let mut id_to_labels: Vec<NodeLabel> = vec![];
    let mut adjacency_lists: Vec<HashSet<usize>> = vec![];
    for (l1, l2) in &label_links {
        assert_ne!(l1, l2);
        let l1idx = if labels_to_id.contains_key(l1) {
            *labels_to_id.get(l1).unwrap()
        } else {
            labels_to_id.insert(*l1, idx);
            id_to_labels.push(*l1);
            adjacency_lists.push(HashSet::new());
            idx += 1;
            idx - 1
        };
        let l2idx = if labels_to_id.contains_key(l2) {
            *labels_to_id.get(l2).unwrap()
        } else {
            labels_to_id.insert(*l2, idx);
            id_to_labels.push(*l2);
            adjacency_lists.push(HashSet::new());
            idx += 1;
            idx - 1
        };
        adjacency_lists[l1idx].insert(l2idx);
        adjacency_lists[l2idx].insert(l1idx);
    }
    let adjacency_lists = adjacency_lists;
    let id_to_labels = id_to_labels;
    let num_nodes = id_to_labels.len();

    // Part 1
    let mut res1 = 0;
    for node1 in 0..num_nodes {
        for &node2 in &adjacency_lists[node1] {
            for &node3 in &adjacency_lists[node2] {
                if adjacency_lists[node1].contains(&node3) {
                    if [
                        id_to_labels[node1],
                        id_to_labels[node2],
                        id_to_labels[node3],
                    ]
                    .into_iter()
                    .any(|l| l[0] == 't')
                    {
                        res1 += 1;
                    }
                }
            }
        }
    }
    assert_eq!(res1 % 6, 0);
    let res1 = res1 / 6;
    println!("{res1}");

    // Part 2
    let clique = find_maximum_clique(
        &mut Vec::new(),
        (0..num_nodes).collect(),
        &adjacency_lists,
        1,
    );
    println!("{}", print_clique(clique.iter(), &id_to_labels));
}
