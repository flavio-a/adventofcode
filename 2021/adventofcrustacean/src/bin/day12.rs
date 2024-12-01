use adventofcrustacean;
// use itertools::Itertools;
use std::collections::HashMap;

type Graph<'a> = HashMap<&'a str, Vec<&'a str>>;
type Visited<'a> = HashMap<&'a str, u8>;

fn parse_edge(s: &str) -> (&str, &str) {
    let mut tmp = s.split("-");
    let v1 = tmp.next().unwrap();
    let v2 = tmp.next().unwrap();
    assert_eq!(tmp.next(), None);
    return (v1, v2);
}

fn add_edge<'a>(graph: &mut Graph<'a>, start: &'a str, end: &'a str) {
    let slist = graph.entry(start).or_insert(vec![]);
    (*slist).push(end);
}

fn is_big(cave: &str) -> bool {
    cave.to_uppercase() == cave
}

fn count_paths(graph: &Graph, node: &str, visited: &mut Visited, jolly: bool) -> u32 {
    if node == "end" {
        return 1;
    }
    if !is_big(node) {
        *visited.get_mut(node).unwrap() += 1;
    }
    let res = graph
        .get(node)
        .unwrap()
        .iter()
        .map(|n| {
            if *visited.get(n).unwrap() == 0 {
                count_paths(graph, n, visited, jolly)
            } else if !jolly && *n != "start" {
                // n is visited, but I still have the jolly
                count_paths(graph, n, visited, true)
            } else {
                0
            }
        })
        .sum();
    if !is_big(node) {
        *visited.get_mut(node).unwrap() -= 1;
    }
    return res;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let edges = content.lines().map(parse_edge);
    // Build the graph
    let mut graph: Graph = HashMap::new();
    for (v1, v2) in edges {
        add_edge(&mut graph, v1, v2);
        add_edge(&mut graph, v2, v1);
    }
    // Making graph immutable from here on
    let graph = graph;

    // Part 1
    let mut visited: Visited = HashMap::new();
    for &node in graph.keys() {
        visited.insert(node, 0);
    }
    let r1 = count_paths(&graph, "start", &mut visited, true);
    println!("{}", r1);

    // Part 2
    for &node in visited.keys() {
        assert_eq!(visited.get(node), Some(&0));
    }
    let r2 = count_paths(&graph, "start", &mut visited, false);
    println!("{}", r2);
}
