use crustofcode::*;
// use itertools::Itertools;
use pathfinding::directed::dijkstra;
use std::cmp;
use std::collections::HashMap;

type NodeName = String;
type NodeId = usize;

// A node with names
#[derive(Debug)]
struct NodeS {
    name: NodeName,
    flow: i64,
    neighbours: Vec<NodeName>,
}

// A node with indices
#[derive(Debug)]
struct NodeI {
    name: NodeName,
    id: NodeId,
    flow: i64,
    neighbours: Vec<NodeId>,
}

// A final node, in the complete graph without the 0 flow entries
#[derive(Debug)]
struct Node {
    name: NodeName,
    flow: i64,
    distances: Vec<i64>,
}

fn parse_line(line: String) -> NodeS {
    // Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
    let line = line
        .replace("Valve ", "")
        .replace(" has flow rate=", ", ")
        .replace(" tunnels lead to valves ", "")
        .replace(" tunnel leads to valve ", "");
    let mut t = line.split(";");
    let (name, flow) = match t.next().unwrap().split(", ").collect::<Vec<&str>>()[..] {
        [name, flow] => (name.to_owned(), str2int(flow)),
        _ => panic!("AAAAA"),
    };
    let neighbours: Vec<NodeName> = t
        .next()
        .unwrap()
        .split(", ")
        .map(|s| s.to_owned())
        .collect();
    assert_eq!(t.next(), None);
    return NodeS {
        name: name,
        flow: flow,
        neighbours: neighbours,
    };
}

fn get_graph(nodes: Vec<NodeS>) -> (Vec<Node>, Vec<NodeId>) {
    // println!("-------- Nodes -------");
    // println!("{:?}", nodes);
    let mut indices: HashMap<NodeName, NodeId> = HashMap::new();
    for (idx, node) in nodes.iter().enumerate() {
        indices.insert(node.name.clone(), idx);
    }
    // println!("-------- Indices -------");
    // println!("{:?}", indices);
    let nodes: Vec<NodeI> = nodes
        .into_iter()
        .enumerate()
        .map(|(idx, ns)| {
            assert_eq!(&idx, indices.get(&ns.name).unwrap());
            NodeI {
                id: idx,
                name: ns.name,
                flow: ns.flow,
                neighbours: ns
                    .neighbours
                    .into_iter()
                    // .map(|n| *indices.get(&n).unwrap())
                    .map(|n| {
                        let a = indices.get(&n);
                        if a.is_none() {
                            println!("Error: {}", n)
                        };
                        *a.unwrap()
                    })
                    .collect(),
            }
        })
        .collect();
    // Now I compute the distances between non 0-flow nodes
    let node_indices = nodes.iter().filter(|n| n.flow > 0).map(|n| n.id).collect();
    let mut final_nodes = vec![];
    for node in &nodes {
        let reachables = dijkstra::dijkstra_all(&node.id, |&i| {
            nodes[i].neighbours.iter().map(|&v| (v, 1_i64))
        });
        let mut distances: Vec<i64> = vec![-1; nodes.len()];
        for (node, (_, d)) in reachables.into_iter() {
            if nodes[node].flow > 0 {
                distances[node] = d;
            }
        }
        final_nodes.push(Node {
            name: node.name.clone(),
            flow: node.flow,
            distances: distances,
        });
    }
    return (final_nodes, node_indices);
}

#[allow(dead_code)]
fn compute_pressure(start: &NodeId, perm: Vec<&NodeId>, nodes: &Vec<Node>) -> i64 {
    let mut time = 30;
    let mut opened = 0;
    let mut res = 0;
    let mut curr_node = &nodes[*start];
    for target in perm {
        let d = curr_node.distances[*target];
        if d + 1 > time {
            res += opened * time;
            return res;
        }
        time -= d + 1;
        res += opened * (d + 1);
        let target_node = &nodes[*target];
        opened += target_node.flow;
        curr_node = target_node;
    }
    res += opened * time;
    return res;
}

#[derive(Debug)]
struct ExplState {
    curr: NodeId,
    time: i64,
    opened: i64,
    released: i64,
    visited: Vec<bool>,
}

fn explore(nodes: &Vec<Node>, node_indices: &Vec<NodeId>, state: &mut ExplState) -> i64 {
    let curr = state.curr;
    let curr_node = &nodes[curr];
    let mut res = 0;
    let mut finished = true;
    for target in node_indices {
        if !(state.visited[*target]) {
            finished = false;
            // Visit that node
            let d = curr_node.distances[*target] + 1;
            if d > state.time {
                // You don't reach the node in time
                let tmp = state.released + state.opened * state.time;
                res = cmp::max(res, tmp);
            } else {
                state.time -= d;
                state.released += state.opened * d;
                state.opened += nodes[*target].flow;
                state.curr = *target;
                state.visited[*target] = true;
                // Recursive exploration of target
                let tmp = explore(nodes, node_indices, state);
                res = cmp::max(res, tmp);
                // Cleanup
                state.visited[*target] = false;
                state.curr = curr; // This is probably useless
                state.opened -= nodes[*target].flow;
                state.released -= state.opened * d;
                state.time += d;
            }
            assert_ne!(res, 0);
        }
    }
    if finished {
        // Visited all nodes, you only have to wait for time to finish.
        assert_eq!(res, 0);
        res = state.released + state.opened * state.time;
    }
    return res;
}

fn main() {
    println!("Day 16");

    let nodes: Vec<NodeS> = read_input_lines().into_iter().map(parse_line).collect();
    let (nodes, node_indices) = get_graph(nodes);

    println!("Number of nonzero nodes: {}", node_indices.len());
    let start_idx: NodeId = nodes.iter().position(|n| n.name == "AA").unwrap();
    println!("Starting idx: {}", start_idx);
    // println!("{:?}", node_indices.iter().map(|&i| (i, &nodes[i].name)).collect::<Vec<(NodeId, &NodeName)>>());
    // for node in &nodes {
    //     if node.flow > 0 {
    //         println!("{} ({})", node.name, node.flow);
    //         println!("{:?}", node.distances.iter().filter(|(i, _)| node_indices.iter().find(|v| v == i).is_some()).map(|(i, (_, d))| (&nodes[*i].name, d)).collect::<Vec<(&NodeName, &i64)>>())
    //     };
    // };

    // Part 1
    let mut initial_state = ExplState {
        curr: start_idx,
        time: 30,
        opened: 0,
        released: 0,
        visited: vec![false; nodes.len()],
    };
    println!("{}", explore(&nodes, &node_indices, &mut initial_state));

    // Part 2
    println!("{}", 0);
}
