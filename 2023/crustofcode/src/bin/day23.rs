use std::collections::HashMap;

use aoc_parse::{parser, prelude::*};
use crustofcode::{option_assert, Dir, UPoint, ALL_DIRS};
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Cell {
    Path,
    Forest,
    Dir(Dir),
}

impl TryFrom<usize> for Cell {
    type Error = &'static str;

    fn try_from(i: usize) -> Result<Self, Self::Error> {
        match i {
            0 => Ok(Cell::Forest),
            1 => Ok(Cell::Path),
            2 => Ok(Cell::Dir(Dir::L)),
            3 => Ok(Cell::Dir(Dir::R)),
            4 => Ok(Cell::Dir(Dir::D)),
            5 => Ok(Cell::Dir(Dir::U)),
            _ => Err("Unexpected plot twist"),
        }
    }
}

impl Cell {
    fn get_dir(&self) -> Option<Dir> {
        match self {
            Cell::Dir(d) => Some(*d),
            _ => None,
        }
    }

    fn flatten(self) -> Self {
        match self {
            Cell::Forest => Cell::Forest,
            _ => Cell::Path,
        }
    }
}

type Board = Vec<Vec<Cell>>;
type Prevs = Vec<UPoint>;
type State = (UPoint, usize, Prevs);

fn successors(board: &Board, pos: UPoint, prevs: &Prevs) -> Vec<UPoint> {
    let dir = board[pos.0][pos.1].get_dir();
    if dir.is_some() {
        return dir
            .unwrap()
            .move_point(pos, board.len(), board[0].len())
            .map_or(vec![], |v| vec![v]);
    }
    assert_eq!(board[pos.0][pos.1], Cell::Path);
    ALL_DIRS
        .iter()
        .filter_map(|d| {
            let newpos = d.move_point(pos, board.len(), board[0].len())?;
            option_assert(!prevs.contains(&newpos))?;
            option_assert(board[newpos.0][newpos.1] != Cell::Forest)?;
            option_assert(board[newpos.0][newpos.1] != Cell::Dir(d.opposite()))?;
            Some(newpos)
        })
        .collect()
}

fn nexts(board: &Board, (curr, dist, mut prevs): State) -> impl Iterator<Item = State> {
    let succs = successors(board, curr, &prevs);
    if succs.len() == 1 && prevs.len() > 0 {
        let idx = prevs.len() - 1;
        prevs[idx] = curr;
    } else {
        prevs.push(curr);
    }
    succs.into_iter().map(move |p| (p, dist + 1, prevs.clone()))
}

fn explore1(board: &Board) -> usize {
    let start: UPoint = (0, board[0].iter().position(|c| *c == Cell::Path).unwrap());
    let mut stack: Vec<State> = vec![(start, 0, Vec::new())];
    let mut res: usize = 0;
    while !stack.is_empty() {
        let newstates = nexts(&board, stack.pop().unwrap()).collect_vec();
        for (curr, d, _) in &newstates {
            if curr.0 == board.len() - 1 && res < *d {
                res = *d;
            }
        }
        stack.extend(newstates);
    }
    return res;
}

type Node = (UPoint, Vec<(usize, usize)>);
type Graph = Vec<Node>;

fn explore_until(
    board: &Board,
    start: &UPoint,
    indexes: &HashMap<UPoint, usize>,
) -> Vec<(usize, usize)> {
    nexts(board, (*start, 0, Vec::new()))
        .into_iter()
        .map(|start_state| {
            let mut newstate = start_state;
            let mut nextstates = nexts(&board, newstate.clone()).collect_vec();
            while nextstates.len() == 1 {
                newstate = nextstates.pop().unwrap();
                nextstates = nexts(&board, newstate.clone()).collect_vec();
            }
            let finalstate = newstate;
            assert!(indexes.contains_key(&finalstate.0));
            assert_eq!(finalstate.2.len(), 1);
            (*indexes.get(&finalstate.0).unwrap(), finalstate.1)
        })
        .collect()
}

fn to_graph(board: &Board) -> Graph {
    let mut hm = HashMap::new();
    let mut res = vec![];
    let mut idx = 0;
    for (i, r) in board.iter().enumerate() {
        for (j, _) in r.iter().enumerate() {
            if board[i][j] == Cell::Path && successors(&board, (i, j), &Vec::new()).len() != 2 {
                res.push(((i, j), Vec::new()));
                hm.insert((i, j), idx.clone());
                assert_eq!(hm.get(&res[idx].0), Some(&idx));
                idx += 1;
                assert_eq!(res.len(), idx);
            }
        }
    }
    let indexes = hm;
    for n in &mut res {
        n.1 = explore_until(&board, &n.0, &indexes);
    }
    return res;
}

type State2 = (usize, usize, Vec<usize>);

fn nexts2(graph: &Graph, (curr, dist, mut prevs): State2) -> Vec<State2> {
    let nexts = graph[curr]
        .1
        .iter()
        .filter(|&(n, _)| !prevs.contains(n))
        .collect_vec();
    prevs.push(curr);
    nexts
        .into_iter()
        .map(|&(n, d)| (n, dist + d, prevs.clone()))
        .collect_vec()
}

fn explore2(graph: &Graph, h: usize) -> usize {
    let start: usize = graph.iter().position(|n| n.0 .0 == 0).unwrap();
    let mut stack: Vec<State2> = vec![(start, 0, Vec::new())];
    let mut res: usize = 0;
    while !stack.is_empty() {
        let newstates = nexts2(graph, stack.pop().unwrap());
        for (curr, d, _) in &newstates {
            if graph[*curr].0 .0 == h - 1 && res < *d {
                res = *d;
            }
        }
        stack.extend(newstates);
    }
    return res;
}

#[allow(dead_code)]
fn print_graph(graph: &Graph) {
    graph
        .iter()
        .enumerate()
        .for_each(|(i, n)| println!("{i} - {n:?}"));
}

fn main() {
    println!("Day 23");

    let cell_p = parser!(c:char_of("#.<>v^") => c.try_into().unwrap());
    let p = parser!(lines(cell_p+));
    let board: Board = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let r1 = explore1(&board);
    println!("{r1}");

    // Part 2
    let board: Board = board
        .into_iter()
        .map(|r| r.into_iter().map(|c| c.flatten()).collect())
        .collect();
    let graph = to_graph(&board);
    // print_graph(&graph);
    let r2 = explore2(&graph, board.len());
    println!("{r2}");
}
