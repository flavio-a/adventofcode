use aoc_parse::{parser, prelude::*};
use crustofcode::{Dir, UPoint};

#[derive(Debug, Clone, Copy)]
struct Instruction {
    dir: Dir,
    len: usize,
}

fn idx2dir(i: usize) -> Dir {
    match i {
        0 => Dir::R,
        1 => Dir::D,
        2 => Dir::L,
        3 => Dir::U,
        _ => panic!("Unexpected plot twist"),
    }
}

fn hex2usize(hex: (usize, usize, usize, usize, usize)) -> usize {
    let hex: [usize; 5] = hex.into();
    let mut res = 0;
    for d in hex {
        res = d + 16 * res;
    }
    return res;
}

fn get_size(instructions: &Vec<Instruction>) -> (UPoint, UPoint) {
    let ((wp, hp), (wm, hm)): ((i32, i32), (i32, i32)) = instructions
        .iter()
        .map(|i| {
            let len: i32 = i32::try_from(i.len).unwrap();
            match i.dir {
                Dir::L => (-len, 0),
                Dir::R => (len, 0),
                Dir::U => (0, -len),
                Dir::D => (0, len),
            }
        })
        .fold(((0, 0), ((0, 0), (0, 0))), |(curr, (resp, resm)), mov| {
            (
                (curr.0 + mov.0, curr.1 + mov.1),
                (
                    (
                        std::cmp::max(curr.0 + mov.0, resp.0),
                        std::cmp::max(curr.1 + mov.1, resp.1),
                    ),
                    (
                        std::cmp::min(curr.0 + mov.0, resm.0),
                        std::cmp::min(curr.1 + mov.1, resm.1),
                    ),
                ),
            )
        })
        .1;
    assert!(wp >= 0 && hp >= 0);
    assert!(wm <= 0 && hm <= 0);
    let start: UPoint = (usize::try_from(-hm).unwrap(), usize::try_from(-wm).unwrap());
    let size: UPoint = (
        usize::try_from(wp - wm).unwrap() + 1,
        usize::try_from(hp - hm).unwrap() + 1,
    );
    return (start, size);
}

// ---------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq)]
struct Edge {
    y: u32,
    len: u32,
    cutting: bool,
}

fn instrs2edge(instrs: &[Instruction], pos: UPoint) -> (Vec<(usize, Edge)>, UPoint) {
    assert_eq!(instrs.len(), 3);
    let y0: u32 = pos.1.try_into().unwrap();
    let len: u32 = instrs[1].len.try_into().unwrap();
    match instrs[1].dir {
        Dir::U => {
            let mut edges = vec![];
            for x in pos.0 - instrs[1].len + 1..pos.0 {
                edges.push((
                    x,
                    Edge {
                        y: y0,
                        len: 1,
                        cutting: true,
                    },
                ));
            }
            (edges, (pos.0 - instrs[1].len, pos.1))
        }
        Dir::D => {
            let mut edges = vec![];
            for x in pos.0 + 1..pos.0 + instrs[1].len {
                edges.push((
                    x,
                    Edge {
                        y: y0,
                        len: 1,
                        cutting: true,
                    },
                ));
            }
            (edges, (pos.0 + instrs[1].len, pos.1))
        }
        Dir::L => (
            vec![(
                pos.0,
                Edge {
                    y: y0 - len,
                    len: len + 1,
                    cutting: instrs[0].dir == instrs[2].dir,
                },
            )],
            (pos.0, pos.1 - instrs[1].len),
        ),
        Dir::R => (
            vec![(
                pos.0,
                Edge {
                    y: y0,
                    len: len + 1,
                    cutting: instrs[0].dir == instrs[2].dir,
                },
            )],
            (pos.0, pos.1 + instrs[1].len),
        ),
    }
}

fn store_edges(
    instructions: &Vec<Instruction>,
    start: &UPoint,
    &(_, h): &UPoint,
) -> Vec<Vec<Edge>> {
    // edges[i] contains all the edges relative to row i
    let mut edges = vec![vec![]; h];
    let mut pos = *start;
    {
        let (newedges, newpos) = instrs2edge(
            &[
                instructions[instructions.len() - 1],
                instructions[0],
                instructions[1],
            ],
            pos,
        );
        for (x, newedge) in newedges {
            edges[x].push(newedge);
        }
        pos = newpos;
    }
    for is in instructions.windows(3) {
        let (newedges, newpos) = instrs2edge(is, pos);
        for (x, newedge) in newedges {
            edges[x].push(newedge);
        }
        pos = newpos;
    }
    {
        let (newedges, _) = instrs2edge(
            &[
                instructions[instructions.len() - 2],
                instructions[instructions.len() - 1],
                instructions[0],
            ],
            pos,
        );
        for (x, newedge) in newedges {
            edges[x].push(newedge);
        }
    }
    for i in 0..edges.len() {
        edges[i].sort_by_key(|e| e.y);
        edges[i].dedup();
    }
    return edges;
}

fn count_row(row: &Vec<Edge>) -> u64 {
    let mut count: u64 = 0;
    let mut last_y: Option<u64> = None;
    for e in row {
        // Add cells in the edge
        count += u64::from(e.len);
        // inside stuff
        if last_y.is_some() {
            assert!(last_y.is_some());
            // inside: add the segment until now
            count += u64::from(e.y) - last_y.unwrap();
            if e.cutting {
                // inside and cutting: get outside
                last_y = None;
            } else {
                // inside but not cutting: update last_y but remains in
                last_y = Some((e.y + e.len).into());
            }
        } else {
            assert!(last_y.is_none());
            if e.cutting {
                // outside and cutting: enters
                last_y = Some((e.y + e.len).into());
            } else {
                // outside and not cutting: nothing happens
            }
        }
    }
    assert!(last_y.is_none());
    return count;
}

fn count_interior(edges: &Vec<Vec<Edge>>) -> u64 {
    edges.iter().map(count_row).sum()
}

fn main() {
    println!("Day 18");

    // Part 1
    let instruction1_p = parser!(d:char_of("RDLU")" " l:usize " (#" (digit_hex digit_hex digit_hex digit_hex digit_hex digit_hex) ")" => Instruction {
        dir: idx2dir(d),
        len: l,
    });
    let p = parser!(lines(instruction1_p));
    let instructions: Vec<Instruction> = p.parse(&crustofcode::read_input()).unwrap();

    let (start, size) = get_size(&instructions);
    let edges = store_edges(&instructions, &start, &size);
    // println!("{:?}", edges);
    let r1 = count_interior(&edges);
    println!("{r1}");

    // Part 2
    let instruction2_p = parser!(char_of("RDLU")" " usize " (#" hex:(digit_hex digit_hex digit_hex digit_hex digit_hex) d:digit ")" => Instruction {
        dir: idx2dir(d),
        len: hex2usize(hex),
    });
    let p = parser!(lines(instruction2_p));
    let instructions: Vec<Instruction> = p.parse(&crustofcode::read_input()).unwrap();

    let size_edge = std::mem::size_of::<Edge>();
    let num_edges: usize = instructions.iter().map(|i| i.len).sum();
    println!(
        "{size_edge} (edge) * {num_edges} = {}MiB",
        (size_edge * num_edges / 1024) / 1024
    );

    let (start, size) = get_size(&instructions);
    let edges = store_edges(&instructions, &start, &size);
    let r2 = count_interior(&edges);
    println!("{r2}");
}
