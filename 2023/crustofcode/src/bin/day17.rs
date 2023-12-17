use aoc_parse::{parser, prelude::*};
use crustofcode::{option_assert, Dir, UPoint};
use itertools::Itertools;
use pathfinding::prelude::dijkstra;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Pos {
    p: UPoint,
    from: Dir,
    len: u8,
}

impl Pos {
    fn successors(&self, city: &Vec<Vec<u32>>) -> Vec<(Pos, u32)> {
        let h = city.len();
        let w = city[0].len();
        let dirs: Vec<Dir> = vec![Dir::U, Dir::D, Dir::L, Dir::R];
        return dirs
            .into_iter()
            .filter_map(|d| {
                option_assert(d != self.from.opposite())?;
                option_assert(!(self.len >= 3 && d == self.from))?;
                let newp = d.move_point(self.p, h, w)?;
                let len = if d == self.from { self.len + 1 } else { 1 };
                Some((
                    Pos {
                        p: newp,
                        from: d,
                        len: len,
                    },
                    city[newp.0][newp.1],
                ))
            })
            .collect_vec();
    }

    fn ultra_successors(&self, city: &Vec<Vec<u32>>) -> Vec<(Pos, u32)> {
        let h = city.len();
        let w = city[0].len();
        let dirs: Vec<Dir> = vec![Dir::U, Dir::D, Dir::L, Dir::R];
        return dirs
            .into_iter()
            .filter_map(|d| {
                // Can't turn too soon
                option_assert(!(self.len < 4 && d != self.from))?;
                option_assert(d != self.from.opposite())?;
                option_assert(!(self.len >= 10 && d == self.from))?;
                let newp = d.move_point(self.p, h, w)?;
                let len = if d == self.from {
                    self.len + 1
                } else {
                    // Can't stop too soon after turn: check if moving four
                    // times is still ok
                    d.move_point(d.move_point(d.move_point(newp, h, w)?, h, w)?, h, w)?;
                    1
                };
                Some((
                    Pos {
                        p: newp,
                        from: d,
                        len: len,
                    },
                    city[newp.0][newp.1],
                ))
            })
            .collect_vec();
    }
}

fn main() {
    println!("Day 17");

    let p = parser!(lines((d:digit => d.try_into().unwrap())+));
    let city: Vec<Vec<u32>> = p.parse(&crustofcode::read_input()).unwrap();

    let start: Pos = Pos {
        p: (0, 0),
        from: Dir::R,
        len: 0,
    };
    let h = city.len();
    let w = city[0].len();
    let is_end = |p: &Pos| p.p.0 == h - 1 && p.p.1 == w - 1;

    // Part 1
    let r1 = dijkstra(&start, |p| p.successors(&city), is_end);
    println!("{}", r1.unwrap().1);

    // Part 2
    let r2 = dijkstra(&start, |p| p.ultra_successors(&city), is_end);
    println!("{}", r2.unwrap().1);
}
