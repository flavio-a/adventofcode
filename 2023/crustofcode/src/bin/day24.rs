use aoc_parse::{parser, prelude::*};

type Point3D = (i64, i64, i64);
struct Hailstone {
    pos: Point3D,
    vel: Point3D,
}

impl From<(i64, i64, i64, i64, i64, i64)> for Hailstone {
    fn from(value: (i64, i64, i64, i64, i64, i64)) -> Self {
        Hailstone {
            pos: (value.0, value.1, value.2),
            vel: (value.3, value.4, value.5),
        }
    }
}

fn get_intersection_2_d(h1: &Hailstone, h2: &Hailstone) -> (f64, f64, f64, f64) {
    // \left( {\frac{{{b_1}{c_2} - {b_2}{c_1}}}{{{a_1}{b_2} - {a_2}{b_1}}},\frac{{{c_1}{a_2} - {c_2}{a_1}}}{{{a_1}{b_2} - {a_2}{b_1}}}} \right)
    let x1 = h1.pos.0 as f64;
    let y1 = h1.pos.1 as f64;
    let vx1 = h1.vel.0 as f64;
    let vy1 = h1.vel.1 as f64;
    let x2 = h2.pos.0 as f64;
    let y2 = h2.pos.1 as f64;
    let vx2 = h2.vel.0 as f64;
    let vy2 = h2.vel.1 as f64;
    let dx = x1 - x2;
    let dy = y1 - y2;
    let det = vy2 * vx1 - vx2 * vy1;
    let t1: f64 = (vx2 * dy - vy2 * dx) / det;
    let t2: f64 = (vx1 * dy - vy1 * dx) / det;
    (t1, t2, x2 + vx2 * t2, y2 + vy2 * t2)
}

fn count_intersections_2_d(hailstones: &Vec<Hailstone>, low: f64, high: f64) -> usize {
    hailstones
        .iter()
        .enumerate()
        .flat_map(|(i, val)| std::iter::repeat(val).zip(hailstones.iter().skip(i + 1)))
        .filter(|(h1, h2)| {
            let (t1, t2, x, y) = get_intersection_2_d(h1, h2);
            t1 >= 0.0 && t2 >= 0.0 && (low <= x && x <= high) && (low <= y && y <= high)
        })
        .count()
}

fn main() {
    println!("Day 24");

    let hailstone_p = parser!(p:(i64 ", " i64 ", " i64 " @ " i64 ", " i64 ", " i64) => p.into());
    let p = parser!(lines(hailstone_p));
    let hailstones: Vec<Hailstone> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let r1 = count_intersections_2_d(&hailstones, 200000000000000.0, 400000000000000.0);
    println!("{r1}");

    // Part 2
}
