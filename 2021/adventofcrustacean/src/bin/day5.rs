use adventofcrustacean;
use std::collections::HashMap;

type Point = (i64, i64);
type Line = (Point, Point);

fn parse_point(s: &str) -> Point {
    let mut cusu = s.trim().split(',').map(adventofcrustacean::str2int);
    (cusu.next().unwrap(), cusu.next().unwrap())
}

fn parse_line(line: &str) -> Line {
    let mut cusu = line.trim().split("->").map(parse_point);
    (cusu.next().unwrap(), cusu.next().unwrap())
}

fn is_ortho(line: &Line) -> bool {
    line.0.0 == line.1.0 || line.0.1 == line.1.1
}

fn sum_points(p1: &Point, p2: &Point) -> Point {
    (p1.0 + p2.0, p1.1 + p2.1)
}

fn points_of(line: &Line) -> Vec<Point> {
    let mut points = vec![line.0];
    let dir = (
        if line.0.0 == line.1.0 { 0 } else { if line.0.0 > line.1.0 { -1 } else { 1 } },
        if line.0.1 == line.1.1 { 0 } else { if line.0.1 > line.1.1 { -1 } else { 1 } }
    );
    let mut last_pt = line.0;
    while last_pt != line.1 {
        last_pt = sum_points(&last_pt, &dir);
        points.push(last_pt);
    }
    points
}

fn main() {
    let content = adventofcrustacean::read_input();
    let lines = content.lines().map(parse_line).collect::<Vec<Line>>();
    // println!("{:?}", lines.collect::<Vec<Line>>());

    // Part 1
    let mut board = HashMap::new();
    for line in lines.iter().filter(|&l| is_ortho(l)) {
        // println!("{:?}", line);
        for point in points_of(&line).iter() {
            // println!("{:?}", point);
            *board.entry(*point).or_insert(0) += 1
        }
    }
    let res1 = board.values().filter(|&&v| v > 1).count();
    println!("{}", res1);

    // Part 2
    // Only adding diagonal lines to board
    for line in lines.iter().filter(|&l| !is_ortho(l)) {
        for point in points_of(&line).iter() {
            *board.entry(*point).or_insert(0) += 1
        }
    }
    let res2 = board.values().filter(|&&v| v > 1).count();
    println!("{}", res2);
}