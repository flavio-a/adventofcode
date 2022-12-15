use crustofcode::*;
use itertools::*;
use std::cmp;

const ROW: i64 = 2000000;

type Range = (i64, i64);
type Sensor = (Point, Point);

fn parse_coord(s: &str) -> Point {
    // x=2, y=18
    let mut p = s.split(", ");
    let x = str2int(drop_prefix(p.next().unwrap(), "x="));
    let y = str2int(drop_prefix(p.next().unwrap(), "y="));
    return (x, y);
}

fn parse_line(line: String) -> Sensor {
    let mut p = line.split(':');
    let sensors = parse_coord(drop_prefix(p.next().unwrap(), "Sensor at "));
    let beacon = parse_coord(drop_prefix(p.next().unwrap(), " closest beacon is at "));
    return (sensors, beacon);
}

// Given a sensor and a line (a value of y), returns the interval "covered" by
// that sensor
fn sensor_interval(((sx, sy), (bx, by)): &Sensor, y: i64) -> Option<Range> {
    let dist = (sx - bx).abs() + (sy - by).abs();
    let delta = dist - (sy - y).abs();
    if delta < 0 {
        None
    } else {
        Some((sx - delta, sx + delta))
    }
}

fn scan_line(sensors: &Vec<Sensor>, y: i64) -> Option<i64> {
    // println!("------- Row {} -------", y);
    let ranges: Vec<Range> = sensors
        .iter()
        .filter_map(|s| sensor_interval(s, y))
        .sorted_by_key(|(s, _)| s.clone())
        .collect();
    // println!("{:?}", ranges);
    assert!(ranges[0].0 <= 0);
    let mut xmax = ranges[0].1;
    let mut i = 1;
    while i < ranges.len() && ranges[i].0 <= xmax + 1 {
        xmax = cmp::max(xmax, ranges[i].1);
        i += 1;
    }
    if i == ranges.len() && xmax >= ROW * 2 {
        return None;
    } else {
        // println!("Matching {}", xmax + 1);
        return Some(xmax + 1);
    };
}

fn main() {
    println!("Day 15");

    let sensors: Vec<Sensor> = read_input_lines().into_iter().map(parse_line).collect();

    // Part 1
    // I'm assuming all the intervals overlap
    let ranges: Vec<Range> = sensors
        .iter()
        .filter_map(|s| sensor_interval(s, ROW))
        .collect();
    println!(
        "{}",
        (ranges.iter().map(|(_, e)| e).max().unwrap()
            - ranges.iter().map(|(s, _)| s).min().unwrap())
    );

    // Part 2
    let possibles: Vec<Point> = (0..=ROW * 2)
        .filter_map(|y| Some((scan_line(&sensors, y)?, y)))
        .collect();
    assert_eq!(possibles.len(), 1);
    println!("{}", possibles[0].0 * 4000000 + possibles[0].1);
}
