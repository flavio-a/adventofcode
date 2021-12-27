use adventofcrustacean;
use itertools::Itertools;
use std::collections::HashSet;

type Coord = i64;
type Point = [Coord; 3];

#[allow(dead_code)]
fn print_point(p: &Point) {
    println!("({}, {}, {})", p[0], p[1], p[2]);
}

#[inline(always)]
fn times_scalar(v: Coord, p: Point) -> Point {
    [p[0] * v, p[1] * v, p[2] * v]
}

#[inline(always)]
fn sum_points(p1: Point, p2: Point) -> Point {
    [p1[0] + p2[0], p1[1] + p2[1], p1[2] + p2[2]]
}

#[inline(always)]
fn diff_points(p1: Point, p2: Point) -> Point {
    sum_points(p1, times_scalar(-1, p2))
}

fn parse_point(s: &str) -> Point {
    let vals: Vec<i64> = s.split(",").map(adventofcrustacean::str2int).collect();
    assert_eq!(vals.len(), 3);
    vals.try_into().unwrap()
}

// Describing a rotation: the first two values describe which axis is x (+0,
// -0, +1, -1, +2, -2), and the second how much should I rotate the plane yz
// (from 0 to 4). I've no fucking idea of what does a specific r means, but I
// don't care since all I care about is to be able to generate all rotations
// and apply a rotation to a point in a deterministic way
type Rotation = (u8, i8, u8);

// Applies the rotation r to p
fn rotate_point(r: &Rotation, p: Point) -> Point {
    let mut res: Point = [0; 3];
    // Permute the three axes by r.0
    let valx: usize = r.0.try_into().unwrap();
    for i in 0..3 {
        res[i] = p[(i + valx) % 3];
    }
    // Adjust sign of res[x] axis, arbitrarly rotating around res[z]
    if r.1 == -1 {
        res = [-res[0], -res[1], res[2]];
    }

    // Here r.0 can be any of the six +-p[0,1,2]
    // Rotate plane yz
    match r.2 {
        0 => { },
        1 => { res = [res[0], -res[2],  res[1]]; },
        2 => { res = [res[0], -res[1], -res[2]]; },
        3 => { res = [res[0],  res[2], -res[1]]; },
        _ => panic!("Unexpected value for rotation of y plane: {}", r.2),
    };
    res
}

const ALL_ROTATIONS: [Rotation; 24] = [
    (0,  1, 0), (0,  1, 1), (0,  1, 2), (0,  1, 3),
    (0, -1, 0), (0, -1, 1), (0, -1, 2), (0, -1, 3),
    (1,  1, 0), (1,  1, 1), (1,  1, 2), (1,  1, 3),
    (1, -1, 0), (1, -1, 1), (1, -1, 2), (1, -1, 3),
    (2,  1, 0), (2,  1, 1), (2,  1, 2), (2,  1, 3),
    (2, -1, 0), (2, -1, 1), (2, -1, 2), (2, -1, 3),
];

fn parse_scanners(mut lines: std::str::Lines) -> Vec<Vec<Point>> {
    let scanner_lines: Vec<&str> = lines.by_ref().take_while(|s| !s.is_empty()).collect();
    if scanner_lines.len() == 0 {
        return vec![];
    }
    let mut scanner_lines = scanner_lines.into_iter();
    assert!(scanner_lines.next().unwrap().starts_with("--- scanner "));
    let scanner = scanner_lines.map(parse_point).collect();

    let mut scanners = parse_scanners(lines);
    scanners.insert(0, scanner);
    return scanners;
}

#[allow(dead_code)]
fn print_scanner(scanner: &Vec<Point>) {
    for point in scanner {
        print_point(point);
    }
}

type DistList = HashSet<Point>;

fn make_distances_list(scanner: &Vec<Point>) -> Vec<DistList> {
    let mut res = vec![];
    // For each beacon, compute the set of differences
    for beacon1 in scanner.iter() {
        let mut b1set = HashSet::new();
        for (_, beacon2) in scanner.iter().enumerate() {
            // Position of 2 relative to 1: beacon2 == beacon1 + diff
            let diff = diff_points(*beacon2, *beacon1);
            if diff != [0, 0, 0] {
                b1set.insert(diff);
            }
        }
        res.push(b1set);
    }
    return res;
}

// Given two scanners, find position and rotation of the second with respect to
// the fisrt or None. Actually it returns the rotation and a vector of
// matchings, that is a mapping from indexes in the first beacon to indexes in
// the second. Hopefully this works, because I'm making assumptions on the
// input. If it doesn't, shit
fn compare_scanners(sc1: &Vec<DistList>, sc2: &Vec<DistList>) -> Option<(Rotation, Vec<Option<usize>>)> {
    // Fix the rotation for sc2
    for rot in ALL_ROTATIONS {
        let rot_sc2: &Vec<DistList> = &sc2.iter().map(|dl| dl.iter().map(|&p| rotate_point(&rot, p)).collect()).collect();
        // For all distlists in sc1, search a matching distlist in sc2
        let mut matchings: Vec<Option<usize>> = vec![None; sc1.len()];
        for (i, dl_1) in sc1.iter().enumerate() {
            let possible_matches: Vec<usize> = rot_sc2.iter()
                .enumerate()
                .filter(|&(_, dl_2)| dl_1.intersection(dl_2).count() >= 11)
                .map(|(j, _)| j)
                .collect();
            if possible_matches.len() >= 2 {
                panic!("Shit, too much possible matches");
            }
            else if possible_matches.len() == 1 {
                matchings[i] = Some(possible_matches[0]);
            }
        }
        if matchings.iter().filter(|&v| v.is_some()).count() >= 12 {
            // Match!
            return Some((rot, matchings));
        }
    }
    return None;
}

// Given two scanners and the result of
// compare_scanners(make_distances_list(sc1), make_distances_list(sc2)) returns
// the relative position of sc2 with respect to sc1
fn relative_pos(sc1: &Vec<Point>, sc2: &Vec<Point>, rot: &Rotation, matchings: &Vec<Option<usize>>) -> Point {
    let (idx1, optidx2) = matchings.iter().enumerate().find(|&(_, v)| v.is_some()).unwrap();
    let bc1 = sc1[idx1];
    let bc2 = rotate_point(rot, sc2[optidx2.unwrap()]);
    // Here pos(sc1) + bc1 = pos(sc2) + bc2
    return diff_points(bc1, bc2);
}


fn manh_dist(p1: &Point, p2: &Point) -> i64 {
    (0..3).map(|i| (p1[i] - p2[i]).abs()).sum()
}

fn main() {
    let scanners_orig = parse_scanners(adventofcrustacean::read_input().lines());

    // Part 1
    let mut scanners = scanners_orig.clone();

    // For each scanner, build a list of (vector) distances between beacons in
    // that scanner. There is one list per beacon, containing the distances. I
    // know that intersecting these isn't exactly what the problem asks for,
    // but let's suppose it's equivalent.
    let mut beacons_dists: Vec<Vec<DistList>> = scanners.iter().map(make_distances_list).collect();

    // Set of scanners to assign
    let mut to_assign: Vec<usize> = vec![];
    for i in 1..scanners.len() {
        to_assign.push(i);
    }
    // Set of assigned scanners. When a scanner is assigned, records it
    // position wrt scanner 0, and modify scanners and beacons_dists to be
    // rotated accordingly
    let mut assigned: Vec<Option<Point>> = vec![None; scanners.len()];
    assigned[0] = Some([0, 0, 0]);
    // Repeat until the set of scanners to assign is empty
    let mut i = 0;
    while to_assign.len() > 0 {
        i = (i + 1) % to_assign.len();
        let round_idx = to_assign[i];
        // Check the round scanner against all scanners already assigned
        for check_idx in 0..assigned.len() {
            if assigned[check_idx].is_some() {
                let cusu = compare_scanners(&beacons_dists[check_idx], &beacons_dists[round_idx]);
                if cusu.is_some() {
                    let (rot, matchings) = cusu.unwrap();
                    let relpos = relative_pos(&scanners[check_idx], &scanners[round_idx], &rot, &matchings);
                    // relpos is the position of round_idx wrt check_idx, that
                    // in general isn't scanner 0, so we have to adjust it.
                    // Note that rotations instead are already fixed since we
                    // keep everything rotated as scanner 0
                    let relpos = sum_points(assigned[check_idx].unwrap(), relpos);

                    // Before storing the finding in assigned, it rotates the
                    // entry in beacons_dists and scanners
                    beacons_dists[round_idx] = beacons_dists[round_idx].iter()
                        .map(|dl| dl.into_iter().map(|&p| rotate_point(&rot, p)).collect())
                        .collect();
                    scanners[round_idx] = scanners[round_idx].iter()
                        .map(|&b| rotate_point(&rot, b))
                        .collect();
                    assigned[round_idx] = Some(relpos);
                    to_assign[i] = assigned.len();
                    // Assigned: no need to check other indexes
                    println!("Matched scanner {} with {}", round_idx, check_idx);
                    break;
                }
            }
        }
        to_assign = to_assign.into_iter().filter(|&v| v < assigned.len()).collect();
    }
    let assigned: Vec<Point> = assigned.into_iter().map(|v| v.unwrap()).collect();

    // for (idx, relpos) in assigned.iter().enumerate() {
    //     print!("Position of scanner {} relative to scanner 0: ", idx);
    //     print_point(&relpos);
    // }

    // Now I only have to count beacons
    let mut beacons: HashSet<Point> = HashSet::new();
    for (idx, sc) in scanners.iter().enumerate() {
        for &bc in sc {
            let bc = sum_points(bc, assigned[idx]);
            beacons.insert(bc);
        }
    }
    println!("{}", beacons.iter().count());

    // Part 2
    println!("{}",
        assigned.iter().cartesian_product(assigned.iter())
        .map(|(sc1, sc2)| manh_dist(sc1, sc2))
        .max().unwrap()
    );
}
