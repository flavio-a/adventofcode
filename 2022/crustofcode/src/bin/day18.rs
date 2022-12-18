use crustofcode::*;
use pathfinding::undirected::connected_components;

type Pos = [usize; 3];

fn parse_line(s: String) -> Pos {
    let mut i = s
        .split(',')
        .take(4)
        .map(|s| s.parse::<usize>().expect("AAAA") + 1);
    // Makes sure all coords are > 0
    let res = [i.next().unwrap(), i.next().unwrap(), i.next().unwrap()];
    assert_eq!(i.next(), None);
    return res;
}

fn max_coord(axis: usize, v: &Vec<Pos>) -> usize {
    v.iter().map(|a| a[axis]).max().unwrap()
}

fn all_neighbours(&[x, y, z]: &Pos) -> [Pos; 6] {
    [
        [x - 1, y, z],
        [x + 1, y, z],
        [x, y + 1, z],
        [x, y - 1, z],
        [x, y, z + 1],
        [x, y, z - 1],
    ]
}

fn neighbours(&[x, y, z]: &Pos, maxes: &Vec<usize>) -> Vec<Pos> {
    all_neighbours(&[x, y, z])
        .into_iter()
        .filter(|&[x, y, z]| !(x >= maxes[0] || y >= maxes[1] || z >= maxes[2]))
        .collect()
}

fn full_neighbours(cube: &Pos, maxes: &Vec<usize>, space: &Vec<Vec<Vec<bool>>>) -> usize {
    neighbours(cube, maxes)
        .into_iter()
        .filter(|&[x, y, z]| space[x][y][z])
        .count()
}

fn main() {
    println!("Day 18");

    let cubes: Vec<Pos> = read_input_lines().into_iter().map(parse_line).collect();
    // Put the cubes in a 3D space
    let maxes: Vec<usize> = (0..=2).map(|axis| max_coord(axis, &cubes) + 1).collect();
    let mut space: Vec<Vec<Vec<bool>>> = vec![vec![vec![false; maxes[2]]; maxes[1]]; maxes[0]];
    for &[x, y, z] in &cubes {
        space[x][y][z] = true;
    }
    println!("Bounds: {}, {}, {}", maxes[0], maxes[1], maxes[2]);

    // Part 1
    println!(
        "{}",
        cubes
            .iter()
            .map(|cube| 6 - full_neighbours(cube, &maxes, &space))
            .sum::<usize>()
    );

    // Part 2
    let mut groups: Vec<Vec<Pos>> = vec![];
    for x in 1..maxes[0] {
        for y in 1..maxes[1] {
            for z in 1..maxes[2] {
                if !space[x][y][z] {
                    let mut g: Vec<Pos> = all_neighbours(&[x, y, z])
                        .into_iter()
                        .filter(|&[x, y, z]| {
                            !(x < maxes[0] && y < maxes[1] && z < maxes[2] && space[x][y][z])
                        })
                        .collect();
                    g.push([x, y, z]);
                    groups.push(g);
                }
            }
        }
    }
    let (v2i, _) = connected_components::separate_components(&groups);
    let outside_group = v2i.get(&[1, 1, 0]).unwrap();
    for x in 1..maxes[0] {
        for y in 1..maxes[1] {
            for z in 1..maxes[2] {
                if v2i.get(&[x, y, z]).unwrap_or(outside_group) != outside_group {
                    space[x][y][z] = true;
                } else {
                    assert!(!space[x][y][z] || v2i.get(&[x, y, z]).is_none());
                }
            }
        }
    }
    println!(
        "{}",
        cubes
            .iter()
            .map(|cube| 6 - full_neighbours(cube, &maxes, &space))
            .sum::<usize>()
    );
}
