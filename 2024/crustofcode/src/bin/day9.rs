use aoc_parse::{parser, prelude::*};
use num::Integer;

#[allow(unused)]
fn print_disk(disk: &Vec<Option<usize>>) {
    println!(
        "{:?}",
        disk.iter()
            .map(
                |i| i.map_or('.', |d| char::from_digit(d.try_into().unwrap(), 10)
                    .unwrap_or('A'))
            )
            .collect::<Vec<_>>()
    )
}

#[allow(unused)]
fn make_disk_test(disk_map: &Vec<usize>) -> Vec<Option<usize>> {
    assert!(disk_map.len().is_odd(), "Length of disk map is not odd");
    let len: usize = disk_map.iter().sum();
    assert!(len < 1_000_000_000, "Disk too long");

    let mut disk: Vec<Option<usize>> = vec![None; len];
    let max_id = disk_map.len() / 2;
    let mut pos: usize = 0;
    for id in 0..=max_id {
        for _ in 0..disk_map[2 * id] {
            disk[pos] = Some(id);
            pos += 1;
        }
        if id != max_id {
            pos += disk_map[2 * id + 1];
        }
    }
    return disk;
}

fn compact(disk: &mut Vec<Option<usize>>) {
    let mut i = disk.len() - 1;
    let mut j = 0;
    while disk[j].is_some() {
        j += 1;
        if j >= disk.len() {
            return;
        }
    }
    while i > j {
        if disk[i].is_some() {
            while disk[j].is_some() {
                j += 1;
            }
            assert_eq!(disk[j], None);
            if i > j {
                disk.swap(i, j);
            }
        }
        i -= 1;
    }
}

fn checksum(disk: &Vec<Option<usize>>) -> usize {
    disk.iter()
        .enumerate()
        .map(|(i, &id)| id.unwrap_or(0) * i)
        .sum()
}

fn convert_disk_map(disk_map: &Vec<usize>) -> Vec<(Option<usize>, usize)> {
    let mut disk_map_plus = Vec::with_capacity(disk_map.len());
    let mut is_file = true;
    let mut id = 0;
    for &seg in disk_map {
        if is_file {
            disk_map_plus.push((Some(id), seg));
            id += 1;
        } else {
            disk_map_plus.push((None, seg));
        }
        is_file = !is_file;
    }
    return disk_map_plus;
}

fn make_disk(disk_map_plus: &Vec<(Option<usize>, usize)>) -> Vec<Option<usize>> {
    let len: usize = disk_map_plus.iter().map(|(_, l)| *l).sum();
    assert!(len < 1_000_000_000, "Disk too long");

    let mut disk: Vec<Option<usize>> = vec![None; len];
    let mut pos: usize = 0;
    for &(id, l) in disk_map_plus {
        if id.is_some() {
            let id = id.unwrap();
            for _ in 0..l {
                disk[pos] = Some(id);
                pos += 1;
            }
        } else {
            pos += l;
        }
    }
    return disk;
}

fn defrag(mut disk_map: Vec<(Option<usize>, usize)>) -> Vec<(Option<usize>, usize)> {
    let mut curr_id = disk_map.last().unwrap().0.unwrap();
    while curr_id > 0 {
        // Look for curr_id in the disk map
        let i = disk_map
            .iter()
            .position(|(d, _)| *d == Some(curr_id))
            .unwrap();
        let len = disk_map[i].1;
        let j = disk_map.iter().position(|(d, l)| d.is_none() && *l >= len);
        if j.is_some() && j.unwrap() < i {
            let j = j.unwrap();
            // Removes the segment from the end. This part is probably useless
            // because they will always be to the right of any file I move
            // later
            let e = disk_map.remove(i);
            // assert_eq!(disk_map[i - 1].0, None);
            disk_map[i - 1].1 += len;
            // Adds the segment curr_id before the empty segment at j
            disk_map[j].1 -= len;
            disk_map.insert(j, e);
        }
        curr_id -= 1;
    }
    return disk_map;
}

fn main() {
    println!("Day 9");

    let p = parser!(line(digit+));
    let disk_map: Vec<usize> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let mut disk = make_disk(&convert_disk_map(&disk_map));
    assert_eq!(disk, make_disk_test(&disk_map));
    // println!("{:?}", disk);
    compact(&mut disk);
    // println!("{:?}", disk);
    let res1 = checksum(&disk);
    println!("{res1}");

    // Part 2
    let disk_map_plus = convert_disk_map(&disk_map);
    let disk_map_plus = defrag(disk_map_plus);
    // print_disk(&make_disk(&disk_map_plus));
    let res2 = checksum(&make_disk(&disk_map_plus));
    println!("{res2}");
}
