use itertools::Itertools;
use std::fs;

pub fn read_input() -> String {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Error: missing filename");
        std::process::exit(-1);
    }

    let filename = &args[1];
    // println!("Input: {}", filename);

    fs::read_to_string(filename).expect("Error while reading input file")
}

pub fn str2int<S: AsRef<str>>(s: S) -> i64 {
    s.as_ref()
        .parse()
        .expect("String parsable to number expected")
}

pub fn lines_owned(s: String) -> Vec<String> {
    s.lines().map(|s| s.to_owned()).collect()
}

/// Read an input file (specified as the first CLI argument) and returns a
/// vector with its lines (owned)
pub fn read_input_lines() -> Vec<String> {
    lines_owned(read_input())
}

/// Parse a string as an integer per line, and returns the resulting vector.
///
/// # Example
/// ```
/// use adventofcrustacean::lines_to_ints;
///
/// let content = String::from("1\n2\n42");
/// let nums = lines_to_ints(content);
///
/// assert_eq!(nums, [1, 2, 42]);
/// ```
pub fn lines_to_ints(content: String) -> Vec<i64> {
    content.lines().map(str2int).collect()
}

/// Visualize a vector of points in the 2d space
pub fn visualize(points: Vec<(i64, i64)>) {
    let sorted = points
        .into_iter()
        .sorted_by(|(x1, y1), (x2, y2)| y1.cmp(y2).then(x1.cmp(x2)))
        .dedup()
        .collect::<Vec<(i64, i64)>>();
    let maxx: i64 = *sorted.iter().map(|(x, _)| x).max().unwrap();
    let maxy: i64 = *sorted.iter().map(|(_, y)| y).max().unwrap();

    let mut x = 0;
    let mut y = 0;
    let mut i = 0;
    while x <= maxx && y <= maxy && i < sorted.len() {
        if *sorted.get(i).unwrap() == (x, y) {
            print!("█");
            i += 1;
        } else {
            print!(" ");
        }
        x += 1;
        if x > maxx {
            println!("");
            x = 0;
            y += 1;
        }
    }
}

/// Visualize a grid of bools
pub fn visualize_grid(points: &Vec<Vec<bool>>) {
    for line in points.iter() {
        for &dot in line.iter() {
            if dot {
                // print!("█");
                print!("#");
            } else {
                // print!(" ");
                print!(".");
            }
        }
        println!("");
    }
}