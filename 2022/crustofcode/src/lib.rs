use itertools::Itertools;
use std::fs;

pub type Point = (i64, i64);
pub type UPoint = (usize, usize);

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

/// Visualize a vector of points in the 2D space, representing points as the
/// attached character
pub fn visualize_as<I>(points: I)
where
    I: Iterator<Item = (Point, char)>,
{
    let sorted: Vec<(Point, char)> = points
        .sorted_by(|((x1, y1), _), ((x2, y2), _)| y1.cmp(y2).then(x1.cmp(x2)))
        .dedup()
        .collect();
    let maxx: i64 = *sorted.iter().map(|((x, _), _)| x).max().unwrap();
    let maxy: i64 = *sorted.iter().map(|((_, y), _)| y).max().unwrap();

    let mut x = 0;
    let mut y = 0;
    let mut i = 0;
    while x <= maxx && y <= maxy && i < sorted.len() {
        if sorted.get(i).unwrap().0 == (x, y) {
            print!("{}", sorted.get(i).unwrap().1);
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

/// Visualize a vector of points in the 2D space as █
pub fn visualize<'a, I>(points: I)
where
    I: Iterator<Item = &'a Point>,
{
    visualize_as(points.map(|&p| (p, '█')));
}

/// Visualize a grid of bools
pub fn visualize_grid(points: &Vec<Vec<bool>>) {
    for line in points.iter() {
        for &dot in line.iter() {
            if dot {
                // print!("█");
                print!("#");
            } else {
                print!(" ");
                // print!(".");
            }
        }
        println!("");
    }
}

pub fn drop_prefix<'a>(s: &'a str, prefix: &str) -> &'a str {
    let l = prefix.chars().count();
    assert_eq!(&s[0..l], prefix.to_string());
    return &s[l..];
}

pub fn fst<T, U>(&(x, _): &(T, U)) -> T
where
    T: Copy,
{
    x
}

pub fn snd<T, U>(&(_, x): &(T, U)) -> U
where
    U: Copy,
{
    x
}
