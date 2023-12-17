use itertools::Itertools;
use std::fs;

// -------------------------------- Read input --------------------------------
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

pub fn drop_prefix<'a>(s: &'a str, prefix: &str) -> &'a str {
    let l = prefix.chars().count();
    assert_eq!(&s[0..l], prefix.to_string());
    return &s[l..];
}

// ----------------------------- Points and grids -----------------------------
pub type Point = (i64, i64);
pub type UPoint = (usize, usize);

pub fn up2p((x, y): UPoint) -> Point {
    (x.try_into().unwrap(), y.try_into().unwrap())
}

/// Visualize a vector of points in the 2D space, representing points as the
/// attached character
pub fn visualize_as<I>(points: I)
where
    I: Iterator<Item = (Point, char)>,
{
    let sorted: Vec<(Point, char)> = points
        .sorted_by(|((x1, y1), _), ((x2, y2), _)| y1.cmp(y2).then(x1.cmp(x2)))
        .dedup_by(|(p1, _), (p2, _)| p1 == p2)
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

/// Visualize a grid of values given a function to display them
pub fn visualize_grid_t<T, F>(points: &Vec<Vec<T>>, f: F)
where
    F: Fn(&T) -> char,
{
    for line in points.iter() {
        for dot in line.iter() {
            print!("{}", f(&dot));
        }
        println!("");
    }
}

// ------------------------------- Projections --------------------------------
pub fn fst<T, U>(&(x, _): &(T, U)) -> T
where
    T: Copy,
{
    x
}

pub fn into_fst<T, U>((x, _): (T, U)) -> T {
    x
}

pub fn snd<T, U>(&(_, x): &(T, U)) -> U
where
    U: Copy,
{
    x
}

pub fn into_snd<T, U>((_, x): (T, U)) -> U {
    x
}

pub fn point_transpose<T>((x, y): (T, T)) -> (T, T) {
    (y, x)
}

// -------------------------------- Directions --------------------------------
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Dir {
    L,
    R,
    U,
    D,
}

impl Dir {
    pub fn opposite(&self) -> Dir {
        match self {
            Dir::D => Dir::U,
            Dir::L => Dir::R,
            Dir::R => Dir::L,
            Dir::U => Dir::D,
        }
    }

    // Add a dir to a Point, staying inside the bounds
    pub fn move_point(&self, (i, j): UPoint, h: usize, w: usize) -> Option<UPoint> {
        match self {
            Dir::R => (j + 1 < w).then(|| (i, j + 1)),
            Dir::L => (j > 0).then(|| (i, j - 1)),
            Dir::U => (i > 0).then(|| (i - 1, j)),
            Dir::D => (i + 1 < h).then(|| (i + 1, j)),
        }
    }
}

// ----------------------------------- Misc -----------------------------------
pub fn option_assert(g: bool) -> Option<()> {
    g.then_some(())
}
