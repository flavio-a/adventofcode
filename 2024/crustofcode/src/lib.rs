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
/// use crustofcode::lines_to_ints;
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
pub type Point = (isize, isize);
pub type UPoint = (usize, usize);

pub fn up2p((x, y): UPoint) -> Point {
    (x.try_into().unwrap(), y.try_into().unwrap())
}

pub fn neighbours(p: &UPoint, h: usize, w: usize) -> Vec<UPoint> {
    Dir4::ALL_DIRS
        .iter()
        .filter_map(|d| d.move_point(*p, h, w))
        .collect()
}

/// Returns the pair (w, h) needed for other grid operations
pub fn get_dimensions<T>(grid: &Vec<Vec<T>>) -> (usize, usize) {
    (grid[0].len(), grid.len())
}

/// Given two positions p, q: UPoint, returns the direction p->q
pub fn direction(p: &UPoint, q: &UPoint) -> Point {
    let p = up2p(*p);
    let q = up2p(*q);
    return (q.0 - p.0, q.1 - p.1);
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
    let maxx: isize = *sorted.iter().map(|((x, _), _)| x).max().unwrap();
    let maxy: isize = *sorted.iter().map(|((_, y), _)| y).max().unwrap();

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
                // print!(" ");
                print!(".");
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

/// Count the number of true cells in a grid
pub fn count_true(board: &Vec<Vec<bool>>) -> usize {
    board
        .iter()
        .map(|r| {
            r.into_iter()
                .map(|c| if *c { 1_usize } else { 0_usize })
                .sum::<usize>()
        })
        .sum()
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir4 {
    L,
    R,
    U,
    D,
}

impl Dir4 {
    pub const ALL_DIRS: [Dir4; 4] = [Dir4::U, Dir4::D, Dir4::L, Dir4::R];

    /// Returns an unique index in the range 0..4 for the direction.
    /// This is guaranteed to return the index in ALL_DIRS (ie. when d: Dir4,
    /// Dir4::ALL_DIRS[d.to_index()] == d)
    ///
    /// ```
    /// use crustofcode::Dir4;
    ///
    /// let d: Dir4 = Dir4::U;
    ///
    /// assert_eq!(d, Dir4::ALL_DIRS[d.to_index()]);
    /// ```
    pub const fn to_index(&self) -> usize {
        match self {
            Dir4::U => 0,
            Dir4::D => 1,
            Dir4::L => 2,
            Dir4::R => 3,
        }
    }

    pub const fn opposite(&self) -> Self {
        match self {
            Dir4::D => Dir4::U,
            Dir4::L => Dir4::R,
            Dir4::R => Dir4::L,
            Dir4::U => Dir4::D,
        }
    }

    pub const fn turn_right(&self) -> Self {
        match self {
            Dir4::D => Dir4::L,
            Dir4::L => Dir4::U,
            Dir4::U => Dir4::R,
            Dir4::R => Dir4::D,
        }
    }

    // Add a dir to a Point, staying inside the bounds
    pub fn move_point(&self, (i, j): UPoint, h: usize, w: usize) -> Option<UPoint> {
        match self {
            Dir4::R => (j + 1 < w).then(|| (i, j + 1)),
            Dir4::L => (j > 0).then(|| (i, j - 1)),
            Dir4::U => (i > 0).then(|| (i - 1, j)),
            Dir4::D => (i + 1 < h).then(|| (i + 1, j)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dir8 {
    L,
    R,
    U,
    D,
    LU,
    RU,
    LD,
    RD,
}

impl Dir8 {
    pub const ALL_DIRS: [Dir8; 8] = [
        Dir8::U,
        Dir8::LU,
        Dir8::L,
        Dir8::LD,
        Dir8::D,
        Dir8::RD,
        Dir8::R,
        Dir8::RU,
    ];

    /// Returns an unique index in the range 0..8 for the direction
    pub const fn to_index(&self) -> usize {
        match self {
            Dir8::U => 0,
            Dir8::LU => 1,
            Dir8::L => 2,
            Dir8::LD => 3,
            Dir8::D => 4,
            Dir8::RD => 5,
            Dir8::R => 6,
            Dir8::RU => 7,
        }
    }

    pub const fn opposite(&self) -> Dir8 {
        match self {
            Dir8::D => Dir8::U,
            Dir8::L => Dir8::R,
            Dir8::R => Dir8::L,
            Dir8::U => Dir8::D,
            Dir8::LU => Dir8::RD,
            Dir8::LD => Dir8::RU,
            Dir8::RU => Dir8::LD,
            Dir8::RD => Dir8::LU,
        }
    }

    // Add a dir to a Point, staying inside the bounds
    pub fn move_point(&self, (i, j): UPoint, h: usize, w: usize) -> Option<UPoint> {
        match self {
            Dir8::R => (j + 1 < w).then(|| (i, j + 1)),
            Dir8::L => (j > 0).then(|| (i, j - 1)),
            Dir8::U => (i > 0).then(|| (i - 1, j)),
            Dir8::D => (i + 1 < h).then(|| (i + 1, j)),
            Dir8::LU => (j > 0 && i > 0).then(|| (i - 1, j - 1)),
            Dir8::RU => (j + 1 < w && i > 0).then(|| (i - 1, j + 1)),
            Dir8::RD => (j + 1 < w && i + 1 < h).then(|| (i + 1, j + 1)),
            Dir8::LD => (j > 0 && i + 1 < h).then(|| (i + 1, j - 1)),
        }
    }
}

// ----------------------------------- Misc -----------------------------------
pub fn option_assert(g: bool) -> Option<()> {
    g.then_some(())
}
