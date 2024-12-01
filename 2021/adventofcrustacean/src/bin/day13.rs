use adventofcrustacean;
use itertools::Itertools;

fn vec2pair<T>(v: Vec<T>) -> (T, T)
where
    T: Copy,
{
    (v[0], v[1])
}

const BEGIN: &str = "fold along ";
// true is x, false is y
fn parse_fold(s: &&str) -> (bool, i64) {
    let s: &str = &(s[BEGIN.len()..]);
    (
        s.chars().nth(0).unwrap() == 'x',
        adventofcrustacean::str2int(&(s[2..])),
    )
}

fn fold(dots: Vec<(i64, i64)>, foldline: (bool, i64)) -> Vec<(i64, i64)> {
    let mut newdots = vec![];
    for (x, y) in dots.into_iter() {
        let x1 = if foldline.0 {
            foldline.1 - (foldline.1 - x).abs()
        } else {
            x
        };
        let y1 = if foldline.0 {
            y
        } else {
            foldline.1 - (foldline.1 - y).abs()
        };
        newdots.push((x1, y1));
    }
    return newdots;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let lines = content.lines().collect::<Vec<&str>>();
    let mut tmp = lines[..].split(|&s| s == "");
    let dots = tmp
        .next()
        .unwrap()
        .iter()
        .map(|s| vec2pair(s.split(",").map(adventofcrustacean::str2int).collect()))
        .collect::<Vec<(i64, i64)>>();
    let mut folds = tmp.next().unwrap().iter().map(parse_fold);

    // Part 1
    let dots1 = fold(dots, folds.next().unwrap())
        .into_iter()
        .unique()
        .collect::<Vec<(i64, i64)>>();
    println!("{}", dots1.iter().count());

    // Part 2
    let findots = folds.fold(dots1, |acc, elem| fold(acc, elem));
    adventofcrustacean::visualize(findots);
}
