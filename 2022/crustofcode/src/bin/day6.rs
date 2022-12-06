use crustofcode;
use std::collections::HashSet;
use std::hash::Hash;

// From https://stackoverflow.com/questions/46766560/how-to-check-if-there-are-duplicates-in-a-slice
fn has_unique_elements<T>(iter: T) -> bool
where
    T: IntoIterator,
    T::Item: Eq + Hash,
{
    let mut uniq = HashSet::new();
    iter.into_iter().all(move |x| uniq.insert(x))
}

fn main() {
    println!("Day 6");

    let datastream = crustofcode::read_input_lines()[0].as_bytes().to_vec();

    // Part 1
    let pos = datastream
        .iter()
        .enumerate()
        .position(|(i, _)| i >= 4 && has_unique_elements(&datastream[(i - 4)..i]))
        .unwrap();
    println!("{}", pos);

    // Part 2
    let pos = datastream
        .iter()
        .enumerate()
        .position(|(i, _)| i >= 14 && has_unique_elements(&datastream[(i - 14)..i]))
        .unwrap();
    println!("{}", pos);
}
