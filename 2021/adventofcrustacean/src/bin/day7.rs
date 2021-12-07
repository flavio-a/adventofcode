use adventofcrustacean;

fn quad_dist(poss: &Vec<i64>, x: i64) -> i64 {
    poss.into_iter().map(|v| (x - v).abs() * ((x - v).abs() + 1) / 2).sum()
}

fn main() {
    let content = adventofcrustacean::read_input();
    let mut crabs = content.trim().split(",").map(adventofcrustacean::str2int).collect::<Vec<i64>>();

    // Part 1
    crabs.sort_unstable();
    let hpos = crabs[crabs.len() / 2];
    println!("{}", crabs.iter().map(|&v| (hpos - v).abs()).sum::<i64>());

    // Part 2
    let hmax = *crabs.iter().max().unwrap();
    println!("{}", (0..hmax).map(|v| quad_dist(&crabs, v)).min().unwrap());
    // The function on the range seems convex, hence we could perform an
    // exponential + binary search: start from a random point (the closer to
    // the result, the better), find the descending direction, exponential
    // search in that direction until the value increases from one point to the
    // next, then binary search the resulting interval.
    //
    // However I'm not implementing it because the bruteforce approach already
    // find the solution in the blink of an aye and I don't feel like it.
}