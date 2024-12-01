use adventofcrustacean;

static mut MEMOIZE: [i64; 256 + 1] = [0; 256 + 1];

// Given a number of days n, compute the number of lanternfish that there are
// after n days starting from a single lanternfish with internal timer 0
fn lanternfish(n: i64) -> i64 {
    unsafe {
        if n <= 0 {
            // < 0 means a fish spawned near the end of days, that hence reaches
            // day 0 without ever spwaning another
            return 1;
        }
        let nusize: usize = usize::try_from(n).unwrap();
        if MEMOIZE[nusize] > 0 {
            return MEMOIZE[nusize];
        } else {
            let mut cusu = 1;
            // After 1, 8, 15, ... days it spawns another fish with timer 8
            for i in (1..n).step_by(7) {
                // n - i is the number of days that fish is alive
                // 8 is its initial timer
                // hence it spawns (n - i) - 8 fishes (including itself)
                cusu += lanternfish(n - i - 8);
            }
            MEMOIZE[nusize] = cusu;
            return cusu;
        }
    }
}

fn main() {
    let content = adventofcrustacean::read_input();
    let fishes = content
        .trim()
        .split(",")
        .map(adventofcrustacean::str2int)
        .collect::<Vec<i64>>();

    // let t = 3_i64;
    // println!("lanternfishes({}) = {}", t, lanternfish(t));

    // Part 1
    const DAYS1: i64 = 80 + 1; // There's an obi-wan error somewhere
    let res1: i64 = fishes.iter().map(|&v| DAYS1 - v).map(lanternfish).sum();
    println!("{}", res1);

    // Part 2
    const DAYS2: i64 = 256 + 1; // There's an obi-wan error somewhere
    let res2: i64 = fishes.iter().map(|&v| DAYS2 - v).map(lanternfish).sum();
    println!("{}", res2);
}
