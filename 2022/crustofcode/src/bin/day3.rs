use crustofcode;

fn split_string(s: &String) -> (&str, &str) {
    let len = s.chars().count();
    assert_eq!(len % 2, 0);
    let half = len / 2;
    return (&s[0..half], &s[half..]);
}

fn get_prio(c: char) -> u32 {
    if c.is_ascii_lowercase() {
        (c as u32) - ('a' as u32) + 1
    } else if c.is_ascii_uppercase() {
        (c as u32) - ('A' as u32) + 27
    } else {
        panic!("AAA")
    }
}

fn get_shared(i: &[String]) -> char {
    let fst: &str = &i[0];
    let rest = &i[1..];
    return fst
        .chars()
        .find(|c| rest.iter().all(|s| s.chars().find(|c1| c1 == c).is_some()))
        .unwrap();
}

fn main() {
    println!("Day 3");

    let rucksacks: Vec<String> = crustofcode::read_input_lines();

    // Part 1
    let shared: Vec<char> = rucksacks
        .iter()
        .map(split_string)
        .map(|(s1, s2)| {
            s1.chars()
                .find(|c| s2.chars().find(|c1| c1 == c).is_some())
                .unwrap()
        })
        .collect();
    println!("{}", shared.into_iter().map(get_prio).sum::<u32>());

    // Part 2
    let badges: Vec<char> = rucksacks.as_slice().chunks(3).map(get_shared).collect();
    println!("{}", badges.into_iter().map(get_prio).sum::<u32>());
}
