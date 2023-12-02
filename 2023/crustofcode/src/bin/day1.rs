use crustofcode;

fn fl_digits(s: &String) -> u32 {
    let fst = s
        .chars()
        .find(|&c| c.is_digit(10))
        .unwrap()
        .to_digit(10)
        .unwrap();
    let lst = s
        .chars()
        .rfind(|&c| c.is_digit(10))
        .unwrap()
        .to_digit(10)
        .unwrap();
    return fst * 10 + lst;
}

fn get_first_digit(s: &String, dir: bool) -> u32 {
    let mut i = if dir { 0 } else { s.len() - 1 };
    while if dir { i < s.len() } else { i > 0 } {
        let si: char = s.chars().nth(i).unwrap();
        if si.is_digit(10) {
            return si.to_digit(10).unwrap();
        }
        let slice = &s[i..];
        if slice.starts_with("one") {
            return 1;
        } else if slice.starts_with("two") {
            return 2;
        } else if slice.starts_with("three") {
            return 3;
        } else if slice.starts_with("four") {
            return 4;
        } else if slice.starts_with("five") {
            return 5;
        } else if slice.starts_with("six") {
            return 6;
        } else if slice.starts_with("seven") {
            return 7;
        } else if slice.starts_with("eight") {
            return 8;
        } else if slice.starts_with("nine") {
            return 9;
        }
        if dir {
            i = i + 1;
        } else {
            i = i - 1;
        }
    }
    panic!("Fuck");
}

fn fl_digits_ma_merda(s: &String) -> u32 {
    return 10 * get_first_digit(s, true) + get_first_digit(s, false);
}

fn main() {
    println!("Day 1");

    let lines: Vec<String> = crustofcode::read_input_lines();

    // Part 1
    println!("{}", lines.iter().map(fl_digits).sum::<u32>());
    // Part 2
    println!("{}", lines.iter().map(fl_digits_ma_merda).sum::<u32>());
}
