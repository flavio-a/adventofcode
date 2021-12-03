use std::fs;

pub fn read_input() -> String {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Error: missing filename");
        std::process::exit(-1);
    }

    let filename = &args[1];
    // println!("Input: {}", filename);

    fs::read_to_string(filename)
        .expect("Error while reading input file")
}

// Ideally I should write functions which return iterators (such as
// content.trim().split("\n")), but I don't know how to specify the return type
// ^^'

pub fn str2int(s: &str) -> i64 {
    s.parse().expect("String parsable to number expected")
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
