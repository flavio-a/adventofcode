use adventofcrustacean;



fn main() {
    let content = adventofcrustacean::read_input();
    // let vals = adventofcrustacean::lines(content);
    // println!("{:?}", vals);

    // Imperative-style solution
    let (mut pos, mut depth) = (0_i64, 0_i64);
    for val in content.trim().split("\n") {
        let cusi = val.split(" ").collect::<Vec<&str>>();
        match cusi[0] {
            "forward" => pos += adventofcrustacean::str2int(&cusi[1]),
            "up" => depth -= adventofcrustacean::str2int(&cusi[1]),
            "down" => depth += adventofcrustacean::str2int(&cusi[1]),
            _ => println!("Unexpected value!"),
        }
    }
    println!("Part1: {} - {} - {}", pos, depth, pos * depth);

    let (mut pos, mut depth, mut aim) = (0_i64, 0_i64, 0_i64);
    for val in content.trim().split("\n") {
        let cusi = val.split(" ").collect::<Vec<&str>>();
        match cusi[0] {
            "forward" => {
                let x = adventofcrustacean::str2int(&cusi[1]);
                pos += x;
                depth += aim * x;
            },
            "up" => aim -= adventofcrustacean::str2int(&cusi[1]),
            "down" => aim += adventofcrustacean::str2int(&cusi[1]),
            _ => println!("Unexpected value!"),
        }
    }
    println!("Part2: {} - {} - {}", pos, depth, pos * depth);
}