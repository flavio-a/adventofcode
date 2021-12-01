use adventofcrustacean;

fn pair_lt(&(x, y): &(&i64, &i64)) -> bool {
    *x < *y
}

fn main() {
    println!("Day 1");

    let content = adventofcrustacean::read_input();
    // let lines: Vec<&str> = content.trim().split("\n").collect();
    let vals = adventofcrustacean::lines_to_ints(content);
    // println!("{:?}", vals);

    // Imperative-style solution
    let mut inc1 = 0_usize;
    let mut inc2 = 0_usize;
    for (i, val) in vals.iter().enumerate() {
        if i >= 1 && val > &vals[i - 1] {
            inc1 += 1;
        }
        if i >= 3 && val > &vals[i - 3] {
            inc2 += 1;
        }
    }
    println!("Imperative-style results: {} - {}", inc1, inc2);

    // Functional-style solution
    let finc1 = vals.iter().zip((&vals[1..]).iter()).filter(pair_lt).count();
    let finc2 = vals.iter().zip((&vals[3..]).iter()).filter(pair_lt).count();
    if inc1 == finc1 && inc2 == finc2 {
        println!("Functional-style results are the same as imperative ones");
    }
    else {
        println!("Mismatch: {} - {}", finc1, finc2);
    }
}