use crustofcode::*;

fn move_at<T>(i: &usize, ef: &mut Vec<(T, i64)>) -> () {
    let len = ef.len();
    let mut val: i64 = ef[*i].1 % i64::try_from(len - 1).unwrap();
    let mut pos: usize = *i;
    if val > 0 {
        while val != 0 {
            ef.swap(pos, (pos + 1) % len);
            pos = (pos + 1) % len;
            val -= 1;
        }
    } else {
        while val != 0 {
            ef.swap(pos, (len + pos - 1) % len);
            pos = (len + pos - 1) % len;
            val += 1;
        }
    }
}

// Curr is the index in the original array, curr_idx is the index in the current array
fn find_next_idx<T>(curr: &usize, curr_idx: &usize, ef: &Vec<(usize, T)>) -> usize {
    let len = ef.len();
    let target = (*curr + 1) % len;
    let mut res: usize = *curr_idx;
    while ef[res].0 != target {
        res = (res + 1) % len;
    }
    return res;
}

fn mix_file(ef: &mut Vec<(usize, i64)>) -> () {
    let len = ef.len();
    let mut curr: usize = 0;
    let mut curr_idx: usize = find_next_idx(&(len - 1), &(len - 1), ef);
    while curr < len {
        move_at(&curr_idx, ef);
        curr_idx = find_next_idx(&curr, &curr_idx, ef);
        curr += 1;
    }
}

fn get_coords(ef: &Vec<(usize, i64)>) -> (i64, i64, i64) {
    let len = ef.len();
    let zero_idx = ef.iter().position(|v| v.1 == 0).unwrap();
    let x = ef[(zero_idx + 1000) % len].1;
    let y = ef[(zero_idx + 2000) % len].1;
    let z = ef[(zero_idx + 3000) % len].1;
    return (x, y, z);
}

fn main() {
    println!("Day 20");

    let encr_file: Vec<i64> = crustofcode::read_input_lines()
        .into_iter()
        .map(str2int)
        .collect();

    // Part 1
    let mut efile1: Vec<(usize, i64)> = encr_file.iter().map(|n| n.clone()).enumerate().collect();
    mix_file(&mut efile1);
    // println!("{:?}", efile1.iter().map(snd).collect::<Vec<i64>>());
    let (x1, y1, z1) = get_coords(&efile1);
    // println!("{} {} {}", x1, y1, z1);
    println!("{}", x1 + y1 + z1);

    // Part 2
    let key = 811589153;
    let mut efile2: Vec<(usize, i64)> = encr_file.iter().map(|n| n * key).enumerate().collect();
    for _ in 0..10 {
        mix_file(&mut efile2);
    }
    let (x2, y2, z2) = get_coords(&efile2);
    // println!("{} {} {}", x2, y2, z2);
    println!("{}", x2 + y2 + z2);
}
