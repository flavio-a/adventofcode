use adventofcrustacean;

fn most_common(lines: &Vec<&str>, idx: usize) -> u8 {
    let mut zeros = 0;
    for val in lines.iter() {
        if val.chars().nth(idx).unwrap() == '0' {
            zeros += 1;
        }
    }
    if zeros > u32::try_from(lines.len()).ok().unwrap() - zeros {
        0
    } else {
        1
    }
}

fn check_nth(val: &str, idx: usize, c: u8) -> bool {
    let comp = if c == 0 { '0' } else { '1' };
    val.chars().nth(idx).unwrap() == comp
}

fn bin2int(bits: &str) -> u32 {
    let mut res = 0_u32;
    for d in bits.chars() {
        res *= 2;
        if d == '1' {
            res += 1;
        }
        // else d == '0', do nothing
    }
    res
}

fn main() {
    let content = adventofcrustacean::read_input();
    let lines = content.lines().collect::<Vec<&str>>();

    // Part 1
    let len = lines[0].len();
    let mut gamma_bin = vec![0_u8; len];
    for i in 0..len {
        gamma_bin[i] = most_common(&lines, i);
    }
    let mut gamma = 0_u32;
    let mut epsilon = 0_u32;
    for i in 0..len {
        gamma *= 2;
        epsilon *= 2;

        gamma += u32::from(gamma_bin[i]);
        epsilon += 1 - u32::from(gamma_bin[i]);
    }
    println!("Part1: {} - {} - {}", gamma, epsilon, gamma * epsilon);

    // Part 2
    let mut oxy = lines.clone();
    let mut co2 = lines.clone();
    for i in 0..len {
        let oxy_bit = most_common(&oxy, i);
        let co2_bit = most_common(&co2, i);
        if oxy.len() > 1 {
            oxy = oxy
                .into_iter()
                .filter(|s| check_nth(s, i, oxy_bit))
                .collect::<Vec<&str>>();
        }
        if co2.len() > 1 {
            co2 = co2
                .into_iter()
                .filter(|s| !check_nth(s, i, co2_bit))
                .collect::<Vec<&str>>();
        }
    }
    let oxy = bin2int(oxy[0]);
    let co2 = bin2int(co2[0]);
    println!("Part1: {} - {} - {}", oxy, co2, oxy * co2);
}
