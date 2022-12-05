use crustofcode;

type Stack = Vec<char>;
type Step = (i64, usize, usize);

fn parse_stack_line(line: &String) -> Vec<char> {
    line.chars()
        .collect::<Vec<char>>()
        .chunks(4)
        .map(|it| it[1])
        .collect()
}

fn parse_procedure_line(line: &String) -> Step {
    let cusu: Vec<&str> = line.split(" ").collect();
    return (
        crustofcode::str2int(cusu[1]),
        (crustofcode::str2int(cusu[3]) - 1).try_into().unwrap(),
        (crustofcode::str2int(cusu[5]) - 1).try_into().unwrap(),
    );
}

fn apply_step(stacks: &mut Vec<Stack>, step: Step) -> () {
    let (num, start, end) = step;
    for _ in 0..num {
        let val = stacks[start].pop().unwrap();
        stacks[end].push(val);
    }
}

fn apply_step2(stacks: &mut Vec<Stack>, step: Step) -> () {
    let (num, start, end) = step;
    let start_len = stacks[start].len();
    let start_idx: usize = (i64::try_from(start_len).unwrap() - num)
        .try_into()
        .unwrap();
    let mut moved: Vec<char> = stacks[start].drain(start_idx..).collect();
    stacks[end].append(&mut moved);
}

fn main() {
    println!("Day 5");

    let input: Vec<String> = crustofcode::read_input_lines();
    let mut cusu = input.as_slice().split(|s| s == "").into_iter();
    let starting_stacks = cusu.next().unwrap();
    let procedure = cusu.next().unwrap();
    assert_eq!(cusu.next(), None);

    let stack_num: usize = (starting_stacks[0].chars().count() + 1) / 4;
    println!("{}", stack_num);

    let mut starting_stacks: Vec<Vec<char>> =
        starting_stacks.into_iter().map(parse_stack_line).collect();
    starting_stacks.pop();
    starting_stacks.reverse();
    // println!("{:?}", starting_stacks);
    let mut stacks: Vec<Stack> = (0..stack_num)
        .map(|_| Vec::with_capacity(starting_stacks.len()))
        .collect();
    for line in &starting_stacks {
        for (i, &c) in line.into_iter().enumerate() {
            if c != ' ' {
                stacks[i].push(c);
            }
        }
    }
    // println!("{:?}", stacks);

    let steps: Vec<Step> = procedure.into_iter().map(parse_procedure_line).collect();
    // println!("{:?}", steps);

    // Part 1
    let mut p1stacks = stacks.clone();
    for &step in steps.iter() {
        apply_step(&mut p1stacks, step);
    }
    println!(
        "{}",
        p1stacks
            .into_iter()
            .map(|s| s.as_slice().last().unwrap().clone())
            .collect::<String>()
    );

    // Part 2
    let mut p2stacks = stacks.clone();
    for &step in steps.iter() {
        apply_step2(&mut p2stacks, step);
    }
    println!(
        "{}",
        p2stacks
            .into_iter()
            .map(|s| s.as_slice().last().unwrap().clone())
            .collect::<String>()
    );
}
