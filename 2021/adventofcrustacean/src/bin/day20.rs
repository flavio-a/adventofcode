use adventofcrustacean;

type Img = Vec<Vec<bool>>;

// true = 1 = light pixel = #, false = 0 = dark pixel = .
fn dot2bit(c: char) -> bool {
    c == '#'
}

fn enhance_point(enstr: &Vec<bool>, img: &Img, i: i32, j: i32, outsider: bool) -> bool {
    let mut bits: Vec<bool> = vec![];
    for di in &[-1, 0, 1] {
        for dj in &[-1, 0, 1] {
            let i1: i32 = i + di;
            let j1: i32 = j + dj;
            if i1 < 1
                || j1 < 1
                || i1 > img.len().try_into().unwrap()
                || j1 > img[0].len().try_into().unwrap()
            {
                // Position outside the image
                bits.push(outsider);
            } else {
                bits.push(img[usize::try_from(i1 - 1).unwrap()][usize::try_from(j1 - 1).unwrap()]);
            }
        }
    }
    let val: usize = bits
        .iter()
        .fold(0, |res, &bit| (res * 2) + if bit { 1 } else { 0 });
    return enstr[val];
}

fn enhance_grid(enstr: &Vec<bool>, img: &Img, outsider: bool) -> Img {
    let mut img1: Img = vec![];
    for i in 0..img.len() + 2 {
        img1.push(vec![false; img[0].len() + 2]);
        for j in 0..img[0].len() + 2 {
            img1[i][j] = enhance_point(
                &enstr,
                &img,
                i.try_into().unwrap(),
                j.try_into().unwrap(),
                outsider,
            );
        }
    }
    return img1;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let mut tmp = content.lines();
    let enhance_line = tmp
        .next()
        .unwrap()
        .chars()
        .map(dot2bit)
        .collect::<Vec<bool>>();
    assert_eq!(tmp.next(), Some(""));

    let img: Img = tmp.map(|s| s.chars().map(dot2bit).collect()).collect();
    // adventofcrustacean::visualize_grid(&img);
    let alt = *enhance_line.get(0).unwrap();

    // Part 1
    let img = enhance_grid(
        &enhance_line,
        &enhance_grid(&enhance_line, &img, false),
        alt,
    );
    // adventofcrustacean::visualize_grid(&img);
    println!(
        "{}",
        img.iter()
            .map(|l| l.iter().map(|&v| if v { 1 } else { 0 }).sum::<u32>())
            .sum::<u32>()
    );

    // Part 2
    let mut img = img;
    for it in 2..50 {
        img = enhance_grid(&enhance_line, &img, alt && (it % 2 == 1));
    }
    println!(
        "{}",
        img.iter()
            .map(|l| l.iter().map(|&v| if v { 1 } else { 0 }).sum::<u32>())
            .sum::<u32>()
    );
}
