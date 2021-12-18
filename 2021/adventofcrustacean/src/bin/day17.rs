use adventofcrustacean;

type Pos = (i64, i64);

fn main() {
    let content = adventofcrustacean::read_input();
    let cusu = content.lines().next().unwrap().split(",").map(adventofcrustacean::str2int).collect::<Vec<i64>>();
    let smol = (cusu[0], cusu[2]);
    let beeg = (cusu[1], cusu[3]);
    // println!("{:?} {:?}", smol, beeg);

    let startpos: Pos = (0, 0);

    // Approx of minimum x such that x + (x-1) + ... + 1 >= smol.x
    let mut minx = ((smol.0 * 2) as f64).sqrt() as i64 - 1;
    while minx * (minx + 1) / 2 < smol.0 {
        minx += 1;
    }
    let minx = minx;

    // Part 1
    let mut found_one = false;
    // Part 2
    let mut sols = 0_u64;

    for y in (smol.1..=(-smol.1)).rev() {
        for x in minx..=beeg.0 {
            let mut pos: Pos = startpos;
            let mut speed: Pos = (x, y);
            while pos.1 > beeg.1 {
                pos = (pos.0 + speed.0, pos.1 + speed.1);
                speed = (if speed.0 > 0 { speed.0 - 1 } else { 0 }, speed.1 - 1);
            }
            // Here pos.y <= beeg.y, so I should check x untile I get pos.y < smol.y
            while pos.1 >= smol.1 {
                if smol.0 <= pos.0 && pos.0 <= beeg.0 {
                    if !found_one {
                        println!("{}", y * (y + 1) / 2);
                        found_one = true;
                    }
                    sols += 1;
                    // println!("Sol: {}, {}", x, y);
                    break;
                }
                // One more step
                pos = (pos.0 + speed.0, pos.1 + speed.1);
                speed = (if speed.0 > 0 { speed.0 - 1 } else { 0 }, speed.1 - 1);
            }
        }
    }

    println!("{}", sols);
}