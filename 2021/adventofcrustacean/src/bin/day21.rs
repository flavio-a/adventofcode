use adventofcrustacean;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash)]
struct Config {
    score: [u16; 2],
    pos: [u16; 2],
    round: usize,
}

impl PartialEq for Config {
    fn eq(&self, other: &Config) -> bool {
        self.score == other.score && self.pos == other.pos && self.round == other.round
    }
}

type DPArr = HashMap<Config, u64>;

const D3RESS: [u64; 10] = [0, 0, 0, 1, 3, 6, 7, 6, 3, 1];
const MAX_SCORE: u16 = 20;

fn new_pos(pos: u16, res: u16) -> u16 {
    (((pos + res) - 1) % 10) + 1
}

fn old_pos(pos: u16, res: u16) -> u16 {
    ((pos + 10 - res - 1) % 10) + 1
}

// Return the value contained in dparr, possibly computing it with DP
fn dpget(dparr: &mut DPArr, conf: Config) -> u64 {
    let tmp = dparr.get(&conf);
    if tmp.is_some() {
        return *tmp.unwrap();
    }
    if conf.score == [0, 0] {
        // The initial config is excluded here because it has already been
        // handled by the above case
        return 0;
    }
    // Who performs the move that brings in configuration conf is the player
    // that doesn't have to move in conf
    let round = 1 - conf.round;
    let target_sc = conf.score[round];
    let points_earned = conf.pos[round];
    // If target score < points earned then this configuration is unreachable
    if target_sc < points_earned {
        return 0;
    }
    let mut worlds: u64 = 0;
    // Cast 3d3 and get result
    for result in 3..=9 {
        // Moves backward
        let starting_pos = old_pos(conf.pos[round], result);
        let mut starting_conf: Config = conf.clone();
        starting_conf.pos[round] = starting_pos;
        starting_conf.score[round] = target_sc - points_earned;
        starting_conf.round = round;
        worlds += dpget(dparr, starting_conf) * D3RESS[usize::try_from(result).unwrap()];
    }
    assert_eq!(dparr.insert(conf, worlds), None);
    return worlds;
}

fn main() {
    let content = adventofcrustacean::read_input();
    let mut tmp = content.lines().map(adventofcrustacean::str2int);
    let start1: u16 = tmp.next().unwrap().try_into().unwrap();
    let start2: u16 = tmp.next().unwrap().try_into().unwrap();
    assert_eq!(tmp.next(), None);
    let intial_conf: Config = Config {
        score: [0, 0],
        pos: [start1, start2],
        round: 0,
    };

    // Debug only
    #[cfg(debug_assertions)]
    for pos in 1..=10 {
        for result in 3..=9 {
            assert_eq!(pos, new_pos(old_pos(pos, result), result));
        }
    }

    // Part 1
    let mut conf = intial_conf.clone();
    let mut dice = 2;
    let mut rolls = 0;
    while conf.score[0] < 1000 && conf.score[1] < 1000 {
        // conf.pos[conf.round] = (((conf.pos[conf.round] + dice * 3) - 1) % 10) + 1;
        conf.pos[conf.round] = new_pos(conf.pos[conf.round], dice * 3);
        conf.score[conf.round] += conf.pos[conf.round];
        dice = (((dice + 3) - 1) % 100) + 1;
        rolls += 3;
        conf.round = 1 - conf.round;
    }
    assert_eq!(
        conf.score[conf.round],
        if conf.score[0] == 1000 {
            conf.score[1]
        } else {
            conf.score[0]
        }
    );
    println!("{}", u32::from(conf.score[conf.round]) * rolls);

    // Part 2
    // DP. dparr stores in how many worlds you get a certain configuration
    let mut dparr: DPArr = HashMap::new();
    dparr.insert(intial_conf, 1);
    for sc0 in 0..=MAX_SCORE {
        for sc1 in 0..=MAX_SCORE {
            for pos0 in 1..=10 {
                for pos1 in 1..=10 {
                    for round in 0..=1 {
                        dpget(
                            &mut dparr,
                            Config {
                                score: [sc0, sc1],
                                pos: [pos0, pos1],
                                round: round,
                            },
                        );
                    }
                }
            }
        }
    }
    // Number of worlds in which p0 wins
    let mut p0wins: u64 = 0;
    let mut p1wins: u64 = 0;
    // Computed taking all states in which p0 has the turn (round == 0) and in
    // one move they win. To do this, sum over all possible dice results,
    // multiplied by D3RES
    // Analogous for p1 (even though probably p0wins is always greater)
    for result in 3..=9 {
        let tmp0: u64 = dparr
            .iter()
            .filter(|&(conf, _)| {
                conf.round == 0 && conf.score[0] + new_pos(conf.pos[0], result) > MAX_SCORE
            })
            .map(|(_, &v)| v)
            .sum();
        p0wins += tmp0 * D3RESS[usize::try_from(result).unwrap()];

        let tmp1: u64 = dparr
            .iter()
            .filter(|&(conf, _)| {
                conf.round == 1 && conf.score[1] + new_pos(conf.pos[1], result) > MAX_SCORE
            })
            .map(|(_, &v)| v)
            .sum();
        p1wins += tmp1 * D3RESS[usize::try_from(result).unwrap()];
    }
    println!("{}", if p0wins > p1wins { p0wins } else { p1wins });
}
