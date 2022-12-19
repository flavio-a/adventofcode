use crustofcode::*;
use std::str::FromStr;

const PRINT: bool = false;

#[derive(Debug, Clone)]
struct Blueprint {
    ore: i64,
    clay: i64,
    obsidian: (i64, i64),
    geode: (i64, i64),
}

#[derive(Debug, PartialEq, Eq)]
struct ParseBlueprintError;

impl FromStr for Blueprint {
    type Err = ParseBlueprintError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
        let t = drop_prefix(&s, "Blueprint ")
            .chars()
            .skip_while(|c| c.is_digit(10))
            .collect::<String>();
        let mut parts = drop_prefix(&t, ":").split(".");
        let ore = str2int(
            drop_prefix(parts.next().unwrap(), " Each ore robot costs ")
                .split(" ")
                .next()
                .unwrap(),
        );
        let clay = str2int(
            drop_prefix(parts.next().unwrap(), " Each clay robot costs ")
                .split(" ")
                .next()
                .unwrap(),
        );
        let mut obsidians =
            drop_prefix(parts.next().unwrap(), " Each obsidian robot costs ").split(" ");
        let mut geodes = drop_prefix(parts.next().unwrap(), " Each geode robot costs ").split(" ");
        Ok(Blueprint {
            ore: ore,
            clay: clay,
            // Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
            obsidian: (
                str2int(obsidians.next().unwrap()),
                str2int(obsidians.nth(2).unwrap()),
            ),
            geode: (
                str2int(geodes.next().unwrap()),
                str2int(geodes.nth(2).unwrap()),
            ),
        })
    }
}

impl Blueprint {
    fn max_ore(&self) -> i64 {
        [self.ore, self.clay, self.obsidian.0, self.geode.0]
            .into_iter()
            .max()
            .unwrap()
    }
}

#[derive(Debug, Clone)]
struct Store {
    ore: i64,
    clay: i64,
    obsidian: i64,
    geodes: i64,
}

impl Store {
    fn le(a: &Store, b: &Store) -> bool {
        a.ore <= b.ore && a.clay <= b.clay && a.obsidian <= b.obsidian && a.geodes <= b.geodes
    }
}

#[derive(Debug, Clone)]
struct State {
    store: Store,
    robots: Store,
}

impl State {
    fn step(&self) -> State {
        State {
            store: Store {
                ore: self.store.ore + self.robots.ore,
                clay: self.store.clay + self.robots.clay,
                obsidian: self.store.obsidian + self.robots.obsidian,
                geodes: self.store.geodes + self.robots.geodes,
            },
            robots: self.robots.clone(),
        }
    }

    fn choices(&self, bp: &Blueprint) -> Vec<State> {
        let mut res = vec![self.step()];
        if self.robots.ore < bp.max_ore() && self.store.ore >= bp.ore {
            let mut new_state = self.step();
            new_state.robots.ore += 1;
            new_state.store.ore -= bp.ore;
            res.push(new_state);
        }
        if self.store.ore >= bp.clay && self.store.ore - self.robots.ore < bp.clay {
            let mut new_state = self.step();
            new_state.robots.clay += 1;
            new_state.store.ore -= bp.clay;
            res.push(new_state);
        }
        if self.store.ore >= bp.obsidian.0 && self.store.clay >= bp.obsidian.1 {
            let mut new_state = self.step();
            new_state.robots.obsidian += 1;
            new_state.store.ore -= bp.obsidian.0;
            new_state.store.clay -= bp.obsidian.1;
            res.push(new_state);
        }
        if self.store.ore >= bp.geode.0 && self.store.obsidian >= bp.geode.1 {
            let mut new_state = self.step();
            new_state.robots.geodes += 1;
            new_state.store.ore -= bp.geode.0;
            new_state.store.obsidian -= bp.geode.1;
            res.push(new_state);
        }
        return res;
    }

    fn le(a: &State, b: &State) -> bool {
        Store::le(&a.store, &b.store) && Store::le(&a.robots, &b.robots)
    }
}

fn explore(step: usize, state: State, bp: &Blueprint, bests: &mut Vec<Vec<State>>) -> State {
    // let cusu = bests[step].iter().find(|s| State::le(&state, s));
    // if cusu.is_some() {
    //     // There is a better state at the same number of step
    //     let e = Store { ore: 0, clay: 0, obsidian: 0, geodes: 0 };
    //     return State { store: e.clone(), robots: e };
    // }
    // // Drop all states which are worse than the current one
    // bests[step].retain(|s| !State::le(s, &state));
    if step == 0 {
        return state;
    }
    if PRINT {
        println!("---- {step} (");
    };
    // Try all choices
    let res = state
        .choices(bp)
        .into_iter()
        .map(|s| {
            if PRINT {
                println!("{s:?}");
            };
            let r = explore(step - 1, s, bp, bests);
            return r;
        })
        .max_by_key(|s| s.store.geodes)
        .unwrap();
    // bests[step].push(state);
    // Return
    if PRINT {
        println!("----)");
    };
    return res;
}

fn get_geodes(bp: &Blueprint, steps: usize) -> i64 {
    let initial_state = State {
        store: Store {
            ore: 0,
            clay: 0,
            obsidian: 0,
            geodes: 0,
        },
        robots: Store {
            ore: 1,
            clay: 0,
            obsidian: 0,
            geodes: 0,
        },
    };
    let mut bests = vec![vec![]; steps + 1];
    let s = explore(steps, initial_state, bp, &mut bests);
    println!("Result {s:?}");
    return s.store.geodes;
}

fn main() {
    println!("Day 19");

    let blueprints: Vec<Blueprint> = crustofcode::read_input_lines()
        .into_iter()
        .map(|s| s.parse().unwrap())
        .collect();

    // get_geodes(&blueprints[0]);
    // println!(
    //     "{:?}",
    //     blueprints.iter().map(get_geodes).collect::<Vec<i64>>()
    // );

    // Part 1
    println!(
        "{:?}",
        blueprints
            .iter()
            .map(|bp| get_geodes(bp, 24))
            .enumerate()
            .map(|(i, g)| g * i64::try_from(i + 1).unwrap())
            .sum::<i64>()
    );

    // Part 2
    // println!("{}", 0);
}
