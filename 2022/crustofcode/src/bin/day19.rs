use crustofcode::*;
use std::str::FromStr;

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
        [self.clay, self.obsidian.0, self.geode.0]
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

#[derive(Debug, Clone)]
struct State {
    store: Store,
    robots: Store,
}

type Choice = usize;

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

    fn choices_heuristics(&self, bp: &Blueprint) -> Vec<(State, Choice)> {
        let mut res = vec![];
        if self.robots.ore < bp.max_ore() {
            res.push((self.step(), 0));
        }
        if self.robots.ore < bp.max_ore() && self.store.ore >= bp.ore {
            let mut new_state = self.step();
            new_state.robots.ore += 1;
            new_state.store.ore -= bp.ore;
            res.push((new_state, 1));
        }
        // The (self.store.ore - self.robots.ore < bp.clay) part is not totally
        // right, but drops lots of paths and only breaks when clay cost really
        // few ores
        if self.store.ore >= bp.clay && self.store.ore - self.robots.ore < bp.clay {
            let mut new_state = self.step();
            new_state.robots.clay += 1;
            new_state.store.ore -= bp.clay;
            res.push((new_state, 2));
        }
        if self.store.ore >= bp.geode.0 && self.store.obsidian >= bp.geode.1 {
            let mut new_state = self.step();
            new_state.robots.geodes += 1;
            new_state.store.ore -= bp.geode.0;
            new_state.store.obsidian -= bp.geode.1;
            res.push((new_state, 4));
        }
        // Push obsidian only if it can't push geodes
        else if self.store.ore >= bp.obsidian.0 && self.store.clay >= bp.obsidian.1 {
            let mut new_state = self.step();
            new_state.robots.obsidian += 1;
            new_state.store.ore -= bp.obsidian.0;
            new_state.store.clay -= bp.obsidian.1;
            res.push((new_state, 3));
        }
        if res.is_empty() {
            res.push((self.step(), 0));
        }
        return res;
    }
}

// The bookkeeping for the choices is actually very slow (~ 2x execution time)
#[cfg(debug_assertions)]
fn explore2(step: usize, state: State, bp: &Blueprint) -> (State, Vec<usize>) {
    if step == 0 {
        return (state, vec![]);
    }
    // Try all choices
    let res = state
        .choices_heuristics(bp)
        .into_iter()
        .map(|(s, c)| {
            let (s1, mut v) = explore2(step - 1, s, bp);
            v.push(c);
            return (s1, v);
        })
        .max_by_key(|(s, _)| s.store.geodes)
        .unwrap();
    // Return
    return res;
}

#[cfg(not(debug_assertions))]
fn explore2(step: usize, state: State, bp: &Blueprint) -> (State, usize) {
    if step == 0 {
        return (state, 0);
    }
    // Try all choices
    let res = state
        .choices_heuristics(bp)
        .into_iter()
        .map(|(s, _)| explore2(step - 1, s, bp))
        .max_by_key(|(s, _)| s.store.geodes)
        .unwrap();
    // Return
    return res;
}

fn get_geodes2(bp: &Blueprint, steps: usize) -> i64 {
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
    let s = explore2(steps, initial_state, bp);
    println!("{s:?}");
    return s.0.store.geodes;
}

fn main() {
    println!("Day 19");

    let blueprints: Vec<Blueprint> = crustofcode::read_input_lines()
        .into_iter()
        .map(|s| s.parse().unwrap())
        .collect();

    // Part 1
    let results1: Vec<i64> = blueprints.iter().map(|bp| get_geodes2(bp, 24)).collect();
    println!(
        "{:?}",
        results1
            .into_iter()
            .enumerate()
            .map(|(i, g)| g * i64::try_from(i + 1).unwrap())
            .sum::<i64>()
    );

    // Part 2
    let results2: Vec<i64> = blueprints
        .iter()
        .take(3)
        .map(|bp| get_geodes2(bp, 32))
        .collect();
    println!("{}", results2.into_iter().product::<i64>());
}
