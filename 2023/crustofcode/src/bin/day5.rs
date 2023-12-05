use aoc_parse::{parser, prelude::*};

type AssocRange = (u64, u64, u64);
type AlmanacMap = Vec<AssocRange>;

fn follow_map(id: u64, map: &AlmanacMap) -> u64 {
    map.iter()
        .find_map(|&(d, s, l)| (s <= id && id < s + l).then(|| d + id - s))
        .unwrap_or(id)
}

trait GetMinLoc {
    fn get_min_location(&mut self, maps: &Vec<AlmanacMap>) -> u64;
}

impl<T: Iterator<Item = u64>> GetMinLoc for T {
    fn get_min_location(&mut self, maps: &Vec<AlmanacMap>) -> u64 {
        self.map(|s| maps.iter().fold(s.to_owned(), follow_map))
            .min()
            .unwrap()
    }
}

fn main() {
    println!("Day 5");

    let seeds_parser = parser!(line("seeds: " repeat_sep(u64, " ")));
    let map_parser = parser!(
        _header: line(alpha+ "-to-" alpha+ " map:")
        assocs: lines(u64 " " u64 " " u64)
        => assocs
    );
    let p = parser!(seeds_parser "\n" sections(map_parser));
    let (seeds, maps) = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    let res1: u64 = seeds.iter().copied().get_min_location(&maps);
    println!("{res1}");

    // Part 2
    let seeds_ranges: Vec<(u64, u64)> = seeds.chunks(2).map(|a| (a[0], a[1])).collect();
    let tot_len: u64 = seeds_ranges.iter().map(crustofcode::snd).sum();
    let digits: usize = (tot_len.ilog10() + 1) as usize;
    println!(
        "> Total ranges: {} (len: {:0digits$})",
        seeds_ranges.len(),
        tot_len
    );
    let res2: u64 = seeds_ranges
        .iter()
        .enumerate()
        .inspect(|(i, &(_, l))| println!("> Starting range {i} (len: {:0digits$})", l))
        .map(|(_, &(s, l))| s..s + l)
        .flatten()
        .get_min_location(&maps);
    println!("{res2}");
}
