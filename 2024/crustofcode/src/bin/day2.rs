use aoc_parse::{parser, prelude::*};

fn make_deltas(rep: &Vec<i32>) -> Vec<i32> {
    std::iter::zip(rep.iter(), rep.iter().skip(1))
        .map(|(&a, &b)| a - b)
        .collect()
}

fn main() {
    println!("Day 2");

    let p = parser!(lines(repeat_sep(i32, " ")));
    let reports: Vec<Vec<i32>> = p.parse(&crustofcode::read_input()).unwrap();
    let tot_num = reports.len();

    // Part 1
    let neg_reports: Vec<Vec<i32>> = reports
        .into_iter()
        .filter(|report| {
            let deltas = make_deltas(report);
            !(deltas.iter().all(|&d| 1 <= d && d <= 3)
                || deltas.iter().all(|&d| -3 <= d && d <= -1))
        })
        .collect();
    let res1 = tot_num - neg_reports.len();
    println!("{res1}");

    // Part 2
    let neg_neg_reports = neg_reports
        .into_iter()
        .filter(|report| {
            let mut report1 = report.clone();
            for i in 0..report.len() {
                report1.clone_from(report);
                report1.remove(i);
                let deltas = make_deltas(&report1);
                if deltas.iter().all(|&d| 1 <= d && d <= 3)
                    || deltas.iter().all(|&d| -3 <= d && d <= -1)
                {
                    return false;
                }
            }
            true
        })
        .count();
    let res2 = tot_num - neg_neg_reports;
    println!("{res2}");
}
