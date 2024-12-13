use aoc_parse::{parser, prelude::*};
use crustofcode::{option_assert, UPoint};

const CONVERSION_ERROR: usize = 10000000000000;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Machine {
    btn_a: UPoint,
    btn_b: UPoint,
    prize: UPoint,
}

impl Machine {
    /// Given a bn, checks whether there exists a matching an
    fn try_bn(&self, bn: usize) -> Option<usize> {
        let dx = self.prize.0 - self.btn_b.0 * bn;
        option_assert(dx % self.btn_a.0 == 0)?;
        let dy = self.prize.1 - self.btn_b.1 * bn;
        option_assert(dy % self.btn_a.1 == 0)?;
        option_assert(dx / self.btn_a.0 == dy / self.btn_a.1)?;
        return Some(dx / self.btn_a.0);
    }

    /// Tries all values between low and high for the number of b presses
    fn solve_machine_range(&self, low: usize, high: usize) -> Option<(usize, usize)> {
        assert!(self.prize.0 >= self.btn_b.0 * high);
        assert!(self.prize.1 >= self.btn_b.1 * high);

        for bn in low..=high {
            let an_option = self.try_bn(bn);
            if an_option.is_some() {
                return Some((an_option.unwrap(), bn));
            }
        }
        return None;
    }

    #[allow(dead_code)]
    fn solve_machine_1(&self) -> Option<usize> {
        let high = std::cmp::min(self.prize.0 / self.btn_b.0, self.prize.1 / self.btn_b.1);
        let (an, bn) = self.solve_machine_range(0, high)?;
        // println!("{an} {bn}");
        return Some(an * 3 + bn);
    }

    fn solve_machine(&self) -> Option<usize> {
        // Tries to solve the system
        // ax * an + bx * bn = px
        // ay * an + by * bn = py
        // for integer solutions

        // Computations yields px * ax - py * ax = bn (bx * ay - by * ax)
        let px: isize = self.prize.0.try_into().unwrap();
        let py: isize = self.prize.1.try_into().unwrap();
        let ax: isize = self.btn_a.0.try_into().unwrap();
        let ay: isize = self.btn_a.1.try_into().unwrap();
        let bx: isize = self.btn_b.0.try_into().unwrap();
        let by: isize = self.btn_b.1.try_into().unwrap();
        let mut pa: isize = px * ay - py * ax;
        let mut ba: isize = bx * ay - by * ax;
        option_assert(ba != 0)?;
        option_assert(pa == 0 || pa.signum() == ba.signum())?;
        if pa < 0 && ba < 0 {
            pa = -pa;
            ba = -ba;
        };
        let (pa, ba): UPoint = (pa.try_into().unwrap(), ba.try_into().unwrap());
        option_assert(pa % ba == 0)?;
        let bn: usize = pa / ba;
        let an = self.try_bn(bn)?;
        // println!("{an} {bn}");
        return Some(an * 3 + bn);
    }
}

fn main() {
    println!("Day 13");

    let mov_parser = parser!("X+" usize ", Y+" usize);
    let prize_parser = parser!("Prize: X=" usize ", Y=" usize);
    let p = parser!(sections(
        btn_a:line("Button A: " mov_parser)
        btn_b:line("Button B: " mov_parser)
        prize:line(prize_parser)
        => Machine{ btn_a: btn_a,  btn_b: btn_b,  prize: prize  }
    ));
    let mut machines: Vec<Machine> = p.parse(&crustofcode::read_input()).unwrap();

    // Part 1
    // let res1: usize = machines.iter().filter_map(|m| m.solve_machine_1()).sum();
    let res1: usize = machines.iter().filter_map(|m| m.solve_machine()).sum();
    println!("{res1}");

    // Part 2
    for machine in &mut machines {
        machine.prize.0 += CONVERSION_ERROR;
        machine.prize.1 += CONVERSION_ERROR;
    }
    let res2: usize = machines.iter().filter_map(|m| m.solve_machine()).sum();
    println!("{res2}");
}
