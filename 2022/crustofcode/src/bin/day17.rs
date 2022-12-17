use crustofcode::*;

type Row = [bool; 7];
type Field = Vec<Row>;

#[allow(dead_code)]
fn visualize_grid1(field: &Field) {
    println!("---------------");
    visualize_grid(&field.iter().rev().map(|a| a.to_vec()).collect());
}

#[derive(Debug, Clone)]
enum Dir {
    Left,
    Right,
}

fn parse_dir(c: char) -> Dir {
    match c {
        '<' => Dir::Left,
        '>' => Dir::Right,
        _ => panic!("Unexpected plot twist"),
    }
}

#[derive(Debug, Clone)]
enum Kind {
    Minus,
    Plus,
    L,
    Pipe,
    Square,
}

#[derive(Debug, Clone)]
struct Rock {
    kind: Kind,
    pos: UPoint,
}

impl Kind {
    fn squares(&self, (x, y): UPoint) -> Vec<UPoint> {
        match self {
            Kind::Minus => vec![(x, y), (x + 1, y), (x + 2, y), (x + 3, y)],
            Kind::Plus => vec![
                (x + 1, y - 1),
                (x, y),
                (x + 1, y),
                (x + 2, y),
                (x + 1, y + 1),
            ],
            Kind::L => vec![
                (x, y),
                (x + 1, y),
                (x + 2, y),
                (x + 2, y + 1),
                (x + 2, y + 2),
            ],
            Kind::Pipe => vec![(x, y), (x, y + 1), (x, y + 2), (x, y + 3)],
            Kind::Square => vec![(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)],
        }
    }

    // y is the height of the heighest rock so far
    fn spawn(self, y: usize) -> Rock {
        let pos = match self {
            Kind::Minus => (2, y + 3),
            Kind::Plus => (2, y + 4),
            Kind::L => (2, y + 3),
            Kind::Pipe => (2, y + 3),
            Kind::Square => (2, y + 3),
        };
        return Rock {
            kind: self,
            pos: pos,
        };
    }

    fn next(&self) -> Kind {
        match self {
            Kind::Minus => Kind::Plus,
            Kind::Plus => Kind::L,
            Kind::L => Kind::Pipe,
            Kind::Pipe => Kind::Square,
            Kind::Square => Kind::Minus,
        }
    }

    fn to_usize(&self) -> usize {
        match self {
            Kind::Minus => 0,
            Kind::Plus => 1,
            Kind::L => 2,
            Kind::Pipe => 3,
            Kind::Square => 4,
        }
    }
}

impl Rock {
    fn squares(&self) -> Vec<UPoint> {
        self.kind.squares(self.pos)
    }

    fn collides(&self, field: &Field) -> bool {
        // println!("Collide: {:?}", self.pos);
        for (x, y) in self.squares() {
            // println!("- {:?}", (x, y));
            if x >= 7 {
                return true;
            }
            if y < field.len() && field[y][x] {
                return true;
            }
        }
        return false;
    }

    // Returns whether it moved or not
    fn push(&mut self, dir: &Dir, field: &Field) -> bool {
        match dir {
            Dir::Left => {
                if self.pos.0 > 0 {
                    self.pos.0 -= 1
                }
            }
            Dir::Right => self.pos.0 += 1,
        };
        if self.collides(field) {
            // Collides after the move, undo
            match dir {
                Dir::Left => self.pos.0 += 1,
                Dir::Right => self.pos.0 -= 1,
            };
            return false;
        } else {
            return true;
        }
    }

    // Returns whether it fell or not
    fn fall(&mut self, field: &Field) -> bool {
        self.pos.1 -= 1;
        if self.collides(field) {
            // Collides after the move, undo
            self.pos.1 += 1;
            return false;
        } else {
            return true;
        }
    }

    // Settles the rock in the field
    fn settle(&self, field: &mut Field) -> () {
        for (x, y) in self.squares() {
            while y >= field.len() {
                field.push([false; 7]);
            }
            assert!(!field[y][x]);
            field[y][x] = true;
        }
    }

    // Make the rock fall to the "ground"
    fn fall_all<'a, I>(&mut self, field: &mut Field, directions: &mut I)
    where
        I: Iterator<Item = &'a Dir>,
    {
        self.push(&directions.next().unwrap(), &field);
        while self.fall(field) {
            self.push(&directions.next().unwrap(), field);
        }
        self.settle(field);
    }

    fn spawn_next(&self, y: usize) -> Rock {
        self.kind.next().spawn(y)
    }

    // Make the rock fall to the "ground"
    fn fall_all2<'a, I>(&mut self, field: &mut Field, directions: &mut I)
    where
        I: Iterator<Item = (usize, &'a Dir)>,
    {
        self.push(&directions.next().unwrap().1, &field);
        while self.fall(field) {
            self.push(&directions.next().unwrap().1, field);
        }
        self.settle(field);
    }
}

fn main() {
    println!("Day 17");

    let pattern: Vec<Dir> = read_input_lines()[0].chars().map(parse_dir).collect();

    // Part 1
    let mut field: Field = vec![[true; 7]];
    let mut directions = pattern.iter().cycle();
    let mut rock = Kind::Minus.spawn(field.len());
    for _ in 0..2022 {
        rock.fall_all(&mut field, &mut directions);
        rock = rock.spawn_next(field.len());
    }
    println!("{}", field.len() - 1);

    // Part 2
    let mut field: Field = vec![[true; 7]];
    let mut directions = pattern.iter().cycle().enumerate().peekable();
    let mut rock = Kind::Minus.spawn(field.len());
    // Things to detect a cycle
    let dirs_len = pattern.len();
    // Values are, in order: whether it's a repetition, the height of the last
    // repetition and the rock index of last repetition
    let mut olds: Vec<[(bool, usize, usize); 5]> = vec![[(false, 0, 0); 5]; dirs_len];
    let mut r = 0;
    let mut hidden_h = 0;
    while r < 1000000000000_usize {
        rock.fall_all2(&mut field, &mut directions);
        // This is to make sure there are no border effect near the beginning
        if r >= dirs_len * 5 && hidden_h == 0 {
            let dir_idx = directions.peek().unwrap().0 % dirs_len;
            let rep = olds[dir_idx][rock.kind.to_usize()];
            if rep.0 {
                let dh = field.len() - rep.1;
                let dr = r - rep.2;
                println!("Delta h {}, delta rocks {}", dh, dr);
                let hidden_rep = (1000000000000_usize - 1 - r) / dr;
                r += hidden_rep * dr;
                hidden_h = hidden_rep * dh;
            };
            olds[dir_idx][rock.kind.to_usize()] = (true, field.len(), r);
        }
        rock = rock.spawn_next(field.len());
        r += 1;
    }
    println!("{}", hidden_h + field.len() - 1);
}
