use crustofcode::*;

type Mossa = (usize, Option<Rot>);

fn parse_path_inner(line: &str, res: &mut Vec<Mossa>) -> () {
    let pos = line.chars().position(|c| !c.is_digit(10));
    if pos.is_none() {
        res.push((line.parse().unwrap(), None));
    } else {
        let pos = pos.unwrap();
        let num: usize = line[0..pos].parse().unwrap();
        let rot = Rot::from_char(line.chars().nth(pos).unwrap());
        res.push((num, Some(rot)));
        parse_path_inner(&line[pos + 1..], res);
    }
}

fn parse_path(line: String) -> Vec<Mossa> {
    let mut res = vec![];
    parse_path_inner(&line, &mut res);
    return res;
}

#[derive(Debug, Clone)]
struct Board {
    height: usize,
    starts: Vec<usize>,
    rows: Vec<Vec<bool>>,
}

fn parse_board<'a, I>(lines: &mut I) -> Board
where
    I: Iterator<Item = &'a String>,
{
    let mut res = Board {
        height: 0,
        starts: vec![],
        rows: vec![],
    };
    for line in lines {
        let mut i = line
            .chars()
            .enumerate()
            .skip_while(|(_, c)| *c == ' ')
            .peekable();
        res.starts.push(i.peek().unwrap().0);
        res.rows.push(i.map(into_snd).map(|c| c == '#').collect());
        res.height += 1;
    }
    return res;
}

#[derive(Debug, Clone)]
struct BoardNeighs {
    board: Board,
    neighbours: Vec<Vec<Neighbours>>,
}

impl Board {
    fn in_range(&self, x: usize, y: usize) -> bool {
        self.starts[x] <= y && y < self.starts[x] + self.rows[x].len()
    }

    fn is_empty(&self, p: &UPoint) -> bool {
        !self.rows[p.0][p.1 - self.starts[p.0]]
    }

    fn get_up(&self, (x, y): UPoint) -> UPoint {
        let mut newx = x;
        if x == 0 || !self.in_range(x - 1, y) {
            while newx < self.height && self.in_range(newx, y) {
                newx += 1;
            }
        }
        return (newx - 1, y);
    }

    fn get_down(&self, (x, y): UPoint) -> UPoint {
        let mut newx = x;
        if x == self.height - 1 || !self.in_range(x + 1, y) {
            while self.in_range(newx, y) {
                if newx == 0 {
                    return (newx, y);
                }
                newx -= 1;
            }
        }
        return (newx + 1, y);
    }

    fn get_neighbours_pos(&self, (x, y): UPoint) -> Neighbours {
        return [
            State {
                pos: self.get_up((x, y)),
                dir: Dir::Up,
            },
            State {
                pos: (
                    x,
                    ((y - self.starts[x]) + self.rows[x].len() - 1) % self.rows[x].len()
                        + self.starts[x],
                ),
                dir: Dir::Left,
            },
            State {
                pos: self.get_down((x, y)),
                dir: Dir::Down,
            },
            State {
                pos: (
                    x,
                    ((y - self.starts[x]) + 1) % self.rows[x].len() + self.starts[x],
                ),
                dir: Dir::Right,
            },
        ];
    }

    fn get_neighbours(self) -> BoardNeighs {
        let mut res = vec![];
        for x in 0..self.height {
            let max = self.starts[x] + self.rows[x].len();
            res.push(vec![]);
            for y in self.starts[x]..max {
                res[x].push(self.get_neighbours_pos((x, y)));
            }
        }
        return BoardNeighs {
            board: self,
            neighbours: res,
        };
    }

    // This shit is taylored to my input, but I don't care
    #[allow(unused_comparisons)]
    fn get_x_minus2(&self, (x, y): UPoint) -> State {
        if x == 0 {
            if 50 <= y && y < 100 {
                // (0, 1-2) -> (3-4, 0)
                // 50-99 => 150-199
                State {
                    pos: (100 + y, 0),
                    dir: Dir::Right,
                }
            } else if 100 <= y && y < 150 {
                // (0, 2-3) -> (4, 0-1)
                // 100, 149 => 0-49
                State {
                    pos: (199, y - 100),
                    dir: Dir::Up,
                }
            } else {
                panic!("AAAA");
            }
        } else if x == 50 {
            if 50 <= y && y < 100 {
                // (1, 1-2) -> (1, 1-2)
                State {
                    pos: (49, y),
                    dir: Dir::Up,
                }
            } else {
                panic!("AAAA");
            }
        } else if x == 100 {
            if 0 <= y && y < 50 {
                // (2, 0-1) -> (1-2, 1)
                // 0, 49 => 50, 99
                State {
                    pos: (50 + y, 50),
                    dir: Dir::Right,
                }
            } else if 50 <= y && y < 100 {
                // (2, 1-2) -> (2, 1-2)
                State {
                    pos: (99, y),
                    dir: Dir::Up,
                }
            } else {
                panic!("AAAAA");
            }
        } else if x == 150 {
            if 0 <= y && y < 50 {
                // (3, 0-1) -> (3, 0-1)
                State {
                    pos: (149, y),
                    dir: Dir::Up,
                }
            } else {
                panic!("AAAAA");
            }
        } else {
            State {
                pos: (x - 1, y),
                dir: Dir::Up,
            }
        }
    }

    #[allow(unused_comparisons)]
    fn get_x_plus2(&self, (x, y): UPoint) -> State {
        if x == 49 {
            if 50 <= y && y < 100 {
                // (1, 1-2) => (1, 1-2)
                State {
                    pos: (50, y),
                    dir: Dir::Down,
                }
            } else if 100 <= y && y < 150 {
                // (1, 2-3) -> (1-2, 2)
                // 100-149 => 50-99
                State {
                    pos: (y - 50, 99),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else if x == 99 {
            if 50 <= y && y < 100 {
                // (2, 1-2) -> (2, 1-2)
                State {
                    pos: (100, y),
                    dir: Dir::Down,
                }
            } else {
                panic!("AAAAA");
            }
        } else if x == 149 {
            if 0 <= y && y < 50 {
                // (3, 0-1) -> (3, 0-1)
                State {
                    pos: (150, y),
                    dir: Dir::Down,
                }
            } else if 50 <= y && y < 100 {
                // (3, 1-2) -> (3-4, 1)
                // 50-99 => 150-199
                State {
                    pos: (100 + y, 49),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else if x == 199 {
            if 0 <= y && y < 50 {
                // (4, 0-1) -> (0, 2-3)
                // 0-49 => 100-149
                State {
                    pos: (0, 100 + y),
                    dir: Dir::Down,
                }
            } else {
                panic!("AAAAA");
            }
        } else {
            State {
                pos: (x + 1, y),
                dir: Dir::Down,
            }
        }
    }

    #[allow(unused_comparisons)]
    fn get_y_minus2(&self, (x, y): UPoint) -> State {
        if y == 0 {
            if 100 <= x && x < 150 {
                // (2-3, 0) -> (1-0, 1)
                // 100-149 => 49-0
                State {
                    pos: (149 - x, 50),
                    dir: Dir::Right,
                }
            } else if 150 <= x && x < 200 {
                // (3-4, 0) -> (0, 1-2)
                // 150-199 => 50-99
                State {
                    pos: (0, x - 100),
                    dir: Dir::Down,
                }
            } else {
                panic!("AAAAA");
            }
        } else if y == 50 {
            if 0 <= x && x < 50 {
                // (0-1, 1) -> (3-2, 0)
                // 0-49 => 149-100
                State {
                    pos: (149 - x, 0),
                    dir: Dir::Right,
                }
            } else if 50 <= x && x < 100 {
                // (1-2, 1) -> (2, 0-1)
                // 50-99 => 0-49
                State {
                    pos: (100, x - 50),
                    dir: Dir::Down,
                }
            } else if 100 <= x && x < 150 {
                // (2-3, 1) -> (2-3, 1)
                State {
                    pos: (x, 49),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else if y == 100 {
            if 0 <= x && x < 50 {
                // (0-1, 2) -> (0-1, 2)
                State {
                    pos: (x, 99),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else {
            State {
                pos: (x, y - 1),
                dir: Dir::Left,
            }
        }
    }

    #[allow(unused_comparisons)]
    fn get_y_plus2(&self, (x, y): UPoint) -> State {
        if y == 49 {
            if 100 <= x && x < 150 {
                // (2-3, 1) -> (2-3, 1)
                State {
                    pos: (x, 50),
                    dir: Dir::Right,
                }
            } else if 150 <= x && x < 200 {
                // (3-4, 1) -> (3, 1-2)
                // 150-199 => 50-99
                State {
                    pos: (149, x - 100),
                    dir: Dir::Up,
                }
            } else {
                panic!("AAAAA");
            }
        } else if y == 99 {
            if 0 <= x && x < 50 {
                // (0-1, 2) -> (0-1, 2)
                State {
                    pos: (x, 100),
                    dir: Dir::Right,
                }
            } else if 50 <= x && x < 100 {
                // (1-2, 2) -> (1, 2-3)
                State {
                    pos: (49, x + 50),
                    dir: Dir::Up,
                }
            } else if 100 <= x && x < 150 {
                // (2-3, 2) -> (1-0, 3)
                State {
                    pos: (149 - x, 149),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else if y == 149 {
            if 0 <= x && x < 50 {
                // (0-1, 3) -> (3-2, 2)
                State {
                    pos: (149 - x, 99),
                    dir: Dir::Left,
                }
            } else {
                panic!("AAAAA");
            }
        } else {
            State {
                pos: (x, y + 1),
                dir: Dir::Right,
            }
        }
    }

    fn get_neighbours_pos2(&self, p: UPoint) -> Neighbours {
        [
            self.get_x_minus2(p),
            self.get_y_minus2(p),
            self.get_x_plus2(p),
            self.get_y_plus2(p),
        ]
    }

    fn get_neighbours2(self) -> BoardNeighs {
        let mut res = vec![];
        for x in 0..self.height {
            let max = self.starts[x] + self.rows[x].len();
            res.push(vec![]);
            for y in self.starts[x]..max {
                res[x].push(self.get_neighbours_pos2((x, y)));
            }
        }
        return BoardNeighs {
            board: self,
            neighbours: res,
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Rot {
    L,
    R,
}

impl Rot {
    fn from_char(c: char) -> Rot {
        match c {
            'L' => Rot::L,
            'R' => Rot::R,
            _ => panic!("AAAAA"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Dir {
    Up,
    Left,
    Down,
    Right,
}

impl Dir {
    fn rotate(&self, r: &Option<Rot>) -> Dir {
        match (self, r) {
            (d, None) => d.clone(),
            (Dir::Up, Some(Rot::L)) => Dir::Left,
            (Dir::Left, Some(Rot::L)) => Dir::Down,
            (Dir::Down, Some(Rot::L)) => Dir::Right,
            (Dir::Right, Some(Rot::L)) => Dir::Up,
            (Dir::Up, Some(Rot::R)) => Dir::Right,
            (Dir::Left, Some(Rot::R)) => Dir::Up,
            (Dir::Down, Some(Rot::R)) => Dir::Left,
            (Dir::Right, Some(Rot::R)) => Dir::Down,
        }
    }

    fn to_idx(&self) -> usize {
        match self {
            Dir::Up => 0,
            Dir::Left => 1,
            Dir::Down => 2,
            Dir::Right => 3,
        }
    }

    fn get_value(&self) -> usize {
        3 - self.to_idx()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    pos: UPoint,
    dir: Dir,
}

// Up, Left, Down, Right
type Neighbours = [State; 4];

impl BoardNeighs {
    fn get_neighs<'a>(&'a self, (x, y): UPoint) -> &'a Neighbours {
        &self.neighbours[x][y - self.board.starts[x]]
    }
}

impl State {
    fn move_one(&mut self, board: &BoardNeighs) -> bool {
        // println!("Neighs of {:?}: {:?}", self.pos, board.get_neighs(self.pos));
        // println!("{}", self.dir.to_idx());
        let news = &board.get_neighs(self.pos)[self.dir.to_idx()];
        if !board.board.is_empty(&news.pos) {
            return false;
        }
        self.pos = news.pos;
        self.dir = news.dir.clone();
        return true;
    }

    fn apply_move(&self, m: &Mossa, board: &BoardNeighs) -> State {
        let mut newstate = State {
            pos: self.pos.clone(),
            dir: self.dir.clone(),
        };
        for _ in 0..m.0 {
            if !newstate.move_one(board) {
                break;
            }
        }
        newstate.dir = newstate.dir.rotate(&m.1);
        return newstate;
    }

    fn get_value(&self) -> usize {
        1000 * (self.pos.0 + 1) + 4 * (self.pos.1 + 1) + self.dir.get_value()
    }
}

fn main() {
    println!("Day 22");

    let input = crustofcode::read_input_lines();
    let mut pieces = input.as_slice().split(|l| l == "");
    let board = parse_board(&mut pieces.next().unwrap().into_iter());
    let path = parse_path(pieces.next().unwrap()[0].clone());
    assert_eq!(pieces.next(), None);

    // Part 1
    let ns = board.clone().get_neighbours();
    let mut pos = State {
        pos: (0, ns.board.starts[0]),
        dir: Dir::Right,
    };
    for mossa in &path {
        pos = pos.apply_move(mossa, &ns);
        // println!("{:?}", pos);
    }
    println!("{}", pos.get_value());

    // Part 2
    let ns = board.get_neighbours2();
    let mut pos = State {
        pos: (0, ns.board.starts[0]),
        dir: Dir::Right,
    };
    for mossa in &path {
        pos = pos.apply_move(mossa, &ns);
    }
    println!("{}", pos.get_value());
}
