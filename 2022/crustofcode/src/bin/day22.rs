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

// Up, Left, Down, Right
type Neighbours = [UPoint; 4];

#[derive(Debug, Clone)]
struct BoardNeighs {
    board: Board,
    neighbours: Vec<Vec<Neighbours>>,
}

impl BoardNeighs {
    fn get_neighs(&self, (x, y): UPoint) -> Neighbours {
        self.neighbours[x][y - self.board.starts[x]]
    }
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
            self.get_up((x, y)),
            (
                x,
                ((y - self.starts[x]) + self.rows[x].len() - 1) % self.rows[x].len()
                    + self.starts[x],
            ),
            self.get_down((x, y)),
            (
                x,
                ((y - self.starts[x]) + 1) % self.rows[x].len() + self.starts[x],
            ),
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

    // // This shit is taylored to my input, but I don't care
    // fn get_x_minus2(&self, (x, y): UPoint) -> UPoint {
    //     if x == 0 {
    //         if 50 <= y && y < 100 {
    //             (149, y)
    //         }
    //         else if 100 <= y && y < 150 {

    //         }
    //         else {
    //             panic!("AAAA");
    //         }
    //     }
    //     else {

    //     }
    // }

    // fn get_neighbours_pos2(&self, (x, y): UPoint) -> Neighbours {
    //     return [
    //     ];
    // }

    // fn get_neighbours2(self) -> BoardNeighs {
    //     let mut res = vec![];
    //     for x in 0..self.height {
    //         let max = self.starts[x] + self.rows[x].len();
    //         res.push(vec![]);
    //         for y in self.starts[x]..max {
    //             res[x].push(self.get_neighbours_pos2((x, y)));
    //         }
    //     }
    //     return BoardNeighs {
    //         board: self,
    //         neighbours: res,
    //     };
    // }
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

impl State {
    fn move_one(&mut self, board: &BoardNeighs) -> bool {
        // println!("Neighs of {:?}: {:?}", self.pos, board.get_neighs(self.pos));
        // println!("{}", self.dir.to_idx());
        let newpos = board.get_neighs(self.pos)[self.dir.to_idx()];
        if !board.board.is_empty(&newpos) {
            return false;
        }
        self.pos = newpos;
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
        newstate.dir = self.dir.rotate(&m.1);
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
    let ns = board.get_neighbours();

    // Part 1
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
    println!("{}", 0);
}
