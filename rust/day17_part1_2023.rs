
use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    fs::File,
    io::{self, BufRead},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn opposite(&self) -> Self {
        match self {
            Direction::Up => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    row: usize,
    col: usize,
    dir: Direction,
    steps_in_dir: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Node {
    state: State,
    cost: usize,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn solve(grid: &Vec<Vec<usize>>) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();

    let start_state_right = State {
        row: 0,
        col: 0,
        dir: Direction::Right,
        steps_in_dir: 0,
    };

    let start_state_down = State {
        row: 0,
        col: 0,
        dir: Direction::Down,
        steps_in_dir: 0,
    };

    let mut dist: HashMap<State, usize> = HashMap::new();
    let mut pq: BinaryHeap<Node> = BinaryHeap::new();

    dist.insert(start_state_right, 0);
    dist.insert(start_state_down, 0);

    pq.push(Node {
        state: start_state_right,
        cost: 0,
    });

     pq.push(Node {
        state: start_state_down,
        cost: 0,
    });
   

    while let Some(Node { state, cost }) = pq.pop() {
        if state.row == rows - 1 && state.col == cols - 1 {
            return cost;
        }

        if cost > *dist.get(&state).unwrap_or(&usize::MAX) {
            continue;
        }

        let directions = [
             (Direction::Up, (-1, 0)),
             (Direction::Down, (1, 0)),
             (Direction::Left, (0, -1)),
             (Direction::Right, (0, 1)),
        ];

        for (next_dir, (dr, dc)) in directions.iter() {
           if next_dir == &state.dir.opposite() {
                continue;
           }
            let next_steps_in_dir = if *next_dir == state.dir {state.steps_in_dir+1} else {1};
            if next_steps_in_dir > 3 {
                continue;
            }

            let next_row = state.row as i32 + dr;
            let next_col = state.col as i32 + dc;

            if next_row >= 0
                && next_row < rows as i32
                && next_col >= 0
                && next_col < cols as i32
            {
                let next_row = next_row as usize;
                let next_col = next_col as usize;
                let next_state = State {
                    row: next_row,
                    col: next_col,
                    dir: *next_dir,
                    steps_in_dir: next_steps_in_dir,
                };
                let next_cost = cost + grid[next_row][next_col];
                if next_cost < *dist.get(&next_state).unwrap_or(&usize::MAX) {
                    dist.insert(next_state, next_cost);
                    pq.push(Node {
                        state: next_state,
                        cost: next_cost,
                    });
                }
            }
        }
    }
    unreachable!("No path found")
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<Vec<usize>> = reader
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect();

    let result = solve(&grid);
    println!("{}", result);

    Ok(())
}
