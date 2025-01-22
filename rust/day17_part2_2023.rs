
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
struct State {
    row: usize,
    col: usize,
    dir: Direction,
    steps: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Node {
    cost: usize,
    state: State,
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

fn solve(grid: &Vec<Vec<u32>>, min_steps: usize, max_steps: usize) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();
    let start = State {
        row: 0,
        col: 0,
        dir: Direction::Right,
        steps: 0,
    };

    let mut dist: std::collections::HashMap<(usize, usize, Direction, usize), usize> =
        std::collections::HashMap::new();
    let mut queue = BinaryHeap::new();
    queue.push(Node { cost: 0, state: start });
    dist.insert((start.row, start.col, start.dir, start.steps), 0);

    while let Some(Node { cost, state }) = queue.pop() {
        if cost > *dist.get(&(state.row, state.col, state.dir, state.steps)).unwrap_or(&usize::MAX) {
            continue;
        }
        if state.row == rows - 1 && state.col == cols - 1 && state.steps >= min_steps {
             return cost;
        }
        
        let moves = match state.dir {
            Direction::Up => [Direction::Up, Direction::Left, Direction::Right],
            Direction::Down => [Direction::Down, Direction::Left, Direction::Right],
            Direction::Left => [Direction::Left, Direction::Up, Direction::Down],
            Direction::Right => [Direction::Right, Direction::Up, Direction::Down],
        };


        for new_dir in moves {
            let new_steps = if new_dir == state.dir {state.steps+1} else {1};
            if new_steps > max_steps {
                continue;
            }

            if new_dir != state.dir && state.steps < min_steps && state.steps != 0{
               continue;
            }

            let (new_row, new_col) = match new_dir {
                Direction::Up => (state.row.checked_sub(1), Some(state.col)),
                Direction::Down => (state.row.checked_add(1), Some(state.col)),
                Direction::Left => (Some(state.row), state.col.checked_sub(1)),
                Direction::Right => (Some(state.row), state.col.checked_add(1)),
            };

            if let (Some(new_row), Some(new_col)) = (new_row, new_col) {
                if new_row < rows && new_col < cols {
                    let new_cost = cost + grid[new_row][new_col] as usize;
                     if new_cost < *dist.get(&(new_row, new_col, new_dir,new_steps)).unwrap_or(&usize::MAX){
                          dist.insert((new_row, new_col, new_dir, new_steps), new_cost);
                          queue.push(Node {
                               cost: new_cost,
                               state: State {
                                    row: new_row,
                                    col: new_col,
                                    dir: new_dir,
                                    steps: new_steps,
                               },
                           });

                    }
                }
             }
        }
    }

    usize::MAX
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let grid: Vec<Vec<u32>> = reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            line.chars()
                .filter_map(|c| c.to_digit(10))
                .collect::<Vec<u32>>()
        })
        .collect();

    let part1 = solve(&grid, 1, 3);
    println!("Part 1: {}", part1);

    let part2 = solve(&grid, 4, 10);
    println!("Part 2: {}", part2);

    Ok(())
}
