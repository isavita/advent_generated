
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;
use std::cmp::Ordering;

#[derive(Clone, Eq, PartialEq)]
struct State {
    grid: Vec<Vec<char>>,
    energy_used: usize,
    path: String,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.energy_used.cmp(&self.energy_used)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl State {
    fn new(grid: Vec<Vec<char>>, energy_used: usize, path: String) -> State {
        State {
            grid,
            energy_used,
            path,
        }
    }

    fn all_done(&self, room_coord_to_want_char: &HashMap<(usize, usize), char>) -> bool {
        for (&coord, &want) in room_coord_to_want_char {
            if self.grid[coord.0][coord.1] != want {
                return false;
            }
        }
        true
    }

    fn get_unsettled_coords(
        &self,
        room_coord_to_want_char: &HashMap<(usize, usize), char>,
    ) -> Vec<(usize, usize)> {
        let mut unsettled = Vec::new();

        for col in 1..self.grid[0].len() {
            if self.grid[1][col..].iter().any(|&c| "ABCD".contains(c)) {
                unsettled.push((1, col));
            }
        }

        for &col in &[3, 5, 7, 9] {
            let mut room_full_from_back = true;
            for row in (2..self.grid.len() - 1).rev() {
                let coord = (row, col);
                let want_char = *room_coord_to_want_char.get(&coord).unwrap();
                let got_char = self.grid[row][col];
                if got_char != '.' {
                    if got_char != want_char {
                        room_full_from_back = false;
                        unsettled.push(coord);
                    } else if !room_full_from_back {
                        unsettled.push(coord);
                    }
                }
            }
        }
        unsettled
    }

    fn get_next_possible_moves(
        &self,
        unsettled_coord: (usize, usize),
        room_coord_to_want_char: &HashMap<(usize, usize), char>,
    ) -> Vec<(usize, usize)> {
        let unsettled_char = self.grid[unsettled_coord.0][unsettled_coord.1];

        let mut possible = Vec::new();
        let started_in_hallway = unsettled_coord.0 == 1;

        let mut queue = vec![unsettled_coord];
        let mut seen = HashSet::new();

        while let Some(front) = queue.pop() {
            if seen.contains(&front) {
                continue;
            }
            seen.insert(front);

            if front != unsettled_coord {
                if ![(1, 3), (1, 5), (1, 7), (1, 9)].contains(&front) {
                    if let Some(&want_char) = room_coord_to_want_char.get(&front) {
                        if want_char == unsettled_char {
                            let mut is_stuck_amphipod = false;
                            let mut room_has_deeper_open_spaces = false;
                            for r in front.0 + 1..self.grid.len() - 1 {
                                let char = self.grid[r][front.1];
                                if char == '.' {
                                    room_has_deeper_open_spaces = true;
                                } else if char != unsettled_char {
                                    is_stuck_amphipod = true;
                                    break;
                                }
                            }

                            if !room_has_deeper_open_spaces && !is_stuck_amphipod {
                                possible.push(front);
                            }
                        }
                    } else {
                        if !started_in_hallway {
                            possible.push(front);
                        }
                    }
                }
            }

            for &(dr, dc) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let next_row = (front.0 as isize + dr) as usize;
                let next_col = (front.1 as isize + dc) as usize;
                if next_row < self.grid.len()
                    && next_col < self.grid[0].len()
                    && self.grid[next_row][next_col] == '.'
                {
                    queue.push((next_row, next_col));
                }
            }
        }
        possible
    }
}

fn calc_energy(c: char, start: (usize, usize), end: (usize, usize)) -> usize {
    let dist = (end.1 as isize - start.1 as isize).abs() as usize
        + (start.0 - 1)
        + (end.0 - 1);

    let energy_per_type = match c {
        'A' => 1,
        'B' => 10,
        'C' => 100,
        'D' => 1000,
        _ => panic!("Invalid char"),
    };
    energy_per_type * dist
}

fn parse_input(input_str: &str) -> State {
    let grid = input_str.lines().map(|line| line.chars().collect()).collect();
    State::new(grid, 0, String::new())
}

fn amphipod(input_str: &str) -> usize {
    let start = parse_input(input_str);
    let room_coord_to_want_char: HashMap<(usize, usize), char> = [
        ((2, 3), 'A'),
        ((3, 3), 'A'),
        ((2, 5), 'B'),
        ((3, 5), 'B'),
        ((2, 7), 'C'),
        ((3, 7), 'C'),
        ((2, 9), 'D'),
        ((3, 9), 'D'),
    ]
    .iter()
    .cloned()
    .collect();

    let mut min_heap = BinaryHeap::new();
    min_heap.push(start);
    let mut seen_grids = HashSet::new();

    while let Some(front) = min_heap.pop() {
        let key = front.grid.iter().flatten().collect::<String>();

        if seen_grids.contains(&key) {
            continue;
        }
        seen_grids.insert(key);

        if front.all_done(&room_coord_to_want_char) {
            return front.energy_used;
        }

        let unsettled_coords = front.get_unsettled_coords(&room_coord_to_want_char);
        for unsettled_coord in unsettled_coords {
            let next_moves =
                front.get_next_possible_moves(unsettled_coord, &room_coord_to_want_char);
            for next_coord in next_moves {
                let mut cp = State::new(front.grid.clone(), front.energy_used, front.path.clone());

                cp.energy_used +=
                    calc_energy(cp.grid[unsettled_coord.0][unsettled_coord.1], unsettled_coord, next_coord);
                cp.grid[next_coord.0][next_coord.1] = cp.grid[unsettled_coord.0][unsettled_coord.1];
                cp.grid[unsettled_coord.0][unsettled_coord.1] = '.';

                min_heap.push(cp);
            }
        }
    }

    panic!("Should return from loop")
}

fn main() {
    let input_str = fs::read_to_string("input.txt").expect("Unable to read file");
    let result = amphipod(input_str.trim());
    println!("{}", result);
}
