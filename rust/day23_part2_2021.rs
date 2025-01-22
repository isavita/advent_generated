
use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    fs,
};

#[derive(Clone, Eq, PartialEq, Debug)]
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

fn parse_input(input: &str) -> State {
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    State {
        grid,
        energy_used: 0,
        path: String::new(),
    }
}

fn amphipod(input: &str) -> usize {
    let mut start = parse_input(input);
    let room_coord_to_want_char: HashMap<[usize; 2], char> = HashMap::from([
        ([2, 3], 'A'),
        ([3, 3], 'A'),
        ([4, 3], 'A'),
        ([5, 3], 'A'),
        ([2, 5], 'B'),
        ([3, 5], 'B'),
        ([4, 5], 'B'),
        ([5, 5], 'B'),
        ([2, 7], 'C'),
        ([3, 7], 'C'),
        ([4, 7], 'C'),
        ([5, 7], 'C'),
        ([2, 9], 'D'),
        ([3, 9], 'D'),
        ([4, 9], 'D'),
        ([5, 9], 'D'),
    ]);

    let mut new_rows = vec![
        "  #D#C#B#A#  ".chars().collect::<Vec<char>>(),
        "  #D#B#A#C#  ".chars().collect::<Vec<char>>(),
    ];

    start.grid.splice(3..3, new_rows.drain(..));

    let mut min_heap: BinaryHeap<State> = BinaryHeap::new();
    min_heap.push(start);
    let mut seen_grids: HashMap<String, bool> = HashMap::new();

    while let Some(front) = min_heap.pop() {
        let key = format!("{:?}", front.grid);
        if seen_grids.contains_key(&key) {
            continue;
        }
        seen_grids.insert(key, true);

        if all_done(&front, &room_coord_to_want_char) {
            return front.energy_used;
        }

        let unsettled_coords = get_unsettled_coords(&front, &room_coord_to_want_char);
        for unsettled_coord in unsettled_coords {
            let next_moves = get_next_possible_moves(&front, unsettled_coord, &room_coord_to_want_char);
            for next_coord in next_moves {
                let mut cp = front.clone();
                let ur = unsettled_coord[0];
                let uc = unsettled_coord[1];
                let nr = next_coord[0];
                let nc = next_coord[1];

                cp.energy_used += calc_energy(cp.grid[ur][uc], unsettled_coord, next_coord);
                cp.path += &format!(
                    "{} {:?}->{:?}{},",
                    cp.grid[ur][uc], unsettled_coord, next_coord, cp.energy_used
                );
                cp.grid[nr][nc] = cp.grid[ur][uc];
                cp.grid[ur][uc] = '.';
                min_heap.push(cp);
            }
        }
    }

    panic!("should return from loop");
}

fn all_done(state: &State, room_coord_to_want_char: &HashMap<[usize; 2], char>) -> bool {
    for (coord, want) in room_coord_to_want_char {
        if state.grid[coord[0]][coord[1]] != *want {
            return false;
        }
    }
    true
}

fn get_unsettled_coords(
    state: &State,
    room_coord_to_want_char: &HashMap<[usize; 2], char>,
) -> Vec<[usize; 2]> {
    let mut unsettled = Vec::new();

    for col in 1..state.grid[0].len() {
        if "ABCD".contains(state.grid[1][col]) {
            unsettled.push([1, col]);
        }
    }

    for &col in &[3, 5, 7, 9] {
        let mut room_full_from_back = true;
        for row in (2..state.grid.len() - 1).rev() {
            let coord = [row, col];
            let want_char = room_coord_to_want_char[&coord];
            let got_char = state.grid[row][col];
            if got_char != '.' {
                if got_char != want_char {
                    room_full_from_back = false;
                    unsettled.push(coord);
                } else if got_char == want_char && !room_full_from_back {
                    unsettled.push(coord);
                }
            }
        }
    }
    unsettled
}

fn is_in_hallway(coord: [usize; 2]) -> bool {
    coord[0] == 1
}

fn get_next_possible_moves(
    state: &State,
    unsettled_coord: [usize; 2],
    room_coord_to_want_char: &HashMap<[usize; 2], char>,
) -> Vec<[usize; 2]> {
    let unsettled_char = state.grid[unsettled_coord[0]][unsettled_coord[1]];

    if !"ABCD".contains(unsettled_char) {
        panic!("unexpected character to get next moves for: {}", unsettled_char);
    }

    let mut possible: Vec<[usize; 2]> = Vec::new();
    let started_in_hallway = is_in_hallway(unsettled_coord);

    let mut queue = vec![unsettled_coord];
    let mut seen: HashMap<[usize; 2], bool> = HashMap::new();
    let coords_in_front_of_rooms: HashMap<[usize; 2], bool> =
        HashMap::from([([1, 3], true), ([1, 5], true), ([1, 7], true), ([1, 9], true)]);

    while let Some(front) = queue.pop() {
        if seen.contains_key(&front) {
            continue;
        }
        seen.insert(front, true);

        if front != unsettled_coord {
           
            if !coords_in_front_of_rooms.contains_key(&front) {
                if let Some(want_char) = room_coord_to_want_char.get(&front) {
                    if *want_char == unsettled_char {
                        let mut is_stuck_amphipod = false;
                        let mut room_has_deeper_open_spaces = false;
                        for r in front[0] + 1..state.grid.len() - 1 {
                            let char = state.grid[r][front[1]];
                            if char == '.' {
                                room_has_deeper_open_spaces = true;
                            }
                            if char != '.' && char != unsettled_char {
                                is_stuck_amphipod = true;
                                break;
                            }
                        }

                        if !room_has_deeper_open_spaces && !is_stuck_amphipod {
                            possible.push(front);
                        }
                    }
                } else if !started_in_hallway {
                     possible.push(front);
                }
            }
        }

        for d in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let next = [
                (front[0] as isize + d.0) as usize,
                (front[1] as isize + d.1) as usize,
            ];
           if state.grid[next[0]][next[1]] == '.' {
                queue.push(next);
            }
        }
    }

    possible
}

fn calc_energy(char: char, start: [usize; 2], end: [usize; 2]) -> usize {
    let dist = end[1].abs_diff(start[1]) + (start[0] - 1) + (end[0] - 1);

    let energy_per_type: HashMap<char, usize> =
        HashMap::from([('A', 1), ('B', 10), ('C', 100), ('D', 1000)]);

    energy_per_type[&char] * dist
}


fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let ans = amphipod(&input.trim());
    println!("{}", ans);
}
