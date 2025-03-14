
use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Eq, PartialEq)]
struct State {
    elevator: usize,
    floors: [Vec<Item>; 4],
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum Item {
    Generator(u8),
    Microchip(u8),
}

impl State {
    fn new(floors: [Vec<Item>; 4]) -> Self {
        State { elevator: 0, floors }
    }

    fn is_valid(&self) -> bool {
        for floor in &self.floors {
            let mut generators = Vec::new();
            let mut microchips = Vec::new();
            for item in floor {
                match item {
                    Item::Generator(id) => generators.push(id),
                    Item::Microchip(id) => microchips.push(id),
                }
            }

            if !generators.is_empty() {
                for microchip_id in microchips {
                    if !generators.contains(&microchip_id) {
                        return false;
                    }
                }
            }
        }
        true
    }


    fn is_goal(&self) -> bool {
        self.floors[0].is_empty() && self.floors[1].is_empty() && self.floors[2].is_empty()
    }

    fn generate_successors(&self) -> Vec<(State, usize)> {
        let mut successors = Vec::new();
        let current_floor = &self.floors[self.elevator];
        let directions = if self.elevator == 0 {
            vec![1]
        } else if self.elevator == 3 {
            vec![-1]
        } else {
            vec![-1, 1]
        };

        for &dir in &directions {
            let next_floor_index = (self.elevator as isize + dir) as usize;

            // Move one item
            for i in 0..current_floor.len() {
                let mut next_state = self.clone();
                let item = next_state.floors[self.elevator].remove(i);
                next_state.floors[next_floor_index].push(item);
                next_state.elevator = next_floor_index;
                if next_state.is_valid() {
                    successors.push((next_state, 1));
                }
            }

            // Move two items
            for i in 0..current_floor.len() {
                for j in i + 1..current_floor.len() {
                    let mut next_state = self.clone();
                    let item1 = next_state.floors[self.elevator].remove(j);
                    let item2 = next_state.floors[self.elevator].remove(i);

                    next_state.floors[next_floor_index].push(item1);
                    next_state.floors[next_floor_index].push(item2);
                    next_state.elevator = next_floor_index;

                    if next_state.is_valid() {
                        successors.push((next_state, 1));
                    }
                }
            }
        }
        successors
    }


    fn normalized(&self) -> Self {
        let mut pairs = Vec::new();
        for (floor_idx, floor) in self.floors.iter().enumerate() {
            for item in floor {
                match item {
                    Item::Generator(id) => {
                        let chip_floor = self.floors.iter().enumerate().find_map(|(chip_floor_idx, chip_floor)| {
                            chip_floor.iter().find(|chip| matches!(chip, Item::Microchip(chip_id) if chip_id == id)).map(|_| chip_floor_idx)
                        });

                        if let Some(chip_floor_idx) = chip_floor {
                            pairs.push((floor_idx, chip_floor_idx));
                        }
                    }
                    _ => {}
                }
            }
        }

        pairs.sort(); // Ensure consistent ordering

        let mut normalized_floors: [Vec<Item>; 4] = Default::default();
        let mut next_id = 0;
        let mut id_map = std::collections::HashMap::new();

        for &(gen_floor, chip_floor) in &pairs {
            let new_id = *id_map.entry((gen_floor, chip_floor)).or_insert(next_id);
           if next_id == new_id {
               next_id +=1;
           }
            normalized_floors[gen_floor].push(Item::Generator(new_id));
            normalized_floors[chip_floor].push(Item::Microchip(new_id));
        }
        
        State {
            elevator: self.elevator,
            floors: normalized_floors,
        }
    }
}


impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let normalized_state = self.normalized();
        normalized_state.elevator.hash(state);
        for floor in &normalized_state.floors {
            let mut floor_items = floor.clone();
            floor_items.sort_by_key(|item| match item {
              Item::Generator(id) => (*id, 0),
              Item::Microchip(id) => (*id, 1),
            });
            floor_items.hash(state); // Hash each floor after sorting
        }
    }
}

fn solve(initial_state: State) -> Option<usize> {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back((initial_state.clone(), 0));
    visited.insert(initial_state);

    while let Some((current_state, steps)) = queue.pop_front() {
        if current_state.is_goal() {
            return Some(steps);
        }

        for (successor, cost) in current_state.generate_successors() {
            if !visited.contains(&successor) {
                visited.insert(successor.clone());
                queue.push_back((successor, steps + cost));
            }
        }
    }

    None
}


fn parse_input(filename: &str) -> State {
    let file = File::open(filename).expect("Failed to open input file");
    let reader = BufReader::new(file);
    let mut floors: [Vec<Item>; 4] = Default::default();
    let mut id_gen = 0;

    for (i, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line");
        let words: Vec<&str> = line.split_whitespace().collect();

        let mut j = 0;
        while j < words.len() {
            if words[j] == "a" {
                if words[j + 2].starts_with("generator") {
                    let element = words[j + 1].split('-').next().unwrap();

                    let element_id = match element {
                        "hydrogen" => 0,
                        "lithium" => 1,
                        "promethium" => 2,
                        "cobalt" => 3,
                        "curium" => 4,
                        "ruthenium" => 5,
                        "plutonium" => 6,
                        _ => { id_gen+=1; id_gen+6 }
                    };
                    floors[i].push(Item::Generator(element_id));
                } else if words[j + 2].starts_with("microchip") {
                    let element = words[j + 1].split('-').next().unwrap();
                    let element_id = match element {
                        "hydrogen" => 0,
                        "lithium" => 1,
                        "promethium" => 2,
                        "cobalt" => 3,
                        "curium" => 4,
                        "ruthenium" => 5,
                        "plutonium" => 6,
                        _ => { id_gen+=1; id_gen+6 }
                    };
                    floors[i].push(Item::Microchip(element_id));
                }
                j += 3; // Skip "a", element, and type
            } else {
                j += 1;
            }
        }
    }
    State::new(floors)
}



fn main() {
    let initial_state = parse_input("input.txt");

    match solve(initial_state) {
        Some(steps) => println!("Minimum steps required: {}", steps),
        None => println!("No solution found."),
    }
}

