
use std::collections::HashMap;
use std::fs;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

#[derive(Clone, Copy)]
struct Rock {
    points: [Point; 5],
    width: i64,
    height: i64,
}

const ROCKS: [Rock; 5] = [
    Rock {
        points: [
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },
            Point { x: 2, y: 0 },
            Point { x: 3, y: 0 },
            Point { x: 0, y: 0 }, // Dummy point
        ],
        width: 4,
        height: 1,
    },
    Rock {
        points: [
            Point { x: 1, y: 0 },
            Point { x: 0, y: 1 },
            Point { x: 1, y: 1 },
            Point { x: 2, y: 1 },
            Point { x: 1, y: 2 },
        ],
        width: 3,
        height: 3,
    },
    Rock {
        points: [
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },
            Point { x: 2, y: 0 },
            Point { x: 2, y: 1 },
            Point { x: 2, y: 2 },
        ],
        width: 3,
        height: 3,
    },
    Rock {
        points: [
            Point { x: 0, y: 0 },
            Point { x: 0, y: 1 },
            Point { x: 0, y: 2 },
            Point { x: 0, y: 3 },
            Point { x: 0, y: 0 },
        ],
        width: 1,
        height: 4,
    },
    Rock {
        points: [
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },
            Point { x: 0, y: 1 },
            Point { x: 1, y: 1 },
            Point { x: 0, y: 0 },
        ],
        width: 2,
        height: 2,
    },
];

fn simulate(jet_pattern: &str, num_rocks: usize) -> i64 {
    let mut chamber: HashMap<Point, bool> = HashMap::new();
    let mut top = 0;
    let mut jet_index = 0;

    for rock_index in 0..num_rocks {
        let rock = ROCKS[rock_index % 5];
        let mut rock_pos = Point {
            x: 2,
            y: top + 3,
        };

        loop {
            // Apply jet
            let jet = jet_pattern.chars().nth(jet_index % jet_pattern.len()).unwrap();
            jet_index += 1;
            let dx = if jet == '<' { -1 } else { 1 };
            let mut can_move = true;
            for point in &rock.points {
                let new_pos = Point {
                    x: rock_pos.x + point.x + dx,
                    y: rock_pos.y + point.y,
                };
                if new_pos.x < 0 || new_pos.x >= 7 || chamber.contains_key(&new_pos) {
                    can_move = false;
                    break;
                }
            }
            if can_move {
                rock_pos.x += dx;
            }

            // Fall down
            let mut can_fall = true;
            for point in &rock.points {
                let new_pos = Point {
                    x: rock_pos.x + point.x,
                    y: rock_pos.y + point.y - 1,
                };
                if new_pos.y < 0 || chamber.contains_key(&new_pos) {
                    can_fall = false;
                    break;
                }
            }
            if can_fall {
                rock_pos.y -= 1;
            } else {
                // Stop and add to chamber
                for point in &rock.points {
                    let final_pos = Point {
                        x: rock_pos.x + point.x,
                        y: rock_pos.y + point.y,
                    };
                    chamber.insert(final_pos, true);
                    top = top.max(final_pos.y + 1);
                }
                break;
            }
        }
    }

    top
}


fn simulate_optimized(jet_pattern: &str, num_rocks: i64) -> i64 {
    let mut chamber: HashMap<Point, bool> = HashMap::new();
    let mut top = 0;
    let mut jet_index = 0;

    let mut seen_states: HashMap<(usize, usize, Vec<i64>), (i64, i64)> = HashMap::new(); // (rock_index, jet_index, top_profile) -> (rock_count, height)
    let mut rock_count = 0;
    let mut added_height = 0;

    while rock_count < num_rocks {
        let rock_index = (rock_count % 5) as usize;
        let rock = ROCKS[rock_index];

        let mut rock_pos = Point {
            x: 2,
            y: top + 3,
        };

        loop {
            let jet_idx = jet_index % jet_pattern.len();
            let jet = jet_pattern.chars().nth(jet_idx).unwrap();
           
            jet_index += 1;
            let dx = if jet == '<' { -1 } else { 1 };

            let mut can_move = true;
            for point in &rock.points {
                let new_pos = Point {
                    x: rock_pos.x + point.x + dx,
                    y: rock_pos.y + point.y,
                };
                if new_pos.x < 0 || new_pos.x >= 7 || chamber.contains_key(&new_pos) {
                    can_move = false;
                    break;
                }
            }
            if can_move {
                rock_pos.x += dx;
            }

            let mut can_fall = true;
            for point in &rock.points {
                 let new_pos = Point {
                    x: rock_pos.x + point.x,
                    y: rock_pos.y + point.y -1
                };

                if new_pos.y < 0 || chamber.contains_key(&new_pos)
                {
                    can_fall = false;
                    break;
                }

            }

            if can_fall {
                rock_pos.y -= 1;
            } else {
                for point in &rock.points {
                    let final_pos = Point {
                        x: rock_pos.x + point.x,
                        y: rock_pos.y + point.y,
                    };
                    chamber.insert(final_pos, true);
                    top = top.max(final_pos.y + 1);
                }
                break;
            }
        }

        let profile = (0..7)
            .map(|x| {
                let mut y = top;
                while y >= 0 && !chamber.contains_key(&Point { x, y }) {
                    y -= 1;
                }
                top - y
            })
            .collect::<Vec<_>>();

        let state = (rock_index, jet_index % jet_pattern.len(), profile);


        if seen_states.contains_key(&state) && rock_count > 2022 {
            let (prev_rock_count, prev_height) = seen_states[&state];
            let cycle_length = rock_count - prev_rock_count;
            let remaining_rocks = num_rocks - rock_count;
            let num_cycles = remaining_rocks / cycle_length;
            added_height += num_cycles * (top - prev_height);
            rock_count += num_cycles * cycle_length;
        
        }
      
        seen_states.insert(state, (rock_count, top));
        rock_count+=1;

    }
    top + added_height
}




fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let jet_pattern = input.trim();

    let part1_result = simulate(jet_pattern, 2022);
    println!("{}", part1_result);

    let part2_result = simulate_optimized(jet_pattern, 1000000000000);
    println!("{}", part2_result);

}
