
use std::fs;

#[derive(Clone, Copy, Debug)]
enum RockType {
    Horizontal,
    Cross,
    L,
    Vertical,
    Square,
}

impl RockType {
    fn get_shape(&self) -> Vec<(i32, i32)> {
        match self {
            RockType::Horizontal => vec![(0, 0), (1, 0), (2, 0), (3, 0)],
            RockType::Cross => vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
            RockType::L => vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
            RockType::Vertical => vec![(0, 0), (0, 1), (0, 2), (0, 3)],
            RockType::Square => vec![(0, 0), (1, 0), (0, 1), (1, 1)],
        }
    }
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let jets: Vec<char> = contents.trim().chars().collect();
    let rocks = [
        RockType::Horizontal,
        RockType::Cross,
        RockType::L,
        RockType::Vertical,
        RockType::Square,
    ];

    let mut chamber: Vec<Vec<bool>> = vec![vec![false; 7]; 10000];
    let mut highest_rock = 0;
    let mut jet_idx = 0;

    for rock_count in 0..2022 {
        let rock_type = rocks[rock_count % rocks.len()];
        let rock_shape = rock_type.get_shape();
        let mut rock_pos = (2, highest_rock + 3);

        loop {
            // Move by jet
            let jet_dir = jets[jet_idx % jets.len()];
            let new_x = rock_pos.0 + if jet_dir == '<' { -1 } else { 1 };

            if new_x >= 0 && new_x + rock_shape.iter().map(|(x, _)| x).max().unwrap() < 7 {
                let mut can_move = true;
                for (dx, dy) in &rock_shape {
                    if chamber[(rock_pos.1 + dy) as usize][(new_x + dx) as usize] {
                        can_move = false;
                        break;
                    }
                }

                if can_move {
                    rock_pos.0 = new_x;
                }
            }
            jet_idx += 1;

            // Move down
            let new_y = rock_pos.1 - 1;
            let mut can_fall = true;

            for (dx, dy) in &rock_shape {
                if new_y + dy < 0 || chamber[(new_y + dy) as usize][(rock_pos.0 + dx) as usize] {
                    can_fall = false;
                    break;
                }
            }

            if can_fall {
                rock_pos.1 = new_y;
            } else {
                for (dx, dy) in &rock_shape {
                    chamber[(rock_pos.1 + dy) as usize][(rock_pos.0 + dx) as usize] = true;
                    highest_rock = highest_rock.max(rock_pos.1 + dy + 1);
                }
                break;
            }
        }
    }

    println!("{}", highest_rock);
}
