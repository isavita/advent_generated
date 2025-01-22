
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn tilt_north(platform: &mut Vec<Vec<char>>) {
    let rows = platform.len();
    let cols = platform[0].len();
    for col in 0..cols {
        let mut next_free_row = 0;
        for row in 0..rows {
            match platform[row][col] {
                'O' => {
                    if row != next_free_row {
                        platform[next_free_row][col] = 'O';
                        platform[row][col] = '.';
                    }
                    next_free_row += 1;
                }
                '#' => next_free_row = row + 1,
                _ => (),
            }
        }
    }
}

fn tilt_south(platform: &mut Vec<Vec<char>>) {
    let rows = platform.len();
    let cols = platform[0].len();
    for col in 0..cols {
        let mut next_free_row = rows - 1;
        for row in (0..rows).rev() {
            match platform[row][col] {
                'O' => {
                    if row != next_free_row {
                        platform[next_free_row][col] = 'O';
                        platform[row][col] = '.';
                    }
                    next_free_row = next_free_row.saturating_sub(1);
                }
                '#' => next_free_row = row.saturating_sub(1),
                _ => (),
            }
        }
    }
}


fn tilt_west(platform: &mut Vec<Vec<char>>) {
    let rows = platform.len();
    let cols = platform[0].len();

    for row in 0..rows {
        let mut next_free_col = 0;
        for col in 0..cols {
            match platform[row][col] {
                'O' => {
                    if col != next_free_col {
                         platform[row][next_free_col] = 'O';
                         platform[row][col] = '.';
                    }
                    next_free_col += 1;
                }
                '#' => next_free_col = col+1,
                _ => (),
            }
        }
    }
}

fn tilt_east(platform: &mut Vec<Vec<char>>) {
     let rows = platform.len();
    let cols = platform[0].len();

    for row in 0..rows {
         let mut next_free_col = cols - 1;
        for col in (0..cols).rev() {
            match platform[row][col] {
                'O' => {
                   if col != next_free_col {
                       platform[row][next_free_col] = 'O';
                       platform[row][col] = '.';
                   }
                   next_free_col = next_free_col.saturating_sub(1);
                }
                '#' => next_free_col = col.saturating_sub(1),
                _ => (),
            }
        }
    }
}

fn calculate_load(platform: &Vec<Vec<char>>) -> usize {
    let rows = platform.len();
    let cols = platform[0].len();
    let mut total_load = 0;
    for row in 0..rows {
        for col in 0..cols {
            if platform[row][col] == 'O' {
                total_load += rows - row;
            }
        }
    }
    total_load
}


fn cycle(platform: &mut Vec<Vec<char>>) {
   tilt_north(platform);
   tilt_west(platform);
   tilt_south(platform);
   tilt_east(platform);
}

fn platform_to_string(platform: &Vec<Vec<char>>) -> String {
   platform.iter()
           .map(|row| row.iter().collect::<String>())
           .collect::<Vec<String>>()
           .join("\n")
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut platform: Vec<Vec<char>> = reader
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    let mut seen_states: HashMap<String, usize> = HashMap::new();
    let mut loads = Vec::new();

     let total_cycles = 1_000_000_000;
     let mut cycle_num = 0;
     let mut found_cycle = false;

    while cycle_num < total_cycles {
        let current_state = platform_to_string(&platform);
        if let Some(prev_cycle) = seen_states.get(&current_state) {
            let cycle_len = cycle_num - prev_cycle;
            let remaining_cycles = total_cycles - cycle_num;
            let remaining_in_cycle = remaining_cycles % cycle_len;
            
            let final_load = loads[prev_cycle + remaining_in_cycle];
            println!("{}", final_load);
            found_cycle = true;
            break;

        } else {
            seen_states.insert(current_state, cycle_num);
            loads.push(calculate_load(&platform));
            cycle(&mut platform);
            cycle_num += 1;
        }

    }

    if !found_cycle {
        println!("{}", calculate_load(&platform));
    }
    

    Ok(())
}
