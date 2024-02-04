
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    let mut seen = std::collections::HashMap::new();
    let mut cycle_start = 0;
    let mut cycle_end = 0;
    let mut minutes = 0;

    while minutes < 1000000000 {
        let mut new_grid = grid.clone();

        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                let mut tree_count = 0;
                let mut lumberyard_count = 0;

                for dy in -1..=1 {
                    for dx in -1..=1 {
                        if dy == 0 && dx == 0 {
                            continue;
                        }

                        let new_y = y as i32 + dy;
                        let new_x = x as i32 + dx;

                        if new_y >= 0 && new_y < grid.len() as i32 && new_x >= 0 && new_x < grid[y].len() as i32 {
                            if grid[new_y as usize][new_x as usize] == '|' {
                                tree_count += 1;
                            } else if grid[new_y as usize][new_x as usize] == '#' {
                                lumberyard_count += 1;
                            }
                        }
                    }
                }

                match grid[y][x] {
                    '.' => {
                        if tree_count >= 3 {
                            new_grid[y][x] = '|';
                        }
                    }
                    '|' => {
                        if lumberyard_count >= 3 {
                            new_grid[y][x] = '#';
                        }
                    }
                    '#' => {
                        if tree_count == 0 || lumberyard_count == 0 {
                            new_grid[y][x] = '.';
                        }
                    }
                    _ => {}
                }
            }
        }

        grid = new_grid;

        minutes += 1;

        let grid_str: String = grid.iter().map(|row| row.iter().collect::<String>()).collect::<String>();

        if seen.contains_key(&grid_str) {
            cycle_start = seen[&grid_str];
            cycle_end = minutes;
            break;
        } else {
            seen.insert(grid_str, minutes);
        }
    }

    let cycle_length = cycle_end - cycle_start;
    let remaining_minutes = 1000000000 - cycle_start;
    let final_cycle = cycle_start + (remaining_minutes % cycle_length);

    let final_grid = seen.iter().find(|(_, &v)| v == final_cycle).unwrap().0;
    let wooded_acres = final_grid.chars().filter(|&c| c == '|').count();
    let lumberyards = final_grid.chars().filter(|&c| c == '#').count();

    println!("{}", wooded_acres * lumberyards);
}
