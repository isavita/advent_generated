
use std::fs;
fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let serial: i32 = contents.trim().parse().expect("Invalid input");
    const GRID_SIZE: usize = 300;
    let mut grid = [[0i32; GRID_SIZE]; GRID_SIZE];
    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            let rack_id = x as i32 + 11;
            let mut power_level = rack_id * (y as i32 + 1);
            power_level += serial;
            power_level *= rack_id;
            power_level = (power_level / 100) % 10;
            power_level -= 5;
            grid[y][x] = power_level;
        }
    }
    let mut max_power = i32::min_value();
    let mut max_x = 0;
    let mut max_y = 0;
    let mut max_size = 0;
    let mut summed_area_table = [[0i32; GRID_SIZE + 1]; GRID_SIZE + 1];
    for y in 1..=GRID_SIZE {
        for x in 1..=GRID_SIZE {
            summed_area_table[y][x] = grid[y - 1][x - 1]
                + summed_area_table[y - 1][x]
                + summed_area_table[y][x - 1]
                - summed_area_table[y - 1][x - 1];
        }
    }
    for size in 1..=GRID_SIZE {
        for y in 0..=GRID_SIZE - size {
            for x in 0..=GRID_SIZE - size {
                let total_power = summed_area_table[y + size][x + size]
                    - summed_area_table[y][x + size]
                    - summed_area_table[y + size][x]
                    + summed_area_table[y][x];
                if total_power > max_power {
                    max_power = total_power;
                    max_x = x + 1;
                    max_y = y + 1;
                    max_size = size;
                }
            }
        }
    }
    println!("{},{},{}", max_x, max_y, max_size);
}
