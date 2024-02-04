
use std::fs;

fn main() {
    let data = fs::read_to_string("input.txt").expect("Unable to read file");
    let serial: i32 = data.trim().parse().expect("Invalid input");

    const GRID_SIZE: usize = 300;
    let mut grid = vec![vec![0; GRID_SIZE]; GRID_SIZE];

    for y in 0..GRID_SIZE {
        for x in 0..GRID_SIZE {
            let rack_id = x as i32 + 11;
            let mut power_level = rack_id * (y as i32 + 1) + serial;
            power_level *= rack_id;
            power_level = (power_level / 100) % 10;
            power_level -= 5;
            grid[y][x] = power_level;
        }
    }

    let mut max_power = std::i32::MIN;
    let mut max_x = 0;
    let mut max_y = 0;
    for y in 0..GRID_SIZE - 2 {
        for x in 0..GRID_SIZE - 2 {
            let mut total_power = 0;
            for dy in 0..3 {
                for dx in 0..3 {
                    total_power += grid[y + dy][x + dx];
                }
            }
            if total_power > max_power {
                max_power = total_power;
                max_x = x + 1;
                max_y = y + 1;
            }
        }
    }

    println!("{},{}", max_x, max_y);
}
