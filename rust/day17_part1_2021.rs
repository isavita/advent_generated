
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut x_min = 0;
    let mut x_max = 0;
    let mut y_min = 0;
    let mut y_max = 0;

    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split(", ").collect();
        let x_range: Vec<&str> = parts[0][15..].split("..").collect();
        let y_range: Vec<&str> = parts[1][2..].split("..").collect();
        x_min = x_range[0].parse().unwrap();
        x_max = x_range[1].parse().unwrap();
        y_min = y_range[0].parse().unwrap();
        y_max = y_range[1].parse().unwrap();
    }

    let mut max_y = -1 << 30;
    for x_vel in -1000..=1000 {
        for y_vel in -1000..=1000 {
            let mut x_pos = 0;
            let mut y_pos = 0;
            let mut cur_x_vel = x_vel;
            let mut cur_y_vel = y_vel;
            let mut highest_y = y_pos;

            loop {
                x_pos += cur_x_vel;
                y_pos += cur_y_vel;

                if x_pos >= x_min && x_pos <= x_max && y_pos >= y_min && y_pos <= y_max {
                    if highest_y > max_y {
                        max_y = highest_y;
                    }
                    break;
                }

                if is_moving_away(x_pos, y_pos, cur_x_vel, cur_y_vel, x_min, x_max, y_min, y_max) {
                    break;
                }

                if cur_x_vel > 0 {
                    cur_x_vel -= 1;
                } else if cur_x_vel < 0 {
                    cur_x_vel += 1;
                }

                cur_y_vel -= 1;
                if y_pos > highest_y {
                    highest_y = y_pos;
                }
            }
        }
    }

    println!("{}", max_y);
}

fn is_moving_away(x_pos: i32, y_pos: i32, x_vel: i32, y_vel: i32, x_min: i32, x_max: i32, y_min: i32, y_max: i32) -> bool {
    (x_pos < x_min && x_vel < 0) || (x_pos > x_max && x_vel > 0) || (y_pos < y_min && y_vel < 0)
}
