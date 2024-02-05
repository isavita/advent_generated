use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Error reading file");

    let lines: Vec<&str> = contents.lines().collect();
    let parts: Vec<&str> = lines[0].split(", ").collect();
    let x_range: Vec<&str> = parts[0][15..].split("..").collect();
    let y_range: Vec<&str> = parts[1][2..].split("..").collect();
    let x_min: i32 = x_range[0].parse().unwrap();
    let x_max: i32 = x_range[1].parse().unwrap();
    let y_min: i32 = y_range[0].parse().unwrap();
    let y_max: i32 = y_range[1].parse().unwrap();

    let mut velocities: std::collections::HashMap<String, bool> = std::collections::HashMap::new();
    for x_vel in -1000..=1000 {
        for y_vel in -1000..=1000 {
            let mut x_pos = 0;
            let mut y_pos = 0;
            let mut cur_x_vel = x_vel;
            let mut cur_y_vel = y_vel;
            let mut in_target_area = false;

            loop {
                x_pos += cur_x_vel;
                y_pos += cur_y_vel;

                if x_pos >= x_min && x_pos <= x_max && y_pos >= y_min && y_pos <= y_max {
                    in_target_area = true;
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
            }

            if in_target_area {
                let velocity_key = format!("{},{}", x_vel, y_vel);
                velocities.insert(velocity_key, true);
            }
        }
    }

    println!("{}", velocities.len());
}

fn is_moving_away(x_pos: i32, y_pos: i32, x_vel: i32, y_vel: i32, x_min: i32, x_max: i32, y_min: i32, y_max: i32) -> bool {
    if x_pos < x_min && x_vel < 0 {
        return true;
    }
    if x_pos > x_max && x_vel > 0 {
        return true;
    }
    if y_pos < y_min && y_vel < 0 {
        return true;
    }
    false
}