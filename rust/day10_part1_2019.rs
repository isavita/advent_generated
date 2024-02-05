
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut asteroids = Vec::new();
    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                asteroids.push((x as i32, y as i32));
            }
        }
    }

    let mut max_detected = 0;
    for &(x1, y1) in &asteroids {
        let mut detected = 0;
        for &(x2, y2) in &asteroids {
            if (x1, y1) == (x2, y2) {
                continue;
            }
            let mut blocked = false;
            for &(x3, y3) in &asteroids {
                if (x1, y1) == (x3, y3) || (x2, y2) == (x3, y3) {
                    continue;
                }
                if is_between(x1, y1, x2, y2, x3, y3) {
                    blocked = true;
                    break;
                }
            }
            if !blocked {
                detected += 1;
            }
        }
        if detected > max_detected {
            max_detected = detected;
        }
    }

    println!("{}", max_detected);
}

fn is_between(x1: i32, y1: i32, x2: i32, y2: i32, x3: i32, y3: i32) -> bool {
    let cross_product = (y3 - y1) * (x2 - x1) - (x3 - x1) * (y2 - y1);
    if cross_product != 0 {
        return false;
    }

    let dot_product = (x3 - x1) * (x2 - x1) + (y3 - y1) * (y2 - y1);
    if dot_product < 0 {
        return false;
    }

    let squared_length = (x2 - x1).pow(2) + (y2 - y1).pow(2);
    if dot_product > squared_length {
        return false;
    }

    true
}
