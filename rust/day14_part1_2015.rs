use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = input.trim().split('\n').collect();

    let mut max_distance = 0;

    for line in lines {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let speed: i32 = parts[3].parse().unwrap();
        let fly_time: i32 = parts[6].parse().unwrap();
        let rest_time: i32 = parts[13].parse().unwrap();
        let cycle_time = fly_time + rest_time;
        let cycles = 2503 / cycle_time;
        let remaining_time = 2503 % cycle_time;
        let distance = cycles * fly_time * speed + std::cmp::min(remaining_time, fly_time) * speed;
        if distance > max_distance {
            max_distance = distance;
        }
    }

    println!("{}", max_distance);
}