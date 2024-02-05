
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut points: Vec<(i32, i32, i32, i32)> = Vec::new();

    for line in input.trim().lines() {
        let parts: Vec<i32> = line
            .split(|c| c == '<' || c == '>' || c == ',')
            .filter_map(|s| s.trim().parse().ok())
            .collect();
        points.push((parts[0], parts[1], parts[2], parts[3]));
    }

    let mut seconds = 0;
    let mut min_height = i32::MAX;

    loop {
        let mut max_height = i32::MIN;
        for i in 0..points.len() {
            points[i].0 += points[i].2;
            points[i].1 += points[i].3;
            max_height = max_height.max(points[i].1);
        }

        if max_height < min_height {
            min_height = max_height;
        } else {
            break;
        }

        seconds += 1;
    }

    println!("{}", seconds);
}
