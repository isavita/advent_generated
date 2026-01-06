
use std::fs;
use std::cmp::max;

fn main() {
    let data = fs::read_to_string("input.txt").unwrap();
    let mut coords = Vec::new();
    for line in data.lines() {
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() == 2 {
            if let (Ok(x), Ok(y)) = (parts[0].trim().parse::<i32>(), parts[1].trim().parse::<i32>()) {
                coords.push((x, y));
            }
        }
    }
    let mut max_area = 0u64;
    for i in 0..coords.len() {
        for j in i..coords.len() {
            let dx = (coords[i].0 - coords[j].0).abs() as u64 + 1;
            let dy = (coords[i].1 - coords[j].1).abs() as u64 + 1;
            max_area = max(max_area, dx * dy);
        }
    }
    println!("Largest area: {}", max_area);
}
