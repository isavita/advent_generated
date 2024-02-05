
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut positions: Vec<i32> = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        let numbers: Vec<&str> = line.split(",").collect();
        for num_str in numbers {
            let num = num_str.parse().expect("Unable to parse number");
            positions.push(num);
        }
    }

    positions.sort();

    let mut min_fuel = i32::MAX;
    for i in positions[0]..=positions[positions.len() - 1] {
        let mut fuel = 0;
        for &pos in &positions {
            fuel += calculate_new_fuel(pos, i);
        }
        if fuel < min_fuel {
            min_fuel = fuel;
        }
    }
    println!("{}", min_fuel);
}

fn calculate_new_fuel(current_position: i32, new_position: i32) -> i32 {
    let diff = (current_position - new_position).abs();
    (diff * (diff + 1)) / 2
}
