use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut positions: Vec<i32> = Vec::new();
    for line in contents.lines() {
        let numbers: Vec<&str> = line.split(",").collect();
        for num_str in numbers {
            let num: i32 = num_str.parse().expect("Invalid number");
            positions.push(num);
        }
    }

    positions.sort();

    let mut min_fuel = i32::MAX;
    for i in positions[0]..=positions[positions.len() - 1] {
        let mut fuel = 0;
        for &pos in &positions {
            fuel += calculate_fuel(pos, i);
        }
        if fuel < min_fuel {
            min_fuel = fuel;
        }
    }
    println!("{}", min_fuel);
}

fn calculate_fuel(current_position: i32, new_position: i32) -> i32 {
    (current_position - new_position).abs()
}