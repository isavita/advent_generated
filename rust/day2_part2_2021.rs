
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let commands: Vec<&str> = input.trim().split('\n').collect();

    let mut horizontal_position = 0;
    let mut depth = 0;
    let mut aim = 0;

    for command in commands {
        let parts: Vec<&str> = command.split(' ').collect();
        let value: i32 = parts[1].parse().unwrap();

        match parts[0] {
            "forward" => {
                horizontal_position += value;
                depth += aim * value;
            }
            "down" => {
                aim += value;
            }
            "up" => {
                aim -= value;
            }
            _ => {}
        }
    }

    println!("{}", horizontal_position * depth);
}
