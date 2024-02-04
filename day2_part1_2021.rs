use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut horizontal_position = 0;
    let mut depth = 0;

    for line in contents.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let value: i32 = parts[1].parse().unwrap();

        match parts[0] {
            "forward" => horizontal_position += value,
            "down" => depth += value,
            "up" => depth -= value,
            _ => (),
        }
    }

    println!("{}", horizontal_position * depth);
}