use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let instructions: Vec<&str> = input.trim().split("\n").collect();

    let mut x = 0;
    let mut y = 0;
    let mut waypoint_x = 10;
    let mut waypoint_y = 1;

    for instruction in instructions {
        let action = &instruction[..1];
        let value: i32 = instruction[1..].parse().unwrap();

        match action {
            "N" => waypoint_y += value,
            "S" => waypoint_y -= value,
            "E" => waypoint_x += value,
            "W" => waypoint_x -= value,
            "L" => {
                for _ in 0..value / 90 {
                    let temp = waypoint_x;
                    waypoint_x = -waypoint_y;
                    waypoint_y = temp;
                }
            }
            "R" => {
                for _ in 0..value / 90 {
                    let temp = waypoint_x;
                    waypoint_x = waypoint_y;
                    waypoint_y = -temp;
                }
            }
            "F" => {
                x += value * waypoint_x;
                y += value * waypoint_y;
            }
            _ => panic!("Invalid action"),
        }
    }

    println!("{}", x.abs() + y.abs());
}