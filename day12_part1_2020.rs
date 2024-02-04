use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let instructions: Vec<(&str, i32)> = input
        .lines()
        .map(|line| {
            let action = &line[..1];
            let value = line[1..].parse().unwrap();
            (action, value)
        })
        .collect();

    let mut x = 0;
    let mut y = 0;
    let mut facing = 0;

    for (action, value) in instructions {
        match action {
            "N" => y += value,
            "S" => y -= value,
            "E" => x += value,
            "W" => x -= value,
            "L" => facing = (facing + 360 - value) % 360,
            "R" => facing = (facing + value) % 360,
            "F" => match facing {
                0 => x += value,
                90 => y -= value,
                180 => x -= value,
                270 => y += value,
                _ => panic!("Invalid facing direction"),
            },
            _ => panic!("Invalid action"),
        }
    }

    println!("{}", x.abs() + y.abs());
}