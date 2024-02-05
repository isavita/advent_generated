use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut floor = 0;
    let mut position = 0;
    let mut basement_position = 0;

    for c in input.chars() {
        position += 1;
        if c == '(' {
            floor += 1;
        } else if c == ')' {
            floor -= 1;
        }

        if floor == -1 && basement_position == 0 {
            basement_position = position;
        }
    }

    println!("{}", floor);
    println!("{}", basement_position);
}