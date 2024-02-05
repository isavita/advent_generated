
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<&str> = input.trim().split(", ").collect();

    let mut x = 0;
    let mut y = 0;
    let mut dir = 0;

    for instruction in instructions {
        let turn = &instruction[..1];
        let dist: i32 = instruction[1..].parse().unwrap();

        if turn == "R" {
            dir = (dir + 1) % 4;
        } else {
            dir = (dir + 3) % 4;
        }

        match dir {
            0 => y += dist,
            1 => x += dist,
            2 => y -= dist,
            3 => x -= dist,
            _ => {}
        }
    }

    println!("{}", x.abs() + y.abs());
}
