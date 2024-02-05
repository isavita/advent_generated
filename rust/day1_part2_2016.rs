use std::fs;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Position {
    x: i32,
    y: i32,
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Error reading the file");
    let instructions: Vec<&str> = contents.trim().split(", ").collect();

    println!("{}", first_revisited_distance(&instructions));
}

fn first_revisited_distance(instructions: &Vec<&str>) -> i32 {
    let mut pos = Position { x: 0, y: 0 };
    let mut visited = std::collections::HashSet::new();
    visited.insert(pos);
    let directions = [Position { x: 0, y: 1 }, Position { x: 1, y: 0 }, Position { x: 0, y: -1 }, Position { x: -1, y: 0 }];
    let mut dir_index = 0;

    for instruction in instructions {
        let turn = &instruction[..1];
        let blocks: i32 = instruction[1..].parse().unwrap();

        if turn == "R" {
            dir_index = (dir_index + 1) % 4;
        } else {
            dir_index = (dir_index + 3) % 4;
        }

        for _ in 0..blocks {
            pos.x += directions[dir_index].x;
            pos.y += directions[dir_index].y;

            if visited.contains(&pos) {
                return pos.x.abs() + pos.y.abs();
            }
            visited.insert(pos);
        }
    }

    -1
}