
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Instruction {
    direction: Direction,
    distance: i64,
}

fn parse_input(filename: &str) -> io::Result<Vec<Instruction>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut instructions = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let direction = match parts[0] {
            "U" => Direction::Up,
            "D" => Direction::Down,
            "L" => Direction::Left,
            "R" => Direction::Right,
            _ => panic!("Invalid direction"),
        };
        let distance: i64 = parts[1].parse().expect("Invalid distance");
        instructions.push(Instruction { direction, distance });
    }
    Ok(instructions)
}


fn calculate_lagoon_size(instructions: &[Instruction]) -> i64 {
    let mut vertices = Vec::new();
    let mut current_x: i64 = 0;
    let mut current_y: i64 = 0;
    let mut perimeter = 0;

    vertices.push((current_x, current_y));
    for instr in instructions {
        match instr.direction {
            Direction::Up => current_y -= instr.distance,
            Direction::Down => current_y += instr.distance,
            Direction::Left => current_x -= instr.distance,
            Direction::Right => current_x += instr.distance,
        }
        vertices.push((current_x, current_y));
        perimeter += instr.distance;
    }

    let area = calculate_area(&vertices);
    area + (perimeter / 2) + 1
}


fn calculate_area(vertices: &[(i64, i64)]) -> i64 {
    let mut area = 0;
    for i in 0..vertices.len() - 1 {
        area += (vertices[i].0 * vertices[i + 1].1) - (vertices[i + 1].0 * vertices[i].1);
    }
     (area.abs() as f64 / 2.0) as i64 
}


fn main() -> io::Result<()> {
    let instructions = parse_input("input.txt")?;
    let lagoon_size = calculate_lagoon_size(&instructions);
    println!("{}", lagoon_size);
    Ok(())
}
