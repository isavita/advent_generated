
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let input = read_input_from_file("input.txt").expect("Failed to read input");
    println!("Part 1: {}", calculate_lagoon_size(&input));
    let converted_input = convert_input(&input);
    println!("Part 2: {}", calculate_lagoon_size(&converted_input));
}

fn read_input_from_file<P: AsRef<Path>>(filename: P) -> io::Result<Vec<String>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    reader.lines().collect()
}

fn parse_instruction(line: &str) -> (char, i64) {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let direction = parts[0].chars().next().unwrap();
    let distance = parts[1].parse::<i64>().unwrap();
    (direction, distance)
}

fn convert_input(input: &Vec<String>) -> Vec<String> {
    input
        .iter()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let color = parts[2].trim_matches(|c| c == '(' || c == ')');
            let distance = i64::from_str_radix(&color[1..6], 16).unwrap();
            let direction = match color.chars().nth(6).unwrap() {
                '0' => 'R',
                '1' => 'D',
                '2' => 'L',
                '3' => 'U',
                _ => panic!("Invalid direction code"),
            };
            format!("{} {}", direction, distance)
        })
        .collect()
}


fn calculate_lagoon_size(input: &Vec<String>) -> i64 {
    let mut vertices: Vec<(i64, i64)> = vec![(0, 0)];
    let mut perimeter: i64 = 0;
    let mut current_x = 0;
    let mut current_y = 0;

    for line in input {
        let (direction, distance) = parse_instruction(line);
        match direction {
            'R' => current_x += distance,
            'L' => current_x -= distance,
            'U' => current_y -= distance,
            'D' => current_y += distance,
            _ => panic!("Invalid direction"),
        }
        vertices.push((current_x, current_y));
        perimeter += distance;
    }
    
    let area: i64 = calculate_area(&vertices);
    
    area + perimeter / 2 + 1
}

fn calculate_area(vertices: &Vec<(i64, i64)>) -> i64 {
    let mut area: i64 = 0;
    let n = vertices.len();
    
    for i in 0..n {
      let j = (i + 1) % n;
      area += vertices[i].0 * vertices[j].1 - vertices[j].0 * vertices[i].1;
    }
    
    area.abs() / 2
}
