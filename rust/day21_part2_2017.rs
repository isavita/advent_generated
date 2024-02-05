
use std::collections::HashMap;
use std::fs;

fn main() {
    let mut rules = HashMap::new();
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    
    for line in input.lines() {
        let parts: Vec<&str> = line.split(" => ").collect();
        rules.insert(parts[0].to_string(), parts[1].to_string());
    }

    let mut grid = vec![
        ".#.".to_string(),
        "..#".to_string(),
        "###".to_string(),
    ];

    for _ in 0..18 {
        let (sub_size, new_size) = if grid.len() % 2 == 0 {
            (2, grid.len() / 2 * 3)
        } else {
            (3, grid.len() / 3 * 4)
        };

        let mut new_grid = vec![String::new(); new_size];

        for y in (0..grid.len()).step_by(sub_size) {
            for x in (0..grid.len()).step_by(sub_size) {
                let mut square = Vec::new();
                for dy in 0..sub_size {
                    square.push(grid[y + dy][x..x + sub_size].to_string());
                }
                let new_square = enhance(square.join("/"), &rules);
                for (dy, row) in new_square.split("/").enumerate() {
                    new_grid[y / sub_size * (sub_size + 1) + dy] += row;
                }
            }
        }
        grid = new_grid;
    }

    let mut count = 0;
    for row in grid {
        for pixel in row.chars() {
            if pixel == '#' {
                count += 1;
            }
        }
    }
    println!("{}", count);
}

fn enhance(input: String, rules: &HashMap<String, String>) -> String {
    if let Some(result) = rules.get(&input) {
        return result.to_string();
    }

    let original = input.clone();
    let mut input = input;
    for _ in 0..4 {
        if let Some(output) = rules.get(&input) {
            return output.to_string();
        }
        input = rotate(&input);
    }
    input = flip(&input);
    for _ in 0..4 {
        if let Some(output) = rules.get(&input) {
            return output.to_string();
        }
        input = rotate(&input);
    }
    String::new()
}

fn rotate(input: &String) -> String {
    let parts: Vec<&str> = input.split("/").collect();
    let size = parts.len();
    let mut new_parts = vec![String::new(); size];

    for x in 0..size {
        let mut new_row = String::new();
        for y in (0..size).rev() {
            new_row += &parts[y][x..x + 1];
        }
        new_parts[x] = new_row;
    }
    new_parts.join("/")
}

fn flip(input: &String) -> String {
    let mut parts: Vec<String> = input.split("/").map(|s| s.to_string()).collect();
    for part in &mut parts {
        *part = reverse(part);
    }
    parts.join("/")
}

fn reverse(input: &String) -> String {
    input.chars().rev().collect()
}
