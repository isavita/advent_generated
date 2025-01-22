
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines().filter_map(Result::ok);

    let instructions = lines.next().unwrap();
    lines.next(); 

    let mut network: HashMap<String, (String, String)> = HashMap::new();
    for line in lines {
        let parts: Vec<&str> = line.split(" = ").collect();
        let node = parts[0].to_string();
        let lr_parts: Vec<&str> = parts[1].trim_matches(|c| c == '(' || c == ')').split(", ").collect();
        network.insert(node, (lr_parts[0].to_string(), lr_parts[1].to_string()));
    }

    let mut current_node = "AAA".to_string();
    let mut steps = 0;
    let instruction_len = instructions.len();

    while current_node != "ZZZ" {
        let instruction = instructions.chars().nth(steps % instruction_len).unwrap();
        let (left, right) = network.get(&current_node).unwrap();
        current_node = match instruction {
            'L' => left.clone(),
            'R' => right.clone(),
            _ => panic!("Invalid instruction"),
        };
        steps += 1;
    }

    println!("{}", steps);
    Ok(())
}
