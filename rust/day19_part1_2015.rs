
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut replacements: Vec<&str> = Vec::new();
    let mut molecule = String::new();

    for line in input.lines() {
        if line.is_empty() {
            continue;
        }
        if line.contains(" => ") {
            replacements.push(line);
        } else {
            molecule = line.to_string();
        }
    }

    let mut molecules: std::collections::HashMap<String, bool> = std::collections::HashMap::new();
    for replacement in replacements {
        let parts: Vec<&str> = replacement.split(" => ").collect();
        for i in 0..molecule.len() {
            if molecule[i..].starts_with(parts[0]) {
                let new_molecule = format!("{}{}{}", &molecule[..i], parts[1], &molecule[i + parts[0].len()..]);
                molecules.insert(new_molecule, true);
            }
        }
    }

    println!("{}", molecules.len());
}
