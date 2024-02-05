
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut orbits: std::collections::HashMap<&str, &str> = std::collections::HashMap::new();

    for line in input.trim().lines() {
        let parts: Vec<&str> = line.split(')').collect();
        orbits.insert(parts[1], parts[0]);
    }

    let mut total_orbits = 0;

    for object in orbits.keys() {
        let mut current_object = object;
        while orbits.contains_key(current_object) {
            current_object = orbits.get(current_object).unwrap();
            total_orbits += 1;
        }
    }

    println!("{}", total_orbits);
}
