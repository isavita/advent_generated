
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines = io::BufReader::new(file).lines();

    let mut orbits = HashMap::new();
    for line in lines {
        let line = line?;
        let parts: Vec<&str> = line.split(')').collect();
        orbits.insert(parts[1].to_string(), parts[0].to_string());
    }

    // Part 1: Calculate total direct and indirect orbits
    let mut total_orbits = 0;
    for object in orbits.keys() {
        let mut current = object;
        while let Some(parent) = orbits.get(current) {
            total_orbits += 1;
            current = parent;
        }
    }
    println!("Total orbits: {}", total_orbits);

    // Part 2: Calculate orbital transfers between YOU and SAN
    let you_parent = orbits.get("YOU").expect("YOU not found");
    let san_parent = orbits.get("SAN").expect("SAN not found");

    let you_ancestors = get_ancestors(&orbits, you_parent);
    let san_ancestors = get_ancestors(&orbits, san_parent);

    let common_ancestor = you_ancestors
        .iter()
        .find(|&ancestor| san_ancestors.contains(ancestor))
        .expect("No common ancestor found");
    
    let you_steps = you_ancestors.iter().position(|x| x == common_ancestor).unwrap();
    let san_steps = san_ancestors.iter().position(|x| x == common_ancestor).unwrap();

    println!("Transfers needed: {}", you_steps + san_steps);

    Ok(())
}


fn get_ancestors(orbits: &HashMap<String, String>, start: &String) -> Vec<String> {
    let mut ancestors = Vec::new();
    let mut current = start;
    while let Some(parent) = orbits.get(current) {
      ancestors.push(current.to_string());
      current = parent;
    }
    ancestors.push(current.to_string()); //Include COM as ancestor
    ancestors
}
