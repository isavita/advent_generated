
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut components: Vec<(u32, u32)> = Vec::new();

    for line in input.lines() {
        let parts: Vec<u32> = line.split('/').map(|x| x.parse().unwrap()).collect();
        components.push((parts[0], parts[1]));
    }

    let max_strength = find_max_strength(&components, 0, 0);
    println!("{}", max_strength);
}

fn find_max_strength(components: &Vec<(u32, u32)>, current_port: u32, current_strength: u32) -> u32 {
    let mut max_strength = current_strength;

    for i in 0..components.len() {
        if components[i].0 == current_port || components[i].1 == current_port {
            let next_port = if components[i].0 == current_port { components[i].1 } else { components[i].0 };
            let new_components = components.iter().cloned().enumerate().filter(|(index, _)| *index != i).map(|(_, x)| x).collect();
            let strength = find_max_strength(&new_components, next_port, current_strength + components[i].0 + components[i].1);
            if strength > max_strength {
                max_strength = strength;
            }
        }
    }

    max_strength
}
