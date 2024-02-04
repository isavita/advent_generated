use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut components: Vec<(u32, u32)> = Vec::new();

    for line in input.lines() {
        let parts: Vec<&str> = line.split('/').collect();
        let port1: u32 = parts[0].parse().unwrap();
        let port2: u32 = parts[1].parse().unwrap();
        components.push((port1, port2));
    }

    let mut max_strength = 0;
    let mut max_length = 0;
    let mut max_strength_longest = 0;

    fn build_bridge(components: &Vec<(u32, u32)>, current_port: u32, used: &mut Vec<bool>, strength: u32, length: u32) -> (u32, u32, u32) {
        let mut max_strength = strength;
        let mut max_length = length;
        let mut max_strength_longest = 0;

        for i in 0..components.len() {
            if !used[i] && (components[i].0 == current_port || components[i].1 == current_port) {
                used[i] = true;
                let next_port = if components[i].0 == current_port { components[i].1 } else { components[i].0 };
                let (strength, length, strength_longest) = build_bridge(components, next_port, used, strength + current_port + next_port, length + 1);
                if length > max_length || (length == max_length && strength_longest > max_strength_longest) {
                    max_strength = strength;
                    max_length = length;
                    max_strength_longest = strength_longest;
                }
                used[i] = false;
            }
        }

        (max_strength, max_length, max_strength_longest)
    }

    for i in 0..components.len() {
        if components[i].0 == 0 {
            let mut used = vec![false; components.len()];
            used[i] = true;
            let next_port = if components[i].0 == 0 { components[i].1 } else { components[i].0 };
            let (strength, length, strength_longest) = build_bridge(&components, next_port, &mut used, components[i].0 + components[i].1, 1);
            if length > max_length || (length == max_length && strength_longest > max_strength_longest) {
                max_strength = strength;
                max_length = length;
                max_strength_longest = strength_longest;
            }
        }
    }

    println!("{}", max_strength);
}