
use std::collections::HashMap;
use std::fs;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coordinate4D {
    x: i32,
    y: i32,
    z: i32,
    w: i32,
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("File reading error");
    let initial_state: Vec<&str> = input.trim().split("\n").collect();
    let mut active_cubes: HashMap<Coordinate4D, bool> = HashMap::new();

    for (y, line) in initial_state.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
            if char == '#' {
                active_cubes.insert(Coordinate4D { x: x as i32, y: y as i32, z: 0, w: 0 }, true);
            }
        }
    }

    for _ in 0..6 {
        active_cubes = simulate_cycle_4d(&active_cubes);
    }

    println!("{}", active_cubes.len());
}

fn simulate_cycle_4d(active_cubes: &HashMap<Coordinate4D, bool>) -> HashMap<Coordinate4D, bool> {
    let mut new_active_cubes: HashMap<Coordinate4D, bool> = HashMap::new();
    let mut neighbor_counts: HashMap<Coordinate4D, i32> = HashMap::new();

    for (&coord, _) in active_cubes.iter() {
        for dw in -1..=1 {
            for dz in -1..=1 {
                for dy in -1..=1 {
                    for dx in -1..=1 {
                        if dw == 0 && dz == 0 && dy == 0 && dx == 0 {
                            continue;
                        }
                        let neighbor = Coordinate4D { x: coord.x + dx, y: coord.y + dy, z: coord.z + dz, w: coord.w + dw };
                        *neighbor_counts.entry(neighbor).or_insert(0) += 1;
                    }
                }
            }
        }
    }

    for (&coord, &count) in neighbor_counts.iter() {
        if count == 3 || (count == 2 && active_cubes.contains_key(&coord)) {
            new_active_cubes.insert(coord, true);
        }
    }

    new_active_cubes
}
