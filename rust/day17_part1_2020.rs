
use std::fs;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coordinate {
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("File reading error");
    let initial_state: Vec<&str> = input.trim().split("\n").collect();
    let mut active_cubes = initial_state
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, char)| {
                if char == '#' {
                    Some(Coordinate { x: x as i32, y: y as i32, z: 0 })
                } else {
                    None
                }
            })
        })
        .collect::<std::collections::HashSet<_>>();

    for _cycle in 0..6 {
        active_cubes = simulate_cycle(&active_cubes);
    }

    println!("{}", active_cubes.len());
}

fn simulate_cycle(active_cubes: &std::collections::HashSet<Coordinate>) -> std::collections::HashSet<Coordinate> {
    let mut new_active_cubes = std::collections::HashSet::new();
    let mut neighbor_counts = std::collections::HashMap::new();

    for &coord in active_cubes {
        for dz in -1..=1 {
            for dy in -1..=1 {
                for dx in -1..=1 {
                    if dz == 0 && dy == 0 && dx == 0 {
                        continue;
                    }
                    let neighbor = Coordinate { x: coord.x + dx, y: coord.y + dy, z: coord.z + dz };
                    *neighbor_counts.entry(neighbor).or_insert(0) += 1;
                }
            }
        }
    }

    for (&coord, &count) in &neighbor_counts {
        if count == 3 || (count == 2 && active_cubes.contains(&coord)) {
            new_active_cubes.insert(coord);
        }
    }

    new_active_cubes
}
