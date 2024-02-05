
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let mut cubes = std::collections::HashMap::new();
    let neighbors = vec![
        Pt3 { x: -1, y: 0, z: 0 },
        Pt3 { x: 1, y: 0, z: 0 },
        Pt3 { x: 0, y: -1, z: 0 },
        Pt3 { x: 0, y: 1, z: 0 },
        Pt3 { x: 0, y: 0, z: -1 },
        Pt3 { x: 0, y: 0, z: 1 },
    ];
    let mut min = Pt3 { x: std::i32::MAX, y: std::i32::MAX, z: std::i32::MAX };
    let mut max = Pt3 { x: std::i32::MIN, y: std::i32::MIN, z: std::i32::MIN };

    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.unwrap();
        if line.is_empty() {
            continue;
        }
        let mut cube = Pt3 { x: 0, y: 0, z: 0 };
        let parts: Vec<&str> = line.split(',').collect();
        cube.x = parts[0].parse().unwrap();
        cube.y = parts[1].parse().unwrap();
        cube.z = parts[2].parse().unwrap();
        cubes.insert(cube, ());
        min.x = min.x.min(cube.x);
        min.y = min.y.min(cube.y);
        min.z = min.z.min(cube.z);
        max.x = max.x.max(cube.x);
        max.y = max.y.max(cube.y);
        max.z = max.z.max(cube.z);
    }

    min.x -= 1;
    min.y -= 1;
    min.z -= 1;
    max.x += 1;
    max.y += 1;
    max.z += 1;

    let mut faces = 0;
    let mut q = vec![min];
    let mut seen = std::collections::HashMap::new();
    seen.insert(min, ());
    while !q.is_empty() {
        let curr = q.remove(0);
        for delta in &neighbors {
            let next = Pt3 { x: curr.x + delta.x, y: curr.y + delta.y, z: curr.z + delta.z };
            if next.x < min.x || next.y < min.y || next.z < min.z || next.x > max.x || next.y > max.y || next.z > max.z {
                continue;
            }
            if cubes.contains_key(&next) {
                faces += 1;
            } else if !seen.contains_key(&next) {
                seen.insert(next, ());
                q.push(next);
            }
        }
    }

    println!("{}", faces);
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Pt3 {
    x: i32,
    y: i32,
    z: i32,
}
