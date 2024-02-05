
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut cubes = std::collections::HashMap::<Point, bool>::new();

    for line in reader.lines() {
        let line = line.unwrap();
        let coords: Vec<&str> = line.split(",").collect();
        let x: i32 = coords[0].parse().unwrap();
        let y: i32 = coords[1].parse().unwrap();
        let z: i32 = coords[2].parse().unwrap();
        cubes.insert(Point { x, y, z }, true);
    }

    let mut surface_area = 0;
    for cube in cubes.keys() {
        surface_area += calculate_exposed_sides(&cube, &cubes);
    }

    println!("{}", surface_area);
}

fn calculate_exposed_sides(p: &Point, cubes: &std::collections::HashMap<Point, bool>) -> i32 {
    let directions = vec![
        Point { x: 1, y: 0, z: 0 },
        Point { x: -1, y: 0, z: 0 },
        Point { x: 0, y: 1, z: 0 },
        Point { x: 0, y: -1, z: 0 },
        Point { x: 0, y: 0, z: 1 },
        Point { x: 0, y: 0, z: -1 },
    ];

    let mut exposed_sides = 6;
    for dir in &directions {
        let adjacent = Point {
            x: p.x + dir.x,
            y: p.y + dir.y,
            z: p.z + dir.z,
        };
        if cubes.contains_key(&adjacent) {
            exposed_sides -= 1;
        }
    }
    exposed_sides
}
