
use std::fs;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let data = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = data.trim().split("\n").collect();
    let wire1 = get_points(lines[0]);
    let wire2 = get_points(lines[1]);

    let mut intersections = std::collections::HashSet::new();
    for p in wire1.keys() {
        if wire2.contains_key(p) {
            intersections.insert(p);
        }
    }

    let mut min_distance = std::i32::MAX;
    for p in intersections {
        let distance = p.x.abs() + p.y.abs();
        if distance < min_distance {
            min_distance = distance;
        }
    }

    println!("{}", min_distance);
}

fn get_points(path: &str) -> std::collections::HashMap<Point, bool> {
    let mut points = std::collections::HashMap::new();
    let mut current = Point { x: 0, y: 0 };
    for movement in path.split(",") {
        let dir = movement.chars().next().unwrap();
        let steps: i32 = movement[1..].parse().unwrap();
        for _ in 0..steps {
            match dir {
                'U' => current.y += 1,
                'D' => current.y -= 1,
                'L' => current.x -= 1,
                'R' => current.x += 1,
                _ => (),
            }
            points.insert(Point { x: current.x, y: current.y }, true);
        }
    }
    points
}
