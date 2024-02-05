
use std::fs;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let data = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = data.trim().split("\n").collect();
    let wire1 = get_points_with_steps(lines[0]);
    let wire2 = get_points_with_steps(lines[1]);

    let mut min_steps = i32::MAX;
    for (p, &steps1) in &wire1 {
        if let Some(&steps2) = wire2.get(&p) {
            let total_steps = steps1 + steps2;
            if total_steps < min_steps {
                min_steps = total_steps;
            }
        }
    }

    println!("{}", min_steps);
}

fn get_points_with_steps(path: &str) -> std::collections::HashMap<Point, i32> {
    let mut points = std::collections::HashMap::new();
    let mut current = Point { x: 0, y: 0 };
    let mut steps = 0;
    for movement in path.split(",") {
        let (dir, dist_str) = movement.split_at(1);
        let dist: i32 = dist_str.parse().unwrap();
        for _ in 0..dist {
            steps += 1;
            match dir {
                "U" => current.y += 1,
                "D" => current.y -= 1,
                "L" => current.x -= 1,
                "R" => current.x += 1,
                _ => {}
            }
            if !points.contains_key(&current) {
                points.insert(current, steps);
            }
        }
    }
    points
}
