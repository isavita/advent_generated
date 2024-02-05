use std::fs;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Error reading file");
    let mut points = std::collections::HashSet::new();
    let mut folds = Vec::new();
    let mut reading_points = true;

    for line in contents.lines() {
        if line.is_empty() {
            reading_points = false;
            continue;
        }
        if reading_points {
            let coords: Vec<&str> = line.split(",").collect();
            let x: i32 = coords[0].parse().unwrap();
            let y: i32 = coords[1].parse().unwrap();
            points.insert(Point { x, y });
        } else {
            folds.push(line.to_string());
        }
    }

    let fold = folds[0].split_whitespace().collect::<Vec<&str>>()[2];
    let axis_value: Vec<&str> = fold.split("=").collect();
    let axis = axis_value[0];
    let value: i32 = axis_value[1].parse().unwrap();

    let mut new_points = std::collections::HashSet::new();
    if axis == "x" {
        for point in points.iter() {
            if point.x > value {
                new_points.insert(Point { x: 2 * value - point.x, y: point.y });
            } else {
                new_points.insert(Point { x: point.x, y: point.y });
            }
        }
    } else {
        for point in points.iter() {
            if point.y > value {
                new_points.insert(Point { x: point.x, y: 2 * value - point.y });
            } else {
                new_points.insert(Point { x: point.x, y: point.y });
            }
        }
    }

    println!("{}", new_points.len());
}