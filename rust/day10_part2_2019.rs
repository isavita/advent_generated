
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashSet;
use std::f64::consts::PI;

#[derive(Debug, Clone, PartialEq)]
struct Asteroid {
    x: i32,
    y: i32,
    angle: f64,
    dist: f64,
}

fn main() {
    let asteroids = read_asteroids("input.txt");
    let (station, _) = find_best_asteroid_location(&asteroids);
    let vaporized = vaporize_asteroids(&asteroids, station);
    if vaporized.len() >= 200 {
        let result = vaporized[199].x * 100 + vaporized[199].y;
        println!("{}", result);
    } else {
        println!("Less than 200 asteroids were vaporized.");
    }
}

fn read_asteroids(filename: &str) -> Vec<Vec<bool>> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut asteroids = Vec::new();
    for line in reader.lines() {
        let line = line.unwrap();
        let asteroid_row: Vec<bool> = line.chars().map(|c| c == '#').collect();
        asteroids.push(asteroid_row);
    }
    asteroids
}

fn vaporize_asteroids(asteroids: &Vec<Vec<bool>>, station: (i32, i32)) -> Vec<Asteroid> {
    let mut targets = Vec::new();
    for (y, row) in asteroids.iter().enumerate() {
        for (x, &is_asteroid) in row.iter().enumerate() {
            if is_asteroid && !(x as i32 == station.0 && y as i32 == station.1) {
                let dx = x as i32 - station.0;
                let dy = y as i32 - station.1;
                let mut angle = (dy as f64).atan2(dx as f64);
                let dist = (dx as f64).hypot(dy as f64);
                if angle < -PI / 2.0 {
                    angle += 2.0 * PI;
                }
                targets.push(Asteroid {
                    x: x as i32,
                    y: y as i32,
                    angle,
                    dist,
                });
            }
        }
    }

    targets.sort_by(|a, b| {
        if a.angle == b.angle {
            a.dist.partial_cmp(&b.dist).unwrap()
        } else {
            a.angle.partial_cmp(&b.angle).unwrap()
        }
    });

    let mut vaporized = Vec::new();
    while !targets.is_empty() {
        let mut last_angle = -f64::MAX;
        let mut i = 0;
        while i < targets.len() {
            if targets[i].angle != last_angle {
                vaporized.push(targets.remove(i));
                last_angle = vaporized.last().unwrap().angle;
            } else {
                i += 1;
            }
        }
    }
    vaporized
}

fn find_best_asteroid_location(asteroids: &Vec<Vec<bool>>) -> ((i32, i32), usize) {
    let mut best_location = (0, 0);
    let mut max_count = 0;
    for (y, row) in asteroids.iter().enumerate() {
        for (x, &is_asteroid) in row.iter().enumerate() {
            if is_asteroid {
                let count = count_visible_asteroids(asteroids, x as i32, y as i32);
                if count > max_count {
                    max_count = count;
                    best_location = (x as i32, y as i32);
                }
            }
        }
    }
    (best_location, max_count)
}

fn count_visible_asteroids(asteroids: &Vec<Vec<bool>>, x: i32, y: i32) -> usize {
    let mut angles = HashSet::new();
    for (other_y, row) in asteroids.iter().enumerate() {
        for (other_x, &is_asteroid) in row.iter().enumerate() {
            if is_asteroid && !(other_x as i32 == x && other_y as i32 == y) {
                let angle = ((other_y as i32 - y) as f64).atan2((other_x as i32 - x) as f64);
                angles.insert((angle * 1000000.0).round() as i64);
            }
        }
    }
    angles.len()
}
