
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

type Point = (i32, i32, i32);

fn manhattan_distance(p1: &Point, p2: &Point) -> i32 {
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs() + (p1.2 - p2.2).abs()
}

fn rotate_point(p: &Point, rot: usize) -> Point {
    let (x, y, z) = *p;
    match rot {
        0 => (x, y, z),
        1 => (x, -y, -z),
        2 => (x, -z, y),
        3 => (x, z, -y),

        4 => (-x, y, -z),
        5 => (-x, -y, z),
        6 => (-x, -z, -y),
        7 => (-x, z, y),

        8 => (y, x, -z),
        9 => (y, -x, z),
        10 => (y, z, x),
        11 => (y, -z, -x),

        12 => (-y, x, z),
        13 => (-y, -x, -z),
        14 => (-y, z, -x),
        15 => (-y, -z, x),


        16 => (z, x, y),
        17 => (z, -x, -y),
        18 => (z, y, -x),
        19 => (z, -y, x),

        20 => (-z, x, -y),
        21 => (-z, -x, y),
        22 => (-z, y, x),
        23 => (-z, -y, -x),
        _ => unreachable!(),
    }
}

fn transform_points(points: &Vec<Point>, transform: &(Point, usize)) -> Vec<Point> {
    let (offset, rot) = *transform;
    points
        .iter()
        .map(|p| {
            let rotated = rotate_point(p, rot);
            (rotated.0 + offset.0, rotated.1 + offset.1, rotated.2 + offset.2)
        })
        .collect()
}

fn find_transform(scanner1: &Vec<Point>, scanner2: &Vec<Point>) -> Option<(Point, usize)> {
    for rot in 0..24 {
        let rotated_scanner2: Vec<Point> = scanner2.iter().map(|p| rotate_point(p, rot)).collect();

        let mut diff_counts: HashMap<Point, usize> = HashMap::new();

        for p1 in scanner1 {
            for p2 in &rotated_scanner2 {
                let diff = (p1.0 - p2.0, p1.1 - p2.1, p1.2 - p2.2);
                *diff_counts.entry(diff).or_insert(0) += 1;
            }
        }
        
        if let Some((diff, count)) = diff_counts.iter().max_by_key(|(_, &count)| count) {
            if *count >= 12 {
                return Some((*diff, rot));
            }
        }
    }
    None
}

fn read_input<P: AsRef<Path>>(filename: P) -> io::Result<Vec<Vec<Point>>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);

    let mut scanners = Vec::new();
    let mut current_scanner = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if line.starts_with("--- scanner") {
            if !current_scanner.is_empty() {
                scanners.push(current_scanner);
                current_scanner = Vec::new();
            }
        } else if !line.is_empty() {
            let coords: Vec<i32> = line
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect();
            current_scanner.push((coords[0], coords[1], coords[2]));
        }
    }
    if !current_scanner.is_empty() {
        scanners.push(current_scanner);
    }
    Ok(scanners)
}

fn main() -> io::Result<()> {
    let scanners = read_input("input.txt")?;

    let num_scanners = scanners.len();
    let mut scanner_positions: HashMap<usize, (Point, usize)> = HashMap::new();
    scanner_positions.insert(0, ((0,0,0), 0));

    let mut resolved_scanners: HashSet<usize> = HashSet::new();
    resolved_scanners.insert(0);

    let mut queue: VecDeque<usize> = VecDeque::new();
    queue.push_back(0);


    while let Some(current_scanner_index) = queue.pop_front() {
        for next_scanner_index in 0..num_scanners {
            if resolved_scanners.contains(&next_scanner_index) {
                continue;
            }

            if let Some(transform) = find_transform(
                &transform_points(&scanners[current_scanner_index], scanner_positions.get(&current_scanner_index).unwrap()), 
                &scanners[next_scanner_index],
            ) {
                scanner_positions.insert(next_scanner_index, transform);
                resolved_scanners.insert(next_scanner_index);
                queue.push_back(next_scanner_index);
            }
        }
    }


    let mut all_beacons: HashSet<Point> = HashSet::new();
    for (i, scanner) in scanners.iter().enumerate() {
        let transform = scanner_positions.get(&i).unwrap();
        let transformed_beacons = transform_points(scanner, transform);
        for beacon in transformed_beacons {
            all_beacons.insert(beacon);
        }
    }


    println!("Total beacons: {}", all_beacons.len());
    
    let mut max_manhattan_dist = 0;
    let scanner_pos_only: Vec<Point> = scanner_positions.values().map(|(p, _)| *p).collect();

    for i in 0..scanner_pos_only.len() {
        for j in i+1..scanner_pos_only.len() {
            let dist = manhattan_distance(&scanner_pos_only[i], &scanner_pos_only[j]);
            max_manhattan_dist = max_manhattan_dist.max(dist);
        }
    }

    println!("Max Manhattan distance: {}", max_manhattan_dist);

    Ok(())
}
