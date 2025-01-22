
use std::fs;
use std::cmp::{max, min};

#[derive(Debug)]
struct Sensor {
    pos: (i64, i64),
    beacon: (i64, i64),
    dist: i64,
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let sensors = parse_sensors(&input);
    println!("{}", distress(&sensors, 4000000));
}

fn parse_sensors(input: &str) -> Vec<Sensor> {
    input.lines().map(|line| {
        let parts: Vec<i64> = line
            .split(|c: char| !c.is_ascii_digit() && c != '-')
            .filter_map(|s| s.parse().ok())
            .collect();
        let pos = (parts[0], parts[1]);
        let beacon = (parts[2], parts[3]);
        let dist = manhattan(pos, beacon);
        Sensor { pos, beacon, dist }
    }).collect()
}

fn distress(sensors: &[Sensor], maxcoord: i64) -> i64 {
    for y in 0..=maxcoord {
        let mut ranges: Vec<(i64, i64)> = Vec::new();
        for sensor in sensors {
            let dist_y = (sensor.pos.1 - y).abs();
            if dist_y > sensor.dist {
                continue;
            }
            let dx = sensor.dist - dist_y;
            ranges.push((sensor.pos.0 - dx, sensor.pos.0 + dx));
        }
        ranges.sort_by_key(|&(a, _)| a);

        let mut merged_ranges: Vec<(i64, i64)> = Vec::new();
        for (start, end) in ranges {
            if let Some(&(prev_start, prev_end)) = merged_ranges.last() {
                if start <= prev_end + 1 {
                  merged_ranges.pop();
                  merged_ranges.push((min(prev_start, start), max(prev_end, end)));
                } else {
                  merged_ranges.push((start, end));
                }
            } else {
              merged_ranges.push((start, end));
            }
        }
        
        if merged_ranges.len() > 1 {
            let x = merged_ranges[0].1 + 1;
            return x * 4000000 + y;
        }
    }
    -1
}

fn manhattan(p: (i64, i64), q: (i64, i64)) -> i64 {
    (p.0 - q.0).abs() + (p.1 - q.1).abs()
}
