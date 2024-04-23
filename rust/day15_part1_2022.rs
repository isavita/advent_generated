use std::fs::read_to_string;
use std::collections::HashSet;

type Point = (i32, i32);

struct Sensor {
    pos: Point,
    beacon: Point,
    dist: i32,
}

impl Sensor {
    fn new(pos: Point, beacon: Point) -> Self {
        let dist = manhattan(pos, beacon);
        Sensor { pos, beacon, dist }
    }
}

fn manhattan(p: Point, q: Point) -> i32 {
    (p.0 - q.0).abs() + (p.1 - q.1).abs()
}

fn impossible(sensors: &[Sensor], y: i32) -> usize {
    let mut pts = HashSet::new();
    for s in sensors {
        let dist = s.dist - (s.pos.1 - y).abs();
        for x in 0..=dist {
            pts.insert(s.pos.0 + x);
            pts.insert(s.pos.0 - x);
        }
    }
    for s in sensors {
        if s.beacon.1 == y {
            pts.remove(&s.beacon.0);
        }
    }
    pts.len()
}

fn main() -> std::io::Result<()> {
    let input = read_to_string("input.txt")?;
    let mut sensors = vec![];
    for line in input.lines() {
        let mut parts = line.split(": ");
        let sensor_str = parts.next().unwrap();
        let beacon_str = parts.next().unwrap();
        let sensor_parts: Vec<_> = sensor_str.split(", ").collect();
        let beacon_parts: Vec<_> = beacon_str.split(", ").collect();
        let sensor_x = sensor_parts[0].trim_start_matches("Sensor at x=").parse().unwrap();
        let sensor_y = sensor_parts[1].trim_start_matches("y=").parse().unwrap();
        let beacon_x = beacon_parts[0].trim_start_matches("closest beacon is at x=").parse().unwrap();
        let beacon_y = beacon_parts[1].trim_start_matches("y=").parse().unwrap();
        let sensor = Sensor::new((sensor_x, sensor_y), (beacon_x, beacon_y));
        sensors.push(sensor);
    }
    println!("{}", impossible(&sensors, 2000000));
    Ok(())
}