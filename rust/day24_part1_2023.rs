
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Copy)]
struct Hailstone {
    px: f64,
    py: f64,
    vx: f64,
    vy: f64,
}

fn parse_hailstone(line: &str) -> Option<Hailstone> {
    let parts: Vec<&str> = line.split(" @ ").collect();
    if parts.len() != 2 {
        return None;
    }

    let pos_parts: Vec<&str> = parts[0].split(", ").collect();
    let vel_parts: Vec<&str> = parts[1].split(", ").collect();

    if pos_parts.len() != 3 || vel_parts.len() != 3 {
        return None;
    }

    Some(Hailstone {
        px: pos_parts[0].trim().parse().ok()?,
        py: pos_parts[1].trim().parse().ok()?,
        vx: vel_parts[0].trim().parse().ok()?,
        vy: vel_parts[1].trim().parse().ok()?,
    })
}

fn intersection(h1: &Hailstone, h2: &Hailstone) -> Option<(f64, f64)> {
    let det = h1.vx * h2.vy - h2.vx * h1.vy;
    if det == 0.0 {
        return None; // Parallel lines
    }

    let t1 = ((h2.px - h1.px) * h2.vy - (h2.py - h1.py) * h2.vx) / det;
    let t2 = ((h2.px - h1.px) * h1.vy - (h2.py - h1.py) * h1.vx) / det;

    if t1 < 0.0 || t2 < 0.0 {
        return None; // Intersection in the past
    }

    let x = h1.px + t1 * h1.vx;
    let y = h1.py + t1 * h1.vy;
    Some((x, y))
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let hailstones: Vec<Hailstone> = reader
        .lines()
        .filter_map(|line| line.ok())
        .filter_map(|line| parse_hailstone(&line))
        .collect();

    let min_coord = 200000000000000.0;
    let max_coord = 400000000000000.0;

    let mut intersection_count = 0;
    for i in 0..hailstones.len() {
        for j in i + 1..hailstones.len() {
            if let Some((x, y)) = intersection(&hailstones[i], &hailstones[j]) {
                if x >= min_coord && x <= max_coord && y >= min_coord && y <= max_coord {
                    intersection_count += 1;
                }
            }
        }
    }

    println!("{}", intersection_count);

    Ok(())
}
