
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut grid = std::collections::HashMap::new();
    let mut parts = Vec::new();
    let mut curr: Option<Part> = None;

    for (y, line) in input.trim().split("\n").enumerate() {
        if let Some(part) = curr.take() {
            parts.push(part);
        }
        for (x, c) in line.chars().enumerate() {
            grid.insert((x as i32, y as i32), c);
            if c.is_digit(10) {
                if let Some(part) = &mut curr {
                    part.n *= 10;
                    part.n += c.to_digit(10).unwrap() as i32;
                    part.xmax = x as i32;
                } else {
                    curr = Some(Part {
                        xmin: x as i32,
                        xmax: x as i32,
                        y: y as i32,
                        n: c.to_digit(10).unwrap() as i32,
                    });
                }
            } else if let Some(part) = curr.take() {
                parts.push(part);
            }
        }
    }

    let mut parts_grid = std::collections::HashMap::new();
    for (i, part) in parts.iter().enumerate() {
        for x in part.xmin..=part.xmax {
            parts_grid.insert((x, part.y), i);
        }
    }

    let mut sum = 0;
    for (p, &c) in &grid {
        if c == '*' {
            let mut neighbor_parts = std::collections::HashSet::new();
            for &n in NEIGHBORS_8.iter() {
                if let Some(&i) = parts_grid.get(&(n.0 + p.0, n.1 + p.1)) {
                    neighbor_parts.insert(i);
                }
            }
            if neighbor_parts.len() == 2 {
                let mut prod = 1;
                for &i in &neighbor_parts {
                    prod *= parts[i].n;
                }
                sum += prod;
            }
        }
    }
    println!("{}", sum);
}

const NEIGHBORS_8: [(i32, i32); 8] = [(0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1)];

struct Part {
    xmin: i32,
    xmax: i32,
    y: i32,
    n: i32,
}
