use std::collections::HashSet;
use std::fs;

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Point {
    x: i32,
    y: i32,
}

struct Orientation {
    points: Vec<Point>,
    w: usize,
    h: usize,
}

struct Shape {
    orientations: Vec<Orientation>,
    area: usize,
}

fn generate_orientations(pts: &[Point]) -> Vec<Orientation> {
    let mut seen: HashSet<Vec<Point>> = HashSet::new();
    let mut res = Vec::new();
    for i in 0..8 {
        let mut trans = Vec::with_capacity(pts.len());
        for p in pts {
            let (nx, ny) = match i {
                0 => (p.x, p.y),
                1 => (p.y, -p.x),
                2 => (-p.x, -p.y),
                3 => (-p.y, p.x),
                4 => (-p.x, p.y),
                5 => (p.y, p.x),
                6 => (p.x, -p.y),
                7 => (-p.y, -p.x),
                _ => (p.x, p.y),
            };
            trans.push(Point { x: nx, y: ny });
        }
        let min_x = trans.iter().map(|p| p.x).min().unwrap();
        let min_y = trans.iter().map(|p| p.y).min().unwrap();
        for p in &mut trans {
            p.x -= min_x;
            p.y -= min_y;
        }
        trans.sort();
        if seen.insert(trans.clone()) {
            let mut max_w = 0;
            let mut max_h = 0;
            for p in &trans {
                if p.x > max_w {
                    max_w = p.x;
                }
                if p.y > max_h {
                    max_h = p.y;
                }
            }
            res.push(Orientation {
                points: trans,
                w: (max_w + 1) as usize,
                h: (max_h + 1) as usize,
            });
        }
    }
    res
}

fn parse_shape(rows: Vec<String>) -> Shape {
    let mut pts = Vec::new();
    for (r, row) in rows.iter().enumerate() {
        for (c, ch) in row.chars().enumerate() {
            if ch == '#' {
                pts.push(Point {
                    x: c as i32,
                    y: r as i32,
                });
            }
        }
    }
    let area = pts.len();
    let orientations = generate_orientations(&pts);
    Shape { orientations, area }
}

fn solve(
    idx: usize,
    grid: &mut [bool],
    w: usize,
    h: usize,
    shapes: &[&Shape],
    rem_area: usize,
    free_area: usize,
) -> bool {
    if idx == shapes.len() {
        return true;
    }
    if rem_area > free_area {
        return false;
    }
    let shape = shapes[idx];
    for orient in &shape.orientations {
        if orient.w > w || orient.h > h {
            continue;
        }
        for r in 0..=h - orient.h {
            for c in 0..=w - orient.w {
                let mut ok = true;
                for p in &orient.points {
                    let pos = (r + p.y as usize) * w + (c + p.x as usize);
                    if grid[pos] {
                        ok = false;
                        break;
                    }
                }
                if !ok {
                    continue;
                }
                for p in &orient.points {
                    let pos = (r + p.y as usize) * w + (c + p.x as usize);
                    grid[pos] = true;
                }
                if solve(
                    idx + 1,
                    grid,
                    w,
                    h,
                    shapes,
                    rem_area - shape.area,
                    free_area - shape.area,
                ) {
                    return true;
                }
                for p in &orient.points {
                    let pos = (r + p.y as usize) * w + (c + p.x as usize);
                    grid[pos] = false;
                }
            }
        }
    }
    false
}

fn main() {
    let content = fs::read_to_string("input.txt").unwrap_or_default();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;
    let mut shapes: Vec<Shape> = Vec::new();
    let mut total_possible = 0usize;
    while i < lines.len() {
        let line = lines[i].trim();
        if line.is_empty() {
            i += 1;
            continue;
        }
        if line.contains('x') && line.contains(':') {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() < 2 {
                i += 1;
                continue;
            }
            let dim = parts[0].trim();
            let dims: Vec<&str> = dim.split('x').collect();
            if dims.len() < 2 {
                i += 1;
                continue;
            }
            let w: usize = dims[0].parse().unwrap_or(0);
            let h: usize = dims[1].parse().unwrap_or(0);
            let counts: Vec<&str> = parts[1].trim().split_whitespace().collect();
            let mut to_fit: Vec<&Shape> = Vec::new();
            let mut total_req_area = 0usize;
            for (idx, cnt) in counts.iter().enumerate() {
                if idx >= shapes.len() {
                    break;
                }
                let qty: usize = cnt.parse().unwrap_or(0);
                for _ in 0..qty {
                    to_fit.push(&shapes[idx]);
                    total_req_area += shapes[idx].area;
                }
            }
            to_fit.sort_by(|a, b| b.area.cmp(&a.area));
            let mut grid = vec![false; w * h];
            if solve(0, &mut grid, w, h, &to_fit, total_req_area, w * h) {
                total_possible += 1;
            }
            i += 1;
        } else if line.ends_with(':') {
            i += 1;
            let mut rows: Vec<String> = Vec::new();
            while i < lines.len() {
                let nxt = lines[i].trim();
                if nxt.is_empty() || nxt.contains(':') {
                    break;
                }
                if nxt.contains('#') || nxt.contains('.') {
                    rows.push(nxt.to_string());
                }
                i += 1;
            }
            if !rows.is_empty() {
                shapes.push(parse_shape(rows));
            }
        } else {
            i += 1;
        }
    }
    println!("{}", total_possible);
}