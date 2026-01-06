
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

fn main() {
    let file = File::open("input.txt").expect("cannot open input.txt");
    let mut pts = Vec::new();
    let mut xs = Vec::new();
    let mut ys = Vec::new();
    for line in BufReader::new(file).lines().flatten() {
        let mut it = line.split(',');
        if let (Some(x), Some(y)) = (it.next(), it.next()) {
            let x = x.trim().parse::<i32>().unwrap();
            let y = y.trim().parse::<i32>().unwrap();
            pts.push((x, y));
            xs.push(x);
            ys.push(y);
        }
    }
    if pts.is_empty() {
        println!("Largest valid area: 0");
        return;
    }

    xs.sort_unstable();
    ys.sort_unstable();
    xs.dedup();
    ys.dedup();

    let xidx: HashMap<_, _> = xs.iter().enumerate().map(|(i, &v)| (v, i)).collect();
    let yidx: HashMap<_, _> = ys.iter().enumerate().map(|(i, &v)| (v, i)).collect();

    let w = 2 * xs.len() + 1;
    let h = 2 * ys.len() + 1;
    let mut col_w = vec![0i64; w];
    let mut row_h = vec![0i64; h];
    col_w[0] = 1;
    for i in 0..xs.len() {
        col_w[2 * i + 1] = 1;
        if i + 1 < xs.len() {
            col_w[2 * i + 2] = (xs[i + 1] - xs[i] - 1).max(0) as i64;
        } else {
            col_w[2 * i + 2] = 1;
        }
    }
    row_h[0] = 1;
    for i in 0..ys.len() {
        row_h[2 * i + 1] = 1;
        if i + 1 < ys.len() {
            row_h[2 * i + 2] = (ys[i + 1] - ys[i] - 1).max(0) as i64;
        } else {
            row_h[2 * i + 2] = 1;
        }
    }

    let mut grid = vec![vec![0i8; w]; h];
    for i in 0..pts.len() {
        let a = pts[i];
        let b = pts[(i + 1) % pts.len()];
        let gx1 = 2 * xidx[&a.0] + 1;
        let gy1 = 2 * yidx[&a.1] + 1;
        let gx2 = 2 * xidx[&b.0] + 1;
        let gy2 = 2 * yidx[&b.1] + 1;
        if gx1 == gx2 {
            let y0 = gy1.min(gy2);
            let y1 = gy1.max(gy2);
            for y in y0..=y1 {
                if row_h[y] > 0 {
                    grid[y][gx1] = 1;
                }
            }
        } else {
            let x0 = gx1.min(gx2);
            let x1 = gx1.max(gx2);
            for x in x0..=x1 {
                if col_w[x] > 0 {
                    grid[gy1][x] = 1;
                }
            }
        }
    }

    let mut queue = Vec::new();
    queue.push((0, 0));
    grid[0][0] = 2;
    const DIRS: [(i32, i32); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    while let Some((cx, cy)) = queue.pop() {
        for &(dx, dy) in &DIRS {
            let nx = cx + dx;
            let ny = cy + dy;
            if nx >= 0 && nx < w as i32 && ny >= 0 && ny < h as i32 {
                let nx = nx as usize;
                let ny = ny as usize;
                if grid[ny][nx] == 0 {
                    grid[ny][nx] = 2;
                    queue.push((nx as i32, ny as i32));
                }
            }
        }
    }

    let mut p = vec![vec![0i64; w]; h];
    for y in 0..h {
        for x in 0..w {
            let val = if grid[y][x] != 2 { col_w[x] * row_h[y] } else { 0 };
            let left = if x > 0 { p[y][x - 1] } else { 0 };
            let up = if y > 0 { p[y - 1][x] } else { 0 };
            let diag = if x > 0 && y > 0 { p[y - 1][x - 1] } else { 0 };
            p[y][x] = val + left + up - diag;
        }
    }

    let mut max_area = 0i64;
    for i in 0..pts.len() {
        for j in i..pts.len() {
            let a = pts[i];
            let b = pts[j];
            let w_area = (a.0 - b.0).abs() as i64 + 1;
            let h_area = (a.1 - b.1).abs() as i64 + 1;
            let area = w_area * h_area;
            if area <= max_area {
                continue;
            }
            let mut gx1 = 2 * xidx[&a.0] + 1;
            let mut gy1 = 2 * yidx[&a.1] + 1;
            let mut gx2 = 2 * xidx[&b.0] + 1;
            let mut gy2 = 2 * yidx[&b.1] + 1;
            if gx1 > gx2 {
                std::mem::swap(&mut gx1, &mut gx2);
            }
            if gy1 > gy2 {
                std::mem::swap(&mut gy1, &mut gy2);
            }
            let total = p[gy2][gx2];
            let left = if gx1 > 0 { p[gy2][gx1 - 1] } else { 0 };
            let up = if gy1 > 0 { p[gy1 - 1][gx2] } else { 0 };
            let diag = if gx1 > 0 && gy1 > 0 { p[gy1 - 1][gx1 - 1] } else { 0 };
            let valid = total - left - up + diag;
            if valid == area {
                max_area = area;
            }
        }
    }

    println!("Largest valid area: {}", max_area);
}
