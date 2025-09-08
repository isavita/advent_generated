
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy)]
struct Hailstone {
    px: i64,
    py: i64,
    pz: i64,
    vx: i64,
    vy: i64,
    vz: i64,
}

fn parse_input(filename: &str) -> Vec<Hailstone> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut hailstones = Vec::new();

    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split(&[',', '@'][..]).map(|s| s.trim()).collect();
        if parts.len() == 6 {
            let px = parts[0].parse().unwrap();
            let py = parts[1].parse().unwrap();
            let pz = parts[2].parse().unwrap();
            let vx = parts[3].parse().unwrap();
            let vy = parts[4].parse().unwrap();
            let vz = parts[5].parse().unwrap();
            hailstones.push(Hailstone { px, py, pz, vx, vy, vz });
        }
    }
    hailstones
}

fn solve_part1(hailstones: &[Hailstone]) -> i64 {
    let min_coord = 200000000000000.0;
    let max_coord = 400000000000000.0;
    let mut count = 0;

    for i in 0..hailstones.len() {
        for j in i + 1..hailstones.len() {
            let h1 = hailstones[i];
            let h2 = hailstones[j];
            let det = h1.vx as i128 * h2.vy as i128 - h1.vy as i128 * h2.vx as i128;
            if det == 0 {
                continue;
            }

            let t1_num = (h2.px as i128 - h1.px as i128) * h2.vy as i128 - (h2.py as i128 - h1.py as i128) * h2.vx as i128;
            let t2_num = (h2.px as i128 - h1.px as i128) * h1.vy as i128 - (h2.py as i128 - h1.py as i128) * h1.vx as i128;
            let t1 = t1_num as f64 / det as f64;
            let t2 = t2_num as f64 / det as f64;

            if t1 > 0.0 && t2 > 0.0 {
                let ix = h1.px as f64 + h1.vx as f64 * t1;
                let iy = h1.py as f64 + h1.vy as f64 * t1;
                if ix >= min_coord && ix <= max_coord && iy >= min_coord && iy <= max_coord {
                    count += 1;
                }
            }
        }
    }
    count
}

fn solve_linear_system(a: &mut [[f64; 7]; 6]) {
    for i in 0..6 {
        let mut max_row = i;
        for k in i + 1..6 {
            if a[k][i].abs() > a[max_row][i].abs() {
                max_row = k;
            }
        }
        for k in i..7 {
            let temp = a[i][k];
            a[i][k] = a[max_row][k];
            a[max_row][k] = temp;
        }

        for k in i + 1..6 {
            let factor = a[k][i] / a[i][i];
            for j in i..7 {
                a[k][j] -= factor * a[i][j];
            }
        }
    }

    for i in (0..6).rev() {
        for j in i + 1..6 {
            a[i][6] -= a[i][j] * a[j][6];
        }
        a[i][6] /= a[i][i];
    }
}

fn solve_part2(hailstones: &[Hailstone]) -> i64 {
    if hailstones.len() < 3 {
        return 0;
    }

    let h0 = hailstones[0];
    let h1 = hailstones[1];
    let h2 = hailstones[2];

    let mut a = [[0.0; 7]; 6];

    let dvx1 = h0.vx as f64 - h1.vx as f64;
    let dvy1 = h0.vy as f64 - h1.vy as f64;
    let dvz1 = h0.vz as f64 - h1.vz as f64;
    let dpx1 = h0.px as f64 - h1.px as f64;
    let dpy1 = h0.py as f64 - h1.py as f64;
    let dpz1 = h0.pz as f64 - h1.pz as f64;

    let dvx2 = h0.vx as f64 - h2.vx as f64;
    let dvy2 = h0.vy as f64 - h2.vy as f64;
    let dvz2 = h0.vz as f64 - h2.vz as f64;
    let dpx2 = h0.px as f64 - h2.px as f64;
    let dpy2 = h0.py as f64 - h2.py as f64;
    let dpz2 = h0.pz as f64 - h2.pz as f64;

    a[0][1] = dvz1;
    a[0][2] = -dvy1;
    a[0][4] = dpz1;
    a[0][5] = -dpy1;
    a[1][0] = -dvz1;
    a[1][2] = dvx1;
    a[1][3] = -dpz1;
    a[1][5] = dpx1;
    a[2][0] = dvy1;
    a[2][1] = -dvx1;
    a[2][3] = dpy1;
    a[2][4] = -dpx1;
    a[3][1] = dvz2;
    a[3][2] = -dvy2;
    a[3][4] = dpz2;
    a[3][5] = -dpy2;
    a[4][0] = -dvz2;
    a[4][2] = dvx2;
    a[4][3] = -dpz2;
    a[4][5] = dpx2;
    a[5][0] = dvy2;
    a[5][1] = -dvx2;
    a[5][3] = dpy2;
    a[5][4] = -dpx2;

    a[0][6] = (h0.py as f64 * h0.vz as f64 - h0.pz as f64 * h0.vy as f64) - (h1.py as f64 * h1.vz as f64 - h1.pz as f64 * h1.vy as f64);
    a[1][6] = (h0.pz as f64 * h0.vx as f64 - h0.px as f64 * h0.vz as f64) - (h1.pz as f64 * h1.vx as f64 - h1.px as f64 * h1.vz as f64);
    a[2][6] = (h0.px as f64 * h0.vy as f64 - h0.py as f64 * h0.vx as f64) - (h1.px as f64 * h1.vy as f64 - h1.py as f64 * h1.vx as f64);
    a[3][6] = (h0.py as f64 * h0.vz as f64 - h0.pz as f64 * h0.vy as f64) - (h2.py as f64 * h2.vz as f64 - h2.pz as f64 * h2.vy as f64);
    a[4][6] = (h0.pz as f64 * h0.vx as f64 - h0.px as f64 * h0.vz as f64) - (h2.pz as f64 * h2.vx as f64 - h2.px as f64 * h2.vz as f64);
    a[5][6] = (h0.px as f64 * h0.vy as f64 - h0.py as f64 * h0.vx as f64) - (h2.px as f64 * h2.vy as f64 - h2.py as f64 * h2.vx as f64);

    solve_linear_system(&mut a);

    let prx = a[0][6].round() as i64;
    let pry = a[1][6].round() as i64;
    let prz = a[2][6].round() as i64;
    prx + pry + prz
}

fn main() {
    let hailstones = parse_input("input.txt");
    println!("Part 1: {}", solve_part1(&hailstones));
    println!("Part 2: {}", solve_part2(&hailstones));
}
