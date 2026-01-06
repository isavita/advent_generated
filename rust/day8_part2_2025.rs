
use std::fs;
use std::io::{self, BufRead};

#[derive(Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

fn dist_sq(a: Point, b: Point) -> i64 {
    let dx = (a.x - b.x) as i64;
    let dy = (a.y - b.y) as i64;
    let dz = (a.z - b.z) as i64;
    dx * dx + dy * dy + dz * dz
}

fn find(parent: &mut Vec<usize>, mut x: usize) -> usize {
    while parent[x] != x {
        let p = parent[x];
        parent[x] = parent[p];
        x = p;
    }
    x
}

fn union(parent: &mut Vec<usize>, rank: &mut Vec<u8>, mut x: usize, mut y: usize) {
    if rank[x] < rank[y] {
        parent[x] = y;
    } else if rank[x] > rank[y] {
        parent[y] = x;
    } else {
        parent[y] = x;
        rank[x] += 1;
    }
}

fn main() {
    let file = fs::File::open("input.txt").unwrap();
    let mut pts = Vec::new();
    for line in io::BufReader::new(file).lines().flatten() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let mut parts = line.split(',');
        if let (Some(x), Some(y), Some(z)) = (parts.next(), parts.next(), parts.next()) {
            if let (Ok(x), Ok(y), Ok(z)) = (x.parse(), y.parse(), z.parse()) {
                pts.push(Point { x, y, z });
            }
        }
    }

    if pts.len() < 2 {
        return;
    }

    let n = pts.len();
    let mut edges = Vec::with_capacity(n * (n - 1) / 2);
    for i in 0..n {
        for j in i + 1..n {
            let d = dist_sq(pts[i], pts[j]);
            edges.push((d, i, j));
        }
    }
    edges.sort_unstable();

    let mut parent: Vec<usize> = (0..n).collect();
    let mut rank = vec![0u8; n];
    let mut comps = n;

    for &(d, u, v) in &edges {
        if comps == 1 {
            break;
        }
        let ru = find(&mut parent, u);
        let rv = find(&mut parent, v);
        if ru != rv {
            union(&mut parent, &mut rank, ru, rv);
            comps -= 1;
            if comps == 1 {
                let p1 = pts[u];
                let p2 = pts[v];
                println!(
                    "Connected {},{},{} and {},{},{}",
                    p1.x, p1.y, p1.z, p2.x, p2.y, p2.z
                );
                println!("Product of X coordinates: {}", p1.x as i64 * p2.x as i64);
            }
        }
    }
}
