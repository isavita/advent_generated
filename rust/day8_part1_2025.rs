
use std::collections::HashMap;
use std::fs;

fn main() {
    let data = fs::read_to_string("input.txt").expect("Cannot read file");
    let mut pts = Vec::new();
    for l in data.lines() {
        let v: Vec<i32> = l
            .split(',')
            .filter_map(|s| s.trim().parse().ok())
            .collect();
        if v.len() == 3 {
            pts.push((v[0], v[1], v[2]));
        }
    }

    if pts.len() < 2 {
        println!("Not enough points to form circuits.");
        return;
    }

    let n = pts.len();
    let mut edges = Vec::new();
    for i in 0..n {
        for j in i + 1..n {
            let dx = pts[i].0 - pts[j].0;
            let dy = pts[i].1 - pts[j].1;
            let dz = pts[i].2 - pts[j].2;
            let d = (dx as i64) * (dx as i64) + (dy as i64) * (dy as i64) + (dz as i64) * (dz as i64);
            edges.push((d, i, j));
        }
    }
    edges.sort_unstable();

    let mut uf = UnionFind::new(n);
    for &(d, u, v) in edges.iter().take(1000.min(edges.len())) {
        uf.union(u, v);
    }

    let mut freq = HashMap::new();
    for i in 0..n {
        *freq.entry(uf.find(i)).or_insert(0) += 1;
    }
    let mut sizes: Vec<_> = freq.values().copied().collect();
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    let prod: i64 = sizes.iter().take(3).product();
    println!("{}", prod);
}

struct UnionFind {
    p: Vec<usize>,
    rank: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            p: (0..n).collect(),
            rank: vec![1; n],
        }
    }
    fn find(&mut self, x: usize) -> usize {
        if self.p[x] != x {
            self.p[x] = self.find(self.p[x]);
        }
        self.p[x]
    }
    fn union(&mut self, x: usize, y: usize) {
        let (rx, ry) = (self.find(x), self.find(y));
        if rx == ry {
            return;
        }
        if self.rank[rx] >= self.rank[ry] {
            self.p[ry] = rx;
            if self.rank[rx] == self.rank[ry] {
                self.rank[rx] += 1;
            }
        } else {
            self.p[rx] = ry;
        }
    }
}
