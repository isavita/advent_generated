
use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    fs::File,
    io::{self, BufRead},
    usize,
};

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    x: usize,
    y: usize,
    d: usize,
    cost: i32,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    let n = grid.len();
    let m = grid[0].len();

    let mut sx = 0;
    let mut sy = 0;
    let mut ex = 0;
    let mut ey = 0;

    for i in 0..n {
        for j in 0..m {
            match grid[i].as_bytes()[j] as char {
                'S' => {
                    sx = i;
                    sy = j;
                }
                'E' => {
                    ex = i;
                    ey = j;
                }
                _ => {}
            }
        }
    }

    let dx: [i32; 4] = [-1, 0, 1, 0];
    let dy: [i32; 4] = [0, 1, 0, -1];

    let mut dist: Vec<Vec<[i32; 4]>> = vec![vec![[i32::MAX; 4]; m]; n];
    dist[sx][sy][1] = 0;

    let mut heap = BinaryHeap::new();
    heap.push(State {
        x: sx,
        y: sy,
        d: 1,
        cost: 0,
    });

    while let Some(u) = heap.pop() {
        if dist[u.x][u.y][u.d] < u.cost {
            continue;
        }

        for ndir in [(u.d + 1) % 4, (u.d + 3) % 4].iter() {
            let nc = u.cost + 1000;
            if nc < dist[u.x][u.y][*ndir] {
                dist[u.x][u.y][*ndir] = nc;
                heap.push(State {
                    x: u.x,
                    y: u.y,
                    d: *ndir,
                    cost: nc,
                });
            }
        }
        let nx = (u.x as i32 + dx[u.d]) as usize;
        let ny = (u.y as i32 + dy[u.d]) as usize;

        if nx < n && ny < m && grid[nx].as_bytes()[ny] as char != '#' {
            let nc = u.cost + 1;
            if nc < dist[nx][ny][u.d] {
                dist[nx][ny][u.d] = nc;
                heap.push(State {
                    x: nx,
                    y: ny,
                    d: u.d,
                    cost: nc,
                });
            }
        }
    }

    let mut best = i32::MAX;
    for d in 0..4 {
        best = best.min(dist[ex][ey][d]);
    }

    let mut used: Vec<Vec<bool>> = vec![vec![false; m]; n];
    let mut rev: Vec<State> = Vec::new();
    for d in 0..4 {
        if dist[ex][ey][d] == best {
            rev.push(State { x: ex, y: ey, d, cost: best });
        }
    }

    let mut vis: Vec<Vec<[bool; 4]>> = vec![vec![[false; 4]; m]; n];
    for s in rev.iter() {
        vis[s.x][s.y][s.d] = true;
    }

    while let Some(u) = rev.pop() {
        used[u.x][u.y] = true;
        let cost_u = dist[u.x][u.y][u.d];
        for pd in [(u.d + 1) % 4, (u.d + 3) % 4].iter() {
            if dist[u.x][u.y][*pd] == cost_u - 1000 {
                if !vis[u.x][u.y][*pd] {
                    vis[u.x][u.y][*pd] = true;
                    rev.push(State { x: u.x, y: u.y, d: *pd, cost: 0 });
                }
            }
        }
        let px = (u.x as i32 - dx[u.d]) as usize;
        let py = (u.y as i32 - dy[u.d]) as usize;
        if px < n && py < m && grid[px].as_bytes()[py] as char != '#' {
            if dist[px][py][u.d] == cost_u - 1 {
                if !vis[px][py][u.d] {
                    vis[px][py][u.d] = true;
                    rev.push(State {
                        x: px,
                        y: py,
                        d: u.d,
                        cost: 0,
                    });
                }
            }
        }
    }

    let mut cnt = 0;
    for i in 0..n {
        for j in 0..m {
            if used[i][j] && grid[i].as_bytes()[j] as char != '#' {
                cnt += 1;
            }
        }
    }

    println!("{}", cnt);
    Ok(())
}
