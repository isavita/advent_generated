
use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    fs::File,
    io::{self, BufRead},
    i32,
};

#[derive(Copy, Clone, Eq, PartialEq)]
struct Node {
    x: usize,
    y: usize,
    d: usize,
    cost: i32,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for Node {
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
    let (mut sx, mut sy, mut ex, mut ey) = (0, 0, 0, 0);

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

    let mut dist = vec![vec![vec![i32::MAX; 4]; m]; n];
    dist[sx][sy][1] = 0;

    let mut heap = BinaryHeap::new();
    heap.push(Node {
        x: sx,
        y: sy,
        d: 1,
        cost: 0,
    });

    while let Some(u) = heap.pop() {
        if dist[u.x][u.y][u.d] < u.cost {
            continue;
        }
        if u.x == ex && u.y == ey {
            println!("{}", u.cost);
            return Ok(());
        }

        for &ndir in &[(u.d + 1) % 4, (u.d + 3) % 4] {
            let nc = u.cost + 1000;
            if nc < dist[u.x][u.y][ndir] {
                dist[u.x][u.y][ndir] = nc;
                heap.push(Node {
                    x: u.x,
                    y: u.y,
                    d: ndir,
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
                heap.push(Node {
                    x: nx,
                    y: ny,
                    d: u.d,
                    cost: nc,
                });
            }
        }
    }
    Ok(())
}
