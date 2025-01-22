
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<Vec<u32>> = reader
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect();

    let rows = grid.len();
    let cols = grid[0].len();

    let mut dist = vec![vec![u32::MAX; cols]; rows];
    dist[0][0] = 0;

    let mut pq = BinaryHeap::new();
    pq.push(Reverse((0, 0, 0)));

    let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    while let Some(Reverse((d, r, c))) = pq.pop() {
        if d > dist[r][c] {
            continue;
        }

        if r == rows - 1 && c == cols - 1 {
            println!("{}", d);
            return Ok(());
        }

        for (dr, dc) in directions.iter() {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;

            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                let nr = nr as usize;
                let nc = nc as usize;
                let new_dist = d + grid[nr][nc];

                if new_dist < dist[nr][nc] {
                    dist[nr][nc] = new_dist;
                    pq.push(Reverse((new_dist, nr, nc)));
                }
            }
        }
    }

    Ok(())
}
