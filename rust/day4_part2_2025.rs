
use std::fs;

fn main() {
    let txt = fs::read_to_string("input.txt").unwrap_or_default();
    let mut grid: Vec<Vec<u8>> = txt
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.bytes().collect())
        .collect();
    if grid.is_empty() {
        println!("Total rolls removed: 0");
        return;
    }
    let (rows, cols) = (grid.len(), grid[0].len());
    let mut total = 0;
    loop {
        let mut remove = Vec::new();
        for r in 0..rows {
            for c in 0..cols {
                if grid[r][c] == b'@' {
                    let mut cnt = 0;
                    for dr in -1i32..=1 {
                        for dc in -1i32..=1 {
                            if dr == 0 && dc == 0 {
                                continue;
                            }
                            let nr = r as i32 + dr;
                            let nc = c as i32 + dc;
                            if nr >= 0
                                && (nr as usize) < rows
                                && nc >= 0
                                && (nc as usize) < cols
                                && grid[nr as usize][nc as usize] == b'@'
                            {
                                cnt += 1;
                            }
                        }
                    }
                    if cnt < 4 {
                        remove.push((r, c));
                    }
                }
            }
        }
        if remove.is_empty() {
            break;
        }
        total += remove.len();
        for (r, c) in remove {
            grid[r][c] = b'.';
        }
    }
    println!("Total rolls removed: {}", total);
}
