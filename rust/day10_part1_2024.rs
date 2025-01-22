
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let grid: Vec<Vec<u8>> = io::BufReader::new(file)
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect()
        })
        .collect();

    let rows = grid.len();
    let cols = grid[0].len();
    let mut total_score = 0;

    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == 0 {
                let score = count_reachable_nines(&grid, r, c, rows, cols);
                total_score += score;
            }
        }
    }

    println!("{}", total_score);
    Ok(())
}


fn count_reachable_nines(grid: &Vec<Vec<u8>>, start_r: usize, start_c: usize, rows: usize, cols: usize) -> usize {
    let mut visited = vec![vec![false; cols]; rows];
    let mut count = 0;
    let mut queue = vec![(start_r, start_c)];
    visited[start_r][start_c] = true;

    while let Some((r, c)) = queue.pop() {
        if grid[r][c] == 9 {
            count += 1;
            continue;
        }
       
        let neighbors = [(r.wrapping_sub(1), c), (r+1, c), (r, c.wrapping_sub(1)), (r, c+1)];
       
        for (nr, nc) in neighbors.iter(){
                if *nr < rows && *nc < cols && !visited[*nr][*nc]{
                    if grid[*nr][*nc] == grid[r][c] +1 {
                        queue.push((*nr, *nc));
                        visited[*nr][*nc] = true;
                    }
                }
            }
    }

    count
}
