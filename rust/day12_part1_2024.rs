
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let grid: Vec<String> = reader.lines().filter_map(Result::ok).collect();

    let total_price = solve(&grid);
    println!("{}", total_price);
    Ok(())
}

fn solve(grid: &[String]) -> usize {
    let rows = grid.len();
    if rows == 0 {
        return 0;
    }
    let cols = grid[0].len();

    let mut visited = vec![vec![false; cols]; rows];
    let mut total_price = 0;

    for r in 0..rows {
        for c in 0..cols {
            if !visited[r][c] {
                let (area, perimeter) = calculate_region(grid, r, c, &mut visited);
                total_price += area * perimeter;
            }
        }
    }
    total_price
}


fn calculate_region(grid: &[String], row: usize, col: usize, visited: &mut Vec<Vec<bool>>) -> (usize, usize) {
    let rows = grid.len();
    let cols = grid[0].len();
    let char = grid[row].as_bytes()[col];
    let mut area = 0;
    let mut perimeter = 0;

    let mut queue = vec![Point { x: row, y: col }];
    visited[row][col] = true;

    while let Some(p) = queue.pop() {
        area += 1;
        let is_border = p.x == 0 || p.x == rows - 1 || p.y == 0 || p.y == cols - 1;

        // Check top
        if p.x > 0 {
            if grid[p.x - 1].as_bytes()[p.y] != char {
                perimeter += 1;
            } else if !visited[p.x - 1][p.y] {
                queue.push(Point { x: p.x - 1, y: p.y });
                visited[p.x - 1][p.y] = true;
            }
        } else if is_border {
            perimeter += 1;
        }
        // Check bottom
        if p.x < rows - 1 {
            if grid[p.x + 1].as_bytes()[p.y] != char {
                perimeter += 1;
            } else if !visited[p.x + 1][p.y] {
                queue.push(Point { x: p.x + 1, y: p.y });
                visited[p.x + 1][p.y] = true;
            }
        } else if is_border {
            perimeter += 1;
        }
        // Check left
        if p.y > 0 {
            if grid[p.x].as_bytes()[p.y - 1] != char {
                perimeter += 1;
            } else if !visited[p.x][p.y - 1] {
                queue.push(Point { x: p.x, y: p.y - 1 });
                visited[p.x][p.y - 1] = true;
            }
        } else if is_border {
            perimeter += 1;
        }
        // Check right
         if p.y < cols - 1 {
             if grid[p.x].as_bytes()[p.y + 1] != char {
                 perimeter += 1;
             } else if !visited[p.x][p.y + 1] {
                 queue.push(Point { x: p.x, y: p.y + 1 });
                visited[p.x][p.y + 1] = true;
            }
        } else if is_border {
            perimeter += 1;
        }
    }
    (area, perimeter)
}
