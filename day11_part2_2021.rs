use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut grid: Vec<Vec<i32>> = Vec::new();
    for line in input.lines() {
        let row: Vec<i32> = line.chars().map(|c| c.to_digit(10).unwrap() as i32).collect();
        grid.push(row);
    }

    let mut step = 0;
    loop {
        step += 1;
        let flashes = simulate_step(&mut grid);
        if flashes == 100 {
            break;
        }
    }

    println!("{}", step);
}

fn simulate_step(grid: &mut Vec<Vec<i32>>) -> i32 {
    let mut flashes = 0;
    let mut flashed: std::collections::HashMap<(usize, usize), bool> = std::collections::HashMap::new();

    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            grid[y][x] += 1;
        }
    }

    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] > 9 {
                flashes += flash(grid, x, y, &mut flashed);
            }
        }
    }

    for (coord, _) in flashed.into_iter() {
        grid[coord.1][coord.0] = 0;
    }

    flashes
}

fn flash(grid: &mut Vec<Vec<i32>>, x: usize, y: usize, flashed: &mut std::collections::HashMap<(usize, usize), bool>) -> i32 {
    if flashed.contains_key(&(x, y)) {
        return 0;
    }

    flashed.insert((x, y), true);
    let mut flashes = 1;
    let directions: Vec<(i32, i32)> = vec![(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

    for dir in directions {
        let new_x = (x as i32 + dir.0) as usize;
        let new_y = (y as i32 + dir.1) as usize;
        if new_x < grid[0].len() && new_y < grid.len() {
            grid[new_y][new_x] += 1;
            if grid[new_y][new_x] > 9 {
                flashes += flash(grid, new_x, new_y, flashed);
            }
        }
    }

    flashes
}