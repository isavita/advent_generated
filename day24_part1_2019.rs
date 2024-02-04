
use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    let mut biodiversity_ratings = HashSet::new();
    let mut biodiversity_rating = 0;

    loop {
        let bio_rating = calculate_biodiversity_rating(&grid);
        if biodiversity_ratings.contains(&bio_rating) {
            println!("{}", bio_rating);
            break;
        }
        biodiversity_ratings.insert(bio_rating);

        grid = evolve(&grid);
    }
}

fn calculate_biodiversity_rating(grid: &Vec<Vec<char>>) -> usize {
    let mut rating = 0;
    let mut power = 1;

    for row in grid {
        for &cell in row {
            if cell == '#' {
                rating += power;
            }
            power *= 2;
        }
    }

    rating
}

fn evolve(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_grid = grid.clone();

    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            let adjacent_bugs = count_adjacent_bugs(&grid, i, j);

            match grid[i][j] {
                '#' => {
                    if adjacent_bugs != 1 {
                        new_grid[i][j] = '.';
                    }
                }
                '.' => {
                    if adjacent_bugs == 1 || adjacent_bugs == 2 {
                        new_grid[i][j] = '#';
                    }
                }
                _ => {}
            }
        }
    }

    new_grid
}

fn count_adjacent_bugs(grid: &Vec<Vec<char>>, i: usize, j: usize) -> usize {
    let mut count = 0;

    if i > 0 && grid[i - 1][j] == '#' {
        count += 1;
    }
    if i < grid.len() - 1 && grid[i + 1][j] == '#' {
        count += 1;
    }
    if j > 0 && grid[i][j - 1] == '#' {
        count += 1;
    }
    if j < grid[i].len() - 1 && grid[i][j + 1] == '#' {
        count += 1;
    }

    count
}
