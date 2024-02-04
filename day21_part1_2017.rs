
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut rules: Vec<(&str, &str)> = Vec::new();
    for line in input.lines() {
        let parts: Vec<&str> = line.split(" => ").collect();
        rules.push((parts[0], parts[1]));
    }

    let mut grid = vec![
        vec!['.', '#', '.'],
        vec!['.', '.', '#'],
        vec!['#', '#', '#'],
    ];

    for _ in 0..5 {
        let size = grid.len();
        let square_size = if size % 2 == 0 { 2 } else { 3 };
        let new_size = size / square_size * (square_size + 1);
        let mut new_grid = vec![vec!['.'; new_size]; new_size];

        for i in 0..size / square_size {
            for j in 0..size / square_size {
                let mut square = vec![vec!['.'; square_size]; square_size];
                for x in 0..square_size {
                    for y in 0..square_size {
                        square[x][y] = grid[i * square_size + x][j * square_size + y];
                    }
                }

                for &(pattern, replacement) in &rules {
                    if matches_pattern(&square, pattern) {
                        let new_square = parse_replacement(replacement);
                        for x in 0..new_square.len() {
                            for y in 0..new_square.len() {
                                new_grid[i * (square_size + 1) + x][j * (square_size + 1) + y] = new_square[x][y];
                            }
                        }
                        break;
                    }
                }
            }
        }

        grid = new_grid;
    }

    let result = grid.iter().map(|row| row.iter().filter(|&&c| c == '#').count()).sum::<usize>();
    println!("{}", result);
}

fn matches_pattern(square: &Vec<Vec<char>>, pattern: &str) -> bool {
    let patterns: Vec<Vec<char>> = pattern.split('/').map(|s| s.chars().collect()).collect();
    for &rotation in &[0, 1, 2, 3] {
        for &flip in &[false, true] {
            let mut transformed = square.clone();
            if flip {
                for row in &mut transformed {
                    row.reverse();
                }
            }
            for _ in 0..rotation {
                transformed = rotate(&transformed);
            }
            if transformed == patterns {
                return true;
            }
        }
    }
    false
}

fn rotate(square: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut rotated = vec![vec!['.'; square.len()]; square.len()];
    for i in 0..square.len() {
        for j in 0..square.len() {
            rotated[j][square.len() - 1 - i] = square[i][j];
        }
    }
    rotated
}

fn parse_replacement(replacement: &str) -> Vec<Vec<char>> {
    replacement.split('/').map(|s| s.chars().collect()).collect()
}
