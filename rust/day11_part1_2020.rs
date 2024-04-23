use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);
    let seating_area: Vec<Vec<char>> = reader.lines()
        .map(|line| line.expect("Error reading line").chars().collect())
        .collect();

    let mut stabilized = false;
    let mut seating_area = seating_area;
    while !stabilized {
        let (new_seating_area, new_stabilized) = simulate_seating(&seating_area);
        seating_area = new_seating_area;
        stabilized = new_stabilized;
    }

    println!("{}", count_occupied_seats(&seating_area));
}

fn simulate_seating(seating_area: &Vec<Vec<char>>) -> (Vec<Vec<char>>, bool) {
    let rows = seating_area.len();
    let cols = seating_area[0].len();
    let mut new_seating_area = vec![vec![' '; cols]; rows];
    for i in 0..rows {
        for j in 0..cols {
            new_seating_area[i][j] = seating_area[i][j];
        }
    }
    let mut stabilized = true;

    for i in 0..rows {
        for j in 0..cols {
            match seating_area[i][j] {
                'L' => {
                    if count_adjacent_occupied(seating_area, i, j) == 0 {
                        new_seating_area[i][j] = '#';
                        stabilized = false;
                    }
                }
                '#' => {
                    if count_adjacent_occupied(seating_area, i, j) >= 4 {
                        new_seating_area[i][j] = 'L';
                        stabilized = false;
                    }
                }
                _ => {}
            }
        }
    }

    (new_seating_area, stabilized)
}

fn count_adjacent_occupied(seating_area: &Vec<Vec<char>>, row: usize, col: usize) -> i32 {
    let mut count = 0;
    for i in row as i32 - 1..=row as i32 + 1 {
        for j in col as i32 - 1..=col as i32 + 1 {
            if i == row as i32 && j == col as i32 {
                continue;
            }
            if i >= 0 && i < seating_area.len() as i32 && j >= 0 && j < seating_area[0].len() as i32 {
                if seating_area[i as usize][j as usize] == '#' {
                    count += 1;
                }
            }
        }
    }
    count
}

fn count_occupied_seats(seating_area: &Vec<Vec<char>>) -> i32 {
    let mut count = 0;
    for row in seating_area {
        for &seat in row {
            if seat == '#' {
                count += 1;
            }
        }
    }
    count
}