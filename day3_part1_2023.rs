
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::char;

fn main() {
    let matrix = read_file_to_matrix("input.txt").unwrap();
    let sum = sum_of_part_numbers(&matrix);
    println!("{}", sum);
}

fn read_file_to_matrix(file_path: &str) -> Result<Vec<Vec<char>>, std::io::Error> {
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);
    let mut matrix = Vec::new();

    for line in reader.lines() {
        let row: Vec<char> = line?.chars().collect();
        matrix.push(row);
    }

    Ok(matrix)
}

fn sum_of_part_numbers(matrix: &Vec<Vec<char>>) -> i32 {
    let mut sum = 0;
    let mut visited = vec![vec![false; matrix[0].len()]; matrix.len()];

    for y in 0..matrix.len() {
        for x in 0..matrix[y].len() {
            if !visited[y][x] && matrix[y][x].is_digit(10) {
                let (number, length) = extract_number(&matrix, x, y);
                if is_adjacent_to_symbol(&matrix, x, y, length) {
                    sum += number;
                }
                for i in 0..length {
                    visited[y][x + i] = true;
                }
            }
        }
    }

    sum
}

fn extract_number(matrix: &Vec<Vec<char>>, x: usize, y: usize) -> (i32, usize) {
    let mut number_str = String::new();
    let mut x = x;

    while x < matrix[y].len() && matrix[y][x].is_digit(10) {
        number_str.push(matrix[y][x]);
        x += 1;
    }

    let number = number_str.parse().unwrap();
    (number, number_str.len())
}

fn is_adjacent_to_symbol(matrix: &Vec<Vec<char>>, x: usize, y: usize, length: usize) -> bool {
    for i in 0..length {
        if check_adjacent(&matrix, x + i, y) {
            return true;
        }
    }
    false
}

fn check_adjacent(matrix: &Vec<Vec<char>>, x: usize, y: usize) -> bool {
    for dy in -1..=1 {
        for dx in -1..=1 {
            let adj_x = x as i32 + dx;
            let adj_y = y as i32 + dy;

            if adj_y >= 0 && adj_y < matrix.len() as i32 && adj_x >= 0 && adj_x < matrix[adj_y as usize].len() as i32 {
                if !matrix[adj_y as usize][adj_x as usize].is_digit(10) && matrix[adj_y as usize][adj_x as usize] != '.' {
                    return true;
                }
            }
        }
    }
    false
}
