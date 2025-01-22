
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Failed to open input.txt");
    let reader = BufReader::new(file);

    let mut screen = [[false; 50]; 6];

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let parts: Vec<&str> = line.split_whitespace().collect();

        match parts[0] {
            "rect" => {
                let dimensions: Vec<&str> = parts[1].split('x').collect();
                let width = dimensions[0].parse::<usize>().unwrap();
                let height = dimensions[1].parse::<usize>().unwrap();
                for y in 0..height {
                    for x in 0..width {
                        screen[y][x] = true;
                    }
                }
            }
            "rotate" => {
                if parts[1] == "row" {
                    let row = parts[2][2..].parse::<usize>().unwrap();
                    let shift = parts[4].parse::<usize>().unwrap();
                    let mut new_row = [false; 50];
                    for x in 0..50 {
                        new_row[(x + shift) % 50] = screen[row][x];
                    }
                    screen[row] = new_row;
                } else if parts[1] == "column" {
                    let col = parts[2][2..].parse::<usize>().unwrap();
                    let shift = parts[4].parse::<usize>().unwrap();
                    let mut new_col = [false; 6];
                    for y in 0..6 {
                        new_col[(y + shift) % 6] = screen[y][col];
                    }
                    for y in 0..6 {
                        screen[y][col] = new_col[y];
                    }
                }
            }
            _ => {}
        }
    }

    // Part 1: Count lit pixels
    let lit_pixels = screen.iter().flatten().filter(|&pixel| *pixel).count();
    println!("Part 1: Lit pixels = {}", lit_pixels);

    // Part 2: Print the display
    println!("Part 2:");
    for row in screen.iter() {
        for &pixel in row.iter() {
            print!("{}", if pixel { '#' } else { '.' });
        }
        println!();
    }
}
