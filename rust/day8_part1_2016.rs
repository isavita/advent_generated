use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut screen = vec![vec![false; 50]; 6];

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts[0] == "rect" {
            let dimensions: Vec<&str> = parts[1].split("x").collect();
            let width = dimensions[0].parse::<usize>().unwrap();
            let height = dimensions[1].parse::<usize>().unwrap();
            for i in 0..height {
                for j in 0..width {
                    screen[i][j] = true;
                }
            }
        } else if parts[0] == "rotate" {
            let index = parts[2].split("=").collect::<Vec<&str>>()[1].parse::<usize>().unwrap();
            let amount = parts[4].parse::<usize>().unwrap();
            if parts[1] == "row" {
                let mut new_row = vec![false; 50];
                for i in 0..50 {
                    new_row[(i + amount) % 50] = screen[index][i];
                }
                screen[index] = new_row;
            } else if parts[1] == "column" {
                let mut new_column = vec![false; 6];
                for i in 0..6 {
                    new_column[(i + amount) % 6] = screen[i][index];
                }
                for i in 0..6 {
                    screen[i][index] = new_column[i];
                }
            }
        }
    }

    let lit_pixels = screen.iter().flatten().filter(|&&x| x).count();
    println!("{}", lit_pixels);
}