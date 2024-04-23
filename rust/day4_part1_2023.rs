use std::fs::File;
use std::io::{BufRead, BufReader};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut total_points = 0;
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<_> = line.split(" | ").collect();
        let winning_numbers: Vec<i32> = parts[0].split_whitespace().map(|x| x.parse::<i32>().unwrap_or(0)).collect();
        let your_numbers: Vec<i32> = parts[1].split_whitespace().map(|x| x.parse::<i32>().unwrap_or(0)).collect();
        total_points += calculate_points(&winning_numbers, &your_numbers);
    }

    println!("{}", total_points);
    Ok(())
}

fn calculate_points(winning_numbers: &Vec<i32>, your_numbers: &Vec<i32>) -> i32 {
    let mut points = 0;
    for &num in your_numbers {
        if winning_numbers.contains(&num) {
            if points == 0 {
                points = 1;
            } else {
                points *= 2;
            }
        }
    }
    points
}