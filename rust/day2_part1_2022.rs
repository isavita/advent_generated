use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut total_score = 0;

    for line in reader.lines() {
        let line = line.expect("Error reading line");
        let opponent = line.chars().nth(0).unwrap();
        let your_move = line.chars().nth(2).unwrap();

        let mut score = 0;
        if your_move == 'X' {
            score = 1;
        } else if your_move == 'Y' {
            score = 2;
        } else if your_move == 'Z' {
            score = 3;
        }

        if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X') {
            score += 6;
        } else if opponent == 'A' && your_move == 'X' || opponent == 'B' && your_move == 'Y' || opponent == 'C' && your_move == 'Z' {
            score += 3;
        }

        total_score += score;
    }

    println!("{}", total_score);
}