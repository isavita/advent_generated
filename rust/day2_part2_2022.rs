use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut total_score = 0;

    for line in reader.lines() {
        let line = line.expect("Error reading line");
        let opponent = line.chars().nth(0).unwrap();
        let round_end = line.chars().nth(2).unwrap();

        let mut your_move = ' ';

        if round_end == 'X' {
            your_move = if opponent == 'A' { 'Z' } else if opponent == 'B' { 'X' } else { 'Y' };
        } else if round_end == 'Y' {
            your_move = if opponent == 'A' { 'X' } else if opponent == 'B' { 'Y' } else { 'Z' };
        } else {
            your_move = if opponent == 'A' { 'Y' } else if opponent == 'B' { 'Z' } else { 'X' };
        }

        let mut score = match your_move {
            'X' => 1,
            'Y' => 2,
            'Z' => 3,
            _ => 0,
        };

        if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X') {
            score += 6;
        } else if (opponent == 'A' && your_move == 'X') || (opponent == 'B' && your_move == 'Y') || (opponent == 'C' && your_move == 'Z') {
            score += 3;
        }

        total_score += score;
    }

    println!("{}", total_score);
}