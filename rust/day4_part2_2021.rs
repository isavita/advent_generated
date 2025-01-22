
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug, Clone)]
struct Board {
    numbers: Vec<Vec<(i32, bool)>>,
    won: bool,
}

impl Board {
    fn mark_number(&mut self, number: i32) {
        for row in &mut self.numbers {
            for (val, marked) in row {
                if *val == number {
                    *marked = true;
                }
            }
        }
    }

    fn check_win(&self) -> bool {
        // Check rows
        for row in &self.numbers {
            if row.iter().all(|(_, marked)| *marked) {
                return true;
            }
        }

        // Check columns
        for col in 0..5 {
            if (0..5).all(|row| self.numbers[row][col].1) {
                return true;
            }
        }

        false
    }

    fn calculate_score(&self, last_called: i32) -> i32 {
        let unmarked_sum: i32 = self
            .numbers
            .iter()
            .flat_map(|row| row.iter())
            .filter(|(_, marked)| !marked)
            .map(|(val, _)| val)
            .sum();
        unmarked_sum * last_called
    }
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut lines = reader.lines();

    // Parse the called numbers
    let called_numbers: Vec<i32> = lines
        .next()
        .unwrap()?
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    // Parse the boards
    let mut boards: Vec<Board> = Vec::new();
    let mut current_board: Vec<Vec<(i32, bool)>> = Vec::new();
    for line in lines {
        let line = line?;
        if line.is_empty() {
            if !current_board.is_empty() {
                boards.push(Board {
                    numbers: current_board,
                    won: false,
                });
                current_board = Vec::new();
            }
        } else {
            let row: Vec<(i32, bool)> = line
                .split_whitespace()
                .map(|s| (s.parse().unwrap(), false))
                .collect();
            current_board.push(row);
        }
    }
    if !current_board.is_empty() {
        boards.push(Board {
            numbers: current_board,
            won: false,
        });
    }

    // Part 1: Find the first winning board
    let mut first_winning_score = 0;
    let mut first_win_found = false;

    // Part 2: Find the last winning board
    let mut last_winning_score = 0;
    let mut remaining_boards = boards.clone();
    
    for number in called_numbers {
        for board_index in 0..remaining_boards.len(){
            
            remaining_boards[board_index].mark_number(number);
             if remaining_boards[board_index].check_win() && !remaining_boards[board_index].won {
                remaining_boards[board_index].won = true;
                
                 if !first_win_found{
                     first_winning_score = remaining_boards[board_index].calculate_score(number);
                    first_win_found = true;
                 }
                 last_winning_score = remaining_boards[board_index].calculate_score(number);
             }
        }
    }

    println!("Part 1: First winning score is {}", first_winning_score);
    println!("Part 2: Last winning score is {}", last_winning_score);

    Ok(())
}
