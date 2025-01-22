
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let lines = read_lines("input.txt")?;
    let mut lines_iter = lines.iter();

    let numbers: Vec<u32> = lines_iter
        .next()
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    let mut boards: Vec<Vec<Vec<(u32, bool)>>> = Vec::new();
    let mut current_board: Vec<Vec<(u32, bool)>> = Vec::new();

    for line in lines_iter {
        if line.is_empty() {
            if !current_board.is_empty() {
                boards.push(current_board);
                current_board = Vec::new();
            }
        } else {
            let row: Vec<(u32, bool)> = line
                .split_whitespace()
                .map(|s| (s.parse().unwrap(), false))
                .collect();
            current_board.push(row);
        }
    }
    if !current_board.is_empty() {
        boards.push(current_board);
    }

    for number in numbers {
        for board in &mut boards {
            for row in board.iter_mut() {
                for (val, marked) in row.iter_mut() {
                    if *val == number {
                        *marked = true;
                    }
                }
            }

            if check_win(board) {
                let unmarked_sum: u32 = board
                    .iter()
                    .flatten()
                    .filter(|(_, marked)| !marked)
                    .map(|(val, _)| val)
                    .sum();
                println!("{}", unmarked_sum * number);
                return Ok(());
            }
        }
    }
    Ok(())
}


fn check_win(board: &Vec<Vec<(u32, bool)>>) -> bool {
    let board_size = board.len();
    // check rows
    for row in board {
        if row.iter().all(|(_, marked)| *marked) {
            return true;
        }
    }
    // check columns
    for col_index in 0..board_size {
        if (0..board_size).all(|row_index| board[row_index][col_index].1) {
             return true
        }
    }
    false
}

fn read_lines<P>(filename: P) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    reader.lines().collect()
}
