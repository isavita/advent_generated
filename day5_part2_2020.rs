
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut seat_ids: Vec<u32> = input
        .lines()
        .map(|line| {
            let mut row_range = (0, 127);
            let mut col_range = (0, 7);
            for c in line.chars() {
                match c {
                    'F' => row_range.1 = (row_range.0 + row_range.1) / 2,
                    'B' => row_range.0 = (row_range.0 + row_range.1 + 1) / 2,
                    'L' => col_range.1 = (col_range.0 + col_range.1) / 2,
                    'R' => col_range.0 = (col_range.0 + col_range.1 + 1) / 2,
                    _ => {}
                }
            }
            row_range.0 * 8 + col_range.0
        })
        .collect();

    seat_ids.sort();

    let mut my_seat_id = 0;
    for i in 1..seat_ids.len() - 1 {
        if seat_ids[i] - seat_ids[i - 1] == 2 {
            my_seat_id = seat_ids[i] - 1;
            break;
        }
    }

    println!("{}", seat_ids.last().unwrap());
    println!("{}", my_seat_id);
}
