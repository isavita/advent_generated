
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut max_seat_id = 0;

    for line in input.lines() {
        let row = line[..7].replace("F", "0").replace("B", "1");
        let col = line[7..].replace("L", "0").replace("R", "1");
        let seat_id = isize::from_str_radix(&row, 2).unwrap() * 8 + isize::from_str_radix(&col, 2).unwrap();
        if seat_id > max_seat_id {
            max_seat_id = seat_id;
        }
    }

    println!("{}", max_seat_id);
}
