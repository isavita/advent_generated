
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parts: Vec<&str> = input.trim().split_whitespace().collect();
    let row: usize = parts[15].trim_matches(',').parse().unwrap();
    let col: usize = parts[17].trim_matches('.').parse().unwrap();

    let mut code: u64 = 20151125;
    let mut r = 1;
    let mut c = 1;

    loop {
        if r == 1 {
            r = c + 1;
            c = 1;
        } else {
            r -= 1;
            c += 1;
        }

        code = (code * 252533) % 33554393;

        if r == row && c == col {
            println!("{}", code);
            break;
        }
    }
}
