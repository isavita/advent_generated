
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    let mut code = String::new();
    let mut current = (1, 1);

    for line in input.lines() {
        for c in line.chars() {
            match c {
                'U' => if current.0 > 0 { current.0 -= 1 },
                'D' => if current.0 < 2 { current.0 += 1 },
                'L' => if current.1 > 0 { current.1 -= 1 },
                'R' => if current.1 < 2 { current.1 += 1 },
                _ => (),
            }
        }
        code.push_str(&keypad[current.0 as usize][current.1 as usize].to_string());
    }

    println!("{}", code);
}
