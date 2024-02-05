use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Error reading file");

    let mut sum = 0;

    for line in contents.lines() {
        if line.is_empty() {
            continue;
        }
        
        let mut first_digit = -1;
        let mut last_digit = -1;

        for c in line.chars() {
            if c.is_digit(10) {
                if first_digit == -1 {
                    first_digit = c.to_digit(10).unwrap() as i32;
                }
                last_digit = c.to_digit(10).unwrap() as i32;
            }
        }

        if first_digit != -1 && last_digit != -1 {
            let value = format!("{}{}", first_digit, last_digit)
                .parse::<i32>().unwrap();
            sum += value;
        }
    }

    println!("{}", sum);
}