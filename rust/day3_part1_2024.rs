
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut total_sum = 0;

    for line in reader.lines() {
        let line = line?;
        let mut chars = line.chars().collect::<Vec<_>>();

        let mut i = 0;
        while i < chars.len() {
            if chars[i] == 'm' && 
               i + 2 < chars.len() &&
               chars[i+1] == 'u' &&
               chars[i+2] == 'l' {

                let mut start = i + 4;
                let mut num1_str = String::new();
                let mut num2_str = String::new();

                while start < chars.len() && chars[start].is_ascii_digit() {
                   num1_str.push(chars[start]);
                   start += 1;
                }

                if start < chars.len() && chars[start] == ',' {
                    start += 1;
                    while start < chars.len() && chars[start].is_ascii_digit() {
                        num2_str.push(chars[start]);
                        start += 1;
                    }
                    if start < chars.len() && chars[start] == ')' {
                        if !num1_str.is_empty() && !num2_str.is_empty() {
                            if let (Ok(num1), Ok(num2)) = (num1_str.parse::<i32>(), num2_str.parse::<i32>()) {
                                total_sum += num1 * num2;
                            }
                        }
                    }
                }
                i = start;
            } else {
                i += 1;
            }
        }
    }
    println!("{}", total_sum);
    Ok(())
}
