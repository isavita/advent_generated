
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut sum = 0;
    let mut mul_enabled = true;

    for line in reader.lines() {
        let line = line?;
        let mut chars = line.chars().collect::<Vec<_>>();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == 'm' && i + 3 < chars.len() {
                if chars[i + 1] == 'u' && chars[i + 2] == 'l' && chars[i+3] == '(' {
                        let mut num1_str = String::new();
                        let mut j = i + 4;
                        while j < chars.len() && chars[j].is_digit(10) {
                            num1_str.push(chars[j]);
                            j += 1;
                        }

                        if j < chars.len() && chars[j] == ',' {
                            let mut num2_str = String::new();
                            j += 1;
                            while j < chars.len() && chars[j].is_digit(10){
                                num2_str.push(chars[j]);
                                j += 1;
                            }
                            
                            if j < chars.len() && chars[j] == ')' {
                                if let (Ok(num1), Ok(num2)) = (num1_str.parse::<i32>(), num2_str.parse::<i32>()) {
                                    if mul_enabled {
                                        sum += num1 * num2;
                                    }
                                    i = j+1;
                                    continue;
                                }

                            }
                        }
                }

             }
            if chars[i] == 'd' && i + 3 < chars.len() {
                if chars[i + 1] == 'o' && chars[i + 2] == '(' && chars[i + 3] == ')' {
                    mul_enabled = true;
                    i +=4;
                    continue;
                }

                if chars[i + 1] == 'o' && chars[i + 2] == 'n' && chars[i + 3] == '\'' && i + 6 < chars.len() {
                    if chars[i+4] == 't' && chars[i + 5] == '(' && chars[i + 6] == ')'{
                         mul_enabled = false;
                        i+=7;
                        continue;
                    }
                  
                }
            }
                
            
            i += 1;
        }
    }

    println!("{}", sum);
    Ok(())
}
