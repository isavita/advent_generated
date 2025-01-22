
use std::fs::File;
use std::io::{self, BufRead};

fn snafu_to_decimal(snafu: &str) -> i64 {
    let mut decimal = 0;
    let mut power = 1;
    for c in snafu.chars().rev() {
        let digit = match c {
            '2' => 2,
            '1' => 1,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            _ => panic!("Invalid SNAFU digit: {}", c),
        };
        decimal += digit * power;
        power *= 5;
    }
    decimal
}

fn decimal_to_snafu(mut decimal: i64) -> String {
    if decimal == 0 {
        return "0".to_string();
    }
    let mut snafu = String::new();
    while decimal != 0 {
        let remainder = decimal % 5;
        match remainder {
            0 => snafu.push('0'),
            1 => snafu.push('1'),
            2 => snafu.push('2'),
            3 => {
                snafu.push('=');
                decimal += 5;
            }
            4 => {
                snafu.push('-');
                decimal += 5;
            }
            _ => panic!("Invalid remainder"),
        }
        decimal /= 5;
    }
    snafu.chars().rev().collect()
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut total_decimal = 0;
    for line in reader.lines() {
        let line = line?;
        let decimal = snafu_to_decimal(&line);
        total_decimal += decimal;
    }
    let result_snafu = decimal_to_snafu(total_decimal);
    println!("{}", result_snafu);

    Ok(())
}
