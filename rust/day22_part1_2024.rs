
use std::fs::File;
use std::io::{self, BufRead};

fn process_secret(mut secret: u64) -> u64 {
    // Step 1
    let step1 = secret * 64;
    secret ^= step1;
    secret %= 16777216;

    // Step 2
    let step2 = secret / 32;
    secret ^= step2;
    secret %= 16777216;

    // Step 3
    let step3 = secret * 2048;
    secret ^= step3;
    secret %= 16777216;

    secret
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut total_sum = 0;

    for line in reader.lines() {
        let line = line?;
        let initial_secret: u64 = line.trim().parse().expect("Invalid input line");

        let mut current_secret = initial_secret;
        for _ in 0..2000 {
            current_secret = process_secret(current_secret);
        }
        total_sum += current_secret;
    }

    println!("{}", total_sum);

    Ok(())
}
