
use std::fs::File;
use std::io::{BufRead, BufReader};

const SIZE: i128 = 119315717514047;
const ITER: i128 = 101741582076661;

fn mod_pow(mut base: i128, mut exp: i128, modulus: i128) -> i128 {
    let mut result = 1;
    base %= modulus;
    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % modulus;
        }
        exp >>= 1;
        base = (base * base) % modulus;
    }
    result
}

fn mod_inv(a: i128, modulus: i128) -> i128 {
    mod_pow(a, modulus - 2, modulus)
}

fn main() -> std::io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut offset = 0;
    let mut increment = 1;

    for line in reader.lines() {
        let line = line?;
        if line == "deal into new stack" {
            increment = (increment * -1 + SIZE) % SIZE;
            offset = (offset + increment + SIZE) % SIZE;
        } else if line.starts_with("cut") {
            let n: i128 = line.split(" ").last().unwrap().parse().unwrap();
            offset = (offset + n * increment + SIZE) % SIZE;
        } else if line.starts_with("deal with increment") {
            let n: i128 = line.split(" ").last().unwrap().parse().unwrap();
            let inv = mod_inv(n, SIZE);
            increment = (increment * inv) % SIZE;
        }
    }

    let final_increment = mod_pow(increment, ITER, SIZE);
    let final_offset = (((mod_pow(increment, ITER, SIZE) - 1 + SIZE) % SIZE) * mod_inv((increment - 1 + SIZE) % SIZE, SIZE) % SIZE * offset) % SIZE;    
    let answer = (2020 * final_increment + final_offset) % SIZE;
    
    println!("{}", answer);

    Ok(())
}
