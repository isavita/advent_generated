
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let public_keys: Vec<u64> = input.lines().map(|x| x.parse().unwrap()).collect();

    let subject_number = 7;
    let mut value = 1;
    let mut loop_size = 0;
    while value != public_keys[0] {
        value *= subject_number;
        value %= 20201227;
        loop_size += 1;
    }

    let mut encryption_key = 1;
    for _ in 0..loop_size {
        encryption_key *= public_keys[1];
        encryption_key %= 20201227;
    }

    println!("{}", encryption_key);
}
