use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut checksum = 0;
    let mut evenly_divisible_sum = 0;

    for line in contents.lines() {
        let numbers: Vec<i32> = line.split_whitespace()
            .map(|x| x.parse().unwrap())
            .collect();

        let max = numbers.iter().max().unwrap();
        let min = numbers.iter().min().unwrap();
        checksum += max - min;

        for i in 0..numbers.len() {
            for j in i+1..numbers.len() {
                if numbers[i] % numbers[j] == 0 {
                    evenly_divisible_sum += numbers[i] / numbers[j];
                } else if numbers[j] % numbers[i] == 0 {
                    evenly_divisible_sum += numbers[j] / numbers[i];
                }
            }
        }
    }

    println!("{}", checksum);
    println!("{}", evenly_divisible_sum);
}