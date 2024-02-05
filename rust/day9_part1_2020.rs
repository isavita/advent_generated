
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let numbers: Vec<i64> = input.lines().map(|line| line.parse().unwrap()).collect();

    let mut invalid_number = 0;

    for i in 25..numbers.len() {
        let mut found = false;
        for j in i - 25..i {
            for k in j + 1..i {
                if numbers[j] + numbers[k] == numbers[i] {
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
        if !found {
            invalid_number = numbers[i];
            break;
        }
    }

    println!("{}", invalid_number);
}
