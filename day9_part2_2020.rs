
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let numbers: Vec<i64> = input.lines().map(|x| x.parse().unwrap()).collect();

    let invalid_number = find_invalid_number(&numbers, 25);
    println!("{}", invalid_number);

    let weakness = find_weakness(&numbers, invalid_number);
    println!("{}", weakness);
}

fn find_invalid_number(numbers: &Vec<i64>, preamble_length: usize) -> i64 {
    for i in preamble_length..numbers.len() {
        let mut found = false;
        for j in i - preamble_length..i {
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
            return numbers[i];
        }
    }
    return -1;
}

fn find_weakness(numbers: &Vec<i64>, target: i64) -> i64 {
    for i in 0..numbers.len() {
        let mut sum = numbers[i];
        let mut j = i + 1;
        while sum < target {
            sum += numbers[j];
            j += 1;
        }
        if sum == target {
            let range = &numbers[i..j];
            return range.iter().min().unwrap() + range.iter().max().unwrap();
        }
    }
    return -1;
}
