
use std::fs::File;
use std::io::{BufRead, BufReader};

fn filter_values(values: Vec<String>, criteria: fn(zeros: i32, ones: i32) -> char) -> String {
    let mut values = values;
    for i in 0..values[0].len() {
        let (mut zeros, mut ones) = (0, 0);
        for val in &values {
            if val.chars().nth(i).unwrap() == '0' {
                zeros += 1;
            } else {
                ones += 1;
            }
        }
        let keep = criteria(zeros, ones);
        values = filter_by_bit(&values, i, keep);
        if values.len() == 1 {
            break;
        }
    }
    values[0].to_string()
}

fn filter_by_bit(values: &Vec<String>, bit_index: usize, keep: char) -> Vec<String> {
    values
        .iter()
        .filter(|&val| val.chars().nth(bit_index).unwrap() == keep)
        .map(|val| val.to_string())
        .collect()
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let values: Vec<String> = reader.lines().map(|line| line.unwrap()).collect();

    let oxygen_generator_rating = filter_values(values.clone(), |zeros, ones| {
        if zeros > ones {
            '0'
        } else {
            '1'
        }
    });
    let oxygen_generator_rating_int = i64::from_str_radix(&oxygen_generator_rating, 2).unwrap();

    let co2_scrubber_rating = filter_values(values, |zeros, ones| {
        if zeros <= ones {
            '0'
        } else {
            '1'
        }
    });
    let co2_scrubber_rating_int = i64::from_str_radix(&co2_scrubber_rating, 2).unwrap();

    println!("{}", oxygen_generator_rating_int * co2_scrubber_rating_int);
}
