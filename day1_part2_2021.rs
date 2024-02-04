
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let measurements: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();

    let mut larger_than_previous = 0;
    let mut previous_sum = 0;

    for i in 0..measurements.len() - 2 {
        let sum = measurements[i] + measurements[i + 1] + measurements[i + 2];
        if i == 0 {
            previous_sum = sum;
        } else {
            if sum > previous_sum {
                larger_than_previous += 1;
            }
            previous_sum = sum;
        }
    }

    println!("{}", larger_than_previous);
}
