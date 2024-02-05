
use std::fs;

fn parse_input(input: Vec<String>) -> Vec<Vec<i32>> {
    let mut histories = Vec::new();
    for line in input {
        let numbers = parse_string_to_ints(line);
        histories.push(numbers);
    }
    histories
}

fn parse_string_to_ints(numbers_line: String) -> Vec<i32> {
    let mut numbers = Vec::new();
    let numbers_parts: Vec<&str> = numbers_line.split(" ").collect();
    for number_str in numbers_parts {
        let number: i32 = number_str.parse().unwrap();
        numbers.push(number);
    }
    numbers
}

fn all_zeros(nums: &Vec<i32>) -> bool {
    nums.iter().all(|&num| num == 0)
}

fn calculate_extrapolation(history: &Vec<i32>) -> Vec<i32> {
    let mut extrapolations = Vec::new();
    for i in 1..history.len() {
        let extrapolation = history[i] - history[i - 1];
        extrapolations.push(extrapolation);
    }
    extrapolations
}

fn calculate_extrapolations(history: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut extrapolations_series = Vec::new();
    extrapolations_series.push(history.clone());

    for i in 1..history.len() {
        let previous_extrapolations = &extrapolations_series[i - 1];
        if all_zeros(previous_extrapolations) {
            return extrapolations_series;
        }

        let extrapolations = calculate_extrapolation(previous_extrapolations);
        extrapolations_series.push(extrapolations);
    }

    extrapolations_series
}

fn solve(input: Vec<String>) -> i32 {
    let histories = parse_input(input);
    let mut res = 0;

    for history in histories {
        let extrapolations_series = calculate_extrapolations(&history);

        let mut past_prediction = 0;
        for i in (0..extrapolations_series.len()).rev() {
            past_prediction = extrapolations_series[i][0] - past_prediction;
        }

        res += past_prediction;
    }

    res
}

fn read_file(file_name: &str) -> Vec<String> {
    let contents = fs::read_to_string(file_name).unwrap();
    contents.trim().split("\n").map(|s| s.to_string()).collect()
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(input));
}
