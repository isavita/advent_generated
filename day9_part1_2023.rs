
use std::fs;

fn parse_input(input: Vec<String>) -> Vec<Vec<i32>> {
    input.iter().map(|line| parse_string_to_ints(line)).collect()
}

fn parse_string_to_ints(numbers_line: &str) -> Vec<i32> {
    numbers_line.split_whitespace().map(|number_str| number_str.parse().unwrap()).collect()
}

fn all_zeros(nums: &Vec<i32>) -> bool {
    nums.iter().all(|&num| num == 0)
}

fn calculate_extrapolation(history: &Vec<i32>) -> Vec<i32> {
    history.windows(2).map(|pair| pair[1] - pair[0]).collect()
}

fn calculate_extrapolations(history: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut extrapolations_series = vec![history.clone()];

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

        let mut future_prediction = 0;
        for i in (0..extrapolations_series.len()).rev() {
            future_prediction = extrapolations_series[i][extrapolations_series[i].len() - 1] + future_prediction;
        }

        res += future_prediction;
    }

    res
}

fn read_file(file_name: &str) -> Vec<String> {
    fs::read_to_string(file_name).unwrap().lines().map(|line| line.to_string()).collect()
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(input));
}
