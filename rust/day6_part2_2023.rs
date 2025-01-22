
use std::fs;

fn calculate_distance(hold_time: i64, race_time: i64) -> i64 {
    let speed = hold_time;
    let travel_time = race_time - hold_time;
    speed * travel_time
}

fn count_winning_ways(race_time: i64, record_distance: i64) -> i64 {
    let mut count = 0;
    for hold_time in 0..=race_time {
        let distance = calculate_distance(hold_time, race_time);
        if distance > record_distance {
            count += 1;
        }
    }
    count
}

fn parse_input_part1(input: &str) -> Vec<(i64, i64)> {
    let lines: Vec<&str> = input.lines().collect();
    let times: Vec<i64> = lines[0]
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse().unwrap())
        .collect();
    let distances: Vec<i64> = lines[1]
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse().unwrap())
        .collect();
    times.into_iter().zip(distances).collect()
}

fn parse_input_part2(input: &str) -> (i64, i64) {
    let lines: Vec<&str> = input.lines().collect();
    let time: i64 = lines[0]
        .split_whitespace()
        .skip(1)
        .collect::<String>()
        .parse()
        .unwrap();
    let distance: i64 = lines[1]
        .split_whitespace()
        .skip(1)
        .collect::<String>()
        .parse()
        .unwrap();
    (time, distance)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");

    // Part 1
    let races_part1 = parse_input_part1(&input);
    let result_part1: i64 = races_part1
        .iter()
        .map(|&(time, distance)| count_winning_ways(time, distance))
        .product();
    println!("Part 1 Result: {}", result_part1);

    // Part 2
    let (time_part2, distance_part2) = parse_input_part2(&input);
    let result_part2 = count_winning_ways(time_part2, distance_part2);
    println!("Part 2 Result: {}", result_part2);
}
