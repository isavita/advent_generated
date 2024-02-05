use std::fs;

fn calculate_ways_to_win(time: i32, record: i32) -> i32 {
    let mut ways_to_win = 0;
    for hold_time in 1..time {
        let travel_time = time - hold_time;
        let distance = hold_time * travel_time;
        if distance > record {
            ways_to_win += 1;
        }
    }
    ways_to_win
}

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Error reading file");

    let mut times: Vec<i32> = Vec::new();
    let mut distances: Vec<i32> = Vec::new();

    let lines: Vec<&str> = contents.trim().split("\n").collect();
    for (i, line) in lines.iter().enumerate() {
        let values: Vec<i32> = line.split(" ")
            .filter_map(|x| x.parse().ok())
            .collect();
        if i == 0 {
            times = values;
        } else {
            distances = values;
        }
    }

    let mut total_ways = 1;
    for i in 0..times.len() {
        let ways = calculate_ways_to_win(times[i], distances[i]);
        total_ways *= ways;
    }

    println!("{}", total_ways);
}