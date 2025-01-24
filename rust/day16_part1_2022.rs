
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct Valve {
    flow_rate: i32,
    tunnels: Vec<String>,
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);

    let mut valves: HashMap<String, Valve> = HashMap::new();
    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let parts: Vec<&str> = line.split(';').collect();
        let valve_part: Vec<&str> = parts[0].split_whitespace().collect();
        let name = valve_part[1].to_string();
        let flow_rate = valve_part[4]
            .split('=')
            .nth(1)
            .unwrap()
            .parse::<i32>()
            .unwrap();

        let tunnels_part: Vec<&str> = if parts[1].contains("valves") {
            parts[1]
                .trim()
                .split("valves ")
                .nth(1)
                .unwrap()
                .split(", ")
                .collect()
        } else {
            parts[1]
                .trim()
                .split("valve ")
                .nth(1)
                .unwrap()
                .split(", ")
                .collect()
        };

        let tunnels = tunnels_part.iter().map(|&s| s.to_string()).collect();
        valves.insert(name, Valve { flow_rate, tunnels });
    }

    let distances = calculate_distances(&valves);
    let max_pressure = find_max_pressure(&valves, &distances, 30);
    println!("{}", max_pressure);
}

fn calculate_distances(valves: &HashMap<String, Valve>) -> HashMap<(String, String), i32> {
    let mut distances: HashMap<(String, String), i32> = HashMap::new();
    for start in valves.keys() {
        for end in valves.keys() {
            if start == end {
                distances.insert((start.clone(), end.clone()), 0);
            } else {
                let distance = bfs(valves, start, end);
                distances.insert((start.clone(), end.clone()), distance);
            }
        }
    }
    distances
}

fn bfs(valves: &HashMap<String, Valve>, start: &str, end: &str) -> i32 {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back((start.to_string(), 0));
    visited.insert(start.to_string());

    while let Some((current, distance)) = queue.pop_front() {
        if current == end {
            return distance;
        }
        if let Some(valve) = valves.get(&current) {
            for neighbor in &valve.tunnels {
                if !visited.contains(neighbor) {
                    queue.push_back((neighbor.clone(), distance + 1));
                    visited.insert(neighbor.clone());
                }
            }
        }
    }
    i32::MAX
}

fn find_max_pressure(
    valves: &HashMap<String, Valve>,
    distances: &HashMap<(String, String), i32>,
    time_limit: i32,
) -> i32 {
    let mut max_pressure = 0;
    let start_valve = "AA".to_string();
    let non_zero_valves: Vec<String> = valves
        .iter()
        .filter(|(_, v)| v.flow_rate > 0)
        .map(|(k, _)| k.clone())
        .collect();

    fn dfs(
        valves: &HashMap<String, Valve>,
        distances: &HashMap<(String, String), i32>,
        current_valve: &str,
        remaining_time: i32,
        opened_valves: &mut HashSet<String>,
        current_pressure: i32,
        max_pressure: &mut i32,
        non_zero_valves: &[String],
    ) {
        *max_pressure = (*max_pressure).max(current_pressure);

        for next_valve in non_zero_valves {
            if !opened_valves.contains(next_valve) {
                let travel_time = distances
                    .get(&(current_valve.to_string(), next_valve.clone()))
                    .unwrap();
                let time_after_open = remaining_time - travel_time - 1;
                if time_after_open >= 0 {
                    opened_valves.insert(next_valve.clone());
                    let valve_flow = valves.get(next_valve).unwrap().flow_rate;
                    dfs(
                        valves,
                        distances,
                        next_valve,
                        time_after_open,
                        opened_valves,
                        current_pressure + valve_flow * time_after_open,
                        max_pressure,
                        non_zero_valves,
                    );
                    opened_valves.remove(next_valve);
                }
            }
        }
    }

    let mut opened_valves = HashSet::new();
    dfs(
        valves,
        distances,
        &start_valve,
        time_limit,
        &mut opened_valves,
        0,
        &mut max_pressure,
        &non_zero_valves,
    );
    max_pressure
}
