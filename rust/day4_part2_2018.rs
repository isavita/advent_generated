
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
enum EventType {
    BeginsShift(u32),
    FallsAsleep,
    WakesUp,
}

#[derive(Debug)]
struct Event {
    year: u32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    event_type: EventType,
}

fn parse_event(line: &str) -> Event {
    let parts: Vec<&str> = line.split(|c| c == '[' || c == ']').collect();
    let date_time: Vec<&str> = parts[1].split(' ').collect();
    let date: Vec<&str> = date_time[0].split('-').collect();
    let time: Vec<&str> = date_time[1].split(':').collect();
    let event_str = parts[2].trim();

    let year = date[0].parse().unwrap();
    let month = date[1].parse().unwrap();
    let day = date[2].parse().unwrap();
    let hour = time[0].parse().unwrap();
    let minute = time[1].parse().unwrap();

    let event_type = if event_str.starts_with("Guard") {
        let guard_id: String = event_str
            .split_whitespace()
            .nth(1)
            .unwrap()
            .chars()
            .skip(1)
            .collect();
        EventType::BeginsShift(guard_id.parse().unwrap())
    } else if event_str == "falls asleep" {
        EventType::FallsAsleep
    } else {
        EventType::WakesUp
    };

    Event {
        year,
        month,
        day,
        hour,
        minute,
        event_type,
    }
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);

    let mut events: Vec<Event> = reader
        .lines()
        .map(|line| parse_event(&line.unwrap()))
        .collect();

    events.sort_by(|a, b| {
        (a.year, a.month, a.day, a.hour, a.minute)
            .cmp(&(b.year, b.month, b.day, b.hour, b.minute))
    });

    let mut guard_sleep_times: HashMap<u32, Vec<u32>> = HashMap::new();
    let mut current_guard_id = 0;
    let mut sleep_start_minute = 0;

    for event in events {
        match event.event_type {
            EventType::BeginsShift(id) => current_guard_id = id,
            EventType::FallsAsleep => sleep_start_minute = event.minute,
            EventType::WakesUp => {
                let sleep_end_minute = event.minute;
                let sleep_times = guard_sleep_times.entry(current_guard_id).or_insert(vec![0; 60]);
                for minute in sleep_start_minute..sleep_end_minute {
                    sleep_times[minute as usize] += 1;
                }
            }
        }
    }

    // Strategy 1
    let (guard_id_most_sleep, sleep_minutes) = guard_sleep_times
        .iter()
        .max_by_key(|(_, minutes)| minutes.iter().sum::<u32>())
        .unwrap();

    let most_sleep_minute = sleep_minutes
        .iter()
        .enumerate()
        .max_by_key(|(_, &count)| count)
        .unwrap()
        .0;

    println!(
        "Strategy 1: Guard ID * Minute = {}",
        guard_id_most_sleep * (most_sleep_minute as u32)
    );

    // Strategy 2
    let (guard_id_most_frequent, minute_most_frequent) = guard_sleep_times
        .iter()
        .map(|(&id, minutes)| {
            let (minute, _) = minutes
                .iter()
                .enumerate()
                .max_by_key(|(_, &count)| count)
                .unwrap();
            (id, minute, minutes[minute])
        })
        .max_by_key(|&(_, _, count)| count)
        .map(|(id, minute, _)| (id, minute))
        .unwrap();

    println!(
        "Strategy 2: Guard ID * Minute = {}",
        guard_id_most_frequent * (minute_most_frequent as u32)
    );
}
