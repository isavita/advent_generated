
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct LogEntry {
    year: u32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    message: String,
}

fn parse_log_entry(line: &str) -> LogEntry {
    let parts: Vec<&str> = line.split(|c| c == '[' || c == ']' || c == ' ').collect();
    let date_parts: Vec<&str> = parts[1].split('-').collect();
    let time_parts: Vec<&str> = parts[2].split(':').collect();
    let message = parts[4..].join(" ");

    LogEntry {
        year: date_parts[0].parse().unwrap(),
        month: date_parts[1].parse().unwrap(),
        day: date_parts[2].parse().unwrap(),
        hour: time_parts[0].parse().unwrap(),
        minute: time_parts[1].parse().unwrap(),
        message,
    }
}

fn main() -> std::io::Result<()> {
    // Read input from file
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut log_entries: Vec<LogEntry> = Vec::new();
    for line in reader.lines() {
        log_entries.push(parse_log_entry(&line?));
    }

    // Sort log entries chronologically
    log_entries.sort();

    // Process log entries
    let mut guard_sleep_minutes: HashMap<u32, [u32; 60]> = HashMap::new();
    let mut current_guard_id: Option<u32> = None;
    let mut sleep_start_minute: Option<u32> = None;

    for entry in log_entries {
        if entry.message.starts_with("Guard") {
            let parts: Vec<&str> = entry.message.split_whitespace().collect();
            current_guard_id = Some(parts[1][1..].parse().unwrap()); // Extract ID
        } else if entry.message == "falls asleep" {
            sleep_start_minute = Some(entry.minute);
        } else if entry.message == "wakes up" {
            if let (Some(guard_id), Some(start_minute)) = (current_guard_id, sleep_start_minute) {
                let sleep_minutes = guard_sleep_minutes.entry(guard_id).or_insert([0; 60]);
                for minute in start_minute..entry.minute {
                    sleep_minutes[minute as usize] += 1;
                }
            }
            sleep_start_minute = None; // Reset sleep start minute
        }
    }

    // Find the guard with the most minutes asleep
    let mut max_sleep_guard: Option<u32> = None;
    let mut max_sleep_minutes: u32 = 0;

    for (&guard_id, sleep_minutes) in &guard_sleep_minutes {
        let total_sleep_minutes: u32 = sleep_minutes.iter().sum();
        if total_sleep_minutes > max_sleep_minutes {
            max_sleep_minutes = total_sleep_minutes;
            max_sleep_guard = Some(guard_id);
        }
    }
   
    // Find the minute the guard spends asleep the most
    let mut max_minute: Option<usize> = None;
    let mut max_minute_count = 0;

     if let Some(guard_id) = max_sleep_guard {
        let sleep_minutes = &guard_sleep_minutes[&guard_id];

        for (i, &count) in sleep_minutes.iter().enumerate() {
            if count > max_minute_count {
                max_minute_count = count;
                max_minute = Some(i);
            }
        }

        // Calculate and print the result
        if let Some(minute) = max_minute {
            println!("{}", guard_id * (minute as u32));
        }
    }
    Ok(())
}
