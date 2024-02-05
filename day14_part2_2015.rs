
use std::fs::File;
use std::io::{self, BufRead, BufReader};

struct Reindeer {
    speed: i32,
    fly_time: i32,
    rest_time: i32,
    distance: i32,
    points: i32,
    flying: bool,
    time_in_mode: i32,
}

fn main() {
    match read_reindeer_details("input.txt") {
        Ok(mut reindeers) => {
            simulate_race_with_points(&mut reindeers, 2503);
            let max_points = find_max_points(&reindeers);
            println!("{}", max_points);
        }
        Err(e) => println!("Error reading input: {}", e),
    }
}

fn read_reindeer_details(filename: &str) -> io::Result<Vec<Reindeer>> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut reindeers = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let speed = parts[3].parse::<i32>().unwrap();
        let fly_time = parts[6].parse::<i32>().unwrap();
        let rest_time = parts[13].parse::<i32>().unwrap();

        reindeers.push(Reindeer {
            speed,
            fly_time,
            rest_time,
            distance: 0,
            points: 0,
            flying: true,
            time_in_mode: 0,
        });
    }

    Ok(reindeers)
}

fn simulate_race_with_points(reindeers: &mut [Reindeer], total_seconds: i32) {
    for _ in 0..total_seconds {
        let mut max_distance = 0;
        for reindeer in reindeers.iter_mut() {
            if reindeer.flying {
                reindeer.distance += reindeer.speed;
            }
            reindeer.time_in_mode += 1;
            if (reindeer.flying && reindeer.time_in_mode == reindeer.fly_time)
                || (!reindeer.flying && reindeer.time_in_mode == reindeer.rest_time)
            {
                reindeer.flying = !reindeer.flying;
                reindeer.time_in_mode = 0;
            }
            if reindeer.distance > max_distance {
                max_distance = reindeer.distance;
            }
        }
        for reindeer in reindeers.iter_mut() {
            if reindeer.distance == max_distance {
                reindeer.points += 1;
            }
        }
    }
}

fn find_max_points(reindeers: &[Reindeer]) -> i32 {
    reindeers.iter().map(|r| r.points).max().unwrap_or(0)
}
