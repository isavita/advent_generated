
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct RangeMap {
    src_start: i64,
    dest_start: i64,
    length: i64,
}

fn reverse_convert_number(number: i64, ranges: &[RangeMap]) -> i64 {
    for r in ranges.iter().rev() {
        if number >= r.dest_start && number < r.dest_start + r.length {
            return r.src_start + (number - r.dest_start);
        }
    }
    number
}

fn is_in_seed_ranges(number: i64, ranges: &[[i64; 2]]) -> bool {
    for r in ranges {
        if number >= r[0] && number < r[0] + r[1] {
            return true;
        }
    }
    false
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut seed_ranges: Vec<[i64; 2]> = Vec::new();
    let mut current_ranges: Vec<RangeMap> = Vec::new();
    let mut maps: Vec<Vec<RangeMap>> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if line.contains("map:") {
            if !current_ranges.is_empty() {
                maps.push(current_ranges);
                current_ranges = Vec::new();
            }
        } else if line.starts_with("seeds:") {
            let parts: Vec<&str> = line[7..].split_whitespace().collect();
            for i in (0..parts.len()).step_by(2) {
                let start = parts[i].parse::<i64>().unwrap();
                let length = parts[i + 1].parse::<i64>().unwrap();
                seed_ranges.push([start, length]);
            }
        } else {
            let numbers: Vec<&str> = line.split_whitespace().collect();
            if numbers.len() == 3 {
                let src_start = numbers[1].parse::<i64>().unwrap();
                let dest_start = numbers[0].parse::<i64>().unwrap();
                let length = numbers[2].parse::<i64>().unwrap();
                current_ranges.push(RangeMap {
                    src_start,
                    dest_start,
                    length,
                });
            }
        }
    }
    if !current_ranges.is_empty() {
        maps.push(current_ranges);
    }

    let mut location: i64 = 0;
    loop {
        let mut seed = location;
        for map in maps.iter().rev() {
            seed = reverse_convert_number(seed, map);
        }
        if is_in_seed_ranges(seed, &seed_ranges) {
            println!("{}", location);
            break;
        }
        location += 1;
    }
    Ok(())
}
