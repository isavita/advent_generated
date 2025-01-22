
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
struct MapRange {
    dest_start: u64,
    source_start: u64,
    length: u64,
}

fn parse_almanac(lines: &Vec<String>) -> (Vec<u64>, Vec<Vec<MapRange>>) {
    let mut seeds: Vec<u64> = Vec::new();
    let mut maps: Vec<Vec<MapRange>> = Vec::new();
    let mut current_map: Vec<MapRange> = Vec::new();
    let mut parsing_seeds = true;
    let mut parsing_map = false;

    for line in lines {
        if parsing_seeds {
            if line.starts_with("seeds: ") {
                seeds = line[7..]
                    .split_whitespace()
                    .map(|s| s.parse::<u64>().unwrap())
                    .collect();
                parsing_seeds = false;
            }
        } else if line.contains("map:") {
            if parsing_map {
                maps.push(current_map);
            }
            current_map = Vec::new();
            parsing_map = true;
        } else if parsing_map && !line.is_empty() {
            let parts: Vec<u64> = line
                .split_whitespace()
                .map(|s| s.parse::<u64>().unwrap())
                .collect();
            current_map.push(MapRange {
                dest_start: parts[0],
                source_start: parts[1],
                length: parts[2],
            });
        }
    }
    if parsing_map {
        maps.push(current_map);
    }
    (seeds, maps)
}

fn map_value(value: u64, ranges: &Vec<MapRange>) -> u64 {
    for range in ranges {
        if value >= range.source_start && value < range.source_start + range.length {
            return range.dest_start + (value - range.source_start);
        }
    }
    value
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let lines = read_lines(path)?;

    let (seeds, maps) = parse_almanac(&lines);

    let mut min_location = u64::MAX;
    for seed in seeds {
        let mut current_value = seed;
        for map in &maps {
            current_value = map_value(current_value, map);
        }
        min_location = min_location.min(current_value);
    }
    println!("{}", min_location);
    Ok(())
}

fn read_lines<P>(filename: P) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file)
        .lines()
        .filter_map(Result::ok)
        .collect())
}
