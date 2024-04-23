use std::fs::File;
use std::io::{self, BufRead};
use std::time::Instant;

struct Disc {
    positions: i32,
    initial_position: i32,
}

fn main() -> io::Result<()> {
    let start = Instant::now();
    let file = File::open("input.txt")?;
    let lines = io::BufReader::new(file).lines();

    let mut discs = Vec::new();
    for line in lines {
        let line = line?;
        let parts: Vec<_> = line.split_whitespace().collect();
        let positions = parts[3].parse().unwrap();
        let initial_position = parts[11].trim_end_matches('.').parse().unwrap();
        discs.push(Disc {
            positions,
            initial_position,
        });
    }

    let mut time = 0;
    loop {
        let mut aligned = true;
        for (i, disc) in discs.iter().enumerate() {
            let position = (disc.initial_position + time + i as i32 + 1) % disc.positions;
            if position != 0 {
                aligned = false;
                break;
            }
        }
        if aligned {
            println!("Part 1: {}", time);
            break;
        }
        time += 1;
    }

    discs.push(Disc {
        positions: 11,
        initial_position: 0,
    });

    time = 0;
    loop {
        let mut aligned = true;
        for (i, disc) in discs.iter().enumerate() {
            let position = (disc.initial_position + time + i as i32 + 1) % disc.positions;
            if position != 0 {
                aligned = false;
                break;
            }
        }
        if aligned {
            println!("Part 2: {}", time);
            break;
        }
        time += 1;
    }

    let duration = start.elapsed();
    println!("Time: {:.2?}", duration);
    Ok(())
}