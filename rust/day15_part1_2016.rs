
use std::fs::File;
use std::io::{self, BufRead};

struct Disc {
    total_positions: i32,
    start_position: i32,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut discs = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<_> = line.split_whitespace().collect();
        let total_positions = parts[3].parse::<i32>().unwrap();
        let start_position = parts[11].trim_end_matches('.').parse::<i32>().unwrap();
        discs.push(Disc {
            total_positions,
            start_position,
        });
    }

    let mut time = 0;
    loop {
        let mut all_zero = true;
        for (i, disc) in discs.iter().enumerate() {
            if (disc.start_position + time + i as i32 + 1) % disc.total_positions != 0 {
                all_zero = false;
                break;
            }
        }
        if all_zero {
            println!("{}", time);
            break;
        }
        time += 1;
    }
    Ok(())
}
