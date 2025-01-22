
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::cmp::max;

#[derive(Debug)]
struct Nanobot {
    x: i32,
    y: i32,
    z: i32,
    r: i32,
}

fn manhattan_distance(bot1: &Nanobot, bot2: &Nanobot) -> i32 {
    (bot1.x - bot2.x).abs() + (bot1.y - bot2.y).abs() + (bot1.z - bot2.z).abs()
}


fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut nanobots: Vec<Nanobot> = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(", ").collect();
        let pos_part = parts[0];
        let r_part = parts[1];
        let pos_values: Vec<i32> = pos_part[5..pos_part.len() - 1].split(",").map(|s| s.parse().unwrap()).collect();
        let r_value: i32 = r_part[2..].parse().unwrap();

        nanobots.push(Nanobot {x: pos_values[0], y: pos_values[1], z: pos_values[2], r: r_value});
    }

    let mut strongest_bot_index = 0;
    let mut max_radius = 0;
    for (i, bot) in nanobots.iter().enumerate() {
        if bot.r > max_radius {
            max_radius = bot.r;
            strongest_bot_index = i;
        }
    }

    let strongest_bot = &nanobots[strongest_bot_index];

    let mut in_range_count = 0;
    for bot in &nanobots {
        if manhattan_distance(strongest_bot, bot) <= strongest_bot.r {
            in_range_count += 1;
        }
    }


    println!("{}", in_range_count);


    Ok(())
}
