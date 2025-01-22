
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::str::FromStr;

#[derive(Debug, Default)]
struct CubeSet {
    red: u32,
    green: u32,
    blue: u32,
}

impl CubeSet {
    fn from_str(s: &str) -> Self {
        let mut set = CubeSet::default();
        for part in s.split(", ") {
            let parts: Vec<&str> = part.split_whitespace().collect();
            if parts.len() != 2 {
                continue;
            }
            let count = u32::from_str(parts[0]).unwrap_or(0);
            match parts[1] {
                "red" => set.red = count,
                "green" => set.green = count,
                "blue" => set.blue = count,
                _ => {}
            }
        }
        set
    }
    fn is_possible(&self, max_red: u32, max_green: u32, max_blue: u32) -> bool {
        self.red <= max_red && self.green <= max_green && self.blue <= max_blue
    }
}

struct Game {
    id: u32,
    sets: Vec<CubeSet>,
}

impl Game {
    fn from_str(s: &str) -> Option<Self> {
        let parts: Vec<&str> = s.split(": ").collect();
        if parts.len() != 2 {
            return None;
        }
        let id = parts[0].split_whitespace().last()?.parse().ok()?;
        let sets = parts[1]
            .split("; ")
            .map(CubeSet::from_str)
            .collect::<Vec<_>>();
        Some(Game { id, sets })
    }

    fn is_possible(&self, max_red: u32, max_green: u32, max_blue: u32) -> bool {
        self.sets
            .iter()
            .all(|set| set.is_possible(max_red, max_green, max_blue))
    }
}


fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    let max_red = 12;
    let max_green = 13;
    let max_blue = 14;
    let mut sum_of_ids = 0;

    for line in reader.lines() {
        let line = line?;
        if let Some(game) = Game::from_str(&line) {
            if game.is_possible(max_red, max_green, max_blue) {
                sum_of_ids += game.id;
            }
        }
    }
    println!("{}", sum_of_ids);
    Ok(())
}
