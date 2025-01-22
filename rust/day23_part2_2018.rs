
use std::{
    fs::File,
    io::{self, BufRead},
    path::Path,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Coordinate {
    x: i32,
    y: i32,
    z: i32,
}

impl Coordinate {
    fn distance(&self, other: &Coordinate) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs() + (self.z - other.z).abs()
    }
}

type Bots = std::collections::HashMap<Coordinate, Vec<i32>>;

fn new_bots(input: &[String]) -> Bots {
    let mut m = Bots::new();

    for data in input {
        let parts: Vec<&str> = data
            .split(|c| c == '<' || c == ',' || c == '>' || c == '=' || c == ' ')
            .filter(|s| !s.is_empty())
            .collect();
        let c = Coordinate {
            x: parts[1].parse().unwrap(),
            y: parts[2].parse().unwrap(),
            z: parts[3].parse().unwrap(),
        };
        let r = parts[5].parse().unwrap();
        m.entry(c).or_default().push(r);
    }

    m
}

fn have_in_range(bots: &Bots, pos: &Coordinate) -> usize {
    bots.iter()
        .flat_map(|(c, rs)| rs.iter().filter(move |&r| pos.distance(c) <= *r))
        .count()
}

fn closest_success(bots: &Bots) -> i32 {
    let mut cur;
    let mut top_left = Coordinate {
        x: 0,
        y: 0,
        z: 0,
    };
    let mut bottom_right = Coordinate {
        x: 0,
        y: 0,
        z: 0,
    };
    let mut zoom = 1 << 30;

    loop {
        let mut zoomed_bots: Bots = Bots::new();
        let mut best = Coordinate {
            x: 0,
            y: 0,
            z: 0,
        };
        let mut best_count = 0;

        for (c, rs) in bots.iter() {
            for r in rs {
                let zc = Coordinate {
                    x: c.x / zoom,
                    y: c.y / zoom,
                    z: c.z / zoom,
                };
                zoomed_bots.entry(zc).or_default().push(r / zoom);
            }
        }

        for x in top_left.x..=bottom_right.x {
            for y in top_left.y..=bottom_right.y {
                for z in top_left.z..=bottom_right.z {
                    cur = Coordinate { x, y, z };
                    let c = have_in_range(&zoomed_bots, &cur);

                    if c < best_count {
                        continue;
                    }
                    let zero = Coordinate { x: 0, y: 0, z: 0 };
                    if c == best_count && zero.distance(&cur) >= zero.distance(&best) {
                        continue;
                    }
                    best = cur;
                    best_count = c;
                }
            }
        }
        
        top_left.x = (best.x - 1) << 1;
        top_left.y = (best.y - 1) << 1;
        top_left.z = (best.z - 1) << 1;
        bottom_right.x = (best.x + 1) << 1;
        bottom_right.y = (best.y + 1) << 1;
        bottom_right.z = (best.z + 1) << 1;

        zoom >>= 1;
        let zero = Coordinate { x: 0, y: 0, z: 0 };
        if zoom == 0 {
            return zero.distance(&best);
        }
    }
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|line| line.expect("Could not read line"))
        .collect();

    let bots = new_bots(&lines);
    println!("{}", closest_success(&bots));

    Ok(())
}
