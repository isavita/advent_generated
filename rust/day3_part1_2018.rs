
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
struct Claim {
    id: i32,
    left: i32,
    top: i32,
    width: i32,
    height: i32,
}

impl Claim {
    fn parse(s: &str) -> Result<Claim, Box<dyn std::error::Error>> {
        let parts: Vec<&str> = s
            .split(|c: char| !c.is_numeric())
            .filter(|s| !s.is_empty())
            .collect();
        let nums: Vec<i32> = parts.iter().map(|&s| s.parse().unwrap()).collect();
        Ok(Claim {
            id: nums[0],
            left: nums[1],
            top: nums[2],
            width: nums[3],
            height: nums[4],
        })
    }
}

fn read_claims<P: AsRef<Path>>(filename: P) -> Result<Vec<Claim>, Box<dyn std::error::Error>> {
    let file = File::open(filename)?;
    let buf_reader = io::BufReader::new(file);
    buf_reader
        .lines()
        .map(|line| Claim::parse(&line?))
        .collect()
}

fn count_overlapping_inches(claims: &[Claim]) -> i32 {
    let mut fabric: HashMap<(i32, i32), i32> = HashMap::new();
    for claim in claims {
        for i in claim.left..claim.left + claim.width {
            for j in claim.top..claim.top + claim.height {
                *fabric.entry((i, j)).or_insert(0) += 1;
            }
        }
    }

    fabric.values().filter(|&&count| count > 1).count() as i32
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let claims = read_claims("input.txt")?;
    let overlapping = count_overlapping_inches(&claims);
    println!("{}", overlapping);
    Ok(())
}
