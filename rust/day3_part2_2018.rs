
use std::fs::File;
use std::io::{BufReader, BufRead};

struct Claim {
    id: i32,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

fn main() {
    let claims = read_claims("input.txt").unwrap();

    let mut fabric = vec![vec![0; 1000]; 1000];

    for claim in &claims {
        for y in claim.y..claim.y + claim.height {
            for x in claim.x..claim.x + claim.width {
                fabric[y as usize][x as usize] += 1;
            }
        }
    }

    for claim in &claims {
        let mut overlap = false;
        for y in claim.y..claim.y + claim.height {
            for x in claim.x..claim.x + claim.width {
                if fabric[y as usize][x as usize] > 1 {
                    overlap = true;
                    break;
                }
            }
            if overlap {
                break;
            }
        }
        if !overlap {
            println!("{}", claim.id);
            return;
        }
    }
}

fn read_claims(filename: &str) -> Result<Vec<Claim>, std::io::Error> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut claims = Vec::new();

    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split_whitespace().collect();
        let id: i32 = parts[0][1..].parse().unwrap();
        let coords: Vec<&str> = parts[2][..parts[2].len() - 1].split(',').collect();
        let x: i32 = coords[0].parse().unwrap();
        let y: i32 = coords[1].parse().unwrap();
        let dims: Vec<&str> = parts[3].split('x').collect();
        let width: i32 = dims[0].parse().unwrap();
        let height: i32 = dims[1].parse().unwrap();
        claims.push(Claim { id, x, y, width, height });
    }

    Ok(claims)
}
