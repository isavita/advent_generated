
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut robots: Vec<[i32; 4]> = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let p_part = parts[0].trim_start_matches("p=");
        let v_part = parts[1].trim_start_matches("v=");
        let pos: Vec<i32> = p_part.split(',').map(|s| s.parse().unwrap()).collect();
        let vel: Vec<i32> = v_part.split(',').map(|s| s.parse().unwrap()).collect();
        robots.push([pos[0], pos[1], vel[0], vel[1]]);
    }

    const WIDTH: i32 = 101;
    const HEIGHT: i32 = 103;

    for _ in 0..100 {
        for r in &mut robots {
            r[0] = (r[0] + r[2]).rem_euclid(WIDTH);
            r[1] = (r[1] + r[3]).rem_euclid(HEIGHT);
        }
    }

    let (mut q1, mut q2, mut q3, mut q4) = (0, 0, 0, 0);
    for r in &robots {
        let (x, y) = (r[0], r[1]);
        if x == 50 || y == 51 { continue; }
        match (x < 50, y < 51) {
            (true, true) => q1 += 1,
            (false, true) => q2 += 1,
            (true, false) => q3 += 1,
            (false, false) => q4 += 1,
        }
    }

    println!("{}", q1 * q2 * q3 * q4);
    Ok(())
}
