
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug, Clone)]
struct Particle {
    p: [i32; 3],
    v: [i32; 3],
    a: [i32; 3],
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    let mut particles: Vec<Particle> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(", ").collect();

        let mut p = Particle {
            p: [0; 3],
            v: [0; 3],
            a: [0; 3],
        };

        for (i, part) in parts.iter().enumerate() {
            let coords: Vec<i32> = part[3..part.len() - 1]
                .split(',')
                .map(|x| x.parse::<i32>().unwrap())
                .collect();

            for (j, &coord) in coords.iter().enumerate() {
                match i {
                    0 => p.p[j] = coord,
                    1 => p.v[j] = coord,
                    2 => p.a[j] = coord,
                    _ => (),
                }
            }
        }

        particles.push(p);
    }

    for _tick in 0..1000 {
        let mut positions: HashMap<String, i32> = HashMap::new();

        for particle in particles.iter_mut() {
            for j in 0..3 {
                particle.v[j] += particle.a[j];
                particle.p[j] += particle.v[j];
            }

            let pos_str = format!("{},{},{}", particle.p[0], particle.p[1], particle.p[2]);
            *positions.entry(pos_str).or_insert(0) += 1;
        }

        particles.retain(|particle| {
            let pos_str = format!("{},{},{}", particle.p[0], particle.p[1], particle.p[2]);
            positions[&pos_str] == 1
        });
    }

    println!("{}", particles.len());

    Ok(())
}
