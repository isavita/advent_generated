use std::fs::File;
use std::io::{BufReader, prelude::*};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Coordinate {
    q: i32,
    r: i32,
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut black_tiles = std::collections::HashMap::<Coordinate, bool>::new();
    let directions = vec![
        ("e", Coordinate { q: 1, r: 0 }),
        ("se", Coordinate { q: 0, r: 1 }),
        ("sw", Coordinate { q: -1, r: 1 }),
        ("w", Coordinate { q: -1, r: 0 }),
        ("nw", Coordinate { q: 0, r: -1 }),
        ("ne", Coordinate { q: 1, r: -1 }),
    ]
    .iter()
    .cloned()
    .collect::<std::collections::HashMap<_, _>>();

    for line in reader.lines().map(|l| l.unwrap()) {
        let mut coord = Coordinate { q: 0, r: 0 };

        let mut i = 0;
        while i < line.len() {
            let dir = match line.chars().nth(i).unwrap() {
                'e' | 'w' => &line[i..=i],
                'n' | 's' => {
                    i += 1;
                    &line[i - 1..=i]
                }
                _ => "",
            };
            i += 1;
            
            let move_val = directions.get(dir).unwrap();
            coord.q += move_val.q;
            coord.r += move_val.r;
        }

        *black_tiles.entry(coord).or_insert(false) ^= true;
    }

    let count = black_tiles.values().filter(|b| **b).count();
    println!("{}", count);
}