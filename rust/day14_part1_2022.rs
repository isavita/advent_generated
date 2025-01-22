
use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let lines = read_lines("input.txt").expect("Could not read file");
    let mut rocks: HashSet<(i32, i32)> = HashSet::new();
    let mut max_y = 0;

    for line in lines {
        let points: Vec<(i32, i32)> = line
            .split(" -> ")
            .map(|s| {
                let coords: Vec<i32> = s
                    .split(',')
                    .map(|n| n.parse().expect("Not a number"))
                    .collect();
                (coords[0], coords[1])
            })
            .collect();

        for i in 0..points.len() - 1 {
            let (x1, y1) = points[i];
            let (x2, y2) = points[i + 1];
            if x1 == x2 {
                for y in y1.min(y2)..=y1.max(y2) {
                    rocks.insert((x1, y));
                    max_y = max_y.max(y);
                }
            } else if y1 == y2 {
                for x in x1.min(x2)..=x1.max(x2) {
                    rocks.insert((x, y1));
                }
            }
        }
    }

    let mut sand_count = 0;
    let sand_start = (500, 0);
    loop {
        let mut sand_pos = sand_start;
        loop {
            if sand_pos.1 > max_y {
                println!("{}", sand_count);
                return;
            }

            let next_pos = (sand_pos.0, sand_pos.1 + 1);
            let next_pos_left = (sand_pos.0 - 1, sand_pos.1 + 1);
            let next_pos_right = (sand_pos.0 + 1, sand_pos.1 + 1);

            if !rocks.contains(&next_pos) {
                sand_pos = next_pos;
            } else if !rocks.contains(&next_pos_left) {
                sand_pos = next_pos_left;
            } else if !rocks.contains(&next_pos_right) {
                sand_pos = next_pos_right;
            } else {
                rocks.insert(sand_pos);
                sand_count += 1;
                break;
            }
        }
    }
}

fn read_lines<P>(filename: P) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    reader.lines().collect()
}
