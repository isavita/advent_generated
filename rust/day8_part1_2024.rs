
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    let h = grid.len();
    let w = if h > 0 { grid[0].len() } else { 0 };

    let mut antennas: HashMap<char, Vec<[i32; 2]>> = HashMap::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, c) in row.chars().enumerate() {
            if c != '.' {
                antennas.entry(c).or_default().push([y as i32, x as i32]);
            }
        }
    }

    let mut antinodes = HashMap::new();
    for coords in antennas.values() {
        let n = coords.len();
        for i in 0..n {
            for j in i + 1..n {
                let a = coords[i];
                let b = coords[j];
                let p1 = [2 * a[0] - b[0], 2 * a[1] - b[1]];
                let p2 = [2 * b[0] - a[0], 2 * b[1] - a[1]];

                 if p1[0] >= 0 && p1[0] < h as i32 && p1[1] >= 0 && p1[1] < w as i32{
                        antinodes.insert(p1,true);
                 }
                if p2[0] >= 0 && p2[0] < h as i32 && p2[1] >= 0 && p2[1] < w as i32{
                        antinodes.insert(p2,true);
                }
            }
        }
    }

    println!("{}", antinodes.len());
    Ok(())
}
