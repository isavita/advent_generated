
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn gcd(mut a: i32, mut b: i32) -> i32 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a.abs()
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    let h = grid.len() as i32;
    let w = if h > 0 { grid[0].len() as i32 } else { 0 };

    let mut antennas: HashMap<char, Vec<[i32; 2]>> = HashMap::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, c) in row.chars().enumerate() {
            if c != '.' {
                antennas.entry(c).or_default().push([y as i32, x as i32]);
            }
        }
    }

    let mut lines_per_freq: HashMap<char, HashMap<(i32, i32, i32), bool>> = HashMap::new();
    for (f, coords) in antennas {
        let lines = lines_per_freq.entry(f).or_default();
        let n = coords.len();
        for i in 0..n {
            for j in i + 1..n {
                let a = coords[i];
                let b = coords[j];
                let dy = b[0] - a[0];
                let dx = b[1] - a[1];
                let g = gcd(dy, dx);
                let mut sy = dy / g;
                let mut sx = dx / g;
                if sx < 0 || (sx == 0 && sy < 0) {
                    sx = -sx;
                    sy = -sy;
                }
                let c = sy * a[1] - sx * a[0];
                lines.insert((sx, sy, c), true);
            }
        }
    }

    let mut antinodes: HashMap<[i32; 2], bool> = HashMap::new();
    for lines in lines_per_freq.values() {
        for &(sx, sy, c) in lines.keys() {
            if sx == 0 && sy == 0 {
                continue;
            }
           if sy == 0 {
                if c % sx == 0 {
                    let y = -c / sx;
                    if y >= 0 && y < h {
                        for x in 0..w {
                             antinodes.insert([y, x], true);
                        }
                    }
                }
            } else if sx == 0 {
                if c % sy == 0 {
                    let x = c / sy;
                    if x >= 0 && x < w {
                         for y in 0..h {
                             antinodes.insert([y, x], true);
                        }
                    }
                }
            } else {
                 for y in 0..h {
                    let val = c + sx * y;
                    if val % sy == 0 {
                        let x = val / sy;
                        if x >= 0 && x < w {
                            antinodes.insert([y, x], true);
                         }
                    }
                }
            }
        }
    }

    println!("{}", antinodes.len());
    Ok(())
}
