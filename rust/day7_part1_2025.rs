
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("open input.txt");
    let grid: Vec<Vec<char>> = BufReader::new(file)
        .lines()
        .filter_map(Result::ok)
        .filter(|l| !l.is_empty())
        .map(|l| l.chars().collect())
        .collect();
    if grid.is_empty() {
        println!("Empty grid");
        return;
    }
    let (h, w) = (grid.len(), grid[0].len());
    let (sx, sy) = (0..w)
        .flat_map(|x| (0..h).map(move |y| (x, y)))
        .find(|&(x, y)| grid[y][x] == 'S')
        .expect("S not found");
    let mut active = vec![false; w];
    active[sx] = true;
    let mut splits = 0;
    for y in sy..h {
        let mut next = vec![false; w];
        for x in 0..w {
            if !active[x] {
                continue;
            }
            if grid[y][x] == '^' {
                splits += 1;
                if x > 0 {
                    next[x - 1] = true;
                }
                if x + 1 < w {
                    next[x + 1] = true;
                }
            } else {
                next[x] = true;
            }
        }
        active = next;
        if !active.iter().any(|&b| b) {
            break;
        }
    }
    println!("Total times the beam is split: {}", splits);
}
