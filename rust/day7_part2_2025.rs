
use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap_or_default();
    let grid: Vec<&str> = input.lines().filter(|l| !l.is_empty()).collect();
    if grid.is_empty() {
        println!("0");
        return;
    }
    let h = grid.len();
    let w = grid[0].len();
    let mut sx = 0;
    let mut sy = 0;
    for (y, &row) in grid.iter().enumerate() {
        if let Some(x) = row.find('S') {
            sx = x;
            sy = y;
            break;
        }
    }
    let mut cnt: HashMap<usize, u128> = HashMap::new();
    *cnt.entry(sx).or_insert(0) += 1;
    for y in sy..h {
        let mut nxt: HashMap<usize, u128> = HashMap::new();
        for (&x, &c) in &cnt {
            if grid[y].as_bytes()[x] == b'^' {
                *nxt.entry(x.wrapping_sub(1)).or_insert(0) += c;
                *nxt.entry(x + 1).or_insert(0) += c;
            } else {
                *nxt.entry(x).or_insert(0) += c;
            }
        }
        cnt = nxt;
    }
    let sum: u128 = cnt.values().sum();
    println!("{}", sum);
}
