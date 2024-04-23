use std::fs;
use std::collections::HashSet;

type Point = (i32, i32);

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let result = visited(&input, 10);
    println!("{}", result);
}

fn visited(input: &str, rope_len: usize) -> usize {
    let mut rope: Vec<Point> = vec![(0, 0); rope_len];
    let mut visited: HashSet<Point> = HashSet::new();
    for line in input.lines() {
        let mut parts = line.split_whitespace();
        let dir = match parts.next().unwrap().as_bytes()[0] {
            b'N' | b'U' | b'^' => (0, 1),
            b'E' | b'R' | b'>' => (1, 0),
            b'S' | b'D' | b'v' => (0, -1),
            b'W' | b'L' | b'<' => (-1, 0),
            _ => unreachable!(),
        };
        let n: i32 = parts.next().unwrap().parse().unwrap();
        for _ in 0..n {
            rope[0].0 += dir.0;
            rope[0].1 += dir.1;
            for i in 1..rope_len {
                let dx = rope[i-1].0 - rope[i].0;
                let dy = rope[i-1].1 - rope[i].1;
                if dx.abs() > 1 || dy.abs() > 1 {
                    rope[i].0 += dx.signum();
                    rope[i].1 += dy.signum();
                }
            }
            visited.insert(rope[rope_len-1]);
        }
    }
    visited.len()
}