
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let points: Vec<Vec<i32>> = input
        .trim()
        .lines()
        .map(|line| line.split(',').map(|num| num.parse().unwrap()).collect())
        .collect();

    let mut constellations = 0;
    let mut visited = vec![false; points.len()];

    for i in 0..points.len() {
        if !visited[i] {
            constellations += 1;
            dfs(i, &points, &mut visited);
        }
    }

    println!("{}", constellations);
}

fn dfs(i: usize, points: &Vec<Vec<i32>>, visited: &mut Vec<bool>) {
    visited[i] = true;

    for j in 0..points.len() {
        if !visited[j] && distance(&points[i], &points[j]) <= 3 {
            dfs(j, points, visited);
        }
    }
}

fn distance(p1: &Vec<i32>, p2: &Vec<i32>) -> i32 {
    p1.iter().zip(p2.iter()).map(|(a, b)| (a - b).abs()).sum()
}
