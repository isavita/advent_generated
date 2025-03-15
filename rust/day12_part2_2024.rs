
use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_label(dx: i32, dy: i32) -> &'static str {
    match (dx, dy) {
        (-1, 0) => "left",
        (1, 0) => "right",
        (0, -1) => "up",
        _ => "down",
    }
}

fn add_outer(label: &str, side: &mut Side, x: i32, y: i32) {
    let key = if label == "up" || label == "down" {
        (y, x)
    } else {
        (x, y)
    };
    match label {
        "left" => side.left.insert(key),
        "up" => side.up.insert(key),
        "right" => side.right.insert(key),
        "down" => side.down.insert(key),
        _ => unreachable!(),
    };
}

fn check(ary: &[(i32, i32)], key: (i32, i32)) -> bool {
    let (i, j) = key;
    for &(di, dj) in &[(0, -1), (0, 1)] {
        let neighbor = (i + di, j + dj);
        if ary.contains(&neighbor) {
            return true;
        }
    }
    false
}

fn count_outer(side: &Side) -> usize {
    let mut outer = 0;
    for (label, keys) in &[
        ("left", &side.left),
        ("up", &side.up),
        ("right", &side.right),
        ("down", &side.down),
    ] {
        let mut sorted_keys: Vec<_> = keys.iter().copied().collect();
        if *label == "up" || *label == "down" {
            sorted_keys.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
        } else {
            sorted_keys.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));
        }

        let mut temp = Vec::new();
        for key in sorted_keys {
            if !check(&temp, key) {
                outer += 1;
            }
            temp.push(key);
        }
    }
    outer
}

#[derive(Default)]
struct Side {
    left: HashSet<(i32, i32)>,
    up: HashSet<(i32, i32)>,
    right: HashSet<(i32, i32)>,
    down: HashSet<(i32, i32)>,
}

fn solve() -> usize {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);
    let mut graph: Vec<Vec<char>> = reader
        .lines()
        .map(|line| line.unwrap().trim().chars().collect())
        .filter(|line: &Vec<char>| !line.is_empty())
        .collect();

    let h = graph.len();
    let w = graph[0].len();
    let moves = [(-1, 0), (0, -1), (1, 0), (0, 1)];
    let mut total_sum = 0;

    for y in 0..h {
        for x in 0..w {
            if graph[y][x] == '.' {
                continue;
            }

            let mut area = 0;
            let target = graph[y][x];
            let mut visited = HashSet::new();
            let mut side = Side::default();
            let mut q = VecDeque::new();
            q.push_back((x as i32, y as i32, ""));

            while let Some((cx, cy, label)) = q.pop_front() {
                if graph[cy as usize][cx as usize] != target {
                    if !label.is_empty() && !visited.contains(&(cx, cy)) {
                        add_outer(label, &mut side, cx, cy);
                    }
                    continue;
                }

                visited.insert((cx, cy));
                area += 1;
                graph[cy as usize][cx as usize] = '.';

                for &(dx, dy) in &moves {
                    let nx = cx + dx;
                    let ny = cy + dy;

                    if nx >= 0 && nx < w as i32 && ny >= 0 && ny < h as i32 {
                        q.push_back((nx, ny, get_label(dx, dy)));
                    } else {
                        add_outer(get_label(dx, dy), &mut side, nx, ny);
                    }
                }
            }

            let outer = count_outer(&side);
            total_sum += area * outer;
        }
    }
    total_sum
}

fn main() {
    let result = solve();
    println!("{}", result);
}
