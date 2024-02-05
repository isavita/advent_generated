
use std::fs;

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct State {
    pos: Point,
    keys: i32,
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Error reading the file");
    let mut grid = Vec::new();
    let mut start = Point { x: 0, y: 0 };
    let mut key_map = std::collections::HashMap::new();
    let mut key_counter = 0;

    for (y, line) in contents.lines().enumerate() {
        grid.push(line.to_string());
        for (x, &c) in line.as_bytes().iter().enumerate() {
            if c as char == '@' {
                start = Point { x: x as i32, y: y as i32 };
            } else if c >= b'a' && c <= b'z' {
                key_map.insert(c as char, key_counter);
                key_counter += 1;
            }
        }
    }

    println!("{}", find_shortest_path(&grid, start, &key_map));
}

fn find_shortest_path(grid: &Vec<String>, start: Point, key_map: &std::collections::HashMap<char, i32>) -> i32 {
    let dirs = [Point { x: 0, y: -1 }, Point { x: -1, y: 0 }, Point { x: 0, y: 1 }, Point { x: 1, y: 0 }];
    let mut visited = std::collections::HashSet::new();
    let mut queue = vec![State { pos: start, keys: 0 }];
    let mut steps = 0;

    while !queue.is_empty() {
        let size = queue.len();
        for _ in 0..size {
            let current = queue.remove(0);

            if current.keys == (1 << key_map.len()) - 1 {
                return steps;
            }

            for d in dirs.iter() {
                let next = Point { x: current.pos.x + d.x, y: current.pos.y + d.y };
                if next.x >= 0 && next.x < grid[0].len() as i32 && next.y >= 0 && next.y < grid.len() as i32 {
                    let c = grid[next.y as usize].as_bytes()[next.x as usize] as char;
                    if c != '#' && !(c >= 'A' && c <= 'Z' && current.keys & (1 << key_map[&(c.to_ascii_lowercase())]) == 0) {
                        let mut new_keys = current.keys;
                        if c >= 'a' && c <= 'z' {
                            new_keys |= 1 << key_map[&c];
                        }
                        let new_state = State { pos: next, keys: new_keys };
                        if !visited.contains(&new_state) {
                            visited.insert(new_state);
                            queue.push(new_state);
                        }
                    }
                }
            }
        }
        steps += 1;
    }

    return -1;
}
