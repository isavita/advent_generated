
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    positions: Vec<Point>,
    keys: u32, // Bitmask of collected keys
}

fn read_input(filename: &str) -> (Vec<Vec<char>>, Vec<Point>, usize) {
    let file = File::open(filename).expect("Failed to open file");
    let reader = BufReader::new(file);
    let mut grid: Vec<Vec<char>> = Vec::new();
    let mut start_positions: Vec<Point> = Vec::new();
    let mut num_keys = 0;

    for (y, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line");
        let mut row: Vec<char> = Vec::new();
        for (x, c) in line.chars().enumerate() {
            if c == '@' {
                start_positions.push(Point { x, y });
                row.push('.');
            } else {
                row.push(c);
                if c.is_ascii_lowercase() {
                    num_keys += 1;
                }
            }
        }
        grid.push(row);
    }

    (grid, start_positions, num_keys)
}

fn solve(grid: &[Vec<char>], start_positions: Vec<Point>, num_keys: usize) -> i32 {
    let all_keys = (1 << num_keys) - 1;
    let mut queue: VecDeque<(State, i32)> = VecDeque::new();
    let mut visited: HashSet<State> = HashSet::new();

    let initial_state = State {
        positions: start_positions,
        keys: 0,
    };

    queue.push_back((initial_state.clone(), 0));
    visited.insert(initial_state);

    let directions = [(0, -1), (0, 1), (-1, 0), (1, 0)];

    while let Some((current_state, steps)) = queue.pop_front() {
        if current_state.keys == all_keys {
            return steps;
        }

        for (robot_index, &pos) in current_state.positions.iter().enumerate() {
            let mut q: VecDeque<(Point, i32)> = VecDeque::new();
            let mut v: HashSet<Point> = HashSet::new();
            q.push_back((pos, 0));
            v.insert(pos);
            while let Some((current_pos, dist)) = q.pop_front() {
                for &(dx, dy) in &directions {
                    let new_x = current_pos.x as isize + dx;
                    let new_y = current_pos.y as isize + dy;

                    if new_x < 0
                        || new_y < 0
                        || new_x >= grid[0].len() as isize
                        || new_y >= grid.len() as isize
                    {
                        continue;
                    }

                    let new_x = new_x as usize;
                    let new_y = new_y as usize;
                    let new_pos = Point { x: new_x, y: new_y };
                    let c = grid[new_y][new_x];

                    if c == '#' || v.contains(&new_pos) {
                        continue;
                    }

                    if c.is_ascii_uppercase() && (current_state.keys & (1 << (c.to_ascii_lowercase() as u8 - b'a'))) == 0
                    {
                        continue;
                    }
                    v.insert(new_pos);
                    
                    if c.is_ascii_lowercase() {
                        let key_bit = 1 << (c as u8 - b'a');
                        let mut new_keys = current_state.keys | key_bit;
                        let mut new_positions = current_state.positions.clone();
                        new_positions[robot_index] = new_pos;
                        
                        
                        let new_state = State { positions: new_positions, keys: new_keys};

                        if !visited.contains(&new_state){
                            visited.insert(new_state.clone());
                            queue.push_back((new_state, steps + dist + 1));
                        }
                        
                    } else {
                        q.push_back((new_pos, dist+1));
                    }

                }

            }
        }
    }

    -1 // Should never reach here if all keys are reachable
}

fn main() {
    let (grid, start_positions, num_keys) = read_input("input.txt");

    let part1_result = solve(&grid, start_positions.clone(), num_keys);
    println!("Part 1: {}", part1_result);

    let mut grid_part2 = grid.clone();
    let original_start = start_positions[0];
    let x = original_start.x;
    let y = original_start.y;

    grid_part2[y - 1][x - 1] = '@';
    grid_part2[y - 1][x] = '#';
    grid_part2[y - 1][x + 1] = '@';
    grid_part2[y][x - 1] = '#';
    grid_part2[y][x] = '#';
    grid_part2[y][x + 1] = '#';
    grid_part2[y + 1][x - 1] = '@';
    grid_part2[y + 1][x] = '#';
    grid_part2[y + 1][x + 1] = '@';
    
    let start_positions_part2 = vec![
        Point {x: x-1, y: y-1},
        Point {x: x+1, y: y-1},
        Point {x: x-1, y: y+1},
        Point {x: x+1, y: y+1}
    ];
    
    let part2_result = solve(&grid_part2, start_positions_part2, num_keys);    
    println!("Part 2: {}", part2_result);    
}
