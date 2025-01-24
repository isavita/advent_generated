
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

fn bfs(grid: &[Vec<char>], start: Point, end: Point) -> Option<usize> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut queue = VecDeque::new();
    let mut visited = vec![vec![false; cols]; rows];
    let mut distance = vec![vec![usize::MAX; cols]; rows];

    queue.push_back(start);
    visited[start.y][start.x] = true;
    distance[start.y][start.x] = 0;

    while let Some(curr) = queue.pop_front() {
        if curr == end {
            return Some(distance[curr.y][curr.x]);
        }

        let dx = [0, 0, 1, -1];
        let dy = [1, -1, 0, 0];

        for i in 0..4 {
            let nx = curr.x as isize + dx[i];
            let ny = curr.y as isize + dy[i];

            if nx >= 0
                && nx < cols as isize
                && ny >= 0
                && ny < rows as isize
                && grid[ny as usize][nx as usize] != '#'
                && !visited[ny as usize][nx as usize]
            {
                queue.push_back(Point {
                    x: nx as usize,
                    y: ny as usize,
                });
                visited[ny as usize][nx as usize] = true;
                distance[ny as usize][nx as usize] = distance[curr.y][curr.x] + 1;
            }
        }
    }

    None
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);

    let mut grid: Vec<Vec<char>> = Vec::new();
    let mut start = Point { x: 0, y: 0 };
    let mut end = Point { x: 0, y: 0 };

    for (y, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line");
        let mut row = Vec::new();
        for (x, c) in line.chars().enumerate() {
            row.push(c);
            if c == 'S' {
                start = Point { x, y };
            } else if c == 'E' {
                end = Point { x, y };
            }
        }
        grid.push(row);
    }

    let optimal_distance = bfs(&grid, start, end).expect("No path found from start to end");

    let mut cheat_savings: HashMap<usize, usize> = HashMap::new();
    let mut count_100_plus = 0;

    for start_y in 0..grid.len() {
        for start_x in 0..grid[0].len() {
            if grid[start_y][start_x] == '#' {
                continue;
            }

            for cheat_len in 1..=2 {
                let dx = [0, 0, 1, -1];
                let dy = [1, -1, 0, 0];

                for dir in 0..4 {
                    let mut end_x = start_x as isize;
                    let mut end_y = start_y as isize;
                    let mut valid_cheat = true;

                    for _ in 0..cheat_len {
                        end_x += dx[dir];
                        end_y += dy[dir];

                        if end_x < 0
                            || end_x >= grid[0].len() as isize
                            || end_y < 0
                            || end_y >= grid.len() as isize
                        {
                            valid_cheat = false;
                            break;
                        }
                    }
                    if !valid_cheat {
                        continue;
                    }
                    if grid[end_y as usize][end_x as usize] == '#' {
                        continue;
                    }
                    let mut cheated_grid = grid.clone();
                    let mut current_x = start_x as isize;
                    let mut current_y = start_y as isize;

                    for _ in 0..cheat_len {
                        cheated_grid[current_y as usize][current_x as usize] = '.';
                        current_x += dx[dir];
                        current_y += dy[dir];
                        
                    }

                    let dist_to_cheat_start =
                        bfs(&grid, start, Point { x: start_x, y: start_y });
                    let dist_from_cheat_end = bfs(
                        &cheated_grid,
                        Point {
                            x: end_x as usize,
                            y: end_y as usize,
                        },
                        end,
                    );

                    if let (Some(d1), Some(d2)) = (dist_to_cheat_start, dist_from_cheat_end) {
                        let cheated_distance = d1 + cheat_len + d2;
                        let saved = optimal_distance as isize - cheated_distance as isize;
                        if saved > 0 {
                            *cheat_savings.entry(saved as usize).or_insert(0) += 1;
                            if saved >= 100 {
                                count_100_plus += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    println!("{}", count_100_plus);
}
