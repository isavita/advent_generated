
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    row: usize,
    col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    point: Point,
    level: i32,
}

fn is_valid(row: i32, col: i32, rows: usize, cols: usize) -> bool {
    row >= 0 && row < rows as i32 && col >= 0 && col < cols as i32
}

fn solve(maze: &Vec<Vec<char>>, part2: bool) -> Option<usize> {
    let rows = maze.len();
    let cols = maze[0].len();
    let mut portals = HashMap::new();
    let mut start = None;
    let mut end = None;

    // Scan maze and identify portals.
    for row in 0..rows {
        for col in 0..cols {
            if maze[row][col].is_ascii_uppercase() {
                let row_i = row as i32;
                let col_i = col as i32;
                let mut label = String::new();
                let mut portal_pos = None;
                if is_valid(row_i + 1, col_i, rows, cols)
                    && maze[row + 1][col].is_ascii_uppercase()
                {
                    label.push(maze[row][col]);
                    label.push(maze[row + 1][col]);
                    if is_valid(row_i + 2, col_i, rows, cols) && maze[row + 2][col] == '.'
                    {
                        portal_pos = Some(Point {
                            row: row + 2,
                            col: col,
                        });
                    } else if is_valid(row_i - 1, col_i, rows, cols) && maze[row - 1][col] == '.' {
                         portal_pos = Some(Point {
                            row: row - 1,
                            col: col,
                        });
                    }
                } else if is_valid(row_i, col_i + 1, rows, cols)
                    && maze[row][col + 1].is_ascii_uppercase()
                {
                    label.push(maze[row][col]);
                    label.push(maze[row][col + 1]);
                    if is_valid(row_i, col_i+ 2, rows, cols) && maze[row][col+2] == '.'
                    {
                        portal_pos = Some(Point {
                            row: row,
                            col: col+2,
                        });
                    } else if is_valid(row_i, col_i - 1, rows, cols) && maze[row][col - 1] == '.'{
                         portal_pos = Some(Point {
                            row: row,
                            col: col - 1,
                        });
                    }
                }

                if let Some(pos) = portal_pos {
                    if label == "AA" {
                        start = Some(pos);
                    } else if label == "ZZ" {
                        end = Some(pos);
                    } else {
                        portals
                            .entry(label)
                            .or_insert_with(Vec::new)
                            .push(pos);
                    }
                }
            }
        }
    }

    if start.is_none() || end.is_none() {
        return None;
    }
    let start_pos = start.unwrap();
    let end_pos = end.unwrap();
    let mut q = VecDeque::new();
    q.push_back(State {
        point: start_pos,
        level: 0,
    });

    let mut visited = HashMap::new();
    visited.insert(State {
        point: start_pos,
        level: 0,
    }, 0);

    let moves: [(i32, i32); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    while let Some(current) = q.pop_front() {
        if current.point == end_pos && current.level == 0 {
            return visited.get(&current).copied();
        }
        let current_row = current.point.row as i32;
        let current_col = current.point.col as i32;

        for (dr, dc) in moves {
            let next_row = current_row + dr;
            let next_col = current_col + dc;

            if is_valid(next_row, next_col, rows, cols) {
                let next_row = next_row as usize;
                let next_col = next_col as usize;
                if maze[next_row][next_col] == '.' {
                    let next = State {
                        point: Point {
                            row: next_row,
                            col: next_col,
                        },
                        level: current.level,
                    };
                    if !visited.contains_key(&next) {
                        visited.insert(next, visited.get(&current).unwrap() + 1);
                        q.push_back(next);
                    }
                }
            }
        }
        
        //check for portals
        for (label, positions) in portals.iter() {
            for pos in positions {
                 if pos.row == current.point.row && pos.col == current.point.col {
                     //handle portal jump
                     let other_portal = positions.iter().filter(|&p| *p != *pos).next();
                     if let Some(other) = other_portal{
                        let mut next_level = current.level;
                        let is_outer = pos.row == 2 || pos.row == rows-3 || pos.col == 2 || pos.col == cols-3;

                         if part2 {
                            if is_outer {
                              if current.level > 0 {
                                  next_level = current.level - 1;
                              } else {
                                  continue; // treat as wall
                              }
                            } else{
                                next_level = current.level+1;
                            }
                        }
                        
                         let next = State {
                            point: *other,
                            level: next_level,
                            };
                        if !visited.contains_key(&next)
                        {
                          visited.insert(next, visited.get(&current).unwrap() + 1);
                          q.push_back(next);
                        }
                     }
                 }
            }
        }
    }
    None
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let maze: Vec<Vec<char>> = reader
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    if let Some(steps) = solve(&maze, false) {
        println!("Part 1: {}", steps);
    } else {
        println!("Part 1: No path found");
    }

    if let Some(steps) = solve(&maze, true) {
        println!("Part 2: {}", steps);
    } else {
        println!("Part 2: No path found");
    }


    Ok(())
}
