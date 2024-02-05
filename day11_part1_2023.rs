
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct Grid {
    width: i32,
    height: i32,
    data: std::collections::HashMap<Coord, u8>,
}

const EMPTY: u8 = b'.';

fn build_grid(input: &[String], empty: u8) -> Grid {
    let mut data = std::collections::HashMap::new();
    let height = input.len() as i32;
    let width = input[0].len() as i32;

    for (y, line) in input.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
            if char as u8 != empty {
                data.insert(Coord { x: x as i32, y: y as i32 }, char as u8);
            }
        }
    }

    Grid { width, height, data }
}

impl Grid {
    fn to_string(&self, empty: u8) -> String {
        let mut result = String::new();

        for y in 0..self.height {
            for x in 0..self.width {
                let coord = Coord { x, y };
                if let Some(&v) = self.data.get(&coord) {
                    result.push(v as char);
                } else {
                    result.push(empty as char);
                }
            }
            result.push('\n');
        }

        result
    }

    fn get_empty_rows(&self) -> Vec<i32> {
        let mut empty_rows = Vec::new();

        for y in 0..self.height {
            let mut is_empty = true;

            let mut x = 0;
            while x < self.width {
                if self.data.get(&Coord { x, y }).is_some() {
                    is_empty = false;
                }
                x += 1;
            }

            if is_empty {
                empty_rows.push(y);
            }
        }

        empty_rows
    }

    fn get_empty_cols(&self) -> Vec<i32> {
        let mut empty_cols = Vec::new();

        for x in 0..self.width {
            let mut is_empty = true;

            let mut y = 0;
            while y < self.height {
                if self.data.get(&Coord { x, y }).is_some() {
                    is_empty = false;
                }
                y += 1;
            }

            if is_empty {
                empty_cols.push(x);
            }
        }

        empty_cols
    }
}

fn calculate_offsets(empty_indexes: &[i32], bound: i32) -> Vec<i32> {
    let mut offsets = vec![0; bound as usize];
    for &idx in empty_indexes {
        for i in (idx + 1) as usize..offsets.len() {
            offsets[i] += 1;
        }
    }
    offsets
}

fn expand_grid(grid: &Grid, expansion_factor: i32) -> Grid {
    let empty_cols = grid.get_empty_cols();
    let empty_rows = grid.get_empty_rows();
    let num_lines_to_add = expansion_factor - 1;

    let mut new_data = std::collections::HashMap::new();
    let d_xs = calculate_offsets(&empty_cols, grid.width);
    let d_ys = calculate_offsets(&empty_rows, grid.height);

    for y in 0..grid.height {
        for x in 0..grid.width {
            let coord = Coord { x, y };
            if let Some(&v) = grid.data.get(&coord) {
                let new_coord = Coord { x: x + d_xs[x as usize] * num_lines_to_add, y: y + d_ys[y as usize] * num_lines_to_add };
                new_data.insert(new_coord, v);
            }
        }
    }

    Grid { width: grid.width + empty_cols.len() as i32 * num_lines_to_add, height: grid.height + empty_rows.len() as i32 * num_lines_to_add, data: new_data }
}

fn abs(x: i32) -> i32 {
    if x < 0 {
        -x
    } else {
        x
    }
}

fn calculate_length(grid: &Grid, c1: Coord, c2: Coord) -> i32 {
    let d_x = abs(c2.x - c1.x);
    let d_y = abs(c2.y - c1.y);
    d_x + d_y
}

fn solve(input: &[String]) -> i32 {
    let grid = build_grid(input, EMPTY);

    let expanded_grid = expand_grid(&grid, 2);

    let mut res = 0;
    let mut already_seen = std::collections::HashSet::new();
    for &coord1 in expanded_grid.data.keys() {
        for &coord2 in already_seen.iter() {
            let length = calculate_length(&expanded_grid, coord1, coord2);
            res += length;
        }
        already_seen.insert(coord1);
    }

    res
}

fn read_file(file_name: &str) -> Vec<String> {
    let file_content = fs::read_to_string(file_name).expect("Error reading the file");
    file_content.trim().split('\n').map(|s| s.to_string()).collect()
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(&input));
}
