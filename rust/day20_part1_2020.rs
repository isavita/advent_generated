
use std::{
    fs::File,
    io::{self, Read},
    str::FromStr,
    collections::HashMap,
};

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let input = input.trim();
    let ans = solve(input);
    println!("{}", ans);
    Ok(())
}

fn solve(input: &str) -> usize {
    let tiles = parse_tiles_from_input(input);
    let edge_size = (tiles.len() as f64).sqrt() as usize;

    let assembled_tiles = backtrack_assemble(&tiles, edge_size).unwrap();

    let product = assembled_tiles[0][0].id
        * assembled_tiles[0][edge_size - 1].id
        * assembled_tiles[edge_size - 1][0].id
        * assembled_tiles[edge_size - 1][edge_size - 1].id;
    product
}

#[derive(Clone)]
struct Tile {
    contents: Vec<Vec<char>>,
    id: usize,
}

fn parse_tiles_from_input(input: &str) -> Vec<Tile> {
    input
        .split("\n\n")
        .map(|block| {
            let mut lines = block.lines();
            let header = lines.next().unwrap();
            let id = header[5..header.len() - 1].parse::<usize>().unwrap();
            let contents = lines
                .map(|line| line.chars().collect::<Vec<_>>())
                .collect::<Vec<_>>();
            Tile { contents, id }
        })
        .collect()
}

fn backtrack_assemble(tiles: &[Tile], edge_size: usize) -> Option<Vec<Vec<Tile>>> {
    let mut assembled_tiles = vec![vec![Tile { contents: vec![], id: 0 }; edge_size]; edge_size];
    let mut used_indices = HashMap::new();
    
    backtrack_assemble_recursive(tiles, &mut assembled_tiles, &mut used_indices, edge_size, 0, 0)
}

fn backtrack_assemble_recursive(tiles: &[Tile], assembled_tiles: &mut Vec<Vec<Tile>>, used_indices: &mut HashMap<usize, bool>, edge_size: usize, row: usize, col: usize) -> Option<Vec<Vec<Tile>>> {
    if row == edge_size {
        return Some(assembled_tiles.clone());
    }

    let next_row = if col == edge_size - 1 { row + 1 } else { row };
    let next_col = if col == edge_size - 1 { 0 } else { col + 1 };

    if assembled_tiles[row][col].id != 0 {
        return backtrack_assemble_recursive(tiles, assembled_tiles, used_indices, edge_size, next_row, next_col);
    }

    for (i, t) in tiles.iter().enumerate() {
        if !used_indices.get(&i).map_or(false, |&v| v) {
            for opt in all_grid_orientations(&t.contents) {
                if row != 0 {
                    let current_top_row = get_row(&opt, true);
                    let bottom_of_above = get_row(&assembled_tiles[row - 1][col].contents, false);
                    if current_top_row != bottom_of_above {
                        continue;
                    }
                }
                if col != 0 {
                    let current_left_col = get_col(&opt, true);
                    let right_col_of_left = get_col(&assembled_tiles[row][col - 1].contents, false);
                    if current_left_col != right_col_of_left {
                        continue;
                    }
                }

                assembled_tiles[row][col] = Tile { contents: opt.clone(), id: t.id };
                used_indices.insert(i, true);

                if let Some(result) = backtrack_assemble_recursive(tiles, assembled_tiles, used_indices, edge_size, next_row, next_col) {
                    return Some(result);
                }
                
                assembled_tiles[row][col] = Tile { contents: vec![], id: 0 };
                used_indices.insert(i, false);
            }
        }
    }

    None
}

fn get_col(grid: &[Vec<char>], first_col: bool) -> String {
    let mut str = String::new();
    for row in grid {
        str.push(if first_col { row[0] } else { row[row.len() - 1] });
    }
    str
}

fn get_row(grid: &[Vec<char>], first_row: bool) -> String {
    let mut str = String::new();
    if let Some(row) = grid.get(if first_row { 0 } else { grid.len() - 1 }) {
        for &c in row {
           str.push(c);
        }
    }
    str
}

fn all_grid_orientations(grid: &[Vec<char>]) -> Vec<Vec<Vec<char>>> {
    let mut orientations = vec![grid.to_vec()];

    for _ in 0..3 {
        orientations.push(rotate_string_grid(orientations.last().unwrap()));
    }

    for i in 0..4 {
        orientations.push(mirror_string_grid(&orientations[i]));
    }

    orientations
}

fn rotate_string_grid(grid: &[Vec<char>]) -> Vec<Vec<char>> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut rotated = vec![vec![' '; rows]; cols];

    for i in 0..rows {
        for j in 0..cols {
            rotated[cols - 1 - j][i] = grid[i][j];
        }
    }
    rotated
}

fn mirror_string_grid(grid: &[Vec<char>]) -> Vec<Vec<char>> {
    grid.iter()
        .map(|row| row.iter().rev().copied().collect())
        .collect()
}
