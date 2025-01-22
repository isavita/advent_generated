
use std::{
    fs,
    collections::{HashMap, HashSet},
};

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let ans = solve(&input.trim());
    println!("{}", ans);
}

fn solve(input: &str) -> usize {
    let tiles = parse_tiles_from_input(input);
    let edge_size = (tiles.len() as f64).sqrt() as usize;
    let assembled_tiles = backtrack_assemble(&tiles, edge_size).expect("No solution found");

    let mut image = Vec::new();
    for big_row in 0..edge_size {
        for sub_row in 0..assembled_tiles[0][0].contents.len() - 2 {
            let mut row = Vec::new();
            for big_col in 0..edge_size {
               row.extend(
                   assembled_tiles[big_row][big_col].contents[sub_row+1][1..assembled_tiles[big_row][big_col].contents[0].len()-1].iter().cloned()
               )
            }
             image.push(row);
        }
    }
    
    let mut monster_coords = Vec::new();
     for opt in all_grid_orientations(image.clone()) {
        monster_coords = find_monster_coords(&opt);
        if !monster_coords.is_empty() {
            image = opt;
            break;
        }
    }


    let mut image_set: HashSet<(usize, usize)> = HashSet::new();
    for (r, row) in image.iter().enumerate(){
        for(c, cell) in row.iter().enumerate(){
            if cell == &'#'{
                image_set.insert((r,c));
            }
        }
    }

    for coord in monster_coords {
       image_set.remove(&(coord[0], coord[1]));
    }


    image_set.len()
}

#[derive(Clone, Debug)]
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
            let tile_id = header[5..header.len() - 1].parse::<usize>().unwrap();
            let contents = lines
                .map(|line| line.chars().collect())
                .collect();
            Tile {
                id: tile_id,
                contents,
            }
        })
        .collect()
}

fn backtrack_assemble(tiles: &[Tile], edge_size: usize) -> Option<Vec<Vec<Tile>>> {
    let mut assembled_tiles: Vec<Vec<Option<Tile>>> = vec![vec![None; edge_size]; edge_size];
    let mut used_indices = HashSet::new();
    backtrack_assemble_recursive(tiles, &mut assembled_tiles, &mut used_indices, edge_size, 0, 0)
}
fn backtrack_assemble_recursive(tiles: &[Tile], assembled_tiles: &mut Vec<Vec<Option<Tile>>>, used_indices: &mut HashSet<usize>, edge_size: usize, row:usize, col: usize) -> Option<Vec<Vec<Tile>>> {
      if row == edge_size {
        let result: Vec<Vec<Tile>> = assembled_tiles.iter().map(|row| {
                row.iter().map(|tile| tile.clone().unwrap()).collect()
            }).collect();
            return Some(result);
      }
       let next_row = if col + 1 == edge_size {row + 1} else {row};
       let next_col = if col + 1 == edge_size {0} else {col+1};


    if assembled_tiles[row][col].is_some() {
        return backtrack_assemble_recursive(tiles, assembled_tiles, used_indices, edge_size, next_row, next_col);
    }
    
    for (i, tile) in tiles.iter().enumerate() {
            if !used_indices.contains(&i) {
                for opt in all_grid_orientations(tile.contents.clone()) {
                    let mut valid = true;
                    if row != 0 {
                            let current_top_row = get_row(&opt, true);
                            let bottom_of_above = get_row(&assembled_tiles[row-1][col].as_ref().unwrap().contents, false);
                            if current_top_row != bottom_of_above {
                                valid = false;
                            }
                    }
                     if valid && col != 0 {
                         let current_left_col = get_col(&opt, true);
                         let right_col_of_left = get_col(&assembled_tiles[row][col-1].as_ref().unwrap().contents, false);
                          if current_left_col != right_col_of_left {
                              valid = false;
                         }
                    }
                    
                    if valid {
                           let mut new_tile = tile.clone();
                           new_tile.contents = opt;
                            assembled_tiles[row][col] = Some(new_tile);
                            used_indices.insert(i);
                            let recurse_result = backtrack_assemble_recursive(tiles, assembled_tiles, used_indices, edge_size, next_row, next_col);
                            if recurse_result.is_some() {
                                return recurse_result;
                            }
                             assembled_tiles[row][col] = None;
                             used_indices.remove(&i);

                    }
                }
            }
        }
        
    None
}


fn get_col(grid: &[Vec<char>], first_col: bool) -> String {
    let mut str = String::new();
    for row in grid {
        if first_col {
            str.push(row[0]);
        } else {
            str.push(row[row.len() - 1]);
        }
    }
    str
}

fn get_row(grid: &[Vec<char>], first_row: bool) -> String {
    let mut str = String::new();
    if first_row {
          for c in 0..grid[0].len(){
             str.push(grid[0][c]);
          }
    } else {
         for c in 0..grid[0].len(){
             str.push(grid[grid.len() -1][c]);
          }
    }
    str
}


const MONSTER: &str = "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ";
fn find_monster_coords(image: &[Vec<char>]) -> Vec<[usize; 2]> {
     let mut monster_offsets = Vec::new();
     let mut monster_height = 0;
     let mut monster_length = 0;

    for (r, line) in MONSTER.lines().enumerate() {
          for(c, char) in line.chars().enumerate(){
              if char == '#'{
                   monster_offsets.push([r,c]);
                }
                monster_length = c + 1;
           }
         monster_height += 1;
    }
    
      let mut monster_starting_coords = Vec::new();
     for r in 0..image.len() - monster_height + 1{
         for c in 0..image[0].len() - monster_length + 1{
            let mut monster_found = true;
               for diff in &monster_offsets {
                     let row_to_check = r + diff[0];
                     let col_to_check = c + diff[1];
                     if image[row_to_check][col_to_check] != '#'{
                        monster_found = false;
                         break;
                     }
                }
            if monster_found {
                monster_starting_coords.push([r,c]);
            }
         }
    }
    
    let mut monster_coords = Vec::new();
    for starting_coord in monster_starting_coords {
      for diff in &monster_offsets{
        monster_coords.push([starting_coord[0] + diff[0], starting_coord[1] + diff[1]]);
      }
    }
    monster_coords
}



fn all_grid_orientations(grid: Vec<Vec<char>>) -> Vec<Vec<Vec<char>>> {
    let mut orientations = vec![grid];

    for _ in 0..3 {
        orientations.push(rotate_grid(orientations.last().unwrap()));
    }

    for i in 0..4 {
        orientations.push(mirror_grid(&orientations[i]));
    }

    orientations
}

fn rotate_grid(grid: &[Vec<char>]) -> Vec<Vec<char>> {
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

fn mirror_grid(grid: &[Vec<char>]) -> Vec<Vec<char>> {
    let mut mirrored = Vec::new();
    for row in grid {
        let mut new_row = Vec::new();
        for c in row.iter().rev(){
            new_row.push(*c);
        }
        mirrored.push(new_row)
    }
    mirrored
}
