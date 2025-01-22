
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let map = read_map_from_file("input.txt").expect("Failed to read map from file");
    let sum_of_scores = calculate_sum_of_trailhead_scores(&map);
    let sum_of_ratings = calculate_sum_of_trailhead_ratings(&map);
    println!("Sum of trailhead scores: {}", sum_of_scores);
    println!("Sum of trailhead ratings: {}", sum_of_ratings);
}

fn read_map_from_file(filename: impl AsRef<Path>) -> io::Result<Vec<Vec<u8>>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut map = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let row: Vec<u8> = line
            .chars()
            .map(|c| c.to_digit(10).unwrap() as u8)
            .collect();
        map.push(row);
    }
    Ok(map)
}

fn calculate_sum_of_trailhead_scores(map: &Vec<Vec<u8>>) -> u32 {
    let mut total_score = 0;
    for (r, row) in map.iter().enumerate() {
        for (c, &height) in row.iter().enumerate() {
            if height == 0 {
                total_score += calculate_trailhead_score(map, r, c);
            }
        }
    }
    total_score
}

fn calculate_trailhead_score(map: &Vec<Vec<u8>>, start_r: usize, start_c: usize) -> u32 {
    let rows = map.len();
    let cols = map[0].len();
    let mut reachable_9s = 0;
    let mut visited = vec![vec![false; cols]; rows];
    let mut queue = vec![(start_r, start_c)];

    while let Some((r, c)) = queue.pop() {
      if visited[r][c] {
          continue;
      }
        visited[r][c] = true;
        
        if map[r][c] == 9 {
            reachable_9s += 1;
            continue;
        }
        
        let current_height = map[r][c];
        
        let neighbors = [(r + 1, c), (r.wrapping_sub(1), c), (r, c+1), (r, c.wrapping_sub(1))];
        for (nr, nc) in neighbors {
            if nr < rows && nc < cols {
              if map[nr][nc] == current_height + 1 {
                  queue.push((nr, nc));
              }
            }
        }
    }
    reachable_9s
}

fn calculate_sum_of_trailhead_ratings(map: &Vec<Vec<u8>>) -> u32 {
    let mut total_rating = 0;
    for (r, row) in map.iter().enumerate() {
        for (c, &height) in row.iter().enumerate() {
            if height == 0 {
                total_rating += calculate_trailhead_rating(map, r, c);
            }
        }
    }
    total_rating
}

fn calculate_trailhead_rating(map: &Vec<Vec<u8>>, start_r: usize, start_c: usize) -> u32 {
    let rows = map.len();
    let cols = map[0].len();
    let mut trail_count = 0;

    fn find_trails(
      map: &Vec<Vec<u8>>,
      r: usize,
      c: usize,
      rows: usize,
      cols: usize,
      trail_count: &mut u32
    ){
        let current_height = map[r][c];

        if current_height == 9 {
            *trail_count += 1;
            return;
        }

         let neighbors = [(r + 1, c), (r.wrapping_sub(1), c), (r, c+1), (r, c.wrapping_sub(1))];
          for (nr, nc) in neighbors {
            if nr < rows && nc < cols {
                if map[nr][nc] == current_height + 1{
                    find_trails(map, nr, nc, rows, cols, trail_count);
                }
            }
         }
    }
     find_trails(map, start_r, start_c, rows, cols, &mut trail_count);
    trail_count
}
