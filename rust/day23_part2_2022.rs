
use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

fn parse_input(filename: &str) -> HashSet<Point> {
    let contents = fs::read_to_string(filename).expect("Unable to read file");
    let mut elves = HashSet::new();
    for (y, line) in contents.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                elves.insert(Point::new(x as i32, y as i32));
            }
        }
    }
    elves
}

fn get_neighbors(point: &Point) -> Vec<Point> {
    vec![
        Point::new(point.x, point.y - 1),     // N
        Point::new(point.x + 1, point.y - 1), // NE
        Point::new(point.x + 1, point.y),     // E
        Point::new(point.x + 1, point.y + 1), // SE
        Point::new(point.x, point.y + 1),     // S
        Point::new(point.x - 1, point.y + 1), // SW
        Point::new(point.x - 1, point.y),     // W
        Point::new(point.x - 1, point.y - 1), // NW
    ]
}


fn get_check_directions(round: usize) -> Vec<Vec<usize>> {
  let directions = vec![
      vec![0, 1, 7], // N, NE, NW
      vec![4, 3, 5], // S, SE, SW
      vec![6, 7, 5], // W, NW, SW
      vec![2, 1, 3], // E, NE, SE
  ];
  
  let mut rotated_directions = Vec::new();

  for i in 0..4 {
      rotated_directions.push(directions[(round + i) % 4].clone());
  }

  rotated_directions
}

fn simulate(mut elves: HashSet<Point>, rounds: usize) -> (HashSet<Point>, usize) {
    let mut no_moves_round = 0;
    for round in 0.. {
      let check_directions = get_check_directions(round);
      let mut proposed_moves: HashMap<Point, Point> = HashMap::new();
      let mut move_counts: HashMap<Point, usize> = HashMap::new();
      
      let mut did_move = false;
      for elf in elves.iter() {
        let neighbors = get_neighbors(elf);
        if neighbors.iter().any(|neighbor| elves.contains(neighbor)) {
          for direction in &check_directions {
              let mut all_clear = true;
            for neighbor_index in direction {
                if elves.contains(&neighbors[*neighbor_index]) {
                    all_clear = false;
                    break;
                }
            }
            if all_clear {
                let proposed_move = neighbors[direction[0]];
                *move_counts.entry(proposed_move).or_insert(0) += 1;
                proposed_moves.insert(*elf, proposed_move);
                break;
            }
          }
        }
      }
      
        let mut new_elves = HashSet::new();
        for elf in elves.iter() {
            if let Some(proposed_move) = proposed_moves.get(elf) {
              if move_counts.get(proposed_move) == Some(&1) {
                new_elves.insert(*proposed_move);
                did_move = true;
              } else {
                new_elves.insert(*elf);
              }
            } else {
              new_elves.insert(*elf);
            }
        }
      
      elves = new_elves;
      
      if round + 1 == rounds {
          return (elves, 0)
      }

      if !did_move {
          no_moves_round = round + 1;
          break;
      }
    }
    (elves, no_moves_round)
}

fn calculate_empty_tiles(elves: &HashSet<Point>) -> i32 {
    let mut min_x = i32::MAX;
    let mut max_x = i32::MIN;
    let mut min_y = i32::MAX;
    let mut max_y = i32::MIN;

    for elf in elves {
        min_x = min_x.min(elf.x);
        max_x = max_x.max(elf.x);
        min_y = min_y.min(elf.y);
        max_y = max_y.max(elf.y);
    }

    let width = max_x - min_x + 1;
    let height = max_y - min_y + 1;
    let total_tiles = width * height;

    total_tiles - elves.len() as i32
}

fn main() {
    let elves = parse_input("input.txt");
    let (final_elves,_) = simulate(elves.clone(), 10);
    let empty_tiles = calculate_empty_tiles(&final_elves);
    println!("Part 1: Empty tiles after 10 rounds: {}", empty_tiles);
    let (_, no_moves_round) = simulate(elves, usize::MAX);
    println!("Part 2: First round with no moves: {}", no_moves_round);
}
