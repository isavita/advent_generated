
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

    fn neighbors(&self) -> Vec<Point> {
        vec![
            Point::new(self.x - 1, self.y - 1),
            Point::new(self.x, self.y - 1),
            Point::new(self.x + 1, self.y - 1),
            Point::new(self.x - 1, self.y),
            Point::new(self.x + 1, self.y),
            Point::new(self.x - 1, self.y + 1),
            Point::new(self.x, self.y + 1),
            Point::new(self.x + 1, self.y + 1),
        ]
    }

    fn north(&self) -> Point {
        Point::new(self.x, self.y - 1)
    }
    fn south(&self) -> Point {
        Point::new(self.x, self.y + 1)
    }
    fn west(&self) -> Point {
        Point::new(self.x - 1, self.y)
    }
    fn east(&self) -> Point {
        Point::new(self.x + 1, self.y)
    }
    fn northwest(&self) -> Point {
        Point::new(self.x - 1, self.y - 1)
    }
    fn northeast(&self) -> Point {
        Point::new(self.x + 1, self.y - 1)
    }
    fn southwest(&self) -> Point {
        Point::new(self.x - 1, self.y + 1)
    }
    fn southeast(&self) -> Point {
        Point::new(self.x + 1, self.y + 1)
    }

    
}

fn parse_input(input: &str) -> HashSet<Point> {
    let mut elves = HashSet::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                elves.insert(Point::new(x as i32, y as i32));
            }
        }
    }
    elves
}

fn simulate_round(elves: &mut HashSet<Point>, directions: &mut Vec<usize>) -> bool {
    let mut proposed_moves: HashMap<Point, Point> = HashMap::new();
    let mut move_counts: HashMap<Point, i32> = HashMap::new();

    for &elf in elves.iter() {
        let neighbors = elf.neighbors();
        if neighbors.iter().any(|p| elves.contains(p)) {
            for dir_idx in directions.iter() {
                let proposed_move = match dir_idx {
                    0 => { // North
                        if !elves.contains(&elf.northwest()) && !elves.contains(&elf.north()) && !elves.contains(&elf.northeast()){
                             Some(elf.north())
                        }
                        else{
                            None
                        }
                    },
                    1 => { // South
                        if !elves.contains(&elf.southwest()) && !elves.contains(&elf.south()) && !elves.contains(&elf.southeast()) {
                            Some(elf.south())
                        }
                        else{
                            None
                        }
                    },
                    2 => { // West
                        if !elves.contains(&elf.northwest()) && !elves.contains(&elf.west()) && !elves.contains(&elf.southwest()) {
                            Some(elf.west())
                        }
                        else{
                            None
                        }
                    },
                    3 => { // East
                        if !elves.contains(&elf.northeast()) && !elves.contains(&elf.east()) && !elves.contains(&elf.southeast()) {
                            Some(elf.east())
                        }
                         else{
                            None
                        }
                    },
                    _ => unreachable!(),
                };
            
                if let Some(move_to) = proposed_move {
                        proposed_moves.insert(elf, move_to);
                        *move_counts.entry(move_to).or_insert(0) += 1;
                        break;
                    }
                
            }
           
        }
    }


    let mut has_moved = false;
    let mut new_elves = HashSet::new();
    for &elf in elves.iter() {
        if let Some(&move_to) = proposed_moves.get(&elf){
            if move_counts[&move_to] == 1{
                new_elves.insert(move_to);
                has_moved = true;
                
            }
            else{
                new_elves.insert(elf);
            }
        }else{
             new_elves.insert(elf);
        }
    }
    *elves = new_elves;
    
    let first_dir = directions.remove(0);
    directions.push(first_dir);

    has_moved
}


fn calculate_empty_tiles(elves: &HashSet<Point>) -> i32 {
    if elves.is_empty() {
        return 0;
    }
    let min_x = elves.iter().map(|p| p.x).min().unwrap();
    let max_x = elves.iter().map(|p| p.x).max().unwrap();
    let min_y = elves.iter().map(|p| p.y).min().unwrap();
    let max_y = elves.iter().map(|p| p.y).max().unwrap();

    let width = max_x - min_x + 1;
    let height = max_y - min_y + 1;

    width * height - elves.len() as i32
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut elves = parse_input(&input);
    let mut directions = vec![0, 1, 2, 3]; 
    let mut round_count = 0;
    let part_1_rounds = 10;
    
    loop{
        round_count +=1;
        if !simulate_round(&mut elves, &mut directions) {
            break;
        }
        if round_count == part_1_rounds{
            println!("Part 1: {}", calculate_empty_tiles(&elves));
        }
    }
  
    println!("Part 2: {}", round_count);
}
