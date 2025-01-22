
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{self, Write};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn to_int(&self) -> i64 {
        match self {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }

    fn opposite(&self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
            Direction::East => Direction::West,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Tile {
    Wall,
    Open,
    Oxygen,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn new(x: i64, y: i64) -> Self {
        Point { x, y }
    }

    fn move_direction(&self, dir: &Direction) -> Point {
        match dir {
            Direction::North => Point::new(self.x, self.y + 1),
            Direction::South => Point::new(self.x, self.y - 1),
            Direction::West => Point::new(self.x - 1, self.y),
            Direction::East => Point::new(self.x + 1, self.y),
        }
    }
}

struct Intcode {
    memory: Vec<i64>,
    pc: usize,
    relative_base: i64,
    input_buffer: VecDeque<i64>,
}

impl Intcode {
    fn new(program: &str) -> Intcode {
        let memory: Vec<i64> = program
            .trim()
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();
        Intcode {
            memory,
            pc: 0,
            relative_base: 0,
            input_buffer: VecDeque::new(),
        }
    }

    fn get_parameter(&mut self, mode: i64, offset: usize) -> i64 {
        let val = self.memory[self.pc + offset] as usize;
        match mode {
            0 => {
                if val >= self.memory.len() {
                    0
                }else {
                     self.memory[val]
                }
            },
            1 => val as i64,
            2 => {
                let addr = (self.relative_base + val as i64) as usize;
                if addr >= self.memory.len(){
                    0
                } else {
                    self.memory[addr]
                }
            }
            _ => panic!("Invalid parameter mode: {}", mode),
        }
    }


    fn get_parameter_address(&mut self, mode: i64, offset: usize) -> usize {
        let val = self.memory[self.pc + offset] as usize;
          match mode {
            0 => val,
            2 => (self.relative_base + val as i64) as usize,
            _ => panic!("Invalid parameter mode: {}", mode),
        }
    }

    fn run(&mut self, input: Option<i64>) -> Option<i64> {
        if let Some(val) = input {
            self.input_buffer.push_back(val);
        }

        loop {
            let opcode = self.memory[self.pc] % 100;
            let mode1 = (self.memory[self.pc] / 100) % 10;
            let mode2 = (self.memory[self.pc] / 1000) % 10;
            let mode3 = (self.memory[self.pc] / 10000) % 10;


            match opcode {
                1 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let dest = self.get_parameter_address(mode3, 3);

                    if dest >= self.memory.len(){
                        self.memory.resize(dest+1,0);
                    }
                    self.memory[dest] = param1 + param2;
                    self.pc += 4;
                }
                2 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                     let dest = self.get_parameter_address(mode3, 3);
                    if dest >= self.memory.len(){
                        self.memory.resize(dest+1,0);
                    }
                    self.memory[dest] = param1 * param2;
                    self.pc += 4;
                }
                3 => {
                    if self.input_buffer.is_empty() {
                         return None;
                    }

                     let dest = self.get_parameter_address(mode1, 1);
                     if dest >= self.memory.len(){
                        self.memory.resize(dest+1,0);
                    }

                    self.memory[dest] = self.input_buffer.pop_front().unwrap();
                    self.pc += 2;
                }
                4 => {
                    let output = self.get_parameter(mode1, 1);
                    self.pc += 2;
                    return Some(output);
                }
                 5 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 != 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 == 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                 7 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let dest = self.get_parameter_address(mode3, 3);

                    if dest >= self.memory.len(){
                        self.memory.resize(dest+1,0);
                    }
                    self.memory[dest] = if param1 < param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                 8 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                     let dest = self.get_parameter_address(mode3, 3);
                    if dest >= self.memory.len(){
                        self.memory.resize(dest+1,0);
                    }
                    self.memory[dest] = if param1 == param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                9 => {
                    let param1 = self.get_parameter(mode1, 1);
                    self.relative_base += param1;
                    self.pc += 2;
                }
                99 => {
                    return None;
                }
                _ => panic!("Unknown opcode: {}", opcode),
            }
        }
    }
}

fn explore_maze(program: &str) -> (HashMap<Point, Tile>, Point) {
    let mut maze: HashMap<Point, Tile> = HashMap::new();
    let mut droid_pos = Point::new(0, 0);
    let mut intcode = Intcode::new(program);
    let mut oxygen_pos = Point::new(0,0);
     maze.insert(droid_pos, Tile::Open);
    let mut path = Vec::new();
    let mut backtrack_dir: Option<Direction> = None;


    loop {
        let mut moved = false;
        for dir in [Direction::North, Direction::South, Direction::West, Direction::East] {
            let next_pos = droid_pos.move_direction(&dir);
           if !maze.contains_key(&next_pos){
                
                 let output = intcode.run(Some(dir.to_int())).unwrap();
                 match output {
                    0 => {maze.insert(next_pos, Tile::Wall);
                    }
                    1 => {
                        maze.insert(next_pos, Tile::Open);
                        droid_pos = next_pos;
                        path.push(dir);
                         moved = true;
                         break;
                    },
                    2 => {
                         maze.insert(next_pos, Tile::Oxygen);
                        droid_pos = next_pos;
                         oxygen_pos = next_pos;
                        path.push(dir);
                         moved = true;
                        break;
                    }
                    _ => panic!("Unexpected output from intcode"),
                }

            }
        }

          if !moved{
             if let Some(prev_dir) = path.pop(){
                    let output = intcode.run(Some(prev_dir.opposite().to_int())).unwrap();
                    if output != 1{
                        panic!("Unexpected output from intcode during backtrack")
                    }
                    droid_pos = droid_pos.move_direction(&prev_dir.opposite());
             }else{
                break;
            }

        }
      

    }
    (maze, oxygen_pos)
}

fn find_shortest_path(maze: &HashMap<Point, Tile>, start: &Point, end: &Point) -> i32 {
    let mut visited: HashMap<Point, i32> = HashMap::new();
    let mut queue: VecDeque<(Point, i32)> = VecDeque::new();

    queue.push_back((*start, 0));
    visited.insert(*start, 0);

    while let Some((curr_pos, dist)) = queue.pop_front() {
        if curr_pos == *end {
            return dist;
        }

        for dir in [Direction::North, Direction::South, Direction::West, Direction::East] {
            let next_pos = curr_pos.move_direction(&dir);
            if let Some(Tile::Open) | Some(Tile::Oxygen) = maze.get(&next_pos){
                    if !visited.contains_key(&next_pos){
                        visited.insert(next_pos, dist + 1);
                        queue.push_back((next_pos, dist+1));
                    }
            }

        }
    }

    -1 // Should not reach here if there is a path
}


fn calculate_oxygen_fill_time(maze: &HashMap<Point, Tile>, oxygen_pos: &Point) -> i32 {
    let mut current_oxygen: HashMap<Point, bool> = HashMap::new();
    let mut open_spaces_count = 0;
    for (pos, tile) in maze{
        if *tile == Tile::Open {
            open_spaces_count +=1;
        }

    }
    current_oxygen.insert(*oxygen_pos, true);
     if open_spaces_count == 0{
        return 0;
    }


    let mut minutes = 0;
    while current_oxygen.len() < open_spaces_count + 1{
        minutes +=1;
        let mut new_oxygen: HashMap<Point, bool> = current_oxygen.clone();
        for (oxygen_pos, _) in current_oxygen.iter(){
            for dir in [Direction::North, Direction::South, Direction::West, Direction::East]{
                 let next_pos = oxygen_pos.move_direction(&dir);
                if let Some(Tile::Open) = maze.get(&next_pos){
                    if !new_oxygen.contains_key(&next_pos){
                        new_oxygen.insert(next_pos, true);
                    }

                }

            }

        }
        current_oxygen = new_oxygen;
    }
    minutes
}



fn main() -> io::Result<()> {
    let contents = fs::read_to_string("input.txt")?;

    let (maze, oxygen_pos) = explore_maze(&contents);
    let start_pos = Point::new(0, 0);

    let shortest_path = find_shortest_path(&maze, &start_pos, &oxygen_pos);

     println!("Shortest path to oxygen system: {}", shortest_path);


    let fill_time = calculate_oxygen_fill_time(&maze, &oxygen_pos);
     println!("Time to fill with oxygen: {}", fill_time);


    Ok(())
}
