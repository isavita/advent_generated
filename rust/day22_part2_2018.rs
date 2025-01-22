
use std::{
    collections::{BinaryHeap, HashMap},
    fs::File,
    io::{self, BufRead},
    usize,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RegionType {
    Rocky,
    Wet,
    Narrow,
}

impl RegionType {
    fn from_erosion_level(erosion_level: u64) -> Self {
        match erosion_level % 3 {
            0 => RegionType::Rocky,
            1 => RegionType::Wet,
            2 => RegionType::Narrow,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Tool {
    ClimbingGear,
    Torch,
    Neither,
}

impl Tool {
    fn allowed_in(&self, region_type: RegionType) -> bool {
        match region_type {
            RegionType::Rocky => *self != Tool::Neither,
            RegionType::Wet => *self != Tool::Torch,
            RegionType::Narrow => *self != Tool::ClimbingGear,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct State {
    x: usize,
    y: usize,
    tool: Tool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Node {
    cost: usize,
    state: State,
}

fn solve_part1(depth: u64, target_x: usize, target_y: usize) -> u64 {
    let mut geologic_indices: HashMap<(usize, usize), u64> = HashMap::new();
    let mut erosion_levels: HashMap<(usize, usize), u64> = HashMap::new();

    for y in 0..=target_y {
        for x in 0..=target_x {
            let geologic_index = if x == 0 && y == 0 || (x == target_x && y == target_y) {
                0
            } else if y == 0 {
                x as u64 * 16807
            } else if x == 0 {
                y as u64 * 48271
            } else {
                let erosion_left = *erosion_levels.get(&(x - 1, y)).unwrap();
                let erosion_up = *erosion_levels.get(&(x, y - 1)).unwrap();
                erosion_left * erosion_up
            };

            geologic_indices.insert((x, y), geologic_index);

            let erosion_level = (geologic_index + depth) % 20183;
            erosion_levels.insert((x, y), erosion_level);
        }
    }

    let mut risk_level = 0;
    for y in 0..=target_y {
        for x in 0..=target_x {
            let region_type = RegionType::from_erosion_level(*erosion_levels.get(&(x, y)).unwrap());
            risk_level += match region_type {
                RegionType::Rocky => 0,
                RegionType::Wet => 1,
                RegionType::Narrow => 2,
            };
        }
    }

    risk_level
}

fn solve_part2(depth: u64, target_x: usize, target_y: usize) -> usize {
    let mut geologic_indices: HashMap<(usize, usize), u64> = HashMap::new();
    let mut erosion_levels: HashMap<(usize, usize), u64> = HashMap::new();
    let max_x = target_x + 50;
    let max_y = target_y + 50;

    for y in 0..=max_y {
        for x in 0..=max_x {
            let geologic_index = if x == 0 && y == 0 || (x == target_x && y == target_y) {
                0
            } else if y == 0 {
                x as u64 * 16807
            } else if x == 0 {
                y as u64 * 48271
            } else {
                let erosion_left = *erosion_levels.get(&(x - 1, y)).unwrap();
                let erosion_up = *erosion_levels.get(&(x, y - 1)).unwrap();
                erosion_left * erosion_up
            };

            geologic_indices.insert((x, y), geologic_index);
            let erosion_level = (geologic_index + depth) % 20183;
            erosion_levels.insert((x, y), erosion_level);
        }
    }

    let start_state = State {
        x: 0,
        y: 0,
        tool: Tool::Torch,
    };
    let target_state = State {
        x: target_x,
        y: target_y,
        tool: Tool::Torch,
    };

    let mut dist: HashMap<State, usize> = HashMap::new();
    let mut queue: BinaryHeap<std::cmp::Reverse<Node>> = BinaryHeap::new();

    dist.insert(start_state, 0);
    queue.push(std::cmp::Reverse(Node {
        cost: 0,
        state: start_state,
    }));

    while let Some(std::cmp::Reverse(current_node)) = queue.pop() {
        let current_cost = current_node.cost;
        let current_state = current_node.state;

        if current_state == target_state {
            return current_cost;
        }

        if let Some(cached_cost) = dist.get(&current_state) {
            if current_cost > *cached_cost {
                continue;
            }
        }

        let current_region_type = RegionType::from_erosion_level(
            *erosion_levels
                .get(&(current_state.x, current_state.y))
                .unwrap(),
        );

        for next_tool in [
            Tool::ClimbingGear,
            Tool::Torch,
            Tool::Neither,
        ] {
            if next_tool.allowed_in(current_region_type) {
              
                let new_cost = if current_state.tool == next_tool {
                  current_cost
                } else {
                  current_cost + 7
                };
              
              let next_state = State {
                  x: current_state.x,
                  y: current_state.y,
                  tool: next_tool
              };
              
              if dist.get(&next_state).map_or(true, |&c| new_cost < c) {
                dist.insert(next_state, new_cost);
                queue.push(std::cmp::Reverse(Node {
                  cost: new_cost,
                  state: next_state
                }));
              }
            }
        }
      
        
        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
          let nx = current_state.x as isize + dx;
          let ny = current_state.y as isize + dy;
          
          if nx < 0 || ny < 0 {
            continue;
          }
          
          let nx = nx as usize;
          let ny = ny as usize;
          
          if nx > max_x || ny > max_y {
              continue;
          }
            
          let next_region_type = RegionType::from_erosion_level(
            *erosion_levels
                .get(&(nx, ny))
                .unwrap(),
          );

          if current_state.tool.allowed_in(next_region_type) {
            let next_state = State {
                x: nx,
                y: ny,
                tool: current_state.tool
            };

            let new_cost = current_cost + 1;

            if dist.get(&next_state).map_or(true, |&c| new_cost < c) {
                dist.insert(next_state, new_cost);
                queue.push(std::cmp::Reverse(Node {
                  cost: new_cost,
                  state: next_state
                }));
            }
          }
      }
    }
    usize::MAX
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let depth = lines
        .next()
        .unwrap()?
        .split(": ")
        .nth(1)
        .unwrap()
        .parse::<u64>()
        .unwrap();

    let target_coords = lines.next().unwrap()?;
    let target_coords_split = target_coords.split(": ").nth(1).unwrap();
    let mut target_coords_iter = target_coords_split.split(",");
    let target_x = target_coords_iter.next().unwrap().parse::<usize>().unwrap();
    let target_y = target_coords_iter.next().unwrap().parse::<usize>().unwrap();

    let part1_result = solve_part1(depth, target_x, target_y);
    println!("Part 1 Result: {}", part1_result);

    let part2_result = solve_part2(depth, target_x, target_y);
    println!("Part 2 Result: {}", part2_result);

    Ok(())
}
