
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnitType {
    Elf,
    Goblin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Unit {
    unit_type: UnitType,
    hp: i32,
    attack_power: i32,
    position: Point,
}

fn read_input(filename: &str) -> (Vec<Vec<char>>, HashMap<Point, Unit>) {
    let file = File::open(filename).expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut grid = Vec::new();
    let mut units = HashMap::new();

    for (y, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line");
        let mut row = Vec::new();
        for (x, c) in line.chars().enumerate() {
            let point = Point { x, y };
            match c {
                'E' => {
                    units.insert(
                        point,
                        Unit {
                            unit_type: UnitType::Elf,
                            hp: 200,
                            attack_power: 3,
                            position: point,
                        },
                    );
                    row.push('.');
                }
                'G' => {
                    units.insert(
                        point,
                        Unit {
                            unit_type: UnitType::Goblin,
                            hp: 200,
                            attack_power: 3,
                            position: point,
                        },
                    );
                    row.push('.');
                }
                _ => row.push(c),
            }
        }
        grid.push(row);
    }

    (grid, units)
}

fn get_neighbors(point: Point, grid: &[Vec<char>]) -> Vec<Point> {
    let mut neighbors = Vec::new();
    let (x, y) = (point.x, point.y);

    if y > 0 {
        neighbors.push(Point { x, y: y - 1 });
    }
    if x > 0 {
        neighbors.push(Point { x: x - 1, y });
    }
    if x < grid[0].len() - 1 {
        neighbors.push(Point { x: x + 1, y });
    }
    if y < grid.len() - 1 {
        neighbors.push(Point { x, y: y + 1 });
    }

    neighbors
}

fn bfs(
    grid: &[Vec<char>],
    start: Point,
    targets: &HashSet<Point>,
    units: &HashMap<Point, Unit>,
) -> Option<(Point, Vec<Point>)> {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut paths: HashMap<Point, Vec<Point>> = HashMap::new();

    queue.push_back(start);
    visited.insert(start);
    paths.insert(start, vec![start]);

    while let Some(current) = queue.pop_front() {
        if targets.contains(&current) {
            return Some((current, paths[&current].clone()));
        }

        for neighbor in get_neighbors(current, grid) {
            if !visited.contains(&neighbor)
                && grid[neighbor.y][neighbor.x] == '.'
                && !units.contains_key(&neighbor)
            {
                visited.insert(neighbor);
                queue.push_back(neighbor);
                let mut new_path = paths[&current].clone();
                new_path.push(neighbor);
                paths.insert(neighbor, new_path);
            }
        }
    }

    None
}

fn combat(
    mut grid: Vec<Vec<char>>,
    mut units: HashMap<Point, Unit>,
    elf_attack_power: i32,
    stop_on_elf_death: bool
) -> (i32, i32, bool) {
    let mut rounds = 0;
    let mut elf_died = false;

    loop {
        let mut unit_positions: Vec<Point> = units.keys().cloned().collect();
        unit_positions.sort_by(|a, b| (a.y, a.x).cmp(&(b.y, b.x)));

        for unit_position in unit_positions.iter() {
            if !units.contains_key(unit_position) {
                continue; 
            }

            let unit = *units.get(unit_position).unwrap();

            let enemy_type = match unit.unit_type {
                UnitType::Elf => UnitType::Goblin,
                UnitType::Goblin => UnitType::Elf,
            };

            let enemies: HashMap<Point, Unit> = units
                .iter()
                .filter(|(_, u)| u.unit_type == enemy_type)
                .map(|(p, u)| (*p, *u))
                .collect();

            if enemies.is_empty() {
                let total_hp: i32 = units.values().map(|u| u.hp).sum();
                return (rounds, total_hp, elf_died);
            }
            
            // Attack if in range
            let mut adjacent_enemies: Vec<Point> = get_neighbors(*unit_position, &grid)
                .into_iter()
                .filter(|p| enemies.contains_key(p))
                .collect();
            
            if !adjacent_enemies.is_empty() {
                adjacent_enemies.sort_by(|a, b| {
                   let unit_a = enemies.get(a).unwrap();
                   let unit_b = enemies.get(b).unwrap();
                   (unit_a.hp, a.y, a.x).cmp(&(unit_b.hp, b.y, b.x)) 
                });
                
                let target_pos = adjacent_enemies[0];
                let mut target = *units.get(&target_pos).unwrap();
                let attack = if unit.unit_type == UnitType::Elf { elf_attack_power } else { unit.attack_power};
                target.hp -= attack;
                
                if target.hp <= 0 {
                    if stop_on_elf_death && target.unit_type == UnitType::Elf {
                       elf_died = true;
                       return (rounds, 0, elf_died);
                    }
                    units.remove(&target_pos);
                } else {
                    units.insert(target_pos, target);
                }
                continue; // Skip moving if we attacked.
            }


            // Move
            let in_range_positions: HashSet<Point> = enemies
                .keys()
                .flat_map(|p| get_neighbors(*p, &grid))
                .filter(|p| grid[p.y][p.x] == '.' && !units.contains_key(p))
                .collect();

            if in_range_positions.is_empty() {
                continue;
            }

            if let Some((nearest_target, path)) = bfs(&grid, *unit_position, &in_range_positions, &units) {
                if path.len() > 1 {
                    let next_step = path[1];
                    let mut moved_unit = *units.get(unit_position).unwrap();
                    moved_unit.position = next_step;
                    units.remove(unit_position);
                    units.insert(next_step, moved_unit);


                    // Attack after move
                    let mut adjacent_enemies: Vec<Point> = get_neighbors(next_step, &grid)
                        .into_iter()
                        .filter(|p| enemies.contains_key(p))
                        .collect();
                    
                    if !adjacent_enemies.is_empty() {
                        adjacent_enemies.sort_by(|a, b| {
                           let unit_a = enemies.get(a).unwrap();
                           let unit_b = enemies.get(b).unwrap();
                           (unit_a.hp, a.y, a.x).cmp(&(unit_b.hp, b.y, b.x)) 
                        });
                        
                        let target_pos = adjacent_enemies[0];
                        let mut target = *units.get(&target_pos).unwrap();
                        let attack = if moved_unit.unit_type == UnitType::Elf { elf_attack_power } else { moved_unit.attack_power};

                        target.hp -= attack;
                        
                        if target.hp <= 0 {
                           if stop_on_elf_death && target.unit_type == UnitType::Elf {
                               elf_died = true;
                               return (rounds, 0, elf_died);
                           }
                            units.remove(&target_pos);
                        } else {
                            units.insert(target_pos, target);
                        }
                    }
                }
            }
        }
        rounds += 1;
    }
}


fn part1(grid: &[Vec<char>], units: &HashMap<Point, Unit>) -> i32 {
    let (rounds, total_hp, _) = combat(grid.to_vec(), units.clone(), 3, false);
    rounds * total_hp
}

fn part2(grid: &[Vec<char>], units: &HashMap<Point, Unit>) -> i32 {
    let mut attack_power = 4;
    loop {
        let (rounds, total_hp, elf_died) = combat(grid.to_vec(), units.clone(), attack_power, true);
        if !elf_died {
            return rounds * total_hp
        }
        attack_power+=1;
    }
}


fn main() {
    let (grid, units) = read_input("input.txt");
    let result1 = part1(&grid, &units);
    println!("{}", result1);
    let result2 = part2(&grid, &units);
    println!("{}", result2);
}
