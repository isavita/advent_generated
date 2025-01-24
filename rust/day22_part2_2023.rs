
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead, BufReader};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
    z: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Brick {
    start: Point,
    end: Point,
}

impl Brick {
    fn from_str(s: &str) -> Self {
        let parts: Vec<&str> = s.split('~').collect();
        let start_coords: Vec<usize> = parts[0].split(',').map(|x| x.parse().unwrap()).collect();
        let end_coords: Vec<usize> = parts[1].split(',').map(|x| x.parse().unwrap()).collect();

        Brick {
            start: Point {
                x: start_coords[0],
                y: start_coords[1],
                z: start_coords[2],
            },
            end: Point {
                x: end_coords[0],
                y: end_coords[1],
                z: end_coords[2],
            },
        }
    }

    fn get_all_points(&self) -> Vec<Point> {
        let mut points = Vec::new();
        for x in cmp::min(self.start.x, self.end.x)..=cmp::max(self.start.x, self.end.x) {
            for y in cmp::min(self.start.y, self.end.y)..=cmp::max(self.start.y, self.end.y) {
                for z in cmp::min(self.start.z, self.end.z)..=cmp::max(self.start.z, self.end.z) {
                    points.push(Point { x, y, z });
                }
            }
        }
        points
    }
}

fn settle_bricks(bricks: &mut Vec<Brick>) {
    bricks.sort_by_key(|b| cmp::min(b.start.z, b.end.z));

    let mut occupied: HashSet<Point> = HashSet::new();

    for i in 0..bricks.len() {
        let mut current_points = bricks[i].get_all_points();
        let mut can_move_down = true;
        while can_move_down {
            let mut next_points = Vec::new();
            let mut can_move = true;

            for point in &current_points {
                if point.z == 1 {
                    can_move = false;
                    break;
                }

                let next_point = Point {
                    x: point.x,
                    y: point.y,
                    z: point.z - 1,
                };

                if occupied.contains(&next_point) {
                    can_move = false;
                    break;
                }
                next_points.push(next_point);
            }
            if can_move {
                current_points = next_points;
            } else {
                can_move_down = false;
            }
        }
        for point in &current_points {
            occupied.insert(point.clone());
        }

        bricks[i].start.z = current_points[0].z;
        bricks[i].end.z = current_points.last().unwrap().z;
    }
}

fn can_disintegrate(
    brick_index: usize,
    bricks: &Vec<Brick>,
    supported_by: &HashMap<usize, HashSet<usize>>,
    supports: &HashMap<usize, HashSet<usize>>,
) -> bool {
    if let Some(supported) = supports.get(&brick_index) {
        for &supported_brick in supported.iter() {
            if let Some(supporters) = supported_by.get(&supported_brick) {
                if supporters.len() == 1 && supporters.contains(&brick_index) {
                    return false;
                }
            }
        }
    }
    true
}

fn calculate_fallen_bricks(
    brick_index: usize,
    bricks: &Vec<Brick>,
    supported_by: &HashMap<usize, HashSet<usize>>,
    supports: &HashMap<usize, HashSet<usize>>,
) -> usize {
    let mut fallen: HashSet<usize> = HashSet::new();
    fallen.insert(brick_index);
    let mut q: Vec<usize> = vec![brick_index];

    while let Some(current_brick) = q.pop() {
        if let Some(supported) = supports.get(&current_brick) {
            for &supported_brick in supported {
                if let Some(supporters) = supported_by.get(&supported_brick) {
                    let mut all_fallen = true;
                    for &supporter in supporters {
                        if !fallen.contains(&supporter) {
                            all_fallen = false;
                            break;
                        }
                    }
                    if all_fallen && !fallen.contains(&supported_brick) {
                        fallen.insert(supported_brick);
                        q.push(supported_brick);
                    }
                }
            }
        }
    }

    fallen.len() - 1
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut bricks: Vec<Brick> = reader.lines().map(|line| Brick::from_str(&line.unwrap())).collect();

    settle_bricks(&mut bricks);

    let mut supported_by: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut supports: HashMap<usize, HashSet<usize>> = HashMap::new();

    for i in 0..bricks.len() {
        for j in 0..bricks.len() {
            if i == j {
                continue;
            }

            let points_i = bricks[i].get_all_points();
            let points_j = bricks[j].get_all_points();

            for point_i in &points_i {
                for point_j in &points_j {
                    if point_i.x == point_j.x && point_i.y == point_j.y && point_i.z + 1 == point_j.z {
                        supports.entry(i).or_default().insert(j);
                        supported_by.entry(j).or_default().insert(i);
                    }
                }
            }
        }
    }

    let mut safe_to_disintegrate_count = 0;
    for i in 0..bricks.len() {
        if can_disintegrate(i, &bricks, &supported_by, &supports) {
            safe_to_disintegrate_count += 1;
        }
    }

    println!("Part 1: {}", safe_to_disintegrate_count);

    let mut total_fallen = 0;
    for i in 0..bricks.len() {
        total_fallen += calculate_fallen_bricks(i, &bricks, &supported_by, &supports);
    }
    println!("Part 2: {}", total_fallen);

    Ok(())
}
