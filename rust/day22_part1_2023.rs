
use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Debug, Clone)]
struct Brick {
    start: Point,
    end: Point,
    points: Vec<Point>,
}

impl Brick {
    fn from_line(line: &str) -> Self {
        let parts: Vec<&str> = line.split('~').collect();
        let start_coords: Vec<i32> = parts[0].split(',').map(|s| s.parse().unwrap()).collect();
        let end_coords: Vec<i32> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();
        let start = Point {
            x: start_coords[0],
            y: start_coords[1],
            z: start_coords[2],
        };
        let end = Point {
            x: end_coords[0],
            y: end_coords[1],
            z: end_coords[2],
        };

        let mut points = Vec::new();
        if start.x != end.x {
            for x in start.x.min(end.x)..=start.x.max(end.x) {
                points.push(Point {
                    x,
                    y: start.y,
                    z: start.z,
                });
            }
        } else if start.y != end.y {
             for y in start.y.min(end.y)..=start.y.max(end.y) {
                points.push(Point {
                    x: start.x,
                    y,
                    z: start.z,
                });
             }
        } else {
           for z in start.z.min(end.z)..=start.z.max(end.z) {
                points.push(Point {
                    x: start.x,
                    y: start.y,
                    z,
                });
            }
        }

        Brick { start, end, points }
    }

    fn move_down(&mut self, dz: i32) {
        self.start.z -= dz;
        self.end.z -= dz;
        for point in &mut self.points {
            point.z -= dz;
        }
    }
}

fn settle_bricks(bricks: &mut Vec<Brick>) {
    bricks.sort_by_key(|brick| brick.start.z);

    let mut occupied: HashMap<(i32, i32, i32), usize> = HashMap::new();

    for i in 0..bricks.len() {
        let mut brick = bricks[i].clone();
        let mut fall_dist = 0;
        loop {
             let mut can_fall = true;
             if brick.points.iter().any(|p| p.z <= 1) {
                 can_fall = false;
             }
           if can_fall {
            for point in &brick.points {
                if occupied.contains_key(&(point.x, point.y, point.z - 1)){
                    can_fall = false;
                    break;
                }
            }
          }
            if can_fall {
              fall_dist +=1;
               brick.move_down(1);
            } else {
                break;
            }

        }
         bricks[i].move_down(fall_dist);
        for point in &bricks[i].points {
            occupied.insert((point.x, point.y, point.z), i);
        }
    }
}
fn get_supports(bricks: &Vec<Brick>) -> (HashMap<usize, HashSet<usize>>, HashMap<usize, HashSet<usize>>) {
    let mut supports: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut supported_by: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut occupied: HashMap<(i32, i32, i32), usize> = HashMap::new();

      for (i, brick) in bricks.iter().enumerate() {
        for point in &brick.points {
            occupied.insert((point.x, point.y, point.z), i);
        }
    }


    for (i, brick) in bricks.iter().enumerate(){
        for point in &brick.points {
            if let Some(supporting_brick_index) = occupied.get(&(point.x,point.y, point.z-1)) {
                if supporting_brick_index != &i {
                  supports.entry(*supporting_brick_index).or_insert(HashSet::new()).insert(i);
                   supported_by.entry(i).or_insert(HashSet::new()).insert(*supporting_brick_index);
                }

            }
        }
    }
    (supports, supported_by)
}


fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut bricks: Vec<Brick> = contents.lines().map(Brick::from_line).collect();

     settle_bricks(&mut bricks);

    let (supports, supported_by) = get_supports(&bricks);


    let mut safe_to_disintegrate = 0;
    for i in 0..bricks.len() {
        let supported_bricks = supports.get(&i).cloned().unwrap_or_default();
         let mut all_supported = true;

        for supported_brick in supported_bricks {
           if supported_by.get(&supported_brick).unwrap().len() == 1{
               all_supported = false;
               break;
           }
        }
        if all_supported {
           safe_to_disintegrate += 1;
        }
    }
    println!("{}", safe_to_disintegrate);


}
