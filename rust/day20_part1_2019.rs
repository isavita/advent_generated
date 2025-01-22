
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

fn read_maze(filename: &str) -> io::Result<(Vec<String>, Point, Point, HashMap<String, Vec<Point>>)> {
    let file = File::open(filename)?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let mut start = Point { x: 0, y: 0 };
    let mut end = Point { x: 0, y: 0 };
    let mut portals: HashMap<String, Vec<Point>> = HashMap::new();
    let height = lines.len();
    let width = lines[0].len();

    for y in 0..height {
        let line = &lines[y];
        for x in 0..width {
            let c = line.chars().nth(x).unwrap();
            if c == '.' {
                let mut label = String::new();
                // Check for adjacent labels
                if y > 0 && lines[y - 1].chars().nth(x).unwrap().is_ascii_uppercase() {
                    if y > 1 && lines[y - 2].chars().nth(x).unwrap().is_ascii_uppercase(){
                      label.push(lines[y-2].chars().nth(x).unwrap());
                       label.push(lines[y-1].chars().nth(x).unwrap());
                       if label == "AA" {
                           start = Point { x, y };
                       } else if label == "ZZ"{
                           end = Point {x,y}
                       } else{
                       portals.entry(label).or_insert_with(Vec::new).push(Point { x, y });
                       }
                    }
                } else if y < height - 1 && lines[y+1].chars().nth(x).unwrap().is_ascii_uppercase(){
                    if y < height - 2 && lines[y+2].chars().nth(x).unwrap().is_ascii_uppercase(){
                         label.push(lines[y+1].chars().nth(x).unwrap());
                       label.push(lines[y+2].chars().nth(x).unwrap());
                         if label == "AA" {
                           start = Point { x, y };
                       } else if label == "ZZ"{
                           end = Point {x,y}
                       } else{
                       portals.entry(label).or_insert_with(Vec::new).push(Point { x, y });
                       }
                    }
                } else if x > 0 && line.chars().nth(x-1).unwrap().is_ascii_uppercase(){
                     if x > 1 && line.chars().nth(x-2).unwrap().is_ascii_uppercase() {
                         label.push(line.chars().nth(x-2).unwrap());
                       label.push(line.chars().nth(x-1).unwrap());
                       if label == "AA" {
                           start = Point { x, y };
                       } else if label == "ZZ"{
                           end = Point {x,y}
                       } else{
                       portals.entry(label).or_insert_with(Vec::new).push(Point { x, y });
                       }
                     }
                } else if x < width -1 && line.chars().nth(x+1).unwrap().is_ascii_uppercase() {
                   if x < width - 2 && line.chars().nth(x+2).unwrap().is_ascii_uppercase() {
                       label.push(line.chars().nth(x+1).unwrap());
                       label.push(line.chars().nth(x+2).unwrap());
                        if label == "AA" {
                           start = Point { x, y };
                       } else if label == "ZZ"{
                           end = Point {x,y}
                       } else{
                       portals.entry(label).or_insert_with(Vec::new).push(Point { x, y });
                       }
                   }
                }
            }
        }
    }
    Ok((lines, start, end, portals))
}


fn solve_maze(
    maze: &Vec<String>,
    start: Point,
    end: Point,
    portals: &HashMap<String, Vec<Point>>,
) -> Option<usize> {
    let height = maze.len();
    let width = maze[0].len();

    let mut visited: HashSet<Point> = HashSet::new();
    let mut queue: VecDeque<(Point, usize)> = VecDeque::new();
    queue.push_back((start, 0));
    visited.insert(start);

    while let Some((current, steps)) = queue.pop_front() {
        if current == end {
            return Some(steps);
        }

        let neighbors = vec![
            Point {
                x: current.x.wrapping_sub(1),
                y: current.y,
            },
            Point {
                x: current.x + 1,
                y: current.y,
            },
            Point {
                x: current.x,
                y: current.y.wrapping_sub(1),
            },
            Point {
                x: current.x,
                y: current.y + 1,
            },
        ];

        for neighbor in neighbors {
           if neighbor.x < width && neighbor.y < height {
                if maze[neighbor.y].chars().nth(neighbor.x).unwrap() == '.' && visited.insert(neighbor) {
                    queue.push_back((neighbor, steps + 1));
                 }
           }
        }

        // Check for portals
        for (label, points) in portals {
            if points.contains(&current) {
               let other_point = points.iter().find(|&p| *p != current).unwrap();
                    if visited.insert(*other_point) {
                        queue.push_back((*other_point, steps + 1));
                    }
             }
        }

    }

    None
}

fn main() -> io::Result<()> {
    let (maze, start, end, portals) = read_maze("input.txt")?;
    
    if let Some(steps) = solve_maze(&maze, start, end, &portals) {
        println!("{}", steps);
    } else {
        println!("No path found.");
    }
    Ok(())
}
