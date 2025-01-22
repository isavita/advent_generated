
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

fn parse_input(filename: &str) -> (HashSet<Point>, i32) {
    let file = File::open(filename).expect("Unable to open file");
    let reader = BufReader::new(file);
    let mut rocks: HashSet<Point> = HashSet::new();
    let mut max_y = 0;

    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        let points: Vec<Point> = line
            .split(" -> ")
            .map(|s| {
                let coords: Vec<i32> = s
                    .split(',')
                    .map(|coord| coord.parse::<i32>().unwrap())
                    .collect();
                Point {
                    x: coords[0],
                    y: coords[1],
                }
            })
            .collect();

        for i in 0..points.len() - 1 {
            let start = points[i];
            let end = points[i + 1];

            if start.x == end.x {
                let y_start = start.y.min(end.y);
                let y_end = start.y.max(end.y);
                for y in y_start..=y_end {
                    rocks.insert(Point { x: start.x, y });
                    max_y = max_y.max(y);
                }
            } else if start.y == end.y {
                let x_start = start.x.min(end.x);
                let x_end = start.x.max(end.x);
                for x in x_start..=x_end {
                    rocks.insert(Point { x, y: start.y });
                }
            }
        }
    }

    (rocks, max_y)
}

fn simulate_sand(rocks: &mut HashSet<Point>, max_y: i32, part2: bool) -> i32 {
    let mut sand_count = 0;
    let sand_source = Point { x: 500, y: 0 };
    let floor_y = max_y + 2;

    'outer: loop {
        let mut sand_pos = sand_source;
        loop {
            if part2 && sand_pos == sand_source && rocks.contains(&sand_pos) {
                break 'outer;
            }

            if !part2 && sand_pos.y > max_y {
                break 'outer;
            }
             
            let next_down = Point {
                x: sand_pos.x,
                y: sand_pos.y + 1,
            };
            let next_down_left = Point {
                x: sand_pos.x - 1,
                y: sand_pos.y + 1,
            };
            let next_down_right = Point {
                x: sand_pos.x + 1,
                y: sand_pos.y + 1,
            };

            if !part2 || next_down.y < floor_y {
                if !rocks.contains(&next_down) {
                    sand_pos = next_down;
                    continue;
                }
            }
            if !part2 || next_down_left.y < floor_y {
                if !rocks.contains(&next_down_left) {
                     sand_pos = next_down_left;
                     continue;
                }
            }
             if !part2 || next_down_right.y < floor_y {
                if !rocks.contains(&next_down_right){
                    sand_pos = next_down_right;
                    continue;
                }
            }
            rocks.insert(sand_pos);
            sand_count += 1;
            break;
        }
    }
    sand_count
}

fn main() {
    let (mut rocks, max_y) = parse_input("input.txt");

    let sand_count_part1 = simulate_sand(&mut rocks.clone(), max_y, false);
    println!("Part 1: {}", sand_count_part1);

    let sand_count_part2 = simulate_sand(&mut rocks, max_y, true);
    println!("Part 2: {}", sand_count_part2);
}
