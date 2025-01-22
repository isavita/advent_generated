
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

impl FromStr for Point {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(|c| c == '<' || c == '>' || c == ',').collect();
        
        let x = parts[1].trim().parse::<i32>().unwrap();
        let y = parts[2].trim().parse::<i32>().unwrap();
        let vx = parts[4].trim().parse::<i32>().unwrap();
        let vy = parts[5].trim().parse::<i32>().unwrap();
    
        Ok(Point { x, y, vx, vy })
    }
}

fn calculate_bounds(points: &[Point]) -> ((i32, i32), (i32, i32)) {
    let mut min_x = i32::MAX;
    let mut max_x = i32::MIN;
    let mut min_y = i32::MAX;
    let mut max_y = i32::MIN;

    for point in points {
        min_x = min_x.min(point.x);
        max_x = max_x.max(point.x);
        min_y = min_y.min(point.y);
        max_y = max_y.max(point.y);
    }

    ((min_x, min_y), (max_x, max_y))
}

fn print_points(points: &[Point]) {
    let ((min_x, min_y), (max_x, max_y)) = calculate_bounds(points);

    let width = (max_x - min_x + 1) as usize;
    let height = (max_y - min_y + 1) as usize;

    if width > 200 || height > 200 {
      return;
    }

    let mut grid = vec![vec!['.'; width]; height];

    for point in points {
        let x = (point.x - min_x) as usize;
        let y = (point.y - min_y) as usize;
        grid[y][x] = '#';
    }

    for row in grid {
        let row_str: String = row.into_iter().collect();
        println!("{}", row_str);
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut points: Vec<Point> = reader
        .lines()
        .filter_map(Result::ok)
        .filter_map(|line| Point::from_str(&line).ok())
        .collect();

    let mut min_area = i64::MAX;
    let mut best_time = 0;

    for time in 0..12000 {
        let mut current_points = points.clone();
        for point in &mut current_points {
          point.x += point.vx * time;
          point.y += point.vy * time;
        }

        let ((min_x, min_y), (max_x, max_y)) = calculate_bounds(&current_points);
        let area = (max_x as i64 - min_x as i64 + 1) * (max_y as i64 - min_y as i64 + 1);

      if area < min_area {
          min_area = area;
          best_time = time;
      }
    }

    let mut final_points = points.clone();
      for point in &mut final_points {
        point.x += point.vx * best_time;
        point.y += point.vy * best_time;
    }

    println!("Best time: {}", best_time);
    print_points(&final_points);

    Ok(())
}
