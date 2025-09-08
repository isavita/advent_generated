
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Copy)]
struct Robot {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

fn mod_positive(a: i32, b: i32) -> i32 {
    ((a % b) + b) % b
}

fn parse_line(line: &str) -> Robot {
    let parts: Vec<&str> = line.split(&['p', '=', ',', 'v', ' ']).collect();
    Robot {
        x: parts[2].parse().unwrap(),
        y: parts[3].parse().unwrap(),
        vx: parts[6].parse().unwrap(),
        vy: parts[7].parse().unwrap(),
    }
}

fn move_robots(robots: &mut [Robot], size_x: i32, size_y: i32) {
    for robot in robots.iter_mut() {
        robot.x = mod_positive(robot.x + robot.vx, size_x);
        robot.y = mod_positive(robot.y + robot.vy, size_y);
    }
}

fn count_quadrants(robots: &[Robot], size_x: i32, size_y: i32) -> i32 {
    let center_x = size_x / 2;
    let center_y = size_y / 2;
    let mut counts = [0; 4];

    for robot in robots {
        let x = robot.x;
        let y = robot.y;
        if x < center_x {
            if y < center_y {
                counts[0] += 1;
            } else if y > center_y {
                counts[1] += 1;
            }
        } else if x > center_x {
            if y < center_y {
                counts[2] += 1;
            } else if y > center_y {
                counts[3] += 1;
            }
        }
    }

    counts.iter().product()
}

fn has_no_overlaps(robots: &[Robot]) -> bool {
    let mut positions = [[0; 103]; 101];
    for robot in robots {
        let x = robot.x as usize;
        let y = robot.y as usize;
        if positions[x][y] > 0 {
            return false;
        }
        positions[x][y] += 1;
    }
    true
}

fn draw_grid(robots: &[Robot], size_x: usize, size_y: usize) {
    let mut grid = vec![vec!['.'; size_x]; size_y];
    for robot in robots {
        let x = robot.x as usize;
        let y = robot.y as usize;
        grid[y][x] = '#';
    }
    for row in grid {
        println!("{}", row.iter().collect::<String>());
    }
}

fn main() {
    let size_x = 101;
    let size_y = 103;
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut robots: Vec<Robot> = reader
        .lines()
        .map(|line| parse_line(&line.unwrap()))
        .collect();

    let mut robots_part1 = robots.clone();
    for _ in 0..100 {
        move_robots(&mut robots_part1, size_x, size_y);
    }
    let safety_factor = count_quadrants(&robots_part1, size_x, size_y);
    println!("Part 1 - Safety Factor after 100 seconds: {}", safety_factor);

    let mut robots_part2 = robots;
    let mut seconds = 0;
    while !has_no_overlaps(&robots_part2) {
        move_robots(&mut robots_part2, size_x, size_y);
        seconds += 1;
        if seconds > 1_000_000 {
            println!("Exceeded maximum iterations without finding a unique position configuration.");
            return;
        }
    }
    println!("Part 2 - Fewest seconds to display Easter egg: {}", seconds);
    println!("Final positions of robots:");
    draw_grid(&robots_part2, size_x as usize, size_y as usize);
}
