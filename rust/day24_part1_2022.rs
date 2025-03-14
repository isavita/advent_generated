
use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: usize, b: usize) -> usize {
    a * b / gcd(a, b)
}

fn read_input(file_path: &str) -> (HashSet<(usize, usize)>, Vec<(usize, usize, char)>, usize, usize) {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);
    let mut walls = HashSet::new();
    let mut blizzards = Vec::new();
    let mut height = 0;
    let mut width = 0;

    for (y, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        width = line.len();
        height = y + 1;
        for (x, char) in line.chars().enumerate() {
            match char {
                '#' => {
                    walls.insert((x, y));
                }
                '>' | '<' | '^' | 'v' => {
                    blizzards.push((x, y, char));
                }
                _ => {}
            }
        }
    }
    (walls, blizzards, height, width)
}

fn find_start_end(walls: &HashSet<(usize, usize)>, height: usize, width: usize) -> ((usize, usize), (usize, usize)) {
    let start = (0..width).find(|&x| !walls.contains(&(x, 0))).unwrap();
    let end = (0..width).find(|&x| !walls.contains(&(x, height - 1))).unwrap();
    ((start, 0), (end, height - 1))
}

fn precompute_blizzards(
    blizzards: &Vec<(usize, usize, char)>,
    width: usize,
    height: usize,
    period: usize,
) -> Vec<HashSet<(usize, usize)>> {
    let mut blizzard_positions = vec![HashSet::new(); period];
    for t in 0..period {
        for &(x, y, dir) in blizzards {
            let (new_x, new_y) = match dir {
                '>' => (1 + (x - 1 + t) % (width - 2), y),
                '<' => (1 + (x - 1 + (period - t % period)) % (width - 2), y),
                'v' => (x, 1 + (y - 1 + t) % (height - 2)),
                '^' => (x, 1 + (y - 1 + (period - t % period)) % (height - 2)),
                _ => unreachable!(),
            };
            blizzard_positions[t].insert((new_x, new_y));
        }
    }
    blizzard_positions
}

fn bfs(
    start: (usize, usize),
    end: (usize, usize),
    walls: &HashSet<(usize, usize)>,
    blizzard_positions: &Vec<HashSet<(usize, usize)>>,
    period: usize,
    width: usize,
    height: usize,
) -> i32 {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back((start.0, start.1, 0));
    visited.insert((start.0, start.1, 0));

    let directions = [(0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)];

    while let Some((x, y, t)) = queue.pop_front() {
        if (x, y) == end {
            return t as i32;
        }

        let next_t = t + 1;
        let blizzards_next = &blizzard_positions[next_t % period];

        for &(dx, dy) in &directions {
            let (nx, ny) = (x as isize + dx, y as isize + dy);
            if nx < 0 || ny < 0 {
                continue;
            }
            let (nx, ny) = (nx as usize, ny as usize);

            if (nx, ny) == end {
                return next_t as i32;
            }

            if (nx, ny) == start {
                let state = (nx, ny, next_t % period);
                if !visited.contains(&state) && !blizzards_next.contains(&(nx, ny)) {
                    visited.insert(state);
                    queue.push_back((nx, ny, next_t));
                }
                continue;
            }

            if nx > 0 && nx < width - 1 && ny > 0 && ny < height - 1 {
                if walls.contains(&(nx, ny)) || blizzards_next.contains(&(nx, ny)) {
                    continue;
                }

                let state = (nx, ny, next_t % period);
                if !visited.contains(&state) {
                    visited.insert(state);
                    queue.push_back((nx, ny, next_t));
                }
            }
        }
    }
    -1
}

fn main() {
    let (walls, blizzards, height, width) = read_input("input.txt");
    let (start, end) = find_start_end(&walls, height, width);
    let period = lcm(width - 2, height - 2);
    let blizzard_positions = precompute_blizzards(&blizzards, width, height, period);
    let minutes = bfs(start, end, &walls, &blizzard_positions, period, width, height);
    println!("{}", minutes);
}
