
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
            width = width.max(x + 1);
        }
        height = y + 1;
    }

    (walls, blizzards, height, width)
}

fn find_start_end(
    walls: &HashSet<(usize, usize)>,
    height: usize,
    width: usize,
) -> ((usize, usize), (usize, usize)) {
    let start = (0..width).find(|&x| !walls.contains(&(x, 0))).unwrap();
    let end = (0..width)
        .find(|&x| !walls.contains(&(x, height - 1)))
        .unwrap();
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
    start_time: usize,
) -> i32 {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back((start.0, start.1, start_time));
    visited.insert((start.0, start.1, start_time % period));

    let directions = [(0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)];

    while let Some((x, y, t)) = queue.pop_front() {
        if (x, y) == end {
            return t as i32;
        }

        let next_t = t + 1;
        let blizzards_next = &blizzard_positions[next_t % period];

        for &(dx, dy) in &directions {
            let (nx, ny) = (x as isize + dx, y as isize + dy);
            if (nx as usize, ny as usize) == end || (nx as usize, ny as usize) == start {
                let state = (nx as usize, ny as usize, next_t % period);
                if !visited.contains(&state) && !blizzards_next.contains(&(nx as usize, ny as usize))
                {
                    visited.insert(state);
                    queue.push_back((nx as usize, ny as usize, next_t));
                }
                continue;
            }

            if nx >= 1
                && nx < (width - 1) as isize
                && ny >= 1
                && ny < (height - 1) as isize
                && !walls.contains(&(nx as usize, ny as usize))
                && !blizzards_next.contains(&(nx as usize, ny as usize))
            {
                let state = (nx as usize, ny as usize, next_t % period);
                if !visited.contains(&state) {
                    visited.insert(state);
                    queue.push_back((nx as usize, ny as usize, next_t));
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

    let time1 = bfs(
        start,
        end,
        &walls,
        &blizzard_positions,
        period,
        width,
        height,
        0,
    );
    if time1 == -1 {
        println!("No path found for trip 1.");
        return;
    }

    let time2 = bfs(
        end,
        start,
        &walls,
        &blizzard_positions,
        period,
        width,
        height,
        time1 as usize,
    );
    if time2 == -1 {
        println!("No path found for trip 2.");
        return;
    }
    let time3 = bfs(
        start,
        end,
        &walls,
        &blizzard_positions,
        period,
        width,
        height,
        time2 as usize,
    );

    if time3 == -1 {
        println!("No path found for trip 3.");
        return;
    }
    println!("{}", time3);
}
