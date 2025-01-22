
use std::{
    collections::HashSet,
    fs::File,
    io::{self, BufRead},
};

fn parse_data(data: &[String]) -> (HashSet<(i64, i64)>, (i64, i64)) {
    let mut garden = HashSet::new();
    let mut start = (-1, -1);
    for (y, line) in data.iter().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != '#' {
                garden.insert((x as i64, y as i64));
            }
            if c == 'S' {
                start = (x as i64, y as i64);
            }
        }
    }

    if start == (-1, -1) {
        panic!("No start found!");
    }
    (garden, start)
}

fn complex_mod(num: (i64, i64), mod_val: i64) -> (i64, i64) {
    ((num.0 + 10 * mod_val) % mod_val, (num.1 + 10 * mod_val) % mod_val)
}

fn calculate_num_ends(
    garden: &HashSet<(i64, i64)>,
    start: (i64, i64),
    num_iterations: i64,
    max_size: i64,
) -> i64 {
    let mut queue = HashSet::new();
    queue.insert(start);

    let mut done = Vec::new();

    for i in 0..3 * max_size {
        if (i % max_size) == (max_size - 1) / 2 {
            done.push(queue.len() as i64);
        }
        if done.len() == 3 {
            break;
        }

        let mut new_queue = HashSet::new();
        let directions = [(1, 0), (-1, 0), (0, 1), (0, -1)];
        for dir in directions {
            for &point in &queue {
                let next_point = (point.0 + dir.0, point.1 + dir.1);
                if garden.contains(&complex_mod(next_point, max_size)) {
                    new_queue.insert(next_point);
                }
            }
        }
        queue = new_queue;
    }

    let quadratic_function = |n: i64, a: i64, b: i64, c: i64| -> i64 {
        a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2))
    };

    quadratic_function(num_iterations / max_size, done[0], done[1], done[2])
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let garden_input: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    let (garden, start) = parse_data(&garden_input);
    let max_size = garden_input.len() as i64;

    let sum = calculate_num_ends(&garden, start, 26501365, max_size);

    println!("{}", sum);
    Ok(())
}
