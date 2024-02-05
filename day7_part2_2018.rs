
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead, BufReader};

struct Step {
    id: char,
    duration: i32,
}

fn main() {
    let (deps, all_steps) = parse_input("input.txt").expect("Failed to parse input");
    let time_taken = simulate_work(deps, all_steps, 5, 60);
    println!("{}", time_taken);
}

fn parse_input(filename: &str) -> io::Result<(HashMap<char, Vec<char>>, HashMap<char, Step>)> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);

    let mut deps = HashMap::new();
    let mut all_steps = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let a = parts[1].chars().next().unwrap();
        let b = parts[7].chars().next().unwrap();

        deps.entry(b).or_insert_with(Vec::new).push(a);
        all_steps.entry(a).or_insert_with(|| Step { id: a, duration: (a as i32 - 'A' as i32) + 61 });
        all_steps.entry(b).or_insert_with(|| Step { id: b, duration: (b as i32 - 'A' as i32) + 61 });
    }

    Ok((deps, all_steps))
}

fn simulate_work(mut deps: HashMap<char, Vec<char>>, mut all_steps: HashMap<char, Step>, num_workers: usize, base_duration: i32) -> i32 {
    let mut workers = vec![0; num_workers];
    let mut tasks = vec![None; num_workers];
    let mut time = 0;

    while !all_steps.is_empty() {
        let mut available: Vec<char> = all_steps.keys().cloned()
            .filter(|&step| !deps.contains_key(&step) || deps[&step].is_empty())
            .filter(|&step| !is_being_worked_on(&tasks, step))
            .collect();
        available.sort_unstable();

        for i in 0..num_workers {
            if workers[i] == 0 {
                if let Some(step) = available.get(0).cloned() {
                    tasks[i] = Some(step);
                    workers[i] = all_steps[&step].duration;
                    available.remove(0);
                }
            }
        }

        let min_duration = *workers.iter().filter(|&&d| d > 0).min().unwrap_or(&0);
        for i in 0..num_workers {
            if workers[i] != 0 {
                workers[i] -= min_duration;
                if workers[i] == 0 {
                    if let Some(step) = tasks[i] {
                        finish_step(&mut deps, &mut all_steps, step);
                    }
                    tasks[i] = None;
                }
            }
        }
        time += min_duration;
    }

    time
}

fn is_being_worked_on(tasks: &[Option<char>], step: char) -> bool {
    tasks.iter().any(|&task| task == Some(step))
}

fn finish_step(deps: &mut HashMap<char, Vec<char>>, all_steps: &mut HashMap<char, Step>, step: char) {
    all_steps.remove(&step);
    for dep_steps in deps.values_mut() {
        if let Some(pos) = dep_steps.iter().position(|&s| s == step) {
            dep_steps.remove(pos);
        }
    }
}
