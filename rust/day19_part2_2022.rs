
use std::{
    cmp::max,
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

#[derive(Debug)]
struct Blueprint {
    id: i32,
    ore_cost: i32,
    clay_ore_cost: i32,
    obsidian_ore_cost: i32,
    obsidian_clay_cost: i32,
    geode_ore_cost: i32,
    geode_obsidian_cost: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    ore: i32,
    clay: i32,
    obsidian: i32,
    geode: i32,
    ore_robots: i32,
    clay_robots: i32,
    obsidian_robots: i32,
    geode_robots: i32,
    time_left: i32,
}

fn main() {
    let blueprints = read_blueprints();
    let init = State {
        ore: 0,
        clay: 0,
        obsidian: 0,
        geode: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
        time_left: 32,
    };

    let prod: i32 = blueprints
        .iter()
        .take(3)
        .map(|b| max_geode(b, init.clone()))
        .product();
    println!("{}", prod);
}

fn read_blueprints() -> Vec<Blueprint> {
    let file = File::open("input.txt").expect("Failed to open input.txt");
    io::BufReader::new(file)
        .lines()
        .map(|line| {
            let line = line.expect("Failed to read line");
            let parts: Vec<&str> = line.split_whitespace().collect();
            Blueprint {
                id: parts[1][0..parts[1].len() - 1].parse().unwrap(),
                ore_cost: parts[6].parse().unwrap(),
                clay_ore_cost: parts[12].parse().unwrap(),
                obsidian_ore_cost: parts[18].parse().unwrap(),
                obsidian_clay_cost: parts[21].parse().unwrap(),
                geode_ore_cost: parts[27].parse().unwrap(),
                geode_obsidian_cost: parts[30].parse().unwrap(),
            }
        })
        .collect()
}

fn max_geode(b: &Blueprint, st: State) -> i32 {
    let mut max_geodes = 0;
    let mut q = vec![st];
    let mut visited: HashMap<State, ()> = HashMap::new();

    while let Some(s) = q.pop() {
        max_geodes = max(max_geodes, s.geode);
        if s.time_left == 0 {
            continue;
        }

        let max_ore_cost = max(
            b.ore_cost,
            max(b.clay_ore_cost, max(b.obsidian_ore_cost, b.geode_ore_cost)),
        );
        let mut s = s;
        if s.ore_robots >= max_ore_cost {
            s.ore_robots = max_ore_cost;
        }
        if s.clay_robots >= b.obsidian_clay_cost {
            s.clay_robots = b.obsidian_clay_cost;
        }
        if s.obsidian_robots >= b.geode_obsidian_cost {
            s.obsidian_robots = b.geode_obsidian_cost;
        }
        if s.ore >= s.time_left * max_ore_cost - s.ore_robots * (s.time_left - 1) {
            s.ore = s.time_left * max_ore_cost - s.ore_robots * (s.time_left - 1);
        }
        if s.clay >= s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1) {
            s.clay = s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1);
        }
        if s.obsidian
            >= s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1)
        {
            s.obsidian =
                s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1);
        }
        if visited.contains_key(&s) {
            continue;
        }
        visited.insert(s.clone(), ());

        q.push(State {
            ore: s.ore + s.ore_robots,
            clay: s.clay + s.clay_robots,
            obsidian: s.obsidian + s.obsidian_robots,
            geode: s.geode + s.geode_robots,
            ore_robots: s.ore_robots,
            clay_robots: s.clay_robots,
            obsidian_robots: s.obsidian_robots,
            geode_robots: s.geode_robots,
            time_left: s.time_left - 1,
        });

        if s.ore >= b.ore_cost {
            q.push(State {
                ore: s.ore - b.ore_cost + s.ore_robots,
                clay: s.clay + s.clay_robots,
                obsidian: s.obsidian + s.obsidian_robots,
                geode: s.geode + s.geode_robots,
                ore_robots: s.ore_robots + 1,
                clay_robots: s.clay_robots,
                obsidian_robots: s.obsidian_robots,
                geode_robots: s.geode_robots,
                time_left: s.time_left - 1,
            });
        }

        if s.ore >= b.clay_ore_cost {
            q.push(State {
                ore: s.ore - b.clay_ore_cost + s.ore_robots,
                clay: s.clay + s.clay_robots,
                obsidian: s.obsidian + s.obsidian_robots,
                geode: s.geode + s.geode_robots,
                ore_robots: s.ore_robots,
                clay_robots: s.clay_robots + 1,
                obsidian_robots: s.obsidian_robots,
                geode_robots: s.geode_robots,
                time_left: s.time_left - 1,
            });
        }

        if s.ore >= b.obsidian_ore_cost && s.clay >= b.obsidian_clay_cost {
            q.push(State {
                ore: s.ore - b.obsidian_ore_cost + s.ore_robots,
                clay: s.clay - b.obsidian_clay_cost + s.clay_robots,
                obsidian: s.obsidian + s.obsidian_robots,
                geode: s.geode + s.geode_robots,
                ore_robots: s.ore_robots,
                clay_robots: s.clay_robots,
                obsidian_robots: s.obsidian_robots + 1,
                geode_robots: s.geode_robots,
                time_left: s.time_left - 1,
            });
        }

        if s.ore >= b.geode_ore_cost && s.obsidian >= b.geode_obsidian_cost {
            q.push(State {
                ore: s.ore - b.geode_ore_cost + s.ore_robots,
                clay: s.clay + s.clay_robots,
                obsidian: s.obsidian - b.geode_obsidian_cost + s.obsidian_robots,
                geode: s.geode + s.geode_robots,
                ore_robots: s.ore_robots,
                clay_robots: s.clay_robots,
                obsidian_robots: s.obsidian_robots,
                geode_robots: s.geode_robots + 1,
                time_left: s.time_left - 1,
            });
        }
    }
    max_geodes
}
