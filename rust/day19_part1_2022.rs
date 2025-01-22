
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp;

#[derive(Debug)]
struct Blueprint {
    id: i32,
    ore_robot_cost: i32,
    clay_robot_cost: i32,
    obsidian_robot_cost_ore: i32,
    obsidian_robot_cost_clay: i32,
    geode_robot_cost_ore: i32,
    geode_robot_cost_obsidian: i32,
}

#[derive(Clone, Debug)]
struct State {
    ore: i32,
    clay: i32,
    obsidian: i32,
    geode: i32,
    ore_robots: i32,
    clay_robots: i32,
    obsidian_robots: i32,
    geode_robots: i32,
    time: i32,
}

fn parse_blueprint(line: &str) -> Blueprint {
    let parts: Vec<&str> = line.split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).collect();
    Blueprint {
        id: parts[0].parse().unwrap(),
        ore_robot_cost: parts[1].parse().unwrap(),
        clay_robot_cost: parts[2].parse().unwrap(),
        obsidian_robot_cost_ore: parts[3].parse().unwrap(),
        obsidian_robot_cost_clay: parts[4].parse().unwrap(),
        geode_robot_cost_ore: parts[5].parse().unwrap(),
        geode_robot_cost_obsidian: parts[6].parse().unwrap(),
    }
}

fn max_geodes(blueprint: &Blueprint, time_limit: i32) -> i32 {
    let initial_state = State {
        ore: 0,
        clay: 0,
        obsidian: 0,
        geode: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
        time: 0,
    };

    let mut max_geodes_found = 0;
    let mut queue = vec![initial_state];
    let mut visited = std::collections::HashSet::new();

    while let Some(current_state) = queue.pop() {
        if current_state.time == time_limit {
            max_geodes_found = cmp::max(max_geodes_found, current_state.geode);
            continue;
        }
        let key = (
            current_state.ore,
            current_state.clay,
            current_state.obsidian,
            current_state.geode,
            current_state.ore_robots,
            current_state.clay_robots,
            current_state.obsidian_robots,
            current_state.geode_robots,
            current_state.time,
        );

        if visited.contains(&key) {
            continue;
        }
        visited.insert(key);

        let mut next_states = Vec::new();

        // Option 1: Build a geode robot
        if current_state.ore >= blueprint.geode_robot_cost_ore && current_state.obsidian >= blueprint.geode_robot_cost_obsidian {
             let mut next_state = current_state.clone();
            next_state.ore -= blueprint.geode_robot_cost_ore;
            next_state.obsidian -= blueprint.geode_robot_cost_obsidian;
            next_state.time +=1;
            next_state.ore += next_state.ore_robots;
            next_state.clay += next_state.clay_robots;
            next_state.obsidian += next_state.obsidian_robots;
            next_state.geode += next_state.geode_robots;
             next_state.geode_robots +=1;
           
            next_states.push(next_state);
        }else{
        // Option 2: Build an obsidian robot
             if current_state.ore >= blueprint.obsidian_robot_cost_ore && current_state.clay >= blueprint.obsidian_robot_cost_clay {
            let mut next_state = current_state.clone();
            next_state.ore -= blueprint.obsidian_robot_cost_ore;
            next_state.clay -= blueprint.obsidian_robot_cost_clay;
            next_state.time +=1;
            next_state.ore += next_state.ore_robots;
            next_state.clay += next_state.clay_robots;
            next_state.obsidian += next_state.obsidian_robots;
            next_state.geode += next_state.geode_robots;
             next_state.obsidian_robots +=1;
           
            next_states.push(next_state);
        }

        // Option 3: Build a clay robot
         if current_state.ore >= blueprint.clay_robot_cost {
              let mut next_state = current_state.clone();
                next_state.ore -= blueprint.clay_robot_cost;
                next_state.time +=1;
                 next_state.ore += next_state.ore_robots;
            next_state.clay += next_state.clay_robots;
            next_state.obsidian += next_state.obsidian_robots;
            next_state.geode += next_state.geode_robots;
                 next_state.clay_robots +=1;
            
                next_states.push(next_state);
            }
          // Option 4: Build an ore robot
         if current_state.ore >= blueprint.ore_robot_cost {
            let mut next_state = current_state.clone();
            next_state.ore -= blueprint.ore_robot_cost;
            next_state.time +=1;
             next_state.ore += next_state.ore_robots;
            next_state.clay += next_state.clay_robots;
            next_state.obsidian += next_state.obsidian_robots;
            next_state.geode += next_state.geode_robots;
             next_state.ore_robots +=1;

            next_states.push(next_state);
          }
        
           }
             // Option 5: Do nothing (collect resources only)
        let mut next_state = current_state.clone();
        next_state.time += 1;
        next_state.ore += next_state.ore_robots;
        next_state.clay += next_state.clay_robots;
        next_state.obsidian += next_state.obsidian_robots;
        next_state.geode += next_state.geode_robots;

        next_states.push(next_state);


        for state in next_states {
            
             let max_ore_needed=cmp::max(cmp::max(blueprint.ore_robot_cost, blueprint.clay_robot_cost) , cmp::max(blueprint.obsidian_robot_cost_ore, blueprint.geode_robot_cost_ore));
            if state.ore > max_ore_needed*4{
                continue;
            }
              let max_clay_needed= blueprint.obsidian_robot_cost_clay;
           if state.clay >max_clay_needed*4{
                continue;
            }
             let max_obsidian_needed= blueprint.geode_robot_cost_obsidian;
            if state.obsidian>max_obsidian_needed*4{
                continue;
            }
            queue.push(state);
        }
    }

    max_geodes_found
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input.txt");
    let reader = BufReader::new(file);

    let mut total_quality_level = 0;

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let blueprint = parse_blueprint(&line);
        let max_geodes = max_geodes(&blueprint, 24);
        let quality_level = blueprint.id * max_geodes;
        total_quality_level += quality_level;
    }

    println!("{}", total_quality_level);
}
