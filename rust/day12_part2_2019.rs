
use std::fs;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Moon {
    pos: [i32; 3],
    vel: [i32; 3],
}

fn apply_gravity(moons: &mut [Moon]) {
    for i in 0..moons.len() {
        for j in i + 1..moons.len() {
            for axis in 0..3 {
                if moons[i].pos[axis] < moons[j].pos[axis] {
                    moons[i].vel[axis] += 1;
                    moons[j].vel[axis] -= 1;
                } else if moons[i].pos[axis] > moons[j].pos[axis] {
                    moons[i].vel[axis] -= 1;
                    moons[j].vel[axis] += 1;
                }
            }
        }
    }
}

fn apply_velocity(moons: &mut [Moon]) {
    for moon in moons {
        for axis in 0..3 {
            moon.pos[axis] += moon.vel[axis];
        }
    }
}

fn calculate_total_energy(moons: &[Moon]) -> i32 {
    let mut total_energy = 0;
    for moon in moons {
        let pot = moon.pos.iter().map(|x| x.abs()).sum::<i32>();
        let kin = moon.vel.iter().map(|x| x.abs()).sum::<i32>();
        total_energy += pot * kin;
    }
    total_energy
}

fn parse_input(input: &str) -> Vec<Moon> {
    input.lines()
        .map(|line| {
            let parts: Vec<i32> = line
                .trim_matches(|c| c == '<' || c == '>')
                .split(", ")
                .map(|s| s.split("=").nth(1).unwrap().parse().unwrap())
                .collect();
            Moon {
                pos: [parts[0], parts[1], parts[2]],
                vel: [0, 0, 0],
            }
        })
        .collect()
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: u64, b: u64) -> u64 {
    a * b / gcd(a, b)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let initial_moons = parse_input(&input);
    
    // Part 1
    let mut moons_part1 = initial_moons.clone();
    for _ in 0..1000 {
        apply_gravity(&mut moons_part1);
        apply_velocity(&mut moons_part1);
    }
    let total_energy = calculate_total_energy(&moons_part1);
    println!("Part 1: Total energy after 1000 steps: {}", total_energy);

    // Part 2
    let mut moons = initial_moons.clone();
    let mut seen_states: Vec<HashSet<(Vec<i32>, Vec<i32>)>> = vec![HashSet::new();3];
    let mut periods: [Option<u64>;3] = [None,None,None];

    let mut steps = 0;
    
    loop {
        if periods.iter().all(|x| x.is_some()){
            break;
        }

        apply_gravity(&mut moons);
        apply_velocity(&mut moons);

        for axis in 0..3{
           let current_state: (Vec<i32>, Vec<i32>) = (moons.iter().map(|m| m.pos[axis]).collect(), moons.iter().map(|m| m.vel[axis]).collect());
           if periods[axis].is_none() {
              if seen_states[axis].contains(&current_state){
                periods[axis] = Some(steps as u64);
              } else {
                seen_states[axis].insert(current_state);
              }
            }
        }

        steps += 1;
    }
    
    let lcm_result = periods.iter().fold(1, |acc, &x| lcm(acc, x.unwrap()));
    println!("Part 2: Steps until repeat: {}", lcm_result);
}
