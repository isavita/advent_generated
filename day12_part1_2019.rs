use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut moons: Vec<(Vec<i32>, Vec<i32>)> = input
        .trim()
        .split('\n')
        .map(|line| {
            let coords: Vec<i32> = line
                .trim_matches(|c| c == '<' || c == '>')
                .split(", ")
                .map(|pair| pair.split('=').nth(1).unwrap().parse().unwrap())
                .collect();
            (coords, vec![0, 0, 0])
        })
        .collect();

    for _ in 0..1000 {
        for i in 0..moons.len() {
            for j in i + 1..moons.len() {
                for k in 0..3 {
                    if moons[i].0[k] < moons[j].0[k] {
                        moons[i].1[k] += 1;
                        moons[j].1[k] -= 1;
                    } else if moons[i].0[k] > moons[j].0[k] {
                        moons[i].1[k] -= 1;
                        moons[j].1[k] += 1;
                    }
                }
            }
        }

        for moon in &mut moons {
            for k in 0..3 {
                moon.0[k] += moon.1[k];
            }
        }
    }

    let total_energy: i32 = moons
        .iter()
        .map(|(pos, vel)| {
            let potential_energy: i32 = pos.iter().map(|&x| x.abs()).sum();
            let kinetic_energy: i32 = vel.iter().map(|&x| x.abs()).sum();
            potential_energy * kinetic_energy
        })
        .sum();

    println!("{}", total_energy);
}