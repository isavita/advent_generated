
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn solve(weights: &Vec<u64>, groups: usize) -> u64 {
    let total_weight: u64 = weights.iter().sum();
    if total_weight % groups as u64 != 0 {
        return u64::MAX; 
    }

    let target_weight = total_weight / groups as u64;
    let mut min_packages = usize::MAX;
    let mut min_qe = u64::MAX;

    fn find_combinations(
        weights: &Vec<u64>,
        index: usize,
        current_group: Vec<u64>,
        current_sum: u64,
        target_weight: u64,
        min_packages: &mut usize,
        min_qe: &mut u64,
    ) {
        if current_sum == target_weight {
           
            if current_group.len() < *min_packages {
                *min_packages = current_group.len();
                *min_qe = current_group.iter().product();
            } else if current_group.len() == *min_packages {
                 let current_qe = current_group.iter().product();
                *min_qe = (*min_qe).min(current_qe);
            }
           
           
            return;
        }

         if index >= weights.len() || current_sum > target_weight {
            return;
        }

         
         
        
        find_combinations(
            weights,
            index + 1,
            current_group.clone(),
            current_sum,
            target_weight,
            min_packages,
             min_qe,
        );
        let mut next_group = current_group.clone();
        next_group.push(weights[index]);
        find_combinations(
            weights,
            index + 1,
            next_group,
            current_sum + weights[index],
            target_weight,
             min_packages,
            min_qe,
        );
    }


    find_combinations(weights, 0, Vec::new(), 0, target_weight, &mut min_packages, &mut min_qe);

    min_qe
}

fn main() {
    let weights: Vec<u64> = read_lines("input.txt")
        .unwrap()
        .map(|line| line.unwrap().parse::<u64>().unwrap())
        .collect();

    let part1_result = solve(&weights, 3);
    println!("Part 1: {}", part1_result);

    let part2_result = solve(&weights, 4);
    println!("Part 2: {}", part2_result);
}
