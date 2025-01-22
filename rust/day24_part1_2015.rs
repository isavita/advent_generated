
use std::fs::File;
use std::io::{BufRead, BufReader};

fn solve() -> u64 {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);
    let packages: Vec<u64> = reader
        .lines()
        .map(|line| line.unwrap().parse::<u64>().unwrap())
        .collect();

    let total_weight: u64 = packages.iter().sum();
    let target_weight = total_weight / 3;

    let mut best_qe = u64::MAX;
    let mut min_count = usize::MAX;

    for count in 1..packages.len() {
        for indices in combinations(&packages, count) {
            let group1: Vec<u64> = indices.iter().map(|&i| packages[i]).collect();
            let sum_group1: u64 = group1.iter().sum();

            if sum_group1 == target_weight {
                let remaining_packages: Vec<u64> = packages
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| !indices.contains(i))
                    .map(|(_, &p)| p)
                    .collect();
                
                if can_split(&remaining_packages, target_weight) {
                    let qe = group1.iter().product();
                    if count < min_count {
                        min_count = count;
                        best_qe = qe;
                    } else if count == min_count && qe < best_qe {
                        best_qe = qe;
                    }
                }
            }
        }
        if min_count != usize::MAX {
            break;
        }
    }

    best_qe
}

fn combinations(packages: &[u64], count: usize) -> Vec<Vec<usize>> {
    let mut result = Vec::new();
    let mut current = Vec::new();
    combinations_helper(packages, count, 0, &mut current, &mut result);
    result
}

fn combinations_helper(
    packages: &[u64],
    count: usize,
    start_index: usize,
    current: &mut Vec<usize>,
    result: &mut Vec<Vec<usize>>,
) {
    if current.len() == count {
        result.push(current.clone());
        return;
    }

    for i in start_index..packages.len() {
        current.push(i);
        combinations_helper(packages, count, i + 1, current, result);
        current.pop();
    }
}
fn can_split(packages: &[u64], target_weight: u64) -> bool {
    for count in 1..=packages.len() {
        for indices in combinations(packages, count) {
            let group: Vec<u64> = indices.iter().map(|&i| packages[i]).collect();
            if group.iter().sum::<u64>() == target_weight {
                return true;
            }
        }
    }
    false
}

fn main() {
    let result = solve();
    println!("{}", result);
}
