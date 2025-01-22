
use std::fs;
use std::collections::HashMap;

fn count_arrangements(springs: &str, groups: &[usize], cache: &mut HashMap<(String, Vec<usize>), usize>) -> usize {
    if let Some(&result) = cache.get(&(springs.to_string(), groups.to_vec())) {
        return result;
    }

    if groups.is_empty() {
        if springs.contains('#') {
            return 0;
        } else {
            return 1;
        }
    }

    if springs.is_empty() {
        return 0;
    }

    let mut count = 0;
    let first_char = springs.chars().next().unwrap();

    if first_char == '.' || first_char == '?' {
        count += count_arrangements(&springs[1..], groups, cache);
    }

    if first_char == '#' || first_char == '?' {
        if groups[0] <= springs.len() && 
            !springs[..groups[0]].contains('.') &&
            (groups[0] == springs.len() || springs.chars().nth(groups[0]) != Some('#')) {
                let next_springs = if groups[0] == springs.len() {
                     ""
                  } else {
                     &springs[(groups[0] + 1)..]
                  };
                count += count_arrangements(next_springs, &groups[1..], cache);
        }
    }
    cache.insert((springs.to_string(), groups.to_vec()), count);
    count
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut total_arrangements_part1 = 0;
    let mut total_arrangements_part2 = 0;

    for line in contents.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let springs = parts[0];
        let groups: Vec<usize> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();

        let mut cache1: HashMap<(String, Vec<usize>), usize> = HashMap::new();
        let arrangements_part1 = count_arrangements(springs, &groups, &mut cache1);
        total_arrangements_part1 += arrangements_part1;

        let unfolded_springs = (0..5).map(|_| springs).collect::<Vec<_>>().join("?");
        let unfolded_groups = groups.iter().cycle().take(groups.len() * 5).cloned().collect::<Vec<_>>();
    
        let mut cache2: HashMap<(String, Vec<usize>), usize> = HashMap::new();
        let arrangements_part2 = count_arrangements(&unfolded_springs, &unfolded_groups, &mut cache2);
        total_arrangements_part2 += arrangements_part2;
    }

    println!("Part 1: {}", total_arrangements_part1);
    println!("Part 2: {}", total_arrangements_part2);
}
