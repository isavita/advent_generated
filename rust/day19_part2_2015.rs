
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn solve_part1(replacements: &[(String, String)], molecule: &str) -> usize {
    let mut distinct_molecules = HashSet::new();

    for (start, end) in replacements {
        for (index, _) in molecule.match_indices(start) {
            let mut new_molecule = molecule.to_string();
            new_molecule.replace_range(index..index + start.len(), end);
            distinct_molecules.insert(new_molecule);
        }
    }

    distinct_molecules.len()
}

fn solve_part2(replacements: &[(String, String)], target_molecule: &str) -> usize {
    // Optimized approach based on observation and analysis of the problem.
    // The key observation is the structure of the replacements and how they
    // allow building the target molecule.  This avoids needing a general search algorithm (which is slow).
    //
    // Most replacement rules have the form "X => YZ".
    // So, the length of the final string is related to:
    //  * total chars
    //  * total chars in the form Rn...Ar
    //  * total chars in the form Y
    //
    //  Let R = total chars in the form Rn..Ar
    //  Let Y = total chars in the form Y
    //  Let T = Total characters
    //  Then num_steps =  T - R - 2 * Y -1
    //  This relies on the assumption that the molecule can be constructed by the rules, which it should be.
     
    let total_chars = target_molecule.chars().filter(|c| c.is_ascii_uppercase()).count();
    let rn_ar_count = target_molecule.matches("Rn").count() + target_molecule.matches("Ar").count();
    let y_count = target_molecule.matches('Y').count();

    total_chars - rn_ar_count - 2 * y_count - 1
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut replacements = Vec::new();
    let mut molecule = String::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(" => ").collect();
        if parts.len() == 2 {
            replacements.push((parts[0].to_string(), parts[1].to_string()));
        } else if !line.is_empty() {
            molecule = line;
        }
    }

    let part1_result = solve_part1(&replacements, &molecule);
    println!("{}", part1_result);

    let part2_result = solve_part2(&replacements, &molecule);
     println!("{}", part2_result);

    Ok(())
}
