
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct Reaction {
    inputs: Vec<(String, i64)>,
    output: (String, i64),
}

fn parse_reaction(line: &str) -> Reaction {
    let parts: Vec<&str> = line.split(" => ").collect();
    let inputs_str = parts[0];
    let output_str = parts[1];

    let inputs: Vec<(String, i64)> = inputs_str
        .split(", ")
        .map(|s| {
            let parts: Vec<&str> = s.split(' ').collect();
            (parts[1].to_string(), parts[0].parse().unwrap())
        })
        .collect();

    let output_parts: Vec<&str> = output_str.split(' ').collect();
    let output = (output_parts[1].to_string(), output_parts[0].parse().unwrap());

    Reaction { inputs, output }
}

fn calculate_ore(
    reactions: &HashMap<String, Reaction>,
    mut needed: HashMap<String, i64>,
) -> i64 {
    let mut ore_needed = 0;
    let mut extra: HashMap<String, i64> = HashMap::new();

    while !needed.is_empty() {
        let mut next_needed: HashMap<String, i64> = HashMap::new();
        for (chemical, amount) in needed {
            if chemical == "ORE" {
                ore_needed += amount;
                continue;
            }

            if let Some(&extra_amount) = extra.get(&chemical) {
               if extra_amount >= amount {
                  *extra.get_mut(&chemical).unwrap() -= amount;
                    continue;
               } else {
                *extra.get_mut(&chemical).unwrap() = 0;
                    let remaining = amount - extra_amount;
                    
                   let reaction = reactions.get(&chemical).unwrap();
                    
                    let multiplier = (remaining + reaction.output.1 - 1) / reaction.output.1;

                    for (input_chem, input_amount) in &reaction.inputs {
                        *next_needed.entry(input_chem.to_string()).or_insert(0) +=
                            input_amount * multiplier;
                    }
                     *extra.entry(chemical).or_insert(0) += (reaction.output.1*multiplier) - remaining;
                    
               }

            } else {
                let reaction = reactions.get(&chemical).unwrap();
                let multiplier = (amount + reaction.output.1 - 1) / reaction.output.1;

                for (input_chem, input_amount) in &reaction.inputs {
                    *next_needed.entry(input_chem.to_string()).or_insert(0) +=
                        input_amount * multiplier;
                }
                *extra.entry(chemical).or_insert(0) += (reaction.output.1*multiplier) - amount;

            }
            
        }
        needed = next_needed;
    }
    ore_needed
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut reactions: HashMap<String, Reaction> = HashMap::new();
    for line in reader.lines() {
        let line = line?;
        let reaction = parse_reaction(&line);
        reactions.insert(reaction.output.0.clone(), reaction);
    }

    let ore_needed = calculate_ore(&reactions, vec![("FUEL".to_string(), 1)].into_iter().collect());
    println!("{}", ore_needed);

    Ok(())
}
