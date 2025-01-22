
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug)]
struct Reaction {
    inputs: Vec<(String, i64)>,
    output: (String, i64),
}

fn parse_reaction(line: &str) -> Result<Reaction, String> {
    let parts: Vec<&str> = line.split(" => ").collect();
    if parts.len() != 2 {
        return Err("Invalid reaction format".to_string());
    }

    let inputs_str = parts[0];
    let output_str = parts[1];

    let inputs: Vec<(String, i64)> = inputs_str
        .split(", ")
        .map(|s| {
            let parts: Vec<&str> = s.split(' ').collect();
            if parts.len() != 2 {
                Err("Invalid input format".to_string())
            } else {
                let amount = i64::from_str(parts[0])
                    .map_err(|_| "Invalid amount".to_string())?;
                Ok((parts[1].to_string(), amount))
            }
        })
        .collect::<Result<_, _>>()?;

    let output_parts: Vec<&str> = output_str.split(' ').collect();
    if output_parts.len() != 2 {
        return Err("Invalid output format".to_string());
    }
    let output_amount = i64::from_str(output_parts[0])
        .map_err(|_| "Invalid output amount".to_string())?;
    let output_name = output_parts[1].to_string();

    Ok(Reaction {
        inputs,
        output: (output_name, output_amount),
    })
}


fn calculate_ore_needed(
    reactions: &HashMap<String, Reaction>,
    fuel_needed: i64,
) -> i64 {
    let mut inventory: HashMap<String, i64> = HashMap::new();
    let mut ore_needed = 0;
    let mut requirements: Vec<(String, i64)> = vec![("FUEL".to_string(), fuel_needed)];

    while let Some((chemical, amount_needed)) = requirements.pop() {
        if chemical == "ORE" {
            ore_needed += amount_needed;
            continue;
        }

        let available = *inventory.get(&chemical).unwrap_or(&0);
        if available >= amount_needed {
            inventory.insert(chemical, available - amount_needed);
            continue;
        }
        let remaining_needed = amount_needed - available;

       
        if let Some(reaction) = reactions.get(&chemical) {
           let multiplier = (remaining_needed + reaction.output.1 - 1) / reaction.output.1;
           
           inventory.insert(chemical, available + multiplier * reaction.output.1 - amount_needed );

            for (input_chem, input_amount) in &reaction.inputs {
                requirements.push((input_chem.clone(), input_amount * multiplier));
            }

        } else {
            panic!("No reaction for chemical: {}", chemical);
        }

    }
    ore_needed
}



fn solve_part2(reactions: &HashMap<String, Reaction>) -> i64 {
    let total_ore: i64 = 1_000_000_000_000;
    let mut low: i64 = 1;
    let mut high: i64 = total_ore;
    let mut max_fuel = 0;

    while low <= high {
        let mid = low + (high - low) / 2;
        let ore_for_mid = calculate_ore_needed(reactions, mid);

        if ore_for_mid <= total_ore {
            max_fuel = mid;
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }

    max_fuel
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut reactions: HashMap<String, Reaction> = HashMap::new();

    for line_result in reader.lines() {
        let line = line_result?;
        let reaction = parse_reaction(&line).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        reactions.insert(reaction.output.0.clone(), reaction);
    }
    

    let ore_needed_for_one_fuel = calculate_ore_needed(&reactions, 1);

    println!("Part 1: Minimum ORE required for 1 FUEL: {}", ore_needed_for_one_fuel);
    println!("Part 2: Max FUEL with 1 trillion ORE: {}", solve_part2(&reactions));

    Ok(())
}
