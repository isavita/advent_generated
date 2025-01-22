
use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone, Copy)]
enum GateType {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
struct Gate {
    gate_type: GateType,
    input1: String,
    input2: String,
    output: String,
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut initial_values: HashMap<String, Option<u8>> = HashMap::new();
    let mut gates: Vec<Gate> = Vec::new();
    let mut reading_values = true;

    for line in contents.lines() {
        if line.is_empty() {
            reading_values = false;
            continue;
        }
        if reading_values {
            let parts: Vec<&str> = line.split(": ").collect();
            let wire_name = parts[0].to_string();
            let value: u8 = parts[1].parse().unwrap();
            initial_values.insert(wire_name, Some(value));
        } else {
            let parts: Vec<&str> = line.split(" -> ").collect();
            let output = parts[1].to_string();
            let gate_parts: Vec<&str> = parts[0].split_whitespace().collect();
            
            if gate_parts.len() == 3 {
                let input1 = gate_parts[0].to_string();
                let gate_type = match gate_parts[1] {
                    "AND" => GateType::And,
                    "OR" => GateType::Or,
                    "XOR" => GateType::Xor,
                    _ => panic!("Unknown gate type"),
                };
                 let input2 = gate_parts[2].to_string();
                gates.push(Gate {
                    gate_type,
                    input1,
                    input2,
                    output,
                });
            } else {
                 panic!("Invalid Gate {}",line);
            }

        }
    }

    let mut wire_values: HashMap<String, Option<u8>> = initial_values.clone();
   
    loop {
        let mut updated = false;
        for gate in &gates {
            if wire_values.get(&gate.output).unwrap_or(&None).is_some() {
                continue;
            }
            let input1_value = *wire_values.get(&gate.input1).unwrap_or(&None);
            let input2_value = *wire_values.get(&gate.input2).unwrap_or(&None);

            if input1_value.is_some() && input2_value.is_some() {
                let value = match gate.gate_type {
                    GateType::And => input1_value.unwrap() & input2_value.unwrap(),
                    GateType::Or => input1_value.unwrap() | input2_value.unwrap(),
                    GateType::Xor => input1_value.unwrap() ^ input2_value.unwrap(),
                };
                wire_values.insert(gate.output.clone(), Some(value));
                updated = true;
            }
        }
         if !updated {
            break;
        }
    }
    
    let mut z_wires: Vec<(&String, &Option<u8>)> = wire_values.iter()
        .filter(|(k,_)| k.starts_with('z'))
        .collect();

    z_wires.sort_by_key(|(k, _)| k.as_str());

    let binary_string: String = z_wires.iter()
        .filter_map(|(_, v)| v.map(|x| x.to_string()))
        .rev()
        .collect();
    
    let decimal_value = u64::from_str_radix(&binary_string, 2).unwrap_or(0);


    println!("{}", decimal_value);
}
