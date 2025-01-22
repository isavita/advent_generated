
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    str::FromStr,
};

#[derive(Debug)]
struct Gate {
    a: String,
    op: String,
    b: String,
}

fn parse(input: &str) -> Option<Vec<(Gate, String)>> {
    let parts: Vec<&str> = input.split("\n\n").collect();
    if parts.len() != 2 {
        return None;
    }

    let mut gates = Vec::new();
    for line in parts[1].lines() {
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split(" -> ").collect();
        if parts.len() != 2 {
            continue;
        }
        let gate_parts: Vec<&str> = parts[0].split(' ').collect();
        if gate_parts.len() != 3 {
            continue;
        }
        gates.push((
            Gate {
                a: gate_parts[0].to_string(),
                op: gate_parts[1].to_string(),
                b: gate_parts[2].to_string(),
            },
            parts[1].to_string(),
        ));
    }
    Some(gates)
}

fn create_lookups(gates: &[(Gate, String)]) -> (HashMap<String, Gate>, HashMap<String, String>) {
    let mut lookup = HashMap::new();
    let mut reverse_lookup = HashMap::new();

    for (gate, output) in gates {
        lookup.insert(output.clone(), Gate {
            a: gate.a.clone(),
            op: gate.op.clone(),
            b: gate.b.clone(),
        });
        let mut inputs = vec![gate.a.clone(), gate.b.clone()];
        inputs.sort();
        let key = format!("{}_{}_{}", inputs[0], gate.op, inputs[1]);
        reverse_lookup.insert(key, output.clone());
    }
    (lookup, reverse_lookup)
}

fn swap(pairs: &mut Vec<[String; 2]>, gates: &mut Vec<(Gate, String)>, a: &str, b: &str) {
    pairs.push([a.to_string(), b.to_string()]);
    for (_, output) in gates.iter_mut() {
        if output == a {
            *output = b.to_string();
        } else if output == b {
            *output = a.to_string();
        }
    }
}

fn get_reverse_lookup_key(a: &str, op: &str, b: &str) -> String {
    let mut inputs = vec![a.to_string(), b.to_string()];
    inputs.sort();
    format!("{}_{}_{}", inputs[0], op, inputs[1])
}

fn solution(mut gates: Vec<(Gate, String)>) -> String {
    let mut pairs = Vec::new();
    let num_z = gates.iter().filter(|(_, output)| output.starts_with('z')).count();
    while pairs.len() < 4 {
        let mut adder = String::new();
        let mut carry = String::new();
        let (lookup, reverse_lookup) = create_lookups(&gates);

        for i in 0..num_z {
            let xi = format!("x{:02}", i);
            let yi = format!("y{:02}", i);
            let zi = format!("z{:02}", i);

            if i == 0 {
                adder = reverse_lookup
                    .get(&get_reverse_lookup_key(&xi, "XOR", &yi))
                    .cloned()
                    .unwrap_or_default();
                carry = reverse_lookup
                    .get(&get_reverse_lookup_key(&xi, "AND", &yi))
                    .cloned()
                    .unwrap_or_default();
            } else {
                let bit = reverse_lookup
                    .get(&get_reverse_lookup_key(&xi, "XOR", &yi))
                    .cloned()
                    .unwrap_or_default();
                if !bit.is_empty() {
                    adder = reverse_lookup
                        .get(&get_reverse_lookup_key(&bit, "XOR", &carry))
                        .cloned()
                        .unwrap_or_default();
                    if !adder.is_empty() {
                        let c1 = reverse_lookup
                            .get(&get_reverse_lookup_key(&xi, "AND", &yi))
                            .cloned()
                            .unwrap_or_default();
                        let c2 = reverse_lookup
                            .get(&get_reverse_lookup_key(&bit, "AND", &carry))
                            .cloned()
                            .unwrap_or_default();
                        carry = reverse_lookup
                            .get(&get_reverse_lookup_key(&c1, "OR", &c2))
                            .cloned()
                            .unwrap_or_default();
                    }
                }
            }

            if adder.is_empty() {
                if let Some(gate) = lookup.get(&zi) {
                    let bit_key = get_reverse_lookup_key(&xi, "XOR", &yi);
                    if let Some(bit) = reverse_lookup.get(&bit_key) {
                         if reverse_lookup.contains_key(&get_reverse_lookup_key(&gate.a, "XOR", &carry)){
                           swap(&mut pairs, &mut gates, bit, &gate.a);
                           break;
                        } else if reverse_lookup.contains_key(&get_reverse_lookup_key(&gate.b, "XOR", &carry)){
                           swap(&mut pairs, &mut gates, bit, &gate.b);
                           break;
                        }
                    }
                }
            } else if adder != zi {
                swap(&mut pairs, &mut gates, &adder, &zi);
                break;
            }
        }
    }
    let mut result: Vec<String> = pairs.into_iter().flat_map(|[a,b]| [a, b]).collect();
    result.sort();
    result.join(",")
}

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    if let Some(gates) = parse(&input) {
        println!("{}", solution(gates));
    } else {
        eprintln!("Error parsing input");
    }
    Ok(())
}
