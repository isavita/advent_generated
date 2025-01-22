
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone, Copy)]
enum Pulse {
    Low,
    High,
}

#[derive(Debug)]
enum ModuleType {
    FlipFlop,
    Conjunction,
    Broadcast,
}

#[derive(Debug)]
struct Module {
    module_type: Option<ModuleType>,
    destinations: Vec<String>,
    state: bool, // For FlipFlop
    memory: HashMap<String, Pulse>, // For Conjunction
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut modules: HashMap<String, Module> = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(" -> ").collect();
        let (module_str, dest_str) = (parts[0], parts[1]);
        let destinations: Vec<String> = dest_str.split(", ").map(|s| s.to_string()).collect();

        let (module_name, module_type) = match module_str.chars().next() {
            Some('%') => (&module_str[1..], Some(ModuleType::FlipFlop)),
            Some('&') => (&module_str[1..], Some(ModuleType::Conjunction)),
            _ => (module_str, Some(ModuleType::Broadcast)),
        };

        modules.insert(
            module_name.to_string(),
            Module {
                module_type,
                destinations,
                state: false,
                memory: HashMap::new(),
            },
        );
    }

    // Initialize conjunction module memories
    let module_names: Vec<String> = modules.keys().cloned().collect();
    for name in module_names.iter() {
        let dests = modules.get(name).unwrap().destinations.clone();
        for dest in dests {
            if let Some(module) = modules.get_mut(&dest) {
                if let Some(ModuleType::Conjunction) = module.module_type {
                    module.memory.insert(name.clone(), Pulse::Low);
                }
            }
        }
    }

    let mut low_pulses = 0;
    let mut high_pulses = 0;

    for _ in 0..1000 {
        let mut queue: VecDeque<(String, String, Pulse)> = VecDeque::new();
        queue.push_back(("button".to_string(), "broadcaster".to_string(), Pulse::Low));
        low_pulses += 1; // button pulse

        while let Some((from, to, pulse)) = queue.pop_front() {
            if let Some(module) = modules.get_mut(&to) {
                match module.module_type {
                    Some(ModuleType::FlipFlop) => {
                        if let Pulse::Low = pulse {
                            module.state = !module.state;
                            let next_pulse = if module.state { Pulse::High } else { Pulse::Low };
                            for dest in &module.destinations {
                                queue.push_back((to.clone(), dest.clone(), next_pulse));
                                match next_pulse {
                                    Pulse::Low => low_pulses +=1,
                                    Pulse::High => high_pulses +=1,
                                }
                            }
                        }
                    },
                    Some(ModuleType::Conjunction) => {
                         module.memory.insert(from, pulse);
                         let next_pulse = if module.memory.values().all(|&p| matches!(p, Pulse::High)) {
                            Pulse::Low
                         } else {
                             Pulse::High
                         };

                        for dest in &module.destinations {
                            queue.push_back((to.clone(), dest.clone(), next_pulse));
                            match next_pulse {
                                Pulse::Low => low_pulses +=1,
                                Pulse::High => high_pulses +=1,
                            }
                        }
                    }
                    Some(ModuleType::Broadcast) => {
                        for dest in &module.destinations {
                            queue.push_back((to.clone(), dest.clone(), pulse));
                            match pulse {
                                Pulse::Low => low_pulses +=1,
                                Pulse::High => high_pulses +=1,
                            }
                        }
                    }
                    None => (),
                }
            }
        }
    }

    println!("{}", low_pulses * high_pulses);

    Ok(())
}
