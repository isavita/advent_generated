
use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
    str::FromStr,
};

#[derive(Debug, Clone)]
enum ModuleType {
    Broadcaster,
    FlipFlop,
    Conjunction,
}

#[derive(Debug, Clone)]
struct FlipFlop {
    module_type: ModuleType,
    connects_to: Vec<String>,
    state: bool,
}

#[derive(Debug, Clone)]
struct Conjunction {
    module_type: ModuleType,
    connects_to: Vec<String>,
    watches: HashMap<String, bool>,
}

#[derive(Debug, Clone)]
struct Broadcaster {
    module_type: ModuleType,
    connects_to: Vec<String>,
}

#[derive(Debug, Clone)]
enum Module {
    Broadcaster(Broadcaster),
    FlipFlop(FlipFlop),
    Conjunction(Conjunction),
}

impl FromStr for ModuleType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "broadcaster" => Ok(ModuleType::Broadcaster),
            "%" => Ok(ModuleType::FlipFlop),
            "&" => Ok(ModuleType::Conjunction),
            _ => Err(()),
        }
    }
}

fn handle_line(line: &str, connections: &mut HashMap<String, Module>) {
    let parts: Vec<&str> = line.split(" -> ").collect();
    let (module_str, connects_to_str) = (parts[0], parts[1]);
    let connects_to: Vec<String> = connects_to_str
        .split(", ")
        .map(|s| s.to_string())
        .collect();

    match ModuleType::from_str(&module_str[0..1]) {
        Ok(ModuleType::Broadcaster) => {
            let module = Broadcaster {
                module_type: ModuleType::Broadcaster,
                connects_to,
            };
            connections.insert(module_str.to_string(), Module::Broadcaster(module));
        }
        Ok(ModuleType::FlipFlop) => {
            let module = FlipFlop {
                module_type: ModuleType::FlipFlop,
                connects_to,
                state: false,
            };
            connections.insert(module_str[1..].to_string(), Module::FlipFlop(module));
        }
        Ok(ModuleType::Conjunction) => {
            let module = Conjunction {
                module_type: ModuleType::Conjunction,
                connects_to,
                watches: HashMap::new(),
            };
            connections.insert(module_str[1..].to_string(), Module::Conjunction(module));
        }
        Err(_) => {
            let module = Broadcaster {
                module_type: ModuleType::Broadcaster,
                connects_to,
            };
           connections.insert(module_str.to_string(), Module::Broadcaster(module));
        }
    }
}

fn complete_watches(connections: &mut HashMap<String, Module>) {
    let mut to_update = Vec::new();
    for (name, module) in connections.iter() {
        if let Module::Conjunction(_) = module {
            to_update.push(name.clone())
        }
    }

    for conj_name in to_update {
        let mut watches = HashMap::new();
        for (name, module) in connections.iter() {
            match module {
                Module::FlipFlop(ff) => {
                    if ff.connects_to.contains(&conj_name) {
                        watches.insert(name.clone(), false);
                    }
                }
                Module::Conjunction(conj) => {
                    if conj.connects_to.contains(&conj_name) {
                        watches.insert(name.clone(), false);
                    }
                }
                _ => {}
            }
        }
        if let Some(Module::Conjunction(conj)) = connections.get_mut(&conj_name) {
           conj.watches = watches;
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    from: String,
    name: String,
    pulse: bool,
}

fn simulate_press(
    connections: &mut HashMap<String, Module>,
    loops: &mut HashMap<String, i32>,
    press_number: i32,
) -> ([i32; 2], bool) {
    let mut queue = Vec::new();
    queue.push(State {
        from: "button".to_string(),
        name: "broadcaster".to_string(),
        pulse: false,
    });

    let mut pulses = [1, 0];
    let mut found = false;
    while let Some(curr_state) = queue.pop() {
        if curr_state.name == "out" {
            continue;
        }

        if curr_state.name == "rx" && !curr_state.pulse {
            found = true;
        }

        let pulse = curr_state.pulse;
       

        if let Some(module) = connections.get_mut(&curr_state.name){
            match module {
                Module::Broadcaster(broadcaster) => {
                    for name in &broadcaster.connects_to {
                        queue.insert(0, State {
                            from: curr_state.name.clone(),
                            name: name.clone(),
                            pulse,
                        });
                       if pulse {
                        pulses[1]+=1;
                       } else {
                        pulses[0]+=1;
                       }
                    }
                },
                Module::FlipFlop(flip_flop) => {
                    if !pulse {
                        flip_flop.state = !flip_flop.state;
                        let next_pulse = flip_flop.state;
                        for name in &flip_flop.connects_to {
                            queue.insert(0, State {
                                from: curr_state.name.clone(),
                                name: name.clone(),
                                pulse: next_pulse,
                            });
                            if next_pulse {
                                pulses[1]+=1;
                            } else {
                                pulses[0]+=1;
                            }
                        }
                    }
                },
                Module::Conjunction(conjunction) => {
                    conjunction.watches.insert(curr_state.from, pulse);

                    let all_true = conjunction.watches.values().all(|&x| x);

                    for name in &conjunction.connects_to {
                        queue.insert(0, State {
                            from: curr_state.name.clone(),
                            name: name.clone(),
                            pulse: !all_true,
                        });
                         if !all_true {
                            pulses[1]+=1;
                         } else {
                            pulses[0]+=1;
                         }
                    }
                    
                    if let Some(curr_loop) = loops.get_mut(&curr_state.name) {
                        if !all_true && *curr_loop == -1 {
                            *curr_loop = press_number;
                        }
                    }
                }
            }
        }
    }
    (pulses, found)
}

fn sum_history(hist: &[[i32; 2]]) -> i32 {
    let mut sum = [0, 0];
    for pulses in hist {
        sum[0] += pulses[0];
        sum[1] += pulses[1];
    }
    sum[0] * sum[1]
}

fn connects_to(from: &str, to: &str, connections: &HashMap<String, Module>) -> bool {
    if let Some(module) = connections.get(from) {
         match module {
            Module::Broadcaster(broadcaster) => broadcaster.connects_to.contains(&to.to_string()),
            Module::FlipFlop(flip_flop) => flip_flop.connects_to.contains(&to.to_string()),
            Module::Conjunction(conjunction) => conjunction.connects_to.contains(&to.to_string()),
         }
    } else {
        false
    }
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut connections = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        handle_line(&line, &mut connections);
    }

    complete_watches(&mut connections);

     let mut px_prev = Vec::new();
    for k in connections.keys() {
        if connects_to(k, "rx", &connections) {
            px_prev.push(k.clone());
        }
    }

     if px_prev.len() != 1 {
        panic!("Error: more than one pxPrev");
    }

    let conj = if let Some(Module::Conjunction(c)) = connections.get(&px_prev[0]){
        c.clone()
    } else {
        panic!("Error: pxPrev is not a conjunction")
    };

    let mut loop_lengths: HashMap<String, i32> = conj.watches.keys().map(|k| (k.clone(), -1)).collect();

    let mut press_number = 0;
    loop {
         press_number+=1;
        let (_, found) = simulate_press(&mut connections, &mut loop_lengths, press_number);
        if found {
            break;
        }
        if loop_lengths.values().all(|&x| x != -1){
            break;
        }
    }


    let sum: i64 = loop_lengths.values().fold(1, |acc, &x| acc * x as i64);
    println!("{}", sum);


    Ok(())
}
