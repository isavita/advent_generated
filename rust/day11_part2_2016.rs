
use std::{
    collections::{HashSet, VecDeque},
    fs,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    elevator_floor: usize,
    floors: [Vec<(usize, bool)>; 4],
}

fn is_valid(floors: &Vec<(usize, bool)>) -> bool {
    let generators: Vec<usize> = floors.iter().filter(|(_, is_gen)| *is_gen).map(|(id, _)| *id).collect();
    if generators.is_empty() {
        return true;
    }
    for (id, is_gen) in floors {
        if !is_gen && !generators.contains(id) {
            return false;
        }
    }
    true
}

fn solve(initial_state: State) -> usize {
    let mut visited: HashSet<State> = HashSet::new();
    let mut queue: VecDeque<(State, usize)> = VecDeque::new();
    queue.push_back((initial_state, 0));

    while let Some((current_state, steps)) = queue.pop_front() {
        if visited.contains(&current_state) {
            continue;
        }
        visited.insert(current_state.clone());

        if current_state.floors[0].is_empty()
            && current_state.floors[1].is_empty()
            && current_state.floors[2].is_empty()
        {
            return steps;
        }
        let current_floor = current_state.elevator_floor;

        for item1_index in 0..current_state.floors[current_floor].len() {
            let item1 = current_state.floors[current_floor][item1_index];
            for item2_index in item1_index..current_state.floors[current_floor].len() {
                let item2 = current_state.floors[current_floor][item2_index];
                for next_floor in if current_floor == 0 {
                    1..2
                } else if current_floor == 3 {
                    2..3
                } else {
                    (current_floor - 1..current_floor).chain(current_floor + 1..current_floor + 2)
                } {
                    let mut next_floors = current_state.floors.clone();
                    let mut next_floor_items: Vec<(usize,bool)> = next_floors[next_floor].clone();
                    let mut current_floor_items: Vec<(usize,bool)> = next_floors[current_floor].clone();
                    current_floor_items.retain(|&x| x!=item1 && x!=item2);
                    if item1 != item2 {
                        next_floor_items.push(item1);
                        next_floor_items.push(item2);
                    } else {
                        next_floor_items.push(item1)
                    }
                    
                    next_floors[current_floor]=current_floor_items;
                    next_floors[next_floor] = next_floor_items;
                    
                    if is_valid(&next_floors[next_floor]) && is_valid(&next_floors[current_floor]){
                        let next_state = State {
                            elevator_floor: next_floor,
                            floors: next_floors,
                        };
                        queue.push_back((next_state, steps + 1));
                    }
                    
                }
            }
            
        }
       
    }
    panic!("No solution found");
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read input");

    let mut initial_state = State {
        elevator_floor: 0,
        floors: [
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ],
    };

    let lines: Vec<&str> = contents.lines().collect();

    let mut id_generator = 0;
    for (i, line) in lines.iter().enumerate(){
        let words: Vec<&str> = line.split_whitespace().collect();
        for (j, word) in words.iter().enumerate(){
           if word.ends_with("generator"){
                let gen_id = if word.starts_with("an"){
                    
                    id_generator = id_generator +1;
                    id_generator
                } else {
                    let gen_name = words[j-1];
                    
                    match gen_name {
                        "hydrogen" => 1,
                        "lithium" => 2,
                        "elerium" => 3,
                        "dilithium" => 4,
                        _ => panic!("unknown generator name")
                    }
                };
                
                initial_state.floors[i].push((gen_id,true));
           } else if word.ends_with("microchip"){
                 let chip_name = words[j-1];
                    let chip_id = match chip_name {
                        "hydrogen-compatible" => 1,
                        "lithium-compatible" => 2,
                        "elerium-compatible" => 3,
                        "dilithium-compatible" => 4,
                        _ => panic!("unknown microchip name")
                    };
               initial_state.floors[i].push((chip_id,false));
           }
        }
        
    }
     println!("{}", solve(initial_state.clone()));

    
    let mut initial_state_part2 = initial_state.clone();

    initial_state_part2.floors[0].push((3,true));
    initial_state_part2.floors[0].push((3,false));
     initial_state_part2.floors[0].push((4,true));
    initial_state_part2.floors[0].push((4,false));

    println!("{}", solve(initial_state_part2));
}
