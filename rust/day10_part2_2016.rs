
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
enum Destination {
    Bot(usize),
    Output(usize),
}

#[derive(Debug)]
struct Instruction {
    bot_id: usize,
    low_dest: Destination,
    high_dest: Destination,
}

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut instructions = HashMap::new();
    let mut bot_chips: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut output_bins: HashMap<usize, usize> = HashMap::new();

    for line in reader.lines() {
        let line = line.expect("Error reading line");
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts[0] == "value" {
            let value = parts[1].parse::<usize>().unwrap();
            let bot_id = parts[5].parse::<usize>().unwrap();
            bot_chips.entry(bot_id).or_insert_with(Vec::new).push(value);
        } else if parts[0] == "bot" {
            let bot_id = parts[1].parse::<usize>().unwrap();
            let low_dest = match parts[5] {
                "bot" => Destination::Bot(parts[6].parse::<usize>().unwrap()),
                "output" => Destination::Output(parts[6].parse::<usize>().unwrap()),
                _ => panic!("Invalid destination type"),
            };
            let high_dest = match parts[10] {
                "bot" => Destination::Bot(parts[11].parse::<usize>().unwrap()),
                "output" => Destination::Output(parts[11].parse::<usize>().unwrap()),
                _ => panic!("Invalid destination type"),
            };
            instructions.insert(
                bot_id,
                Instruction {
                    bot_id,
                    low_dest,
                    high_dest,
                },
            );
        }
    }

    let mut part_1_answer = 0;
    loop {
        let mut bot_to_process = Vec::new();
        for (bot_id, chips) in bot_chips.iter() {
            if chips.len() == 2 {
                bot_to_process.push(*bot_id);
            }
        }
        
        if bot_to_process.is_empty(){
            break;
        }
        
        for bot_id in bot_to_process{
            let chips = bot_chips.get_mut(&bot_id).unwrap();
            chips.sort();
            let low_chip = chips.remove(0);
            let high_chip = chips.remove(0);


             if low_chip == 17 && high_chip == 61 {
                 part_1_answer = bot_id;
             }

            let instruction = instructions.get(&bot_id).unwrap();

            match instruction.low_dest {
                Destination::Bot(dest_bot) => {
                    bot_chips.entry(dest_bot).or_insert_with(Vec::new).push(low_chip);
                }
                Destination::Output(output_id) => {
                    output_bins.insert(output_id, low_chip);
                }
            }

            match instruction.high_dest {
                Destination::Bot(dest_bot) => {
                    bot_chips.entry(dest_bot).or_insert_with(Vec::new).push(high_chip);
                }
                Destination::Output(output_id) => {
                     output_bins.insert(output_id, high_chip);
                }
            }
        }
    }
        
    println!("Part 1: {}", part_1_answer);

    let part2_answer = output_bins.get(&0).unwrap() * output_bins.get(&1).unwrap() * output_bins.get(&2).unwrap();
    println!("Part 2: {}", part2_answer);
}
