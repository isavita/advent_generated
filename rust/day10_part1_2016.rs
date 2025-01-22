
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
enum Target {
    Bot(usize),
    Output(usize),
}

#[derive(Debug)]
struct Instruction {
    bot: usize,
    low_target: Target,
    high_target: Target,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut bot_instructions: HashMap<usize, Instruction> = HashMap::new();
    let mut bot_chips: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut output_bins: HashMap<usize, usize> = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        if line.starts_with("value") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let value: usize = parts[1].parse().unwrap();
            let bot: usize = parts[5].parse().unwrap();
            bot_chips.entry(bot).or_insert(Vec::new()).push(value);
        } else if line.starts_with("bot") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let bot: usize = parts[1].parse().unwrap();
            let low_target = match parts[5] {
                "bot" => Target::Bot(parts[6].parse().unwrap()),
                "output" => Target::Output(parts[6].parse().unwrap()),
                _ => panic!("Invalid low target"),
            };
            let high_target = match parts[10] {
                "bot" => Target::Bot(parts[11].parse().unwrap()),
                "output" => Target::Output(parts[11].parse().unwrap()),
                _ => panic!("Invalid high target"),
            };

            bot_instructions.insert(bot, Instruction { bot, low_target, high_target });
        }
    }

    loop {
        let mut bot_with_two_chips = None;
         for (&bot, chips) in &bot_chips {
           if chips.len() == 2 {
            bot_with_two_chips = Some(bot);
            break;
           }
        }
         if bot_with_two_chips.is_none() {
             break;
         }

         let bot = bot_with_two_chips.unwrap();
         let chips = bot_chips.get_mut(&bot).unwrap();
         chips.sort();
            let low_chip = chips.remove(0);
            let high_chip = chips.remove(0);

            if low_chip == 17 && high_chip == 61 {
                println!("Bot responsible for comparing 17 and 61: {}", bot);
                 return Ok(());
            }

            let instruction = bot_instructions.get(&bot).unwrap();

            match &instruction.low_target {
                Target::Bot(target_bot) => {
                    bot_chips.entry(*target_bot).or_insert(Vec::new()).push(low_chip);
                }
                Target::Output(target_output) => {
                    output_bins.insert(*target_output, low_chip);
                }
            }

            match &instruction.high_target {
                Target::Bot(target_bot) => {
                    bot_chips.entry(*target_bot).or_insert(Vec::new()).push(high_chip);
                }
                Target::Output(target_output) => {
                    output_bins.insert(*target_output, high_chip);
                }
            }

         if bot_chips.get(&bot).unwrap().is_empty(){
             bot_chips.remove(&bot);
         }
    }

   Ok(())
}
