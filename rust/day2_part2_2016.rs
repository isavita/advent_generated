
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let path = Path::new("input.txt");
    let file = File::open(&path).expect("Could not open file");
    let reader = io::BufReader::new(file);

    let instructions: Vec<String> = reader.lines().filter_map(Result::ok).collect();

    let code = get_bathroom_code(&instructions);
    println!("{}", code);
}

fn get_bathroom_code(instructions: &[String]) -> String {
    let mut keypad: HashMap<char, HashMap<char, char>> = HashMap::new();
    keypad.insert('1', HashMap::from([('D', '3')]));
    keypad.insert('2', HashMap::from([('R', '3'), ('D', '6')]));
    keypad.insert('3', HashMap::from([('U', '1'), ('R', '4'), ('D', '7'), ('L', '2')]));
    keypad.insert('4', HashMap::from([('L', '3'), ('D', '8')]));
    keypad.insert('5', HashMap::from([('R', '6')]));
    keypad.insert('6', HashMap::from([('U', '2'), ('R', '7'), ('D', 'A'), ('L', '5')]));
    keypad.insert('7', HashMap::from([('U', '3'), ('R', '8'), ('D', 'B'), ('L', '6')]));
    keypad.insert('8', HashMap::from([('U', '4'), ('R', '9'), ('D', 'C'), ('L', '7')]));
    keypad.insert('9', HashMap::from([('L', '8')]));
    keypad.insert('A', HashMap::from([('U', '6'), ('R', 'B')]));
    keypad.insert('B', HashMap::from([('U', '7'), ('R', 'C'), ('D', 'D'), ('L', 'A')]));
    keypad.insert('C', HashMap::from([('U', '8'), ('L', 'B')]));
    keypad.insert('D', HashMap::from([('U', 'B')]));

    let mut position = '5'; // Start at '5'
    let mut code = String::new();

    for instruction in instructions {
        for move_char in instruction.chars() {
            if let Some(&next_pos) = keypad.get(&position).and_then(|m| m.get(&move_char)) {
                position = next_pos;
            }
        }
        code.push(position);
    }

    code
}
