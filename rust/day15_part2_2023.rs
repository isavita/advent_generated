
use std::fs;

const HASH_TABLE_SIZE: usize = 256;

#[derive(Debug)]
struct Step<'a> {
    label: &'a str,
    num_box: usize,
    operation: char,
    number: u32,
}

fn hash_string(s: &str) -> usize {
    let mut res = 0;
    for char in s.chars() {
        res += char as usize;
        res *= 17;
        res %= HASH_TABLE_SIZE;
    }
    res
}

fn parse_step(step_str: &str) -> Step {
    let label_end = step_str.find(|c: char| c == '=' || c == '-').unwrap();
    let label = &step_str[..label_end];
    let operation = step_str.chars().nth(label_end).unwrap();
    let number = if operation == '=' {
        step_str[label_end + 1..].parse().unwrap()
    } else {
        0
    };

    Step {
        label,
        num_box: hash_string(label),
        operation,
        number,
    }
}

fn get_boxes<'a>(steps_str: impl Iterator<Item = &'a str>) -> [Vec<(&'a str, u32)>; HASH_TABLE_SIZE] {
    let mut boxes: [Vec<(&str, u32)>; HASH_TABLE_SIZE] = [(); HASH_TABLE_SIZE].map(|_| Vec::new());

    for step_str in steps_str {
        let step = parse_step(step_str);
        let box_contents = &mut boxes[step.num_box];

        match step.operation {
            '-' => {
                if let Some(index) = box_contents.iter().position(|(label, _)| *label == step.label) {
                    box_contents.remove(index);
                }
            }
            '=' => {
                if let Some(slot) = box_contents.iter_mut().find(|(label, _)| *label == step.label) {
                    slot.1 = step.number;
                } else {
                    box_contents.push((step.label, step.number));
                }
            }
            _ => panic!("Unexpected operation"),
        }
    }
    boxes
}

fn calculate_power(boxes: &[Vec<(&str, u32)>; HASH_TABLE_SIZE]) -> u32 {
    let mut res = 0;
    for (i_box, box_contents) in boxes.iter().enumerate() {
        for (i_slot, (_, value)) in box_contents.iter().enumerate() {
            res += (i_box as u32 + 1) * (i_slot as u32 + 1) * value;
        }
    }
    res
}

fn solve(input: &str) -> u32 {
    let steps_str = input.split(',');
    let boxes = get_boxes(steps_str);
    calculate_power(&boxes)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    println!("{}", solve(input.trim()));
}
