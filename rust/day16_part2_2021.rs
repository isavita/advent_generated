
use std::fs::File;
use std::io::{self, Read};

fn hex_to_bin(hex: &str) -> String {
    hex.chars()
        .map(|c| match c {
            '0' => "0000",
            '1' => "0001",
            '2' => "0010",
            '3' => "0011",
            '4' => "0100",
            '5' => "0101",
            '6' => "0110",
            '7' => "0111",
            '8' => "1000",
            '9' => "1001",
            'A' => "1010",
            'B' => "1011",
            'C' => "1100",
            'D' => "1101",
            'E' => "1110",
            'F' => "1111",
            _ => "",
        })
        .collect::<String>()
}

fn parse_packet(bits: &str, index: &mut usize, version_sum: &mut u64) -> u64 {
    let version = u64::from_str_radix(&bits[*index..*index + 3], 2).unwrap();
    *version_sum += version;
    *index += 3;

    let type_id = u64::from_str_radix(&bits[*index..*index + 3], 2).unwrap();
    *index += 3;

    if type_id == 4 {
        let mut value_bits = String::new();
        loop {
            let chunk = &bits[*index..*index + 5];
            *index += 5;
            value_bits.push_str(&chunk[1..]);
            if chunk.starts_with('0') {
                break;
            }
        }
        u64::from_str_radix(&value_bits, 2).unwrap()
    } else {
        let length_type_id = &bits[*index..*index + 1];
        *index += 1;
        let mut sub_values = Vec::new();
        if length_type_id == "0" {
            let total_length = u64::from_str_radix(&bits[*index..*index + 15], 2).unwrap();
            *index += 15;
            let start_index = *index;
            while *index - start_index < total_length as usize {
              sub_values.push(parse_packet(bits, index, version_sum));
            }
        } else {
            let num_sub_packets = u64::from_str_radix(&bits[*index..*index + 11], 2).unwrap();
            *index += 11;
            for _ in 0..num_sub_packets {
                sub_values.push(parse_packet(bits, index, version_sum));
            }
        }
        match type_id {
            0 => sub_values.iter().sum(),
            1 => sub_values.iter().product(),
            2 => *sub_values.iter().min().unwrap(),
            3 => *sub_values.iter().max().unwrap(),
            5 => {
              if sub_values[0] > sub_values[1] {1} else {0}
            }
            6 => {
                if sub_values[0] < sub_values[1] {1} else {0}
            },
            7 => {
                if sub_values[0] == sub_values[1] {1} else {0}
            },
            _ => 0,
        }
    }
}

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut hex_input = String::new();
    file.read_to_string(&mut hex_input)?;
    let hex_input = hex_input.trim();
    let binary_input = hex_to_bin(hex_input);

    let mut index = 0;
    let mut version_sum = 0;
    let result = parse_packet(&binary_input, &mut index, &mut version_sum);
    println!("{}", result);
    Ok(())
}
