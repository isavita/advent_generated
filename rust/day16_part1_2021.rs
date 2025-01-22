
use std::fs::File;
use std::io::{self, Read};

fn hex_to_binary(hex: &str) -> String {
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
        .collect()
}

fn parse_packet(binary: &str, index: usize, version_sum: &mut u64) -> (usize, u64) {
    let mut i = index;
    let version = u64::from_str_radix(&binary[i..i + 3], 2).unwrap();
    *version_sum += version;
    i += 3;

    let type_id = u64::from_str_radix(&binary[i..i + 3], 2).unwrap();
    i += 3;

    if type_id == 4 {
        loop {
            let group = &binary[i..i + 5];
            i += 5;
            if group.starts_with('0') {
                break;
            }
        }
    } else {
        let length_type_id = &binary[i..i + 1];
        i += 1;

        if length_type_id == "0" {
            let length = usize::from_str_radix(&binary[i..i + 15], 2).unwrap();
            i += 15;
            let end = i + length;
            while i < end {
                let (new_i, _ ) = parse_packet(binary, i, version_sum);
                i = new_i;
            }
        } else {
            let sub_packet_count = usize::from_str_radix(&binary[i..i + 11], 2).unwrap();
            i += 11;
            for _ in 0..sub_packet_count {
                let (new_i, _ ) = parse_packet(binary, i, version_sum);
                i = new_i;
            }
        }
    }

    (i, *version_sum)
}

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut hex_input = String::new();
    file.read_to_string(&mut hex_input)?;
    let binary_input = hex_to_binary(hex_input.trim());

    let mut version_sum = 0;
    parse_packet(&binary_input, 0, &mut version_sum);

    println!("{}", version_sum);

    Ok(())
}
