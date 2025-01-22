
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
enum Packet {
    Integer(i32),
    List(Vec<Packet>),
}

impl Packet {
    fn from_str_recursive(s: &str, index: &mut usize) -> Packet {
        if s.chars().nth(*index) == Some('[') {
            *index += 1;
            let mut list = Vec::new();
            while s.chars().nth(*index) != Some(']') {
                list.push(Packet::from_str_recursive(s, index));
                if s.chars().nth(*index) == Some(',') {
                    *index += 1;
                }
            }
            *index += 1;
            Packet::List(list)
        } else {
            let mut num_str = String::new();
            while let Some(c) = s.chars().nth(*index) {
                if c.is_ascii_digit() {
                    num_str.push(c);
                    *index += 1;
                } else {
                    break;
                }
            }
            Packet::Integer(num_str.parse().unwrap())
        }
    }
}

impl FromStr for Packet {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut index = 0;
        Ok(Packet::from_str_recursive(s, &mut index))
    }
}

fn compare_packets(left: &Packet, right: &Packet) -> std::cmp::Ordering {
    match (left, right) {
        (Packet::Integer(l), Packet::Integer(r)) => l.cmp(r),
        (Packet::List(l), Packet::List(r)) => {
            for (l_item, r_item) in l.iter().zip(r.iter()) {
                let cmp = compare_packets(l_item, r_item);
                if cmp != std::cmp::Ordering::Equal {
                    return cmp;
                }
            }
            l.len().cmp(&r.len())
        }
        (Packet::Integer(l), Packet::List(_)) => {
            compare_packets(&Packet::List(vec![Packet::Integer(*l)]), right)
        }
        (Packet::List(_), Packet::Integer(r)) => {
            compare_packets(left, &Packet::List(vec![Packet::Integer(*r)]))
        }
    }
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut sum_of_indices = 0;
    let mut pair_index = 1;
    let mut lines = reader.lines();

    while let Some(Ok(left_str)) = lines.next() {
        if let Some(Ok(right_str)) = lines.next() {
            let _ = lines.next();

            let left_packet = Packet::from_str(&left_str).unwrap();
            let right_packet = Packet::from_str(&right_str).unwrap();


            if compare_packets(&left_packet, &right_packet) == std::cmp::Ordering::Less {
                sum_of_indices += pair_index;
            }
            pair_index += 1;
        }
    }

    println!("{}", sum_of_indices);

    Ok(())
}
