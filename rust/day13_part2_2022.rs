
use std::cmp::Ordering;
use std::fs;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Packet {
    Integer(i32),
    List(Vec<Packet>),
}

fn parse_packet(s: &str) -> Packet {
    let mut stack = Vec::new();
    let mut current_num = String::new();

    for c in s.chars() {
        match c {
            '[' => {
                stack.push(Packet::List(Vec::new()));
            }
            ']' => {
                if !current_num.is_empty() {
                    let num = current_num.parse::<i32>().unwrap();
                    match stack.last_mut() {
                        Some(Packet::List(list)) => list.push(Packet::Integer(num)),
                        _ => unreachable!(),
                    }
                    current_num.clear();
                }
                if stack.len() > 1 {
                  let list = stack.pop().unwrap();
                  match stack.last_mut() {
                    Some(Packet::List(l)) => l.push(list),
                    _ => unreachable!(),
                  }
                }
            }
            ',' => {
                if !current_num.is_empty() {
                    let num = current_num.parse::<i32>().unwrap();
                    match stack.last_mut() {
                        Some(Packet::List(list)) => list.push(Packet::Integer(num)),
                        _ => unreachable!(),
                    }
                    current_num.clear();
                }
            }
            _ => {
                current_num.push(c);
            }
        }
    }
    if stack.is_empty(){
      let num = current_num.parse::<i32>().unwrap();
      Packet::Integer(num)
    } else {
      stack.pop().unwrap()
    }
}

fn compare_packets(left: &Packet, right: &Packet) -> Ordering {
    match (left, right) {
        (Packet::Integer(l), Packet::Integer(r)) => l.cmp(r),
        (Packet::List(l), Packet::List(r)) => {
            let mut left_iter = l.iter();
            let mut right_iter = r.iter();

            loop {
                match (left_iter.next(), right_iter.next()) {
                    (Some(left_val), Some(right_val)) => {
                        let comparison = compare_packets(left_val, right_val);
                        if comparison != Ordering::Equal {
                            return comparison;
                        }
                    }
                    (Some(_), None) => return Ordering::Greater,
                    (None, Some(_)) => return Ordering::Less,
                    (None, None) => return Ordering::Equal,
                }
            }
        }
        (Packet::Integer(l), Packet::List(_)) => {
            compare_packets(&Packet::List(vec![Packet::Integer(*l)]), right)
        }
        (Packet::List(_), Packet::Integer(r)) => {
            compare_packets(left, &Packet::List(vec![Packet::Integer(*r)]))
        }
    }
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = contents.lines().collect();
    
    let mut part1_sum = 0;
    let mut all_packets = Vec::new();
    for chunk in lines.chunks(3) {
      if chunk.len() < 2 {
          continue;
      }
        let left = parse_packet(chunk[0]);
        let right = parse_packet(chunk[1]);

        all_packets.push(left.clone());
        all_packets.push(right.clone());

        if compare_packets(&left, &right) == Ordering::Less {
            part1_sum += (chunk.len()/3 + 1);
        }
    }

    println!("Part 1: {}", part1_sum);
    
    let divider1 = parse_packet("[[2]]");
    let divider2 = parse_packet("[[6]]");
    all_packets.push(divider1.clone());
    all_packets.push(divider2.clone());

    all_packets.sort_by(compare_packets);

    let index1 = all_packets.iter().position(|x| *x == divider1).unwrap() + 1;
    let index2 = all_packets.iter().position(|x| *x == divider2).unwrap() + 1;

    println!("Part 2: {}", index1 * index2);
}
