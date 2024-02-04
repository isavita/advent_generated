
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rules: Vec<(&str, Vec<(i32, i32)>)> = parts[0].lines().map(|line| {
        let mut iter = line.split(": ");
        let field = iter.next().unwrap();
        let ranges: Vec<(i32, i32)> = iter.next().unwrap().split(" or ").map(|range| {
            let mut range_iter = range.split("-");
            let start: i32 = range_iter.next().unwrap().parse().unwrap();
            let end: i32 = range_iter.next().unwrap().parse().unwrap();
            (start, end)
        }).collect();
        (field, ranges)
    }).collect();
    let nearby_tickets: Vec<Vec<i32>> = parts[2].lines().skip(1).map(|line| {
        line.split(",").map(|num| num.parse().unwrap()).collect()
    }).collect();
    let mut error_rate = 0;
    for ticket in nearby_tickets.iter() {
        for &num in ticket.iter() {
            let mut valid = false;
            for (_, ranges) in rules.iter() {
                for (start, end) in ranges.iter() {
                    if num >= *start && num <= *end {
                        valid = true;
                        break;
                    }
                }
                if valid {
                    break;
                }
            }
            if !valid {
                error_rate += num;
            }
        }
    }
    println!("{}", error_rate);
}
