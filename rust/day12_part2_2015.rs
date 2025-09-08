
use std::fs;

fn calculate_sum_part1(buffer: &str) -> i64 {
    let mut total_sum = 0;
    let mut ptr = 0;
    let bytes = buffer.as_bytes();
    let len = bytes.len();

    while ptr < len {
        if bytes[ptr] == b'-' || bytes[ptr].is_ascii_digit() {
            let start = ptr;
            ptr += 1;
            while ptr < len && bytes[ptr].is_ascii_digit() {
                ptr += 1;
            }
            if let Ok(num) = buffer[start..ptr].parse::<i64>() {
                total_sum += num;
            }
        } else {
            ptr += 1;
        }
    }
    total_sum
}

fn parse_value(ptr: &mut usize, buffer: &[u8], ignore_red: bool) -> i64 {
    skip_whitespace(ptr, buffer);
    if *ptr >= buffer.len() {
        return 0;
    }

    match buffer[*ptr] {
        b'{' => parse_object(ptr, buffer, ignore_red),
        b'[' => parse_array(ptr, buffer, ignore_red),
        b'"' => {
            let is_red = parse_string_and_check_red(ptr, buffer);
            0
        }
        b'-' | b'0'..=b'9' => parse_number(ptr, buffer),
        _ => {
            if *ptr + 3 < buffer.len() && &buffer[*ptr..*ptr + 4] == b"true" {
                *ptr += 4;
                0
            } else if *ptr + 4 < buffer.len() && &buffer[*ptr..*ptr + 5] == b"false" {
                *ptr += 5;
                0
            } else if *ptr + 3 < buffer.len() && &buffer[*ptr..*ptr + 4] == b"null" {
                *ptr += 4;
                0
            } else {
                *ptr += 1;
                0
            }
        }
    }
}

fn skip_whitespace(ptr: &mut usize, buffer: &[u8]) {
    while *ptr < buffer.len() && buffer[*ptr].is_ascii_whitespace() {
        *ptr += 1;
    }
}

fn parse_string_and_check_red(ptr: &mut usize, buffer: &[u8]) -> bool {
    if buffer[*ptr] != b'"' {
        return false;
    }
    *ptr += 1;
    let start = *ptr;
    while *ptr < buffer.len() && buffer[*ptr] != b'"' {
        if buffer[*ptr] == b'\\' {
            *ptr += 1;
            if *ptr < buffer.len() {
                *ptr += 1;
            }
        } else {
            *ptr += 1;
        }
    }
    let is_red = *ptr - start == 3 && &buffer[start..start + 3] == b"red";
    if *ptr < buffer.len() && buffer[*ptr] == b'"' {
        *ptr += 1;
    }
    is_red
}

fn parse_number(ptr: &mut usize, buffer: &[u8]) -> i64 {
    let start = *ptr;
    if buffer[*ptr] == b'-' {
        *ptr += 1;
    }
    while *ptr < buffer.len() && buffer[*ptr].is_ascii_digit() {
        *ptr += 1;
    }
    if let Ok(num) = std::str::from_utf8(&buffer[start..*ptr]).unwrap().parse::<i64>() {
        num
    } else {
        0
    }
}

fn parse_array(ptr: &mut usize, buffer: &[u8], ignore_red: bool) -> i64 {
    *ptr += 1;
    skip_whitespace(ptr, buffer);
    if *ptr >= buffer.len() {
        return 0;
    }

    if buffer[*ptr] == b']' {
        *ptr += 1;
        return 0;
    }

    let mut sum = 0;
    loop {
        skip_whitespace(ptr, buffer);
        sum += parse_value(ptr, buffer, ignore_red);
        skip_whitespace(ptr, buffer);

        if *ptr >= buffer.len() {
            break;
        }
        if buffer[*ptr] == b']' {
            *ptr += 1;
            break;
        } else if buffer[*ptr] == b',' {
            *ptr += 1;
        } else {
            break;
        }
    }
    sum
}

fn parse_object(ptr: &mut usize, buffer: &[u8], ignore_red: bool) -> i64 {
    *ptr += 1;
    skip_whitespace(ptr, buffer);
    if *ptr >= buffer.len() {
        return 0;
    }

    if buffer[*ptr] == b'}' {
        *ptr += 1;
        return 0;
    }

    let mut sum = 0;
    let mut contains_red = false;

    loop {
        skip_whitespace(ptr, buffer);
        if *ptr >= buffer.len() {
            break;
        }

        if buffer[*ptr] == b'"' {
            parse_string_and_check_red(ptr, buffer);
        } else {
            break;
        }

        skip_whitespace(ptr, buffer);
        if *ptr >= buffer.len() || buffer[*ptr] != b':' {
            break;
        }
        *ptr += 1;
        skip_whitespace(ptr, buffer);

        if ignore_red && *ptr < buffer.len() && buffer[*ptr] == b'"' {
            let peek_ptr = *ptr;
            let is_red = parse_string_and_check_red(&mut peek_ptr.clone(), buffer);
            if is_red {
                contains_red = true;
            }
            parse_value(ptr, buffer, ignore_red);
        } else {
            sum += parse_value(ptr, buffer, ignore_red);
        }

        skip_whitespace(ptr, buffer);
        if *ptr >= buffer.len() {
            break;
        }
        if buffer[*ptr] == b'}' {
            *ptr += 1;
            break;
        } else if buffer[*ptr] == b',' {
            *ptr += 1;
        } else {
            break;
        }
    }

    if ignore_red && contains_red {
        0
    } else {
        sum
    }
}

fn calculate_sum_part2(buffer: &str) -> i64 {
    let bytes = buffer.as_bytes();
    let mut ptr = 0;
    parse_value(&mut ptr, bytes, true)
}

fn main() {
    let buffer = fs::read_to_string("input.txt").unwrap();
    let sum1 = calculate_sum_part1(&buffer);
    let sum2 = calculate_sum_part2(&buffer);
    println!("Part 1 Sum: {}", sum1);
    println!("Part 2 Sum: {}", sum2);
}
