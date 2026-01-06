
use std::fs;
use std::io::{self, BufRead};

fn add(a: &str, b: &str) -> String {
    let mut res = Vec::new();
    let mut carry = 0;
    let (mut i, mut j) = (a.len(), b.len());
    while i > 0 || j > 0 || carry > 0 {
        let da = if i > 0 { a.as_bytes()[i - 1] - b'0' } else { 0 };
        let db = if j > 0 { b.as_bytes()[j - 1] - b'0' } else { 0 };
        let sum = da + db + carry;
        res.push((sum % 10) + b'0');
        carry = sum / 10;
        if i > 0 { i -= 1; }
        if j > 0 { j -= 1; }
    }
    res.reverse();
    String::from_utf8(res).unwrap()
}

fn mul(a: &str, b: &str) -> String {
    if a == "0" || b == "0" { return "0".to_string(); }
    let la = a.len();
    let lb = b.len();
    let mut tmp = vec![0u32; la + lb];
    for i in 0..la {
        let da = (a.as_bytes()[la - 1 - i] - b'0') as u32;
        for j in 0..lb {
            let db = (b.as_bytes()[lb - 1 - j] - b'0') as u32;
            tmp[i + j] += da * db;
        }
    }
    let mut carry = 0;
    for k in 0..tmp.len() {
        let sum = tmp[k] + carry;
        tmp[k] = sum % 10;
        carry = sum / 10;
    }
    let mut len = tmp.len();
    while len > 1 && tmp[len - 1] == 0 { len -= 1; }
    let mut res = Vec::with_capacity(len);
    for i in (0..len).rev() {
        res.push((tmp[i] as u8 + b'0') as char);
    }
    res.iter().collect()
}

fn process_block(lines: &[String], start: usize, end: usize, grand_total: &mut String) {
    let mut nums = Vec::new();
    let mut op = '+';
    for c in start..=end {
        let mut buf = Vec::new();
        for line in lines {
            if c < line.len() {
                let ch = line.as_bytes()[c];
                if ch.is_ascii_digit() {
                    buf.push(ch);
                } else if ch == b'+' || ch == b'*' {
                    op = ch as char;
                }
            }
        }
        if !buf.is_empty() {
            nums.push(String::from_utf8(buf).unwrap());
        }
    }
    if nums.is_empty() { return; }
    let block_res = if op == '*' {
        let mut prod = "1".to_string();
        for n in &nums {
            prod = mul(&prod, n);
        }
        prod
    } else {
        let mut sum = "0".to_string();
        for n in &nums {
            sum = add(&sum, n);
        }
        sum
    };
    *grand_total = add(&grand_total, &block_res);
}

fn main() -> io::Result<()> {
    let file = fs::File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .filter_map(Result::ok)
        .map(|l| l.trim_end().to_string())
        .collect();
    if lines.is_empty() {
        println!("Grand total: 0");
        return Ok(());
    }
    let maxw = lines.iter().map(|l| l.len()).max().unwrap_or(0);
    let is_sep: Vec<bool> = (0..maxw)
        .map(|x| lines.iter().all(|l| x >= l.len() || l.as_bytes()[x].is_ascii_whitespace()))
        .collect();
    let mut grand_total = "0".to_string();
    let mut in_block = false;
    let mut start = 0;
    for x in 0..maxw {
        if !is_sep[x] {
            if !in_block {
                in_block = true;
                start = x;
            }
        } else {
            if in_block {
                process_block(&lines, start, x - 1, &mut grand_total);
                in_block = false;
            }
        }
    }
    if in_block {
        process_block(&lines, start, maxw - 1, &mut grand_total);
    }
    println!("Grand total: {}", grand_total);
    Ok(())
}
