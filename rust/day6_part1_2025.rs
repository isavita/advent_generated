
use std::fs;
use std::io::{self, BufRead};

fn is_sep(col: usize, lines: &[String]) -> bool {
    lines.iter().all(|l| col >= l.len() || l.as_bytes()[col].is_ascii_whitespace())
}

fn bignum_add(a: &str, b: &str) -> String {
    let mut res = Vec::new();
    let mut carry = 0;
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    let mut i = a.len();
    let mut j = b.len();
    while i > 0 || j > 0 || carry > 0 {
        let da = if i > 0 { a_bytes[i - 1] - b'0' } else { 0 };
        let db = if j > 0 { b_bytes[j - 1] - b'0' } else { 0 };
        let sum = da as u32 + db as u32 + carry;
        carry = sum / 10;
        res.push((sum % 10) as u8 + b'0');
        if i > 0 { i -= 1; }
        if j > 0 { j -= 1; }
    }
    while res.len() > 1 && res.last() == Some(&b'0') { res.pop(); }
    res.reverse();
    String::from_utf8(res).unwrap()
}

fn bignum_mul(a: &str, b: &str) -> String {
    let al = a.len();
    let bl = b.len();
    let mut prod = vec![0u32; al + bl];
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();
    for i in (0..al).rev() {
        for j in (0..bl).rev() {
            prod[i + j + 1] += (a_bytes[i] - b'0') as u32 * (b_bytes[j] - b'0') as u32;
        }
    }
    for i in (1..al + bl).rev() {
        prod[i - 1] += prod[i] / 10;
        prod[i] %= 10;
    }
    let start = prod.iter().position(|&x| x != 0).unwrap_or(prod.len() - 1);
    prod[start..].iter().map(|&d| (d as u8 + b'0') as char).collect()
}

fn process_block(sc: usize, ec: usize, lines: &[String], grand: &mut String) {
    let mut nums = Vec::new();
    let mut op = 0;
    for line in lines {
        let e = (ec + 1).min(line.len());
        if sc >= line.len() { continue; }
        let seg = &line[sc..e];
        let trimmed = seg.trim();
        match trimmed {
            "+" => op = 1,
            "*" => op = 2,
            _ if !trimmed.is_empty() => nums.push(trimmed),
            _ => {}
        }
    }
    if nums.is_empty() { return; }
    let mut acc = if op == 2 { "1".to_string() } else { "0".to_string() };
    if op == 1 {
        for n in &nums {
            let t = acc.clone();
            acc = bignum_add(&t, n);
        }
    } else if op == 2 {
        for n in &nums {
            let t = acc.clone();
            acc = bignum_mul(&t, n);
        }
    } else if nums.len() == 1 {
        acc = nums[0].to_string();
    }
    let t = grand.clone();
    *grand = bignum_add(&t, &acc);
}

fn main() -> io::Result<()> {
    let file = fs::File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file).lines().collect::<Result<_, _>>()?;
    let maxw = lines.iter().map(|l| l.len()).max().unwrap_or(0);
    let mut grand = "0".to_string();
    let mut inb = false;
    let mut sc = 0;
    for x in 0..maxw {
        if !is_sep(x, &lines) {
            if !inb { inb = true; sc = x; }
        } else {
            if inb { process_block(sc, x - 1, &lines, &mut grand); inb = false; }
        }
    }
    if inb { process_block(sc, maxw - 1, &lines, &mut grand); }
    println!("Grand total: {}", grand);
    Ok(())
}
