
use std::fs;

fn is_invalid(mut x: u64) -> bool {
    let mut buf = [0u8; 20];
    let mut n = 0;
    while x > 0 {
        buf[n] = b'0' + (x % 10) as u8;
        x /= 10;
        n += 1;
    }
    if n <= 1 { return false; }

    let s = &buf[..n];
    for p in 1..=n/2 {
        if n % p != 0 { continue; }
        let k = n / p;
        if k < 2 { continue; }
        let mut ok = true;
        for i in p..n {
            if s[i] != s[i % p] { ok = false; break; }
        }
        if ok { return true; }
    }
    false
}

fn main() {
    let data = fs::read_to_string("input.txt").unwrap();
    let mut sum = 0u64;
    let mut p = data.as_str();
    while !p.is_empty() {
        p = p.trim_start_matches(|c: char| c.is_whitespace() || c == ',' || c == '\r' || c == '\n');
        if p.is_empty() { break; }
        let dash = p.find('-').unwrap();
        let a: u64 = p[..dash].parse().unwrap();
        let rest = &p[dash+1..];
        let end = rest.find(|c: char| c.is_whitespace() || c == ',' || c == '\r' || c == '\n').unwrap_or(rest.len());
        let b: u64 = rest[..end].parse().unwrap();
        let (mut x, stop) = if a <= b { (a, b) } else { (b, a) };
        while x <= stop {
            if is_invalid(x) { sum += x; }
            if x == u64::MAX { break; }
            x += 1;
        }
        p = &rest[end..];
    }
    println!("{}", sum);
}
