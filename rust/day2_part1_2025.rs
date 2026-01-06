
use std::fs;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let raw = fs::read_to_string("input.txt")?;
    let mut buf = raw.replace(&[' ', '\n', '\r'][..], ",");
    buf.push(',');

    let mut ids = Vec::new();
    let pow10: Vec<u128> = (0..=20).scan(1, |p, _| { let v = *p; *p *= 10; Some(v) }).collect();

    for token in buf.split(',') {
        let mut it = token.splitn(2, '-');
        let (s, e) = match (it.next(), it.next()) {
            (Some(a), Some(b)) => (a.parse::<u128>().unwrap(), b.parse::<u128>().unwrap()),
            _ => continue,
        };
        let (start, end) = if s <= e { (s, e) } else { (e, s) };

        for k in 1..=10 {
            let mul = pow10[k] + 1;
            let min_seed = pow10[k - 1];
            let max_seed = pow10[k] - 1;
            let s_min = ((start + mul - 1) / mul).max(min_seed);
            let s_max = (end / mul).min(max_seed);
            if s_min > s_max {
                continue;
            }
            ids.extend((s_min..=s_max).map(|seed| seed * mul));
        }
    }

    ids.sort_unstable();
    let sum = ids.iter().fold((0, None), |(acc, prev), &x|
        if Some(x) != prev { (acc + x, Some(x)) } else { (acc, prev) }).0;

    writeln!(io::stdout(), "{}", sum)?;
    Ok(())
}
