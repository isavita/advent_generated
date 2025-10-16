
import os

fn ways(t i64, r i64) i64 {
    mut lo, mut hi := i64(0), t
    for lo <= hi {
        m := (lo + hi) / 2
        if m * (t - m) > r {
            hi = m - 1
        } else {
            lo = m + 1
        }
    }
    first := lo
    lo, hi = 0, t
    for lo <= hi {
        m := (lo + hi) / 2
        if m * (t - m) > r {
            lo = m + 1
        } else {
            hi = m - 1
        }
    }
    return hi - first + 1
}

fn main() {
    buf := os.read_file('input.txt') or { panic(err) }
    mut t, mut d := i64(0), i64(0)
    for line in buf.split_into_lines() {
        if line.len == 0 { continue }
        mut digits := ''
        for c in line {
            if c.is_digit() { digits += c.ascii_str() }
        }
        if t == 0 {
            t = digits.i64()
        } else {
            d = digits.i64()
        }
    }
    println(ways(t, d))
}
