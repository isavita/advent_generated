
import os

const total_rows = 400000

fn main() {
    first_row := os.read_file('input.txt') or { panic(err) }.trim_space()
    mut safe := 0
    mut row := []u8{cap: first_row.len}
    row << first_row.bytes()
    for b in row {
        if b == `.` { safe++ }
    }
    mut prev := row.clone()
    for _ in 1 .. total_rows {
        row = []u8{len: prev.len}
        for j in 0 .. prev.len {
            l := if j == 0 { `.` } else { prev[j - 1] }
            c := prev[j]
            r := if j + 1 == prev.len { `.` } else { prev[j + 1] }
            trap := (l == `^` && c == `^` && r == `.`) ||
                   (c == `^` && r == `^` && l == `.`) ||
                   (l == `^` && c == `.` && r == `.`) ||
                   (r == `^` && c == `.` && l == `.`)
            row[j] = if trap { `^` } else { safe++; `.` }
        }
        prev = row.clone()
    }
    println(safe)
}
