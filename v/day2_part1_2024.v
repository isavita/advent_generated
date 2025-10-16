
import os

fn is_safe(levels []int) bool {
    if levels.len < 2 { return false }
    diff := levels[1] - levels[0]
    if diff == 0 { return false }
    inc := diff > 0
    for i := 0; i < levels.len - 1; i++ {
        d := levels[i+1] - levels[i]
        if d == 0 || (inc && d <= 0) || (!inc && d >= 0) { return false }
        ad := if d < 0 { -d } else { d }
        if ad < 1 || ad > 3 { return false }
    }
    return true
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut safe := 0
    for line in txt.split_into_lines() {
        mut levels := []int{}
        for tok in line.split(' ') {
            if tok != '' { levels << tok.int() }
        }
        if is_safe(levels) { safe++ }
    }
    println(safe)
}
