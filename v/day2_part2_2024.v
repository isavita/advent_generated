
import os

fn is_safe(levels []int) bool {
    if levels.len < 2 { return false }
    dir := levels[1] - levels[0]
    if dir == 0 { return false }
    for i := 0; i < levels.len - 1; i++ {
        diff := levels[i+1] - levels[i]
        if diff == 0 || (dir > 0 && diff <= 0) || (dir < 0 && diff >= 0) { return false }
        abs := if diff < 0 { -diff } else { diff }
        if abs < 1 || abs > 3 { return false }
    }
    return true
}

fn is_safe_damp(levels []int) bool {
    if is_safe(levels) { return true }
    mut tmp := []int{cap: levels.len}
    for i := 0; i < levels.len; i++ {
        tmp.clear()
        for j := 0; j < levels.len; j++ {
            if j != i { tmp << levels[j] }
        }
        if is_safe(tmp) { return true }
    }
    return false
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut safe := 0
    for line in txt.split_into_lines() {
        mut levels := []int{}
        for tok in line.split(' ') { levels << tok.int() }
        if is_safe_damp(levels) { safe++ }
    }
    println(safe)
}
