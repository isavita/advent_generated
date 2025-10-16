
import os

fn main() {
    data := os.read_file('input.txt') or { return }
    mut holders := map[string]bool{}
    mut held := map[string]bool{}

    for line in data.split_into_lines() {
        parts := line.split(' ')
        if parts.len == 0 { continue }
        name := parts[0]
        holders[name] = true
        for i in 3 .. parts.len {
            held[parts[i].trim_right(',')] = true
        }
    }

    for k in holders.keys() {
        if !held[k] {
            println(k)
            return
        }
    }
}
