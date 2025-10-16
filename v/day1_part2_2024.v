
import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut left := []int{}
    mut right := []int{ cap: 10000 }
    for line in data.split_into_lines() {
        if line.len == 0 { continue }
        parts := line.split(' ')
        left << parts[0].int()
        right << parts[parts.len-1].int()
    }
    mut freq := map[int]int{}
    for n in right { freq[n]++ }
    mut score := 0
    for n in left { score += n * freq[n] }
    println(score)
}
