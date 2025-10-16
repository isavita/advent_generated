
import os

fn main() {
    content := os.read_file('input.txt') or { return }
    mut total := 0
    for line in content.split_into_lines() {
        colon := line.index(':') or { continue }
        parts := line[colon + 1..].split('|')
        winning := parts[0].split(' ').filter(it != '')
        mut points := 0
        for n in parts[1].split(' ').filter(it != '') {
            if n in winning {
                points = if points == 0 { 1 } else { points * 2 }
            }
        }
        total += points
    }
    println(total)
}
