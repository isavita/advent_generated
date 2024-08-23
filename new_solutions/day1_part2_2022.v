import os

fn main() {
    mut calories := []int{}
    mut current := 0

    lines := os.read_lines('input.txt') or { return }
    for line in lines {
        if line.trim_space() == '' {
            calories << current
            current = 0
        } else {
            current += line.int()
        }
    }
    calories << current

    calories.sort(a > b)
    top_three_total := calories[0] + calories[1] + calories[2]
    println(top_three_total)
}