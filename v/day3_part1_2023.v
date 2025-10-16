
import os

fn main() {
    lines := os.read_lines('input.txt') or { panic('cannot read input.txt') }
    mut sum := 0
    mut visited := []bool{len: lines.len * lines[0].len, init: false}
    for y, line in lines {
        mut x := 0
        for x < line.len {
            if !visited[y * line.len + x] && line[x].is_digit() {
                mut num := 0
                mut start := x
                for x < line.len && line[x].is_digit() {
                    num = num * 10 + int(line[x] - `0`)
                    x++
                }
                for k in start .. x { visited[y * line.len + k] = true }
                if adjacent(lines, start, y, x - start) { sum += num }
            } else { x++ }
        }
    }
    println(sum)
}

fn adjacent(lines []string, x int, y int, len int) bool {
    for dy := y - 1; dy <= y + 1; dy++ {
        for dx := x - 1; dx <= x + len; dx++ {
            if dy < 0 || dy >= lines.len || dx < 0 || dx >= lines[0].len { continue }
            c := lines[dy][dx]
            if c != `.` && !c.is_digit() { return true }
        }
    }
    return false
}
