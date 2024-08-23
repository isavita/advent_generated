import os

fn main() {
    input := os.read_file('input.txt') or { return }
    mut x := 1
    mut cycle := 0
    mut crt := [][]string{len: 6, init: []string{len: 40, init: '.'}}

    for line in input.split('\n') {
        if line == '' { continue }
        parts := line.split(' ')
        instruction := parts[0]
        value := if parts.len > 1 { parts[1].int() } else { 0 }

        if instruction == 'noop' {
            draw_pixel(mut crt, cycle, x)
            cycle++
        } else if instruction == 'addx' {
            draw_pixel(mut crt, cycle, x)
            cycle++
            draw_pixel(mut crt, cycle, x)
            cycle++
            x += value
        }
    }

    for row in crt {
        println(row.join(''))
    }
}

fn draw_pixel(mut crt [][]string, cycle int, x int) {
    row := cycle / 40
    col := cycle % 40
    if col >= x - 1 && col <= x + 1 {
        crt[row][col] = '#'
    }
}