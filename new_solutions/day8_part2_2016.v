import os

fn main() {
    mut screen := [][]bool{len: 6, init: []bool{len: 50, init: false}}
    instructions := os.read_lines('input.txt') or { return }

    for line in instructions {
        if line.starts_with('rect') {
            parts := line[5..].split('x')
            a := parts[0].int()
            b := parts[1].int()
            for y in 0 .. b {
                for x in 0 .. a {
                    screen[y][x] = true
                }
            }
        } else if line.starts_with('rotate row') {
            parts := line[13..].split(' by ')
            y := parts[0].int()
            b := parts[1].int()
            mut row := screen[y]
            for _ in 0 .. b {
                row.prepend(row.pop())
            }
        } else if line.starts_with('rotate column') {
            parts := line[16..].split(' by ')
            x := parts[0].int()
            b := parts[1].int()
            mut column := []bool{len: 6}
            for y in 0 .. 6 {
                column[y] = screen[y][x]
            }
            for _ in 0 .. b {
                column.prepend(column.pop())
            }
            for y in 0 .. 6 {
                screen[y][x] = column[y]
            }
        }
    }

    for row in screen {
        for pixel in row {
            print(if pixel { '#' } else { '.' })
        }
        println('')
    }
}