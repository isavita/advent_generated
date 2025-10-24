
import os { read_file }

const (
    source_x = 500
    source_y = 0
    dx       = [0, -1, 1] // down, down-left, down-right
    dy       = [1, 1, 1]
)

fn main() {
    mut walls := map[string]bool{}
    mut abyss := 0
    for line in read_file('input.txt')!.split_into_lines() {
        mut prev := [0, 0]
        for i, part in line.split(' -> ') {
            xy := part.split(',')
            x := xy[0].int()
            y := xy[1].int()
            if i == 0 {
                prev = [x, y]
            }
            if y > abyss { abyss = y }
            // draw the straight line
            if prev[0] == x { // vertical
                step := if y > prev[1] { 1 } else { -1 }
                mut yy := prev[1]
                for {
                    walls['${x},${yy}'] = true
                    if yy == y { break }
                    yy += step
                }
            } else { // horizontal
                step := if x > prev[0] { 1 } else { -1 }
                mut xx := prev[0]
                for {
                    walls['${xx},${y}'] = true
                    if xx == x { break }
                    xx += step
                }
            }
            prev = [x, y]
        }
    }
    mut sand := map[string]bool{}
    mut count := 0
    for {
        mut x := source_x
        mut y := source_y
        mut settled := false
        for {
            mut moved := false
            for dir in 0 .. 3 {
                nx := x + dx[dir]
                ny := y + dy[dir]
                key := '${nx},${ny}'
                if !walls[key] && !sand[key] {
                    x = nx
                    y = ny
                    moved = true
                    break
                }
            }
            if !moved {
                sand['${x},${y}'] = true
                count++
                settled = true
                break
            }
            if y > abyss {
                println(count)
                return
            }
        }
    }
}
