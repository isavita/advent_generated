import os

fn main() {
    mut vent_map := map[string]int{}
    lines := os.read_lines('input.txt') or { return }

    for line in lines {
        parts := line.split(' -> ')
        start := parts[0].split(',')
        end := parts[1].split(',')
        x1 := start[0].int()
        y1 := start[1].int()
        x2 := end[0].int()
        y2 := end[1].int()

        dx := if x1 < x2 { 1 } else if x1 > x2 { -1 } else { 0 }
        dy := if y1 < y2 { 1 } else if y1 > y2 { -1 } else { 0 }

        mut x := x1
        mut y := y1
        for {
            point := '$x,$y'
            vent_map[point] = vent_map[point] + 1
            if x == x2 && y == y2 { break }
            x += dx
            y += dy
        }
    }

    mut overlap_count := 0
    for count in vent_map.values() {
        if count >= 2 {
            overlap_count++
        }
    }

    println(overlap_count)
}