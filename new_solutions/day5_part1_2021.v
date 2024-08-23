import os

fn main() {
    mut grid := map[string]int{}
    lines := os.read_lines('input.txt') or { panic(err) }

    for line in lines {
        parts := line.split(' -> ')
        start := parts[0].split(',')
        end := parts[1].split(',')

        x1 := start[0].int()
        y1 := start[1].int()
        x2 := end[0].int()
        y2 := end[1].int()

        if x1 == x2 || y1 == y2 {
            x_start := if x1 < x2 { x1 } else { x2 }
            x_end := if x1 < x2 { x2 } else { x1 }
            y_start := if y1 < y2 { y1 } else { y2 }
            y_end := if y1 < y2 { y2 } else { y1 }

            for x in x_start..x_end + 1 {
                for y in y_start..y_end + 1 {
                    key := '$x,$y'
                    grid[key] = grid[key] + 1
                }
            }
        }
    }

    mut overlap_count := 0
    for count in grid.values() {
        if count >= 2 {
            overlap_count++
        }
    }

    println(overlap_count)
}