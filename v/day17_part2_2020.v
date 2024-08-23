import os

fn main() {
    mut active := map[string]bool{}
    lines := os.read_lines('input.txt') or { panic(err) }

    for y, line in lines {
        for x, ch in line {
            if ch == `#` {
                active['$x,$y,0,0'] = true
            }
        }
    }

    for _ in 0 .. 6 {
        mut new_active := map[string]bool{}
        mut neighbor_count := map[string]int{}

        for key in active.keys() {
            coords := key.split(',')
            x := coords[0].int()
            y := coords[1].int()
            z := coords[2].int()
            w := coords[3].int()

            for dx in -1 .. 2 {
                for dy in -1 .. 2 {
                    for dz in -1 .. 2 {
                        for dw in -1 .. 2 {
                            if dx == 0 && dy == 0 && dz == 0 && dw == 0 {
                                continue
                            }
                            neighbor := '${x + dx},${y + dy},${z + dz},${w + dw}'
                            neighbor_count[neighbor]++
                        }
                    }
                }
            }
        }

        for key, count in neighbor_count {
            if (active[key] && (count == 2 || count == 3)) || (!active[key] && count == 3) {
                new_active[key] = true
            }
        }

        active = new_active.clone()
    }

    println(active.len)
}