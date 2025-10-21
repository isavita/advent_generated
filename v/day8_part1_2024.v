
import os

fn main() {
    content := os.read_file('input.txt') or { panic('cannot read input.txt') }
    lines := content.split_into_lines()
    if lines.len == 0 { println(0); return }

    h := lines.len
    w := lines[0].len
    mut antennas := map[u8][][]int{}

    for y in 0 .. h {
        for x in 0 .. w {
            c := lines[y][x]
            if c != `.` {
                antennas[c] << [y, x]
            }
        }
    }

    mut antinodes := map[string]bool{}

    for _, coords in antennas {
        for i in 0 .. coords.len {
            for j in i + 1 .. coords.len {
                a0, a1 := coords[i][0], coords[i][1]
                b0, b1 := coords[j][0], coords[j][1]

                p0 := 2 * a0 - b0
                p1 := 2 * a1 - b1
                q0 := 2 * b0 - a0
                q1 := 2 * b1 - a1

                if p0 >= 0 && p0 < h && p1 >= 0 && p1 < w {
                    antinodes['${p0},${p1}'] = true
                }
                if q0 >= 0 && q0 < h && q1 >= 0 && q1 < w {
                    antinodes['${q0},${q1}'] = true
                }
            }
        }
    }

    println(antinodes.len)
}
