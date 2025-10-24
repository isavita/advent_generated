
import os

fn main() {
    data := os.read_file('input.txt') or { panic('cannot read input.txt') }
    lines := data.trim_space().split_into_lines()

    // collect empty rows
    mut empty_rows := []int{}
    for r in 0 .. lines.len {
        if !lines[r].contains('#') {
            empty_rows << r
        }
    }

    // collect empty columns
    mut empty_cols := []int{}
    for c in 0 .. lines[0].len {
        mut has := false
        for r in 0 .. lines.len {
            if lines[r][c] == `#` {
                has = true
                break
            }
        }
        if !has {
            empty_cols << c
        }
    }

    // collect galaxies
    mut galaxies := [][]int{}
    for r in 0 .. lines.len {
        for c in 0 .. lines[r].len {
            if lines[r][c] == `#` {
                galaxies << [r, c]
            }
        }
    }

    // compute distances
    mut sum := 0
    for i in 0 .. galaxies.len {
        for j in i + 1 .. galaxies.len {
            r1, c1 := galaxies[i][0], galaxies[i][1]
            r2, c2 := galaxies[j][0], galaxies[j][1]
            mut dr := if r1 < r2 { r2 - r1 } else { r1 - r2 }
            mut dc := if c1 < c2 { c2 - c1 } else { c1 - c2 }

            // account for expansion (each empty row/col crossed adds 1 extra)
            for er in empty_rows {
                if (r1 < er && r2 > er) || (r2 < er && r1 > er) {
                    dr += 1
                }
            }
            for ec in empty_cols {
                if (c1 < ec && c2 > ec) || (c2 < ec && c1 > ec) {
                    dc += 1
                }
            }
            sum += dr + dc
        }
    }

    println(sum)
}
