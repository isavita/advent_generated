import os

fn main() {
    mut fabric := [][]int{len: 1000, init: []int{len: 1000, init: 0}}
    claims := os.read_lines('input.txt') or { panic(err) }

    for claim in claims {
        parts := claim.split(' ')
        coords := parts[2].trim_right(':').split(',')
        size := parts[3].split('x')

        x := coords[0].int()
        y := coords[1].int()
        width := size[0].int()
        height := size[1].int()

        for i in 0 .. width {
            for j in 0 .. height {
                fabric[x + i][y + j]++
            }
        }
    }

    mut overlap_count := 0
    for row in fabric {
        for cell in row {
            if cell > 1 {
                overlap_count++
            }
        }
    }

    println(overlap_count)
}