
import os

fn read_input(filename string) (string, [][]bool) {
    data := os.read_file(filename) or { panic(err) }
    parts := data.split('\n\n')
    algorithm := parts[0].trim_space()
    mut image := [][]bool{}
    for line in parts[1].split_into_lines() {
        if line.len == 0 { continue }
        mut row := []bool{cap: line.len}
        for c in line {
            row << (c == `#`)
        }
        image << row
    }
    return algorithm, image
}

fn enhance_image(algorithm string, image [][]bool, use_infinite_lit bool) [][]bool {
    expand_by := 1
    h := image.len + 2*expand_by
    w := image[0].len + 2*expand_by
    mut new_image := [][]bool{len: h, init: []bool{len: w}}

    for y := -expand_by; y < image.len+expand_by; y++ {
        for x := -expand_by; x < image[0].len+expand_by; x++ {
            mut idx := 0
            for dy := -1; dy <= 1; dy++ {
                for dx := -1; dx <= 1; dx++ {
                    idx <<= 1
                    ny := y + dy
                    nx := x + dx
                    lit := if ny >= 0 && ny < image.len && nx >= 0 && nx < image[0].len {
                        image[ny][nx]
                    } else {
                        use_infinite_lit
                    }
                    if lit { idx |= 1 }
                }
            }
            new_image[y+expand_by][x+expand_by] = algorithm[idx] == `#`
        }
    }
    return new_image
}

fn count_lit(image [][]bool) int {
    mut c := 0
    for row in image {
        for pix in row {
            if pix { c++ }
        }
    }
    return c
}

fn main() {
    algorithm, mut image := read_input('input.txt')
    iterations := 50
    for i := 0; i < iterations; i++ {
        image = enhance_image(algorithm, image, i%2==1 && algorithm[0]==`#`)
    }
    println(count_lit(image))
}
