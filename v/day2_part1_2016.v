import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut position := [1, 1] // Starting at '5' (row 1, column 1)
    mut code := ''

    for line in instructions {
        for move in line {
            match move {
                `U` { if position[0] > 0 { position[0]-- } }
                `D` { if position[0] < 2 { position[0]++ } }
                `L` { if position[1] > 0 { position[1]-- } }
                `R` { if position[1] < 2 { position[1]++ } }
                else {}
            }
        }
        code += (position[0] * 3 + position[1] + 1).str() // Convert to button number
    }

    println(code)
}