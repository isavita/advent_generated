import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut position := [2, 0] // Starting at '5' (row 2, column 0)
    keypad := [
        [' ', ' ', '1', ' ', ' '],
        [' ', '2', '3', '4', ' '],
        ['5', '6', '7', '8', '9'],
        [' ', 'A', 'B', 'C', ' '],
        [' ', ' ', 'D', ' ', ' '],
    ]
    mut code := ''

    for line in instructions {
        for move in line {
            match move {
                `U` { if position[0] > 0 && keypad[position[0] - 1][position[1]] != ' ' { position[0]-- } }
                `D` { if position[0] < 4 && keypad[position[0] + 1][position[1]] != ' ' { position[0]++ } }
                `L` { if position[1] > 0 && keypad[position[0]][position[1] - 1] != ' ' { position[1]-- } }
                `R` { if position[1] < 4 && keypad[position[0]][position[1] + 1] != ' ' { position[1]++ } }
                else {}
            }
        }
        code += keypad[position[0]][position[1]].str()
    }
    println(code)
}