import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        eprintln('File not found: $input_file')
        return
    }

    mut valid_count := 0
    lines := os.read_lines(input_file) or { panic(err) }

    for line in lines {
        parts := line.split(' ')
        limits := parts[0].split('-')
        pos1 := limits[0].int() - 1
        pos2 := limits[1].int() - 1
        letter := parts[1][0]
        password := parts[2]

        if (password[pos1] == letter) != (password[pos2] == letter) {
            valid_count++
        }
    }

    println(valid_count)
}