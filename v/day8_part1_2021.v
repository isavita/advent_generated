import os

fn main() {
    input_file := os.read_file('input.txt') or { panic(err) }
    lines := input_file.split('\n')
    mut count := 0

    for line in lines {
        if line.trim_space() == '' {
            continue
        }
        parts := line.split('|')
        output_values := parts[1].split(' ').map(it.trim_space())

        for value in output_values {
            length := value.len
            if length in [2, 3, 4, 7] { // lengths of 2, 3, 4, 7 correspond to unique digits
                count++
            }
        }
    }
    println(count)
}