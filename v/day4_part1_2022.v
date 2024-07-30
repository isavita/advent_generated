import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        println('File not found: $input_file')
        return
    }

    lines := os.read_lines(input_file) or {
        println('Error reading file: $input_file')
        return
    }

    mut count := 0
    for line in lines {
        sections := line.split(',')
        range1 := sections[0].split('-').map(it.int())
        range2 := sections[1].split('-').map(it.int())

        if (range1[0] <= range2[0] && range1[1] >= range2[1]) || (range2[0] <= range1[0] && range2[1] >= range1[1]) {
            count++
        }
    }

    println(count)
}