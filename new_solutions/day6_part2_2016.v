import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        eprintln('File not found: $input_file')
        return
    }

    lines := os.read_lines(input_file) or {
        eprintln('Error reading file: $input_file')
        return
    }

    if lines.len == 0 {
        eprintln('File is empty: $input_file')
        return
    }

    message_len := lines[0].len
    mut columns := [][]rune{len: message_len, init: []rune{}}

    for line in lines {
        for i, c in line {
            columns[i] << c
        }
    }

    mut result := []rune{}
    for column in columns {
        mut counts := map[rune]int{}
        for c in column {
            counts[c]++
        }
        mut min_char := ` `
        mut min_count := int(1e9)
        for c, count in counts {
            if count < min_count {
                min_count = count
                min_char = c
            }
        }
        result << min_char
    }

    println(result.string())
}