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

    bit_count := lines[0].len
    mut gamma_rate := 0
    mut epsilon_rate := 0

    for i in 0 .. bit_count {
        mut count := 0
        for line in lines {
            if line[i] == `1` {
                count++
            }
        }
        if count > lines.len / 2 {
            gamma_rate |= 1 << (bit_count - i - 1)
        } else {
            epsilon_rate |= 1 << (bit_count - i - 1)
        }
    }

    power_consumption := gamma_rate * epsilon_rate
    println(power_consumption)
}