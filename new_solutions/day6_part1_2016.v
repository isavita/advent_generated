import os

fn main() {
    lines := os.read_lines('input.txt') or { panic(err) }
    if lines.len == 0 {
        return
    }

    mut message := []string{len: lines[0].len, init: ''}

    for line in lines {
        for i, c in line {
            message[i] += c.ascii_str()
        }
    }

    mut result := ''
    for column in message {
        mut freq := map[string]int{}
        for c in column {
            freq[c.ascii_str()]++
        }
        mut most_common := ''
        mut max_count := 0
        for c, count in freq {
            if count > max_count {
                most_common = c
                max_count = count
            }
        }
        result += most_common
    }

    println(result)
}