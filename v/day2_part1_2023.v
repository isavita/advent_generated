import os

fn main() {
    input_file := 'input.txt'
    if !os.exists(input_file) {
        eprintln('File not found: $input_file')
        return
    }

    data := os.read_file(input_file) or {
        eprintln('Error reading file: $input_file')
        return
    }

    mut total := 0
    for line in data.split('\n') {
        if line.trim_space() == '' {
            continue
        }
        parts := line.split(':')
        game_id := parts[0].split(' ')[1].int()
        mut possible := true

        for subset in parts[1].split(';') {
            counts := parse_counts(subset)
            if counts[0] > 12 || counts[1] > 13 || counts[2] > 14 {
                possible = false
                break
            }
        }
        if possible {
            total += game_id
        }
    }

    println(total)
}

fn parse_counts(subset string) []int {
    mut red := 0
    mut green := 0
    mut blue := 0
    for part in subset.split(',') {
        trimmed := part.trim_space()
        if trimmed.ends_with('red') {
            red = trimmed.split(' ')[0].int()
        } else if trimmed.ends_with('green') {
            green = trimmed.split(' ')[0].int()
        } else if trimmed.ends_with('blue') {
            blue = trimmed.split(' ')[0].int()
        }
    }
    return [red, green, blue]
}