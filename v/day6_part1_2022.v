import os

fn main() {
    input := os.read_file('input.txt') or {
        eprintln('Error reading file: $err')
        return
    }
    datastream := input.trim_space()
    mut marker_position := -1

    for i in 3 .. datastream.len {
        if is_unique(datastream[i-3..i+1]) {
            marker_position = i + 1
            break
        }
    }

    println(marker_position)
}

fn is_unique(s string) bool {
    return s[0] != s[1] && s[0] != s[2] && s[0] != s[3] &&
           s[1] != s[2] && s[1] != s[3] &&
           s[2] != s[3]
}