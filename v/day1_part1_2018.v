import os

fn main() {
    changes := os.read_lines('input.txt') or {
        eprintln('Error reading file: $err')
        return
    }

    mut frequency := 0
    for change in changes {
        frequency += change.int()
    }

    println(frequency)
}