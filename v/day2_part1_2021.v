import os

fn main() {
    mut horizontal := 0
    mut depth := 0

    lines := os.read_lines('input.txt') or {
        eprintln('Error reading file: $err')
        return
    }

    for line in lines {
        parts := line.split(' ')
        command := parts[0]
        value := parts[1].int()

        match command {
            'forward' { horizontal += value }
            'down'    { depth += value }
            'up'      { depth -= value }
            else {}
        }
    }

    println(horizontal * depth)
}