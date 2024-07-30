import os

fn main() {
    mut horizontal := 0
    mut depth := 0
    mut aim := 0

    lines := os.read_lines('input.txt') or { panic(err) }
    
    for line in lines {
        parts := line.split(' ')
        command := parts[0]
        value := parts[1].int()

        match command {
            'forward' {
                horizontal += value
                depth += aim * value
            }
            'down' {
                aim += value
            }
            'up' {
                aim -= value
            }
            else {}
        }
    }

    println(horizontal * depth)
}