import os

fn main() {
    instructions := os.read_file('input.txt') or {
        eprintln('Error reading file')
        return
    }
    mut floor := 0
    for ch in instructions {
        match ch {
            `(` { floor++ }
            `)` { floor-- }
            else {}
        }
    }
    println(floor)
}