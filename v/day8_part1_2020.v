import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut accumulator := 0
    mut visited := []bool{len: instructions.len, init: false}
    mut pc := 0

    for pc < instructions.len {
        if visited[pc] {
            println(accumulator)
            return
        }
        visited[pc] = true
        parts := instructions[pc].split(' ')
        op := parts[0]
        arg := parts[1].int()

        match op {
            'acc' { 
                accumulator += arg
                pc++
            }
            'jmp' { 
                pc += arg
            }
            'nop' { 
                pc++
            }
            else {}
        }
    }
}