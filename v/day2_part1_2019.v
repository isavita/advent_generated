import os

fn run_intcode(mut program []int) {
    mut i := 0
    for {
        opcode := program[i]
        match opcode {
            1 {
                a, b, c := program[program[i + 1]], program[program[i + 2]], program[i + 3]
                program[c] = a + b
            }
            2 {
                a, b, c := program[program[i + 1]], program[program[i + 2]], program[i + 3]
                program[c] = a * b
            }
            99 {
                break
            }
            else {
                panic('Unknown opcode: $opcode')
            }
        }
        i += 4
    }
}

fn main() {
    input := os.read_file('input.txt') or { panic('Failed to read input.txt') }
    mut program := input.split(',').map(it.int())
    
    program[1] = 12
    program[2] = 2
    
    run_intcode(mut program)
    println(program[0])
}