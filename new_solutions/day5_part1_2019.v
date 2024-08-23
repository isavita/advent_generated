import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut program := input.split(',').map(it.int())
    mut i := 0

    for {
        opcode := program[i] % 100
        modes := program[i] / 100
        match opcode {
            1 {
                a := param(program, i + 1, modes % 10)
                b := param(program, i + 2, modes / 10 % 10)
                program[program[i + 3]] = a + b
                i += 4
            }
            2 {
                a := param(program, i + 1, modes % 10)
                b := param(program, i + 2, modes / 10 % 10)
                program[program[i + 3]] = a * b
                i += 4
            }
            3 {
                program[program[i + 1]] = 1
                i += 2
            }
            4 {
                println(param(program, i + 1, modes % 10))
                i += 2
            }
            99 {
                return
            }
            else {
                panic('Unknown opcode: $opcode')
            }
        }
    }
}

fn param(program []int, index int, mode int) int {
    return if mode == 0 { program[program[index]] } else { program[index] }
}