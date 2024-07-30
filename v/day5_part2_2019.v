import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut program := input.split(',').map(it.int())

    mut i := 0
    input_value := 5
    mut output := 0

    for {
        opcode := program[i] % 100
        modes := program[i] / 100
        param1_mode := modes % 10
        param2_mode := modes / 10

        match opcode {
            1 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                program[program[i + 3]] = p1 + p2
                i += 4
            }
            2 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                program[program[i + 3]] = p1 * p2
                i += 4
            }
            3 {
                program[program[i + 1]] = input_value
                i += 2
            }
            4 {
                output = get_value(program, i + 1, param1_mode)
                println(output)
                i += 2
            }
            5 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                if p1 != 0 {
                    i = p2
                } else {
                    i += 3
                }
            }
            6 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                if p1 == 0 {
                    i = p2
                } else {
                    i += 3
                }
            }
            7 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                program[program[i + 3]] = if p1 < p2 { 1 } else { 0 }
                i += 4
            }
            8 {
                p1 := get_value(program, i + 1, param1_mode)
                p2 := get_value(program, i + 2, param2_mode)
                program[program[i + 3]] = if p1 == p2 { 1 } else { 0 }
                i += 4
            }
            99 {
                return
            }
            else {
                panic('Invalid opcode')
            }
        }
    }
}

fn get_value(program []int, pos int, mode int) int {
    return if mode == 0 { program[program[pos]] } else { program[pos] }
}