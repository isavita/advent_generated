import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut program := input.split(',').map(it.trim_space().int())
    program << []int{len: 10000} // Extend memory

    mut painted_panels := map[string]int{}
    mut robot_position := [0, 0]
    mut robot_direction := 0

    mut i := 0
    mut relative_base := 0
    mut output_count := 0
    mut paint_color := 0
    mut turn_direction := 0

    for {
        opcode := program[i] % 100
        modes := [program[i] / 100 % 10, program[i] / 1000 % 10, program[i] / 10000 % 10]

        match opcode {
            1 {
                a := get_value(program, i + 1, modes[0], relative_base)
                b := get_value(program, i + 2, modes[1], relative_base)
                set_value(mut program, i + 3, modes[2], relative_base, a + b)
                i += 4
            }
            2 {
                a := get_value(program, i + 1, modes[0], relative_base)
                b := get_value(program, i + 2, modes[1], relative_base)
                set_value(mut program, i + 3, modes[2], relative_base, a * b)
                i += 4
            }
            3 {
                input_val := painted_panels['${robot_position[0]},${robot_position[1]}'] or { 0 }
                set_value(mut program, i + 1, modes[0], relative_base, input_val)
                i += 2
            }
            4 {
                output_val := get_value(program, i + 1, modes[0], relative_base)
                if output_count % 2 == 0 {
                    paint_color = output_val
                } else {
                    turn_direction = output_val
                    painted_panels['${robot_position[0]},${robot_position[1]}'] = paint_color
                    robot_direction = (robot_direction + if turn_direction == 0 { -1 } else { 1 }) % 4
                    if robot_direction < 0 { robot_direction += 4 }
                    match robot_direction {
                        0 { robot_position[1]-- }
                        1 { robot_position[0]++ }
                        2 { robot_position[1]++ }
                        3 { robot_position[0]-- }
                        else {}
                    }
                }
                output_count++
                i += 2
            }
            5 {
                if get_value(program, i + 1, modes[0], relative_base) != 0 {
                    i = get_value(program, i + 2, modes[1], relative_base)
                } else {
                    i += 3
                }
            }
            6 {
                if get_value(program, i + 1, modes[0], relative_base) == 0 {
                    i = get_value(program, i + 2, modes[1], relative_base)
                } else {
                    i += 3
                }
            }
            7 {
                a := get_value(program, i + 1, modes[0], relative_base)
                b := get_value(program, i + 2, modes[1], relative_base)
                set_value(mut program, i + 3, modes[2], relative_base, if a < b { 1 } else { 0 })
                i += 4
            }
            8 {
                a := get_value(program, i + 1, modes[0], relative_base)
                b := get_value(program, i + 2, modes[1], relative_base)
                set_value(mut program, i + 3, modes[2], relative_base, if a == b { 1 } else { 0 })
                i += 4
            }
            9 {
                relative_base += get_value(program, i + 1, modes[0], relative_base)
                i += 2
            }
            99 {
                println(painted_panels.len)
                return
            }
            else {
                panic('Unknown opcode: $opcode')
            }
        }
    }
}

fn get_value(program []int, index int, mode int, relative_base int) int {
    match mode {
        0 { return program[program[index]] }
        1 { return program[index] }
        2 { return program[relative_base + program[index]] }
        else { return 0 }
    }
}

fn set_value(mut program []int, index int, mode int, relative_base int, value int) {
    match mode {
        0 { program[program[index]] = value }
        2 { program[relative_base + program[index]] = value }
        else {}
    }
}