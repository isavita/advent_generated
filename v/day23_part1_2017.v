module main

import os

fn main() {
    mut registers := map[string]int{}
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut mul_count := 0
    mut pc := 0

    for pc < instructions.len {
        parts := instructions[pc].split(' ')
        cmd := parts[0]
        x := parts[1]
        y := if parts.len > 2 { parts[2] } else { '' }

        match cmd {
            'set' {
                registers[x] = get_value(y, registers)
            }
            'sub' {
                registers[x] -= get_value(y, registers)
            }
            'mul' {
                registers[x] *= get_value(y, registers)
                mul_count++
            }
            'jnz' {
                if get_value(x, registers) != 0 {
                    pc += get_value(y, registers)
                    continue
                }
            }
            else {}
        }
        pc++
    }
    println(mul_count)
}

fn get_value(val string, registers map[string]int) int {
    return if val.is_int() { val.int() } else { registers[val] }
}