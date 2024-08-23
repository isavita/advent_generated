import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }

    for a in 0 .. 10000 {
        if check_clock_signal(instructions, a) {
            println(a)
            break
        }
    }
}

fn check_clock_signal(instructions []string, a int) bool {
    mut registers := map[string]int{}
    registers['a'] = a
    mut last_output := -1
    mut count := 0
    mut pc := 0

    for pc < instructions.len {
        parts := instructions[pc].split(' ')
        match parts[0] {
            'cpy' {
                registers[parts[2]] = get_value(parts[1], registers)
            }
            'inc' {
                registers[parts[1]]++
            }
            'dec' {
                registers[parts[1]]--
            }
            'jnz' {
                if get_value(parts[1], registers) != 0 {
                    pc += get_value(parts[2], registers)
                    continue
                }
            }
            'out' {
                output := get_value(parts[1], registers)
                if output != 0 && output != 1 {
                    return false
                }
                if output == last_output {
                    return false
                }
                last_output = output
                count++
                if count > 10 {
                    return true
                }
            }
            else {}
        }
        pc++
    }
    return false
}

fn get_value(s string, registers map[string]int) int {
    return if s.is_int() { s.int() } else { registers[s] }
}