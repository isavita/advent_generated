import os

fn main() {
    instructions := os.read_lines('input.txt') or { panic(err) }
    mut registers := {'a': 0, 'b': 0, 'c': 0, 'd': 0}
    execute_instructions(instructions, mut registers)
    println(registers['a'])
}

fn execute_instructions(instructions []string, mut registers map[string]int) {
    mut i := 0
    for i < instructions.len {
        parts := instructions[i].split(' ')
        match parts[0] {
            'cpy' { registers[parts[2]] = get_value(parts[1], registers) }
            'inc' { registers[parts[1]]++ }
            'dec' { registers[parts[1]]-- }
            'jnz' {
                if get_value(parts[1], registers) != 0 {
                    i += parts[2].int()
                    continue
                }
            }
            else { // Handle unexpected instructions
                println('Unknown instruction: $parts[0]')
            }
        }
        i++
    }
}

fn get_value(s string, registers map[string]int) int {
    return if s in registers { registers[s] } else { s.int() }
}