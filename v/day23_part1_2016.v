import os

fn main() {
	mut instructions := read_instructions('input.txt')
	mut registers := {'a': 7, 'b': 0, 'c': 0, 'd': 0}
	execute_instructions(mut instructions, mut registers)
	println(registers['a'])
}

fn read_instructions(filename string) []string {
	return os.read_lines(filename) or { [] }
}

fn execute_instructions(mut instructions []string, mut registers map[string]int) {
	mut pc := 0
	for pc < instructions.len {
		fields := instructions[pc].split(' ')
		match fields[0] {
			'cpy' {
				x := get_value(fields[1], registers)
				registers[fields[2]] = x
			}
			'inc' {
				registers[fields[1]]++
			}
			'dec' {
				registers[fields[1]]--
			}
			'jnz' {
				x := get_value(fields[1], registers)
				if x != 0 {
					pc += get_value(fields[2], registers) - 1
				}
			}
			'tgl' {
				x := get_value(fields[1], registers)
				tgt := pc + x
				if tgt >= 0 && tgt < instructions.len {
					instructions[tgt] = toggle_instruction(instructions[tgt])
				}
			}
			else {}
		}
		pc++
	}
}

fn get_value(s string, registers map[string]int) int {
	return if s.is_int() { s.int() } else { registers[s] }
}

fn toggle_instruction(instr string) string {
	mut parts := instr.split(' ')
	match parts[0] {
		'inc' { parts[0] = 'dec' }
		'dec', 'tgl' { parts[0] = 'inc' }
		'jnz' { parts[0] = 'cpy' }
		'cpy' { parts[0] = 'jnz' }
		else {}
	}
	return parts.join(' ')
}