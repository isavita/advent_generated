import os

fn main() {
	mut registers := {'a': 0, 'b': 0, 'c': 1, 'd': 0}
	instructions := os.read_lines('input.txt') or { panic(err) }
	mut pc := 0

	for pc < instructions.len {
		parts := instructions[pc].split(' ')
		match parts[0] {
			'cpy' {
				x := value(parts[1], registers)
				registers[parts[2]] = x
			}
			'inc' {
				registers[parts[1]]++
			}
			'dec' {
				registers[parts[1]]--
			}
			'jnz' {
				x := value(parts[1], registers)
				if x != 0 {
					pc += parts[2].int()
					continue
				}
			}
			else {}
		}
		pc++
	}

	println(registers['a'])
}

fn value(s string, registers map[string]int) int {
	return if s.is_int() { s.int() } else { registers[s] }
}