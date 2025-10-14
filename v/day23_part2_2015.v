import os

fn main() {
	content := os.read_file('input.txt') or { panic(err) }
	mut lines := []string{}
	for l in content.split('\n') {
		trimmed := l.trim_space()
		if trimmed.len > 0 {
			lines << trimmed
		}
	}
	mut a := u64(1)
	mut b := u64(0)
	mut ip := 0
	for ip >= 0 && ip < lines.len {
		parts := lines[ip].split(' ')
		instr := parts[0]
		match instr {
			'hlf' {
				if parts[1] == 'a' { a /= 2 } else { b /= 2 }
				ip++
			}
			'tpl' {
				if parts[1] == 'a' { a *= 3 } else { b *= 3 }
				ip++
			}
			'inc' {
				if parts[1] == 'a' { a++ } else { b++ }
				ip++
			}
			'jmp' {
				offset := parts[1].int()
				ip += offset
			}
			'jie' {
				reg := parts[1].trim_right(',')
				offset := parts[2].int()
				target := if reg == 'a' { a } else { b }
				if target % 2 == 0 { ip += offset } else { ip++ }
			}
			'jio' {
				reg := parts[1].trim_right(',')
				offset := parts[2].int()
				target := if reg == 'a' { a } else { b }
				if target == 1 { ip += offset } else { ip++ }
			}
			else { eprintln('Unknown instruction: $instr at line $ip'); break }
		}
	}
	println(b)
}