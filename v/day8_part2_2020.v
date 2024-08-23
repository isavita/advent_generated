import os

fn main() {
	mut instructions := os.read_lines('input.txt') or { panic(err) }
	for i in 0 .. instructions.len {
		if instructions[i].starts_with('acc') {
			continue
		}
		mut modified_instructions := instructions.clone()
		if instructions[i].starts_with('nop') {
			modified_instructions[i] = instructions[i].replace('nop', 'jmp')
		} else if instructions[i].starts_with('jmp') {
			modified_instructions[i] = instructions[i].replace('jmp', 'nop')
		}
		mut accumulator := 0
		mut visited := []bool{len: modified_instructions.len}
		mut pc := 0
		for pc < modified_instructions.len {
			if visited[pc] {
				break
			}
			visited[pc] = true
			parts := modified_instructions[pc].split(' ')
			op := parts[0]
			arg := parts[1].int()
			match op {
				'acc' { accumulator += arg; pc++ }
				'jmp' { pc += arg }
				'nop' { pc++ }
				else {}
			}
		}
		if pc == modified_instructions.len {
			println(accumulator)
			return
		}
	}
}