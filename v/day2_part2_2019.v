import os

fn main() {
	data := os.read_file('input.txt') or { return }
	strs := data.trim_space().split(',')
	original := strs.map(it.int())

	for noun in 0 .. 100 {
		for verb in 0 .. 100 {
			mut memory := original.clone()
			memory[1] = noun
			memory[2] = verb
			if execute(mut memory) == 19690720 {
				println(100 * noun + verb)
				return
			}
		}
	}
}

fn execute(mut memory []int) int {
	mut i := 0
	for i < memory.len {
		match memory[i] {
			1 { memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]] }
			2 { memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]] }
			99 { return memory[0] }
			else {}
		}
		i += 4
	}
	return memory[0]
}