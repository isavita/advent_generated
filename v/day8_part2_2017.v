import os

fn main() {
	mut registers := map[string]int{}
	mut highest_value := 0

	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(' ')
		reg := parts[0]
		op := parts[1]
		amount := parts[2].int()
		cond_reg := parts[4]
		cond_op := parts[5]
		cond_val := parts[6].int()

		cond := match cond_op {
			'>'  { registers[cond_reg] > cond_val }
			'>=' { registers[cond_reg] >= cond_val }
			'<'  { registers[cond_reg] < cond_val }
			'<=' { registers[cond_reg] <= cond_val }
			'==' { registers[cond_reg] == cond_val }
			'!=' { registers[cond_reg] != cond_val }
			else { false }
		}

		if cond {
			if op == 'inc' {
				registers[reg] += amount
			} else {
				registers[reg] -= amount
			}
			highest_value = if registers[reg] > highest_value { registers[reg] } else { highest_value }
		}
	}

	println(highest_value)
}