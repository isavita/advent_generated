import os

fn main() {
	mut registers := map[string]int{}
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
			'>' { registers[cond_reg] > cond_val }
			'>=' { registers[cond_reg] >= cond_val }
			'<' { registers[cond_reg] < cond_val }
			'<=' { registers[cond_reg] <= cond_val }
			'==' { registers[cond_reg] == cond_val }
			'!=' { registers[cond_reg] != cond_val }
			else { false }
		}

		if cond {
			match op {
				'inc' { registers[reg] += amount }
				'dec' { registers[reg] -= amount }
				else { /* Handle unexpected operation */ }
			}
		}
	}

	mut max_value := 0
	for value in registers.values() {
		if value > max_value {
			max_value = value
		}
	}

	println(max_value)
}