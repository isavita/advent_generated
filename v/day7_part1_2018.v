import os

fn main() {
	mut prerequisites := map[string][]string{}
	mut steps := map[string]bool{}

	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		parts := line.split(' ')
		before := parts[1]
		after := parts[7]

		if after !in prerequisites {
			prerequisites[after] = []string{}
		}
		prerequisites[after] << before
		steps[before] = true
		steps[after] = true
	}

	mut order := []string{}
	for steps.len > 0 {
		mut available := []string{}
		for step in steps.keys() {
			if !prerequisites[step].any(it in steps) {
				available << step
			}
		}
		available.sort()
		next_step := available[0]
		order << next_step
		steps.delete(next_step)
	}

	println(order.join(''))
}