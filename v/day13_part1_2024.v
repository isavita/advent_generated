import os

struct Machine {
mut:
	ax int
	ay int
	bx int
	by int
	px int
	py int
}

fn solve_machine(m Machine) int {
	mut min_cost := -1
	for a_count := 0; a_count <= 100; a_count++ {
		for b_count := 0; b_count <= 100; b_count++ {
			x := m.ax * a_count + m.bx * b_count
			y := m.ay * a_count + m.by * b_count
			if x == m.px && y == m.py {
				cost := a_count * 3 + b_count
				if min_cost == -1 || cost < min_cost {
					min_cost = cost
				}
			}
		}
	}
	return min_cost
}

fn main() {
	content := os.read_file('input.txt') or { return }
	lines := content.split('\n')
	mut solved_count := 0
	mut total_cost := 0
	mut current_machine := Machine{}
	mut found_a := false
	mut found_b := false
	mut found_p := false
	for line in lines {
		if line.trim_space() == '' {
			if found_a && found_b && found_p {
				cost := solve_machine(current_machine)
				if cost != -1 {
					solved_count++
					total_cost += cost
				}
			}
			found_a = false
			found_b = false
			found_p = false
		} else {
			if line.starts_with('Button A: X+') {
				current_machine.ax = line.after('X+').before(',').int()
				current_machine.ay = line.after('Y+').int()
				found_a = true
			} else if line.starts_with('Button B: X+') {
				current_machine.bx = line.after('X+').before(',').int()
				current_machine.by = line.after('Y+').int()
				found_b = true
			} else if line.starts_with('Prize: X=') {
				current_machine.px = line.after('X=').before(',').int()
				current_machine.py = line.after('Y=').int()
				found_p = true
			}
		}
	}
	if found_a && found_b && found_p {
		cost := solve_machine(current_machine)
		if cost != -1 {
			solved_count++
			total_cost += cost
		}
	}
	println('$solved_count $total_cost')
}