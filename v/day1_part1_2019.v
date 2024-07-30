import os

fn process_line(line string) ?int {
	return line.trim_space().int()
}

fn get_total(masses []int) f64 {
	mut total := 0.0
	for mass in masses {
		total += f64(mass / 3) - 2
	}
	return total
}

fn main() {
	mut masses := []int{}
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		if mass := process_line(line) {
			masses << mass
		}
	}
	println(get_total(masses))
}