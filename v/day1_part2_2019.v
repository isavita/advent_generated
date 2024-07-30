import os

fn process_line(line string) ?int {
	return line.trim_space().int()
}

fn calc_fuel_mass(mass int) int {
	fuel := (mass / 3) - 2
	if fuel <= 0 {
		return 0
	}
	return fuel + calc_fuel_mass(fuel)
}

fn get_total(masses []int) int {
	mut total := 0
	for mass in masses {
		total += calc_fuel_mass(mass)
	}
	return total
}

fn main() {
	lines := os.read_lines('input.txt') or { return }
	mut masses := []int{}

	for line in lines {
		if mass := process_line(line) {
			masses << mass
		}
	}
	total := get_total(masses)
	println(total)
}