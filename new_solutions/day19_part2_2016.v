import os

fn main() {
	mut elves := os.read_file('input.txt') or { panic(err) }.trim_space().int()
	mut highest_power_of_3 := 1

	for highest_power_of_3 * 3 <= elves {
		highest_power_of_3 *= 3
	}

	result := if elves == highest_power_of_3 {
		elves
	} else {
		elves - highest_power_of_3
	}

	println(result)
}