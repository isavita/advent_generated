import os

fn read_input(file string) []int {
	mut frequencies := []int{}
	lines := os.read_lines(file) or { panic(err) }
	for line in lines {
		frequencies << line.int()
	}
	return frequencies
}

fn calibrate(frequencies []int) int {
	mut current_frequency := 0
	for change in frequencies {
		current_frequency += change
	}
	return current_frequency
}

fn first_repeated_frequency(frequencies []int) int {
	mut seen := map[int]bool{}
	mut current_frequency := 0
	mut index := 0
	for {
		if seen[current_frequency] {
			return current_frequency
		}
		seen[current_frequency] = true
		current_frequency += frequencies[index]
		index = (index + 1) % frequencies.len
	}
	// This return is unreachable but satisfies the compiler
	// If you want to ensure the function always returns an int, you can return 0 or any default value
	return 0
}

fn main() {
	frequencies := read_input('input.txt')
	resulting_frequency := calibrate(frequencies)
	println('Resulting frequency: $resulting_frequency')
	repeated_frequency := first_repeated_frequency(frequencies)
	println('First repeated frequency: $repeated_frequency')
}