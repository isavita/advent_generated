import os

fn main() {
	total_elves := read_input('input.txt')
	winner := find_winning_elf(total_elves)
	println(winner)
}

fn read_input(filename string) int {
	content := os.read_file(filename) or { panic(err) }
	return content.trim_space().int()
}

fn find_winning_elf(total_elves int) int {
	mut highest_power_of_two := 1
	for highest_power_of_two * 2 <= total_elves {
		highest_power_of_two *= 2
	}
	return (total_elves - highest_power_of_two) * 2 + 1
}