import os

fn main() {
	data := os.read_file('input.txt') or { return }
	starting_numbers := data.trim_space().split(',')

	mut spoken := map[int]int{}
	mut last_spoken := 0

	for i, number in starting_numbers {
		if i == starting_numbers.len - 1 {
			last_spoken = number.int()
		} else {
			spoken[number.int()] = i + 1
		}
	}

	for turn := starting_numbers.len + 1; turn <= 30000000; turn++ {
		next_number := if spoken[last_spoken] > 0 { turn - 1 - spoken[last_spoken] } else { 0 }
		spoken[last_spoken] = turn - 1
		last_spoken = next_number
	}

	println(last_spoken)
}