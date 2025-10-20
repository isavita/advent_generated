
import os

fn read_input() string {
	return os.read_file('input.txt') or { return '' }
}

fn next_sequence(s string) string {
	mut result := ''
	mut i := 0
	for i < s.len {
		mut count := 1
		digit := s[i]
		for i + count < s.len && s[i + count] == digit {
			count++
		}
		result += count.str() + digit.ascii_str()
		i += count
	}
	return result
}

fn look_and_say(s string, n int) string {
	mut seq := s
	for _ in 0..n {
		seq = next_sequence(seq)
	}
	return seq
}

fn main() {
	initial := read_input()
	println(look_and_say(initial, 40).len)
}
