
import os

fn decompress_length(s string) int {
	mut length := 0
	mut i := 0
	for i < s.len {
		if s[i] == `(` {
			j := s.index_after(')', i)
			marker := s[i + 1..j]
			parts := marker.split('x')
			chars_to_repeat := parts[0].int()
			repeat_count := parts[1].int()
			length += chars_to_repeat * repeat_count
			i = j + chars_to_repeat + 1
		} else {
			length++
			i++
		}
	}
	return length
}

fn main() {
	if !os.exists('input.txt') {
		println('Error: input.txt not found')
		return
	}
	input := os.read_file('input.txt') or {
		println('Error reading input.txt: $err')
		return
	}
	compressed_string := input.trim_space()
	decompressed_length := decompress_length(compressed_string)
	println(decompressed_length)
}
