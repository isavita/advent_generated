import os

fn react(polymer string) string {
	mut i := 0
	for i < polymer.len - 1 {
		if polymer[i] != polymer[i + 1] && (polymer[i].ascii_str().to_lower() == polymer[i + 1].ascii_str().to_lower()) {
			return react(polymer[0..i] + polymer[i + 2..])
		}
		i++
	}
	return polymer
}

fn main() {
	input := os.read_file('input.txt') or { return }
	result := react(input.trim_space())
	println(result.len)
}