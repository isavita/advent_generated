import os

fn main() {
	mut nice_count := 0
	lines := os.read_lines('input.txt') or { panic(err) }

	for line in lines {
		if is_nice(line) {
			nice_count++
		}
	}

	println(nice_count)
}

fn is_nice(s string) bool {
	vowels := 'aeiou'
	mut vowel_count := 0
	mut has_double := false

	for i in 0 .. s.len {
		if vowels.contains(s[i].ascii_str()) {
			vowel_count++
		}
		if i > 0 && s[i] == s[i - 1] {
			has_double = true
		}
		if i > 0 && (s[i - 1..i + 1] == 'ab' || s[i - 1..i + 1] == 'cd' || s[i - 1..i + 1] == 'pq' || s[i - 1..i + 1] == 'xy') {
			return false
		}
	}

	return vowel_count >= 3 && has_double
}