import os

fn is_valid_password(password int) bool {
	mut has_double := false
	s := password.str()

	for i := 0; i < s.len - 1; i++ {
		if s[i] > s[i + 1] {
			return false
		}
		if s[i] == s[i + 1] {
			if (i == 0 || s[i] != s[i - 1]) && (i + 2 >= s.len || s[i] != s[i + 2]) {
				has_double = true
			}
		}
	}
	return has_double
}

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	range_str := data.trim_space()
	ranges := range_str.split('-')
	start := ranges[0].int()
	end := ranges[1].int()

	mut count := 0
	for i in start..end + 1 {
		if is_valid_password(i) {
			count++
		}
	}
	println(count)
}