module main

import os

fn read_input(filename string) string {
	content := os.read_file(filename) or { '' }
	lines := content.split_into_lines()
	if lines.len > 0 {
		return lines[0].trim_space()
	}
	return ''
}

fn has_straight(p []byte) bool {
	for i := 0; i <= p.len - 3; i++ {
		if p[i] + 1 == p[i+1] && p[i] + 2 == p[i+2] {
			return true
		}
	}
	return false
}

fn contains_invalid(p []byte) bool {
	for ch in p {
		if ch == `i` || ch == `o` || ch == `l` {
			return true
		}
	}
	return false
}

fn has_two_pairs(p []byte) bool {
	mut count := 0
	mut i := 0
	for i < p.len - 1 {
		if p[i] == p[i+1] {
			count++
			i += 2
		} else {
			i++
		}
	}
	return count >= 2
}

fn is_valid(p string) bool {
	bytes := p.bytes()
	return has_straight(bytes) && !contains_invalid(bytes) && has_two_pairs(bytes)
}

fn increment_password(p string) string {
	mut bytes := p.bytes()
	for i := bytes.len - 1; i >= 0; i-- {
		bytes[i] += 1
		if bytes[i] > `z` {
			bytes[i] = `a`
			continue
		}
		break
	}
	return bytes.bytestr()
}

fn find_next_password(p string) string {
	mut cur := p
	for {
		cur = increment_password(cur)
		if is_valid(cur) {
			break
		}
	}
	return cur
}

fn main() {
	current := read_input('input.txt')
	first := find_next_password(current)
	second := find_next_password(first)
	println(second)
}