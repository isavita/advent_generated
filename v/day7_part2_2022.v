import os

struct Directory {
	name string
mut:
	size int
}

fn main() {
	mut dirs := []Directory{}
	mut stack := []int{cap: 1000}

	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		if line.starts_with('$ cd') {
			if line == '$ cd ..' {
				size := stack.pop()
				stack[stack.len - 1] += size
				dirs << Directory{name: '', size: size}
			} else if line == '$ cd /' {
				stack = []int{cap: 1000}
				stack << 0
			} else {
				stack << 0
			}
		} else if line.starts_with('$ ls') {
			// Do nothing
		} else if line.starts_with('dir') {
			// Do nothing
		} else {
			size := line.split(' ')[0].int()
			stack[stack.len - 1] += size
		}
	}

	for stack.len > 1 {
		size := stack.pop()
		stack[stack.len - 1] += size
		dirs << Directory{name: '', size: size}
	}
	dirs << Directory{name: '', size: stack[0]}

	total_space := 70000000
	needed_space := 30000000
	used_space := dirs.last().size
	current_unused_space := total_space - used_space
	required_space := needed_space - current_unused_space

	mut min_size := total_space
	for dir in dirs {
		if dir.size >= required_space && dir.size < min_size {
			min_size = dir.size
		}
	}
	println(min_size)
}