
import os

struct Mirror {
mut:
	rows []int
	cols []int
}

fn parse_mirror(mirror_str []string) Mirror {
	mut mirror := Mirror{
		rows: []int{len: mirror_str.len}
		cols: []int{len: mirror_str[0].len}
	}
	for y in 0 .. mirror_str.len {
		for x in 0 .. mirror_str[0].len {
			mirror.rows[y] <<= 1
			mirror.cols[x] <<= 1
			if mirror_str[y][x] == `#` {
				mirror.rows[y]++
				mirror.cols[x]++
			}
		}
	}
	return mirror
}

fn get_mirror_axis(lines []int) int {
	for i in 1 .. lines.len {
		mut is_mirror := true
		for j := 0; is_mirror && j < i && i + j < lines.len; j++ {
			if lines[i - 1 - j] != lines[i + j] {
				is_mirror = false
			}
		}
		if is_mirror {
			return i
		}
	}
	return 0
}

fn solve(input []string) int {
	mut mirrors := []Mirror{}
	mut mirror_str := []string{}
	for line in input {
		if line == '' {
			mirrors << parse_mirror(mirror_str)
			mirror_str.clear()
		} else {
			mirror_str << line
		}
	}
	mirrors << parse_mirror(mirror_str)
	mut res := 0
	for mirror in mirrors {
		res += get_mirror_axis(mirror.cols)
		res += get_mirror_axis(mirror.rows) * 100
	}
	return res
}

fn main() {
	content := os.read_file('input.txt') or { panic(err) }
	lines := content.split_into_lines()
	println(solve(lines))
}
