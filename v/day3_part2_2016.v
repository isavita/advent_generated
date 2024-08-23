import os

fn main() {
	mut triangles := [][]int{len: 3, init: []int{cap: 0}}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		sides := line.split(' ').filter(it != '').map(it.int())
		for i in 0 .. 3 {
			triangles[i] << sides[i]
		}
	}

	mut count := 0
	for t in triangles {
		for i in 0 .. t.len / 3 {
			a := t[i * 3]
			b := t[i * 3 + 1]
			c := t[i * 3 + 2]
			if is_valid_triangle(a, b, c) {
				count++
			}
		}
	}
	println(count)
}

fn is_valid_triangle(a int, b int, c int) bool {
	return a + b > c && a + c > b && b + c > a
}