import os

fn main() {
	mut x := [1]
	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		if line == 'noop' {
			x << x[x.len - 1]
		} else {
			n := line.split(' ')[1].int()
			x << x[x.len - 1]
			x << x[x.len - 1] + n
		}
	}
	mut sum := 0
	for i in 0 .. x.len {
		if (i - 19) % 40 == 0 {
			sum += (i + 1) * x[i]
		}
	}
	println(sum)
}