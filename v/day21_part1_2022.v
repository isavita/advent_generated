
import os

struct Node {
mut:
	name string
	job  string
	val  i64
	calc bool
}

fn calc(name string, mut jobs map[string]Node) i64 {
	mut n := jobs[name] or { panic('missing $name') }
	if n.calc { return n.val }

	if n.job.len > 0 && n.job[0].is_digit() {
		n.val = n.job.i64()
		n.calc = true
		jobs[name] = n
		return n.val
	}
	parts := n.job.split(' ')
	a := calc(parts[0], mut jobs)
	b := calc(parts[2], mut jobs)
	n.val = match parts[1] {
		'+' { a + b }
		'-' { a - b }
		'*' { a * b }
		'/' { a / b }
		else { panic('bad op') }
	}
	n.calc = true
	jobs[name] = n
	return n.val
}

fn main() {
	raw := os.read_file('input.txt') or { panic(err) }
	mut jobs := map[string]Node{}
	for line in raw.split_into_lines() {
		parts := line.split(': ')
		jobs[parts[0]] = Node{parts[0], parts[1], 0, false}
	}
	println(calc('root', mut jobs))
}
