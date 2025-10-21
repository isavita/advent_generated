
import os

struct Monkey {
mut:
	name string
	val i64
	has_val bool
	op_str string
	left_name string
	right_name string
	left_ptr &Monkey = unsafe { nil }
	right_ptr &Monkey = unsafe { nil }
}

fn (m &Monkey) solve() (i64, bool) {
	if m.has_val {
		return m.val, true
	}
	if m.left_ptr != unsafe { nil } && m.right_ptr != unsafe { nil } {
		l, lok := m.left_ptr.solve()
		r, rok := m.right_ptr.solve()
		if lok && rok {
			match m.op_str {
				'+' { return l + r, true }
				'-' { return l - r, true }
				'*' { return l * r, true }
				'/' { return l / r, true }
				'==' { return if l == r { 0 } else { 1 }, true }
				else { return 0, false }
			}
		}
	}
	return 0, false
}

fn (m &Monkey) expect(x i64) i64 {
	if m.name == 'humn' { return x }
	l, lok := m.left_ptr.solve()
	r, rok := m.right_ptr.solve()
	if !lok {
		match m.op_str {
			'+' { return m.left_ptr.expect(x - r) }
			'-' { return m.left_ptr.expect(x + r) }
			'*' { return m.left_ptr.expect(x / r) }
			'/' { return m.left_ptr.expect(x * r) }
			'==' { return m.left_ptr.expect(r) }
			else { panic('') }
		}
	} else if !rok {
		match m.op_str {
			'+' { return m.right_ptr.expect(x - l) }
			'-' { return m.right_ptr.expect(l - x) }
			'*' { return m.right_ptr.expect(x / l) }
			'/' { return m.right_ptr.expect(l / x) }
			'==' { return m.right_ptr.expect(l) }
			else { panic('') }
		}
	}
	panic('impossible')
}

fn main() {
	mut monkeys := map[string]&Monkey{}
	lines := os.read_lines('input.txt') or { panic('') }
	for line in lines {
		parts := line.split(': ')
		name := parts[0]
		rest := parts[1]
		mut m := &Monkey{
			name: name
		}
		if rest[0].is_digit() || (rest.len > 1 && rest[0] == `-` && rest[1].is_digit()) {
			m.val = rest.i64()
			m.has_val = true
		} else {
			sp := rest.split(' ')
			m.left_name = sp[0]
			m.op_str = sp[1]
			m.right_name = sp[2]
		}
		monkeys[name] = m
	}
	for mut m in monkeys.values() {
		if m.left_name != '' {
			m.left_ptr = monkeys[m.left_name]
			m.right_ptr = monkeys[m.right_name]
		}
	}
	mut root := monkeys['root']
	mut humn := monkeys['humn']
	humn.has_val = false
	root.op_str = '=='
	println(root.expect(0))
}
