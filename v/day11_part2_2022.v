
import os

struct Monkey {
mut:
	items       []u64
	op          string
	op_val      u64
	div         u64
	next_t      u32
	next_f      u32
	inspections u64
}

fn parse(s string) Monkey {
	lines := s.split_into_lines()
	mut items := []u64{}
	for item in lines[1].split(': ')[1].split(', ') {
		items << item.u64()
	}
	op_parts := lines[2].split('= ')[1].split(' ')
	op := op_parts[1]
	op_val := if op_parts[2] == 'old' { u64(0) } else { op_parts[2].u64() }
	div := lines[3].split(' divisible by ')[1].u64()
	next_t := lines[4].after(' throw to monkey ').u32()
	next_f := lines[5].after(' throw to monkey ').u32()
	return Monkey{items, op, op_val, div, next_t, next_f, 0}
}

fn monkey_business(mut monkeys []Monkey, rounds int, worry bool) u64 {
	mut div := u64(1)
	for m in monkeys {
		div *= m.div
	}
	for _ in 0 .. rounds {
		for mut m in monkeys {
			for m.items.len > 0 {
				m.inspections++
				mut item := m.items[0]
				m.items = m.items[1..]
				op_val := if m.op_val == 0 { item } else { m.op_val }
				item = if m.op == '+' { item + op_val } else { item * op_val }
				if worry {
					item %= div
				} else {
					item /= 3
				}
				target := if item % m.div == 0 { m.next_t } else { m.next_f }
				monkeys[target].items << item
			}
		}
	}
	mut inspections := []u64{}
	for m in monkeys {
		inspections << m.inspections
	}
	inspections.sort(a > b)
	return inspections[0] * inspections[1]
}

fn main() {
	data := os.read_file('input.txt') or { return }
	mut monkeys := []Monkey{}
	for block in data.split('\n\n') {
		monkeys << parse(block)
	}
	println(monkey_business(mut monkeys, 10000, true))
}
