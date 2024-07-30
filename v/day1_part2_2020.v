import os

fn main() {
	mut expenses := []int{}
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		expenses << line.int()
	}
	for i in 0 .. expenses.len {
		for j in (i + 1) .. expenses.len {
			for k in (j + 1) .. expenses.len {
				if expenses[i] + expenses[j] + expenses[k] == 2020 {
					println(expenses[i] * expenses[j] * expenses[k])
					return
				}
			}
		}
	}
}