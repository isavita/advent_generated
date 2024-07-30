module main

import os

const hash_table_size = 256

struct Step {
	label     string
	num_box   int
	operation string
	number    int
}

fn hash_string(str string) int {
	mut res := 0
	for c in str {
		res += int(c)
		res *= 17
		res %= hash_table_size
	}
	return res
}

fn parse_step(step_str string) Step {
	label := step_str.trim_right("=-0123456789")
	num_box := hash_string(label)
	operation := step_str[label.len..label.len + 1]
	mut number := 0
	if operation == '=' {
		number = step_str[label.len + 1..].int()
	}
	return Step{label, num_box, operation, number}
}

fn get_boxes(steps_str []string) map[int][]map[string]int {
	mut boxes := map[int][]map[string]int{}

	for step_str in steps_str {
		step := parse_step(step_str)
		mut box_contents := boxes[step.num_box] or { []map[string]int{} }

		match step.operation {
			'-' {
				for i in 0 .. box_contents.len {
					if step.label in box_contents[i] {
						box_contents.delete(i)
						break
					}
				}
			}
			'=' {
				mut found := false
				for mut content in box_contents {
					if step.label in content {
						content[step.label] = step.number
						found = true
						break
					}
				}
				if !found {
					box_contents << {step.label: step.number}
				}
			}
			else {}
		}

		if box_contents.len == 0 {
			boxes.delete(step.num_box)
		} else {
			boxes[step.num_box] = box_contents
		}
	}

	return boxes
}

fn calculate_power(boxes map[int][]map[string]int) int {
	mut res := 0
	for i_box in 0 .. hash_table_size {
		if contents := boxes[i_box] {
			for i_slot in 0 .. contents.len {
				for value in contents[i_slot].values() {
					res += (i_box + 1) * (i_slot + 1) * value
				}
			}
		}
	}
	return res
}

fn solve(input []string) int {
	steps_str := input[0].split(',')
	boxes := get_boxes(steps_str)
	return calculate_power(boxes)
}

fn main() {
	input := os.read_file('input.txt') or { panic(err) }
	println(solve(input.split('\n')))
}