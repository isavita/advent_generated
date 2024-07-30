module main

import os

fn item_priority(item u8) int {
	if item >= `a` && item <= `z` {
		return int(item - `a` + 1)
	}
	return int(item - `A` + 27)
}

fn main() {
	mut sum := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		half := line.len / 2
		first_compartment := line[..half]
		second_compartment := line[half..]

		mut compartment_map := map[u8]int{}
		for item in first_compartment {
			compartment_map[item]++
		}
		for item in second_compartment {
			if item in compartment_map {
				sum += item_priority(item)
				break
			}
		}
	}
	println(sum)
}