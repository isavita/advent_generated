
module main

import os

struct Machine {
	a_x int
	a_y int
	b_x int
	b_y int
	prize_x i64
	prize_y i64
}

fn parse_input(input string) []Machine {
	mut machines := []Machine{}
	lines := input.trim('\n').split_into_lines()
	
	for i := 0; i < lines.len; i += 4 {
		// Parse button A
		a_parts := lines[i].split(' ')
		a_x := a_parts[2].trim_left('X+').int()
		a_y := a_parts[3].trim_left('Y+').int()
		
		// Parse button B
		b_parts := lines[i+1].split(' ')
		b_x := b_parts[2].trim_left('X+').int()
		b_y := b_parts[3].trim_left('Y+').int()
		
		// Parse prize
		prize_parts := lines[i+2].split(' ')
		prize_x := prize_parts[1].trim_left('X=').i64()
		prize_y := prize_parts[2].trim_left('Y=').i64()
		
		machines << Machine{a_x, a_y, b_x, b_y, prize_x, prize_y}
	}
	
	return machines
}

fn solve_machine(machine Machine, offset i64) ?i64 {
	prize_x := machine.prize_x + offset
	prize_y := machine.prize_y + offset
	
	// Solve the system of linear equations:
	// a_x * n_a + b_x * n_b = prize_x
	// a_y * n_a + b_y * n_b = prize_y
	
	// Using Cramer's rule
	det := machine.a_x * machine.b_y - machine.a_y * machine.b_x
	if det == 0 {
		return none
	}
	
	num_a := prize_x * machine.b_y - prize_y * machine.b_x
	num_b := machine.a_x * prize_y - machine.a_y * prize_x
	
	if num_a % det != 0 || num_b % det != 0 {
		return none
	}
	
	n_a := num_a / det
	n_b := num_b / det
	
	if n_a < 0 || n_b < 0 {
		return none
	}
	
	return n_a * 3 + n_b
}

fn solve(input string, offset i64) i64 {
	machines := parse_input(input)
	mut total := i64(0)
	
	for machine in machines {
		if result := solve_machine(machine, offset) {
			total += result
		}
	}
	
	return total
}

fn main() {
	input := os.read_file('input.txt') or {
		eprintln('Failed to read input.txt')
		return
	}
	
	// Part 1
	println('Part 1: ${solve(input, 0)}')
	
	// Part 2
	println('Part 2: ${solve(input, 10000000000000)}')
}
