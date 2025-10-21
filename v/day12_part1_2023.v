
import os

struct Row {
	springs string
	group   []int
}

fn parse_input(input []string) []Row {
	mut rows := []Row{}
	for line in input {
		parts := line.split(' ')
		springs := parts[0]
		nums := parts[1].split(',')
		mut group := []int{cap: nums.len}
		for n in nums {
			group << n.int()
		}
		rows << Row{springs, group}
	}
	return rows
}

fn count_arrangements_recursive(row Row, i_springs int, i_group int, i_contiguous int, mut cache map[int]int) int {
	key := i_springs * 1000000 + i_group * 1000 + i_contiguous
	if key in cache {
		return cache[key]
	}
	if i_springs == row.springs.len {
		res := if i_group == row.group.len && i_contiguous == 0 {
			1
		} else if i_group == row.group.len - 1 && i_contiguous == row.group[i_group] {
			1
		} else {
			0
		}
		cache[key] = res
		return res
	}
	mut res := 0
	c := row.springs[i_springs]
	if c == `.` || c == `?` {
		if i_contiguous == 0 {
			res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous, mut cache)
		} else if i_contiguous == row.group[i_group] {
			res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, mut cache)
		}
	}
	if c == `#` || c == `?` {
		if i_group < row.group.len && i_contiguous < row.group[i_group] {
			res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous + 1, mut cache)
		}
	}
	cache[key] = res
	return res
}

fn count_arrangements(row Row) int {
	mut cache := map[int]int{}
	return count_arrangements_recursive(row, 0, 0, 0, mut cache)
}

fn solve(input []string) int {
	rows := parse_input(input)
	mut res := 0
	for row in rows {
		res += count_arrangements(row)
	}
	return res
}

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	input := data.trim_space().split_into_lines()
	println(solve(input))
}
