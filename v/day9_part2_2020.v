import os

fn read_input(file string) []int {
	data := os.read_file(file) or { return []int{} }
	return data.split('\n').filter(it != '').map(it.int())
}

fn find_first_invalid(numbers []int, preamble_length int) ?int {
	for i in preamble_length..numbers.len {
		mut found := false
		for j in 0..preamble_length {
			for k in j + 1..preamble_length {
				if numbers[i] == numbers[i - preamble_length + j] + numbers[i - preamble_length + k] {
					found = true
					break
				}
			}
			if found {
				break
			}
		}
		if !found {
			return numbers[i]
		}
	}
	return none
}

fn find_encryption_weakness(numbers []int, target int) ?int {
	for i in 0..numbers.len {
		mut sum := 0
		mut min_val := numbers[i]
		mut max_val := numbers[i]
		for j in i..numbers.len {
			sum += numbers[j]
			if numbers[j] < min_val {
				min_val = numbers[j]
			}
			if numbers[j] > max_val {
				max_val = numbers[j]
			}
			if sum == target {
				return min_val + max_val
			}
			if sum > target {
				break
			}
		}
	}
	return none
}

fn main() {
	numbers := read_input('input.txt')
	preamble_length := 25

	first_invalid := find_first_invalid(numbers, preamble_length) or { return }
	println('First invalid number: $first_invalid')

	weakness := find_encryption_weakness(numbers, first_invalid) or { return }
	println('Encryption weakness: $weakness')
}