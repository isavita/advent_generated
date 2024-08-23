import os

fn main() {
	mut sum := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		mut first_digit := -1
		mut last_digit := -1

		for ch in line {
			if ch.is_digit() {
				if first_digit == -1 {
					first_digit = ch.ascii_str().int()
				}
				last_digit = ch.ascii_str().int()
			}
		}

		if first_digit != -1 && last_digit != -1 {
			calibration_value := first_digit * 10 + last_digit
			sum += calibration_value
		}
	}

	println(sum)
}