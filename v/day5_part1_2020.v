import os

fn main() {
	mut max_seat_id := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		pass := line.replace('F', '0').replace('B', '1').replace('L', '0').replace('R', '1')
		seat_id := decode(pass)
		if seat_id > max_seat_id {
			max_seat_id = seat_id
		}
	}

	println(max_seat_id)
}

fn decode(pass string) int {
	row := binary_to_int(pass[..7])
	column := binary_to_int(pass[7..])
	return row * 8 + column
}

fn binary_to_int(binary_str string) int {
	mut result := 0
	for i, c in binary_str {
		if c == `1` {
			result |= 1 << (binary_str.len - i - 1)
		}
	}
	return result
}