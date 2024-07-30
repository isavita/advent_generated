import os

fn main() {
	mut total_score := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		opponent := line[0]
		your_move := line[2]
		mut score := match your_move {
			`X` { 1 }
			`Y` { 2 }
			`Z` { 3 }
			else { 0 }
		}

		if (opponent == `A` && your_move == `Y`) || (opponent == `B` && your_move == `Z`) || (opponent == `C` && your_move == `X`) {
			score += 6
		} else if (opponent == `A` && your_move == `X`) || (opponent == `B` && your_move == `Y`) || (opponent == `C` && your_move == `Z`) {
			score += 3
		}

		total_score += score
	}

	println(total_score)
}