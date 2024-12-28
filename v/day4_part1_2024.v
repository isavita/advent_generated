
import os

fn main() {
	// Read the input file
	lines := os.read_lines('input.txt') or {
		eprintln('Error reading input file: ${err}')
		return
	}

	// Initialize the count of XMAS occurrences
	mut count := 0

	// Define the target word
	target := 'XMAS'

	// Iterate over each cell in the grid
	for i, line in lines {
		for j, _ in line {
			// Check for XMAS in all 8 directions
			for di := -1; di <= 1; di++ {
				for dj := -1; dj <= 1; dj++ {
					if di == 0 && dj == 0 {
						continue // Skip the current cell itself
					}
					// Check if XMAS exists in the current direction
					if check_direction(lines, i, j, di, dj, target) {
						count++
					}
				}
			}
		}
	}

	// Print the total count
	println(count)
}

// Function to check for the target word in a specific direction
fn check_direction(grid []string, row int, col int, drow int, dcol int, target string) bool {
	mut r := row
	mut c := col
	for k := 0; k < target.len; k++ {
		// Check boundaries
		if r < 0 || r >= grid.len || c < 0 || c >= grid[r].len {
			return false
		}
		// Check if the current character matches the target
		if grid[r][c] != target[k] {
			return false
		}
		// Move to the next cell in the direction
		r += drow
		c += dcol
	}
	return true
}
