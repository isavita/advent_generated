
import os

fn main() {
	// Read input from file
	lines := os.read_lines('input.txt') or { panic(err) }

	// Parse towel patterns
	patterns := lines[0].split(', ')

	// Count possible designs
	mut possible_designs := 0
	for line in lines[2..] {
		if can_make_design(line, patterns) {
			possible_designs++
		}
	}

	// Print the result
	println(possible_designs)
}

// Checks if a design can be made using the given patterns
fn can_make_design(design string, patterns []string) bool {
	mut dp := []bool{len: design.len + 1, init: false}
	dp[0] = true

	for i := 0; i < design.len; i++ {
		if !dp[i] {
			continue
		}
		for pattern in patterns {
			if i + pattern.len <= design.len && design[i..i + pattern.len] == pattern {
				dp[i + pattern.len] = true
			}
		}
	}

	return dp[design.len]
}
