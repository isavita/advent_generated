
import os

fn main() {
	// Read connections from input file
	lines := os.read_lines('input.txt') or { panic(err) }

	// Build adjacency list representation of the network
	mut adj := map[string][]string{}
	for line in lines {
		parts := line.split('-')
		a := parts[0]
		b := parts[1]
		if a !in adj {
			adj[a] = []string{}
		}
		if b !in adj {
			adj[b] = []string{}
		}
		adj[a] << b
		adj[b] << a
	}

	// Find sets of three interconnected computers
	mut count := 0
	for a in adj.keys() {
		for b in adj[a] {
			if a > b { continue } // Avoid duplicates (a,b,c) == (b,a,c)
			for c in adj[b] {
				if b > c { continue } // Avoid duplicates
				if c in adj[a] {
					// Check if at least one computer starts with 't'
					if a.starts_with('t') || b.starts_with('t') || c.starts_with('t') {
						count++
					}
				}
			}
		}
	}

	// Print the result
	println(count)
}
