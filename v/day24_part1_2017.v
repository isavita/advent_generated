import os

struct Component {
	port1 int
	port2 int
}

fn read_components(filename string) []Component {
	mut components := []Component{}
	lines := os.read_lines(filename) or { return components }
	for line in lines {
		ports := line.split('/')
		components << Component{ports[0].int(), ports[1].int()}
	}
	return components
}

fn find_strongest_bridge(components []Component, port int, mut used []bool) int {
	mut max_strength := 0
	for i, c in components {
		if !used[i] && (c.port1 == port || c.port2 == port) {
			used[i] = true
			next_port := if c.port1 == port { c.port2 } else { c.port1 }
			strength := c.port1 + c.port2 + find_strongest_bridge(components, next_port, mut used)
			if strength > max_strength {
				max_strength = strength
			}
			used[i] = false
		}
	}
	return max_strength
}

fn main() {
	components := read_components('input.txt')
	mut used := []bool{len: components.len, init: false}
	strength := find_strongest_bridge(components, 0, mut used)
	println(strength)
}