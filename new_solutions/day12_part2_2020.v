import os

fn rotate_waypoint(wx int, wy int, degrees int) (int, int) {
	mut new_wx := wx
	mut new_wy := wy
	for _ in 0 .. (degrees / 90) {
		new_wx, new_wy = new_wy, -new_wx
	}
	return new_wx, new_wy
}

fn main() {
	mut ship_x := 0
	mut ship_y := 0
	mut waypoint_x := 10
	mut waypoint_y := 1

	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		action := line[0]
		value := line[1..].int()

		match action {
			`N` { waypoint_y += value }
			`S` { waypoint_y -= value }
			`E` { waypoint_x += value }
			`W` { waypoint_x -= value }
			`L` { waypoint_x, waypoint_y = rotate_waypoint(waypoint_x, waypoint_y, 360 - value) }
			`R` { waypoint_x, waypoint_y = rotate_waypoint(waypoint_x, waypoint_y, value) }
			`F` {
				ship_x += waypoint_x * value
				ship_y += waypoint_y * value
			}
			else {}
		}
	}

	println(abs(ship_x) + abs(ship_y))
}

fn abs(x int) int {
	return if x < 0 { -x } else { x }
}