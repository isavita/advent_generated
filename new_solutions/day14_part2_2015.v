import os

struct Reindeer {
	name       string
	speed      int
	fly_time   int
	rest_time  int
mut:
	distance   int
	points     int
	flying     bool
	time_left  int
}

fn main() {
	mut reindeer := []Reindeer{}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(' ')
		reindeer << Reindeer{
			name: parts[0],
			speed: parts[3].int(),
			fly_time: parts[6].int(),
			rest_time: parts[13].int(),
			flying: true,
			time_left: parts[6].int(),
		}
	}

	for _ in 0 .. 2503 {
		mut max_distance := 0
		for mut r in reindeer {
			if r.flying {
				r.distance += r.speed
			}
			r.time_left--
			if r.time_left == 0 {
				r.flying = !r.flying
				r.time_left = if r.flying { r.fly_time } else { r.rest_time }
			}
			if r.distance > max_distance {
				max_distance = r.distance
			}
		}
		for mut r in reindeer {
			if r.distance == max_distance {
				r.points++
			}
		}
	}

	mut max_points := 0
	for r in reindeer {
		if r.points > max_points {
			max_points = r.points
		}
	}
	println(max_points)
}