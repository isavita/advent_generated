import os

fn main() {
	mut total_power := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(':')
		game_id := parts[0].trim_space()
		sets := parts[1].split(';')

		mut max_red := 0
		mut max_green := 0
		mut max_blue := 0

		for set in sets {
			mut red := 0
			mut green := 0
			mut blue := 0
			cubes := set.split(',')

			for cube in cubes {
				cube_info := cube.trim_space().split(' ')
				count := cube_info[0].int()
				color := cube_info[1]

				match color {
					'red' { red = count }
					'green' { green = count }
					'blue' { blue = count }
					else {}
				}
			}

			if red > max_red { max_red = red }
			if green > max_green { max_green = green }
			if blue > max_blue { max_blue = blue }
		}

		power := max_red * max_green * max_blue
		total_power += power
	}

	println(total_power)
}