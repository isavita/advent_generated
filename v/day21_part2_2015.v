module main

import os
import strconv

struct Item {
	cost   int
	damage int
	armor  int
}

struct Character {
	hit_points int
	damage     int
	armor      int
}

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	lines := data.split('\n')
	boss := Character{
		hit_points: parse_stat(lines[0]),
		damage:     parse_stat(lines[1]),
		armor:      parse_stat(lines[2]),
	}

	weapons := [
		Item{cost: 8, damage: 4},
		Item{cost: 10, damage: 5},
		Item{cost: 25, damage: 6},
		Item{cost: 40, damage: 7},
		Item{cost: 74, damage: 8},
	]

	armors := [
		Item{cost: 0, armor: 0},
		Item{cost: 13, armor: 1},
		Item{cost: 31, armor: 2},
		Item{cost: 53, armor: 3},
		Item{cost: 75, armor: 4},
		Item{cost: 102, armor: 5},
	]

	rings := [
		Item{cost: 0},
		Item{cost: 25, damage: 1},
		Item{cost: 50, damage: 2},
		Item{cost: 100, damage: 3},
		Item{cost: 20, armor: 1},
		Item{cost: 40, armor: 2},
		Item{cost: 80, armor: 3},
	]

	mut max_cost := 0
	for w in weapons {
		for a in armors {
			for ri in 0 .. rings.len {
				for rj in (ri + 1) .. rings.len {
					player := Character{hit_points: 100, damage: w.damage + rings[ri].damage + rings[rj].damage, armor: a.armor + rings[ri].armor + rings[rj].armor}
					cost := w.cost + a.cost + rings[ri].cost + rings[rj].cost
					if !player_wins(player, boss) && cost > max_cost {
						max_cost = cost
					}
				}
			}
		}
	}

	println(max_cost)
}

fn parse_stat(line string) int {
	parts := line.split(': ')
	return strconv.atoi(parts[1]) or { 0 }
}

fn player_wins(player Character, boss Character) bool {
	player_damage := max(1, player.damage - boss.armor)
	boss_damage := max(1, boss.damage - player.armor)

	player_turns := (boss.hit_points + player_damage - 1) / player_damage
	boss_turns := (player.hit_points + boss_damage - 1) / boss_damage

	return player_turns <= boss_turns
}

fn max(a int, b int) int {
	return if a > b { a } else { b }
}