import os

fn main() {
	mut bag_rules := map[string]map[string]int{}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(' bags contain ')
		outer_bag := parts[0]
		inner_bags := parts[1].split(', ')

		for inner in inner_bags {
			if inner.contains('no other bags') {
				continue
			}
			count := inner.split(' ')[0].int()
			color := inner.split(' ')[1..3].join(' ')
			if outer_bag !in bag_rules {
				bag_rules[outer_bag] = map[string]int{}
			}
			bag_rules[outer_bag][color] = count
		}
	}

	result := count_bags(bag_rules, 'shiny gold')
	println(result)
}

fn count_bags(rules map[string]map[string]int, bag string) int {
	mut total := 0
	for inner_bag, count in rules[bag] {
		total += count + count * count_bags(rules, inner_bag)
	}
	return total
}