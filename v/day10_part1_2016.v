
import os

struct Bot {
mut:
	chips []int
	low_type string
	low_id int
	high_type string
	high_id int
}

fn main() {
	lines := os.read_lines('input.txt') or { panic(err) }

	mut bots := map[int]Bot{}
	mut outputs := map[int][]int{}

	// Parse instructions and initialize bots
	for line in lines {
		parts := line.split(' ')
		if parts[0] == 'value' {
			value := parts[1].int()
			bot_id := parts[5].int()
			if bot_id !in bots {
				bots[bot_id] = Bot{}
			}
			bots[bot_id].chips << value
		} else {
			bot_id := parts[1].int()
			low_type := parts[5]
			low_id := parts[6].int()
			high_type := parts[10]
			high_id := parts[11].int()
			if bot_id !in bots {
				bots[bot_id] = Bot{}
			}
			bots[bot_id].low_type = low_type
			bots[bot_id].low_id = low_id
			bots[bot_id].high_type = high_type
			bots[bot_id].high_id = high_id
		}
	}

	// Process bots until all chips are distributed
	for {
		mut processed := false
		for bot_id, mut bot in bots {
			if bot.chips.len == 2 {
				processed = true
				bot.chips.sort()
				low := bot.chips[0]
				high := bot.chips[1]

				if low == 17 && high == 61 {
					println(bot_id)
					return
				}

				if bot.low_type == 'bot' {
					if bot.low_id !in bots {
						bots[bot.low_id] = Bot{}
					}
					bots[bot.low_id].chips << low
				} else {
					if bot.low_id !in outputs {
						outputs[bot.low_id] = []int{}
					}
					outputs[bot.low_id] << low
				}

				if bot.high_type == 'bot' {
					if bot.high_id !in bots {
						bots[bot.high_id] = Bot{}
					}
					bots[bot.high_id].chips << high
				} else {
					if bot.high_id !in outputs {
						outputs[bot.high_id] = []int{}
					}
					outputs[bot.high_id] << high
				}

				bot.chips = []int{}
			}
		}
		if !processed {
			break
		}
	}
}
