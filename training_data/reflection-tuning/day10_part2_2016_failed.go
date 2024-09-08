package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Bot struct {
	chips    []int
	lowDest  string
	highDest string
}

func main() {
	bots := make(map[string]*Bot)
	outputs := make(map[string]int)
	queue := make([]string, 0)

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if parts[0] == "value" {
			value, _ := strconv.Atoi(parts[1])
			botID := parts[5]
			addChip(bots, botID, value, &queue)
		} else {
			botID := parts[1]
			lowDest := parts[5] + " " + parts[6]
			highDest := parts[10] + " " + parts[11]
			bots[botID] = &Bot{chips: make([]int, 0), lowDest: lowDest, highDest: highDest}
		}
	}

	part1 := processQueue(bots, outputs, queue)
	fmt.Println("Part 1:", part1)

	part2 := outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
	fmt.Println("Part 2:", part2)
}

func addChip(bots map[string]*Bot, botID string, value int, queue *[]string) {
	if _, exists := bots[botID]; !exists {
		bots[botID] = &Bot{chips: make([]int, 0)}
	}
	bots[botID].chips = append(bots[botID].chips, value)
	if len(bots[botID].chips) == 2 {
		*queue = append(*queue, botID)
	}
}

func processQueue(bots map[string]*Bot, outputs map[string]int, queue []string) string {
	for len(queue) > 0 {
		botID := queue[0]
		queue = queue[1:]
		bot := bots[botID]

		if len(bot.chips) != 2 {
			continue
		}

		low, high := bot.chips[0], bot.chips[1]
		if low > high {
			low, high = high, low
		}

		if low == 17 && high == 61 {
			return botID
		}

		bot.chips = bot.chips[:0]

		processDestination(bots, outputs, bot.lowDest, low, &queue)
		processDestination(bots, outputs, bot.highDest, high, &queue)
	}
	return ""
}

func processDestination(bots map[string]*Bot, outputs map[string]int, dest string, value int, queue *[]string) {
	if strings.HasPrefix(dest, "output") {
		outputs[dest] = value
	} else {
		addChip(bots, strings.Split(dest, " ")[1], value, queue)
	}
}
