package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Bot struct {
	chips []int
	lowDest, highDest string
}

func main() {
	bots := make(map[string]*Bot)
	outputs := make(map[string][]int)
	instructions := []string{}

	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	for _, inst := range instructions {
		if strings.HasPrefix(inst, "value") {
			parts := strings.Fields(inst)
			value, _ := strconv.Atoi(parts[1])
			bot := parts[5]
			if _, exists := bots[bot]; !exists {
				bots[bot] = &Bot{chips: []int{}}
			}
			bots[bot].chips = append(bots[bot].chips, value)
		} else {
			parts := strings.Fields(inst)
			bot := parts[1]
			lowDest := parts[5] + " " + parts[6]
			highDest := parts[10] + " " + parts[11]
			if _, exists := bots[bot]; !exists {
				bots[bot] = &Bot{chips: []int{}}
			}
			bots[bot].lowDest = lowDest
			bots[bot].highDest = highDest
		}
	}

	part1Answer := 0
	for len(instructions) > 0 {
		newInstructions := []string{}
		for _, inst := range instructions {
			if strings.HasPrefix(inst, "bot") {
				parts := strings.Fields(inst)
				bot := parts[1]
				if len(bots[bot].chips) == 2 {
					low, high := min(bots[bot].chips[0], bots[bot].chips[1]), max(bots[bot].chips[0], bots[bot].chips[1])
					if low == 17 && high == 61 {
						part1Answer, _ = strconv.Atoi(bot)
					}
					giveTo(bots, outputs, bots[bot].lowDest, low)
					giveTo(bots, outputs, bots[bot].highDest, high)
					bots[bot].chips = []int{}
				} else {
					newInstructions = append(newInstructions, inst)
				}
			}
		}
		instructions = newInstructions
	}

	part2Answer := outputs["output 0"][0] * outputs["output 1"][0] * outputs["output 2"][0]

	fmt.Printf("Part 1: %d\n", part1Answer)
	fmt.Printf("Part 2: %d\n", part2Answer)
}

func giveTo(bots map[string]*Bot, outputs map[string][]int, dest string, value int) {
	if strings.HasPrefix(dest, "bot") {
		if _, exists := bots[dest]; !exists {
			bots[dest] = &Bot{chips: []int{}}
		}
		bots[dest].chips = append(bots[dest].chips, value)
	} else {
		outputs[dest] = append(outputs[dest], value)
	}
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
