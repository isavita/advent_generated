package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type bot struct {
	lowTo, highTo string
	chips         []int
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	bots := make(map[string]*bot)
	outputs := make(map[string]int)
	valueRegex := regexp.MustCompile(`value (\d+) goes to (bot \d+)`)
	givesRegex := regexp.MustCompile(`(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)`)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if valueRegex.MatchString(line) {
			matches := valueRegex.FindStringSubmatch(line)
			value, _ := strconv.Atoi(matches[1])
			botID := matches[2]

			if _, exists := bots[botID]; !exists {
				bots[botID] = &bot{}
			}
			bots[botID].chips = append(bots[botID].chips, value)

		} else if givesRegex.MatchString(line) {
			matches := givesRegex.FindStringSubmatch(line)
			botID, lowTo, highTo := matches[1], matches[2], matches[3]

			if _, exists := bots[botID]; !exists {
				bots[botID] = &bot{}
			}
			bots[botID].lowTo = lowTo
			bots[botID].highTo = highTo
		}
	}

	for {
		action := false
		for _, b := range bots {
			if len(b.chips) == 2 {
				action = true
				low, high := minMax(b.chips[0], b.chips[1])
				b.chips = nil

				giveChip(bots, outputs, b.lowTo, low)
				giveChip(bots, outputs, b.highTo, high)
			}
		}
		if !action {
			break
		}
	}

	result := outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
	fmt.Println(result)
}

func giveChip(bots map[string]*bot, outputs map[string]int, target string, value int) {
	if strings.HasPrefix(target, "bot") {
		if _, exists := bots[target]; !exists {
			bots[target] = &bot{}
		}
		bots[target].chips = append(bots[target].chips, value)
	} else if strings.HasPrefix(target, "output") {
		outputs[target] = value
	}
}

func minMax(a, b int) (int, int) {
	if a < b {
		return a, b
	}
	return b, a
}