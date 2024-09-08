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
	lowTo  string
	highTo string
}

func main() {
	bots := make(map[int]*Bot)
	outputs := make(map[int][]int)
	instructions := make([]string, 0)

	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "value") {
			parts := strings.Fields(line)
			value, _ := strconv.Atoi(parts[1])
			botNum, _ := strconv.Atoi(parts[5])
			if bots[botNum] == nil {
				bots[botNum] = &Bot{chips: make([]int, 0)}
			}
			bots[botNum].chips = append(bots[botNum].chips, value)
		} else {
			instructions = append(instructions, line)
		}
	}

	for len(instructions) > 0 {
		var nextInstructions []string
		for _, inst := range instructions {
			parts := strings.Fields(inst)
			botNum, _ := strconv.Atoi(parts[1])
			lowType, lowNum := parts[5], parts[6]
			highType, highNum := parts[10], parts[11]

			if bots[botNum] == nil {
				bots[botNum] = &Bot{chips: make([]int, 0)}
			}
			bots[botNum].lowTo = lowType + " " + lowNum
			bots[botNum].highTo = highType + " " + highNum

			if len(bots[botNum].chips) == 2 {
				low, high := min(bots[botNum].chips[0], bots[botNum].chips[1]), max(bots[botNum].chips[0], bots[botNum].chips[1])
				
				if (low == 17 && high == 61) || (low == 61 && high == 17) {
					fmt.Println(botNum)
					return
				}

				giveTo := func(to string, value int) {
					if strings.HasPrefix(to, "bot") {
						toBot, _ := strconv.Atoi(strings.Fields(to)[1])
						if bots[toBot] == nil {
							bots[toBot] = &Bot{chips: make([]int, 0)}
						}
						bots[toBot].chips = append(bots[toBot].chips, value)
					} else {
						toOutput, _ := strconv.Atoi(strings.Fields(to)[1])
						outputs[toOutput] = append(outputs[toOutput], value)
					}
				}

				giveTo(bots[botNum].lowTo, low)
				giveTo(bots[botNum].highTo, high)
				bots[botNum].chips = []int{}
			} else {
				nextInstructions = append(nextInstructions, inst)
			}
		}
		instructions = nextInstructions
	}
}
