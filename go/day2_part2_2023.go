package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	regex := regexp.MustCompile(`Game (\d+): (.+)`)
	cubeRegex := regexp.MustCompile(`(\d+) (red|green|blue)`)
	totalPower := 0

	for scanner.Scan() {
		line := scanner.Text()
		matches := regex.FindStringSubmatch(line)

		if len(matches) == 3 {
			rounds := strings.Split(matches[2], ";")
			maxRed, maxGreen, maxBlue := 0, 0, 0

			for _, round := range rounds {
				cubes := cubeRegex.FindAllStringSubmatch(round, -1)
				red, green, blue := 0, 0, 0

				for _, cube := range cubes {
					count, _ := strconv.Atoi(cube[1])
					switch cube[2] {
					case "red":
						red += count
					case "green":
						green += count
					case "blue":
						blue += count
					}
				}

				if red > maxRed {
					maxRed = red
				}
				if green > maxGreen {
					maxGreen = green
				}
				if blue > maxBlue {
					maxBlue = blue
				}
			}

			power := maxRed * maxGreen * maxBlue
			totalPower += power
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}

	fmt.Println(totalPower)
}