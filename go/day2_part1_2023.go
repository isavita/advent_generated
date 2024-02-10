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
	totalSum := 0

	for scanner.Scan() {
		line := scanner.Text()
		matches := regex.FindStringSubmatch(line)

		if len(matches) == 3 {
			gameId, _ := strconv.Atoi(matches[1])
			rounds := strings.Split(matches[2], ";")
			isValid := true

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

					if red > 12 || green > 13 || blue > 14 {
						isValid = false
						break
					}
				}

				if !isValid {
					break
				}
			}

			if isValid {
				totalSum += gameId
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}

	fmt.Println(totalSum)
}