package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Reindeer struct {
	speed      int
	flyTime    int
	restTime   int
	distance   int
	flying     bool
	timeInMode int
}

func main() {
	reindeers, err := readReindeerDetails("input.txt")
	if err != nil {
		fmt.Println("Error reading input:", err)
		return
	}

	simulateRace(&reindeers, 2503)
	maxDistance := findMaxDistance(reindeers)
	fmt.Println(maxDistance)
}

func readReindeerDetails(filename string) ([]Reindeer, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var reindeers []Reindeer
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		speed, _ := strconv.Atoi(parts[3])
		flyTime, _ := strconv.Atoi(parts[6])
		restTime, _ := strconv.Atoi(parts[13])

		reindeers = append(reindeers, Reindeer{speed: speed, flyTime: flyTime, restTime: restTime, flying: true})
	}

	return reindeers, scanner.Err()
}

func simulateRace(reindeers *[]Reindeer, totalSeconds int) {
	for i := 0; i < totalSeconds; i++ {
		for j := range *reindeers {
			reindeer := &(*reindeers)[j]
			if reindeer.flying {
				reindeer.distance += reindeer.speed
				reindeer.timeInMode++
				if reindeer.timeInMode == reindeer.flyTime {
					reindeer.flying = false
					reindeer.timeInMode = 0
				}
			} else {
				reindeer.timeInMode++
				if reindeer.timeInMode == reindeer.restTime {
					reindeer.flying = true
					reindeer.timeInMode = 0
				}
			}
		}
	}
}

func findMaxDistance(reindeers []Reindeer) int {
	maxDistance := 0
	for _, reindeer := range reindeers {
		if reindeer.distance > maxDistance {
			maxDistance = reindeer.distance
		}
	}
	return maxDistance
}