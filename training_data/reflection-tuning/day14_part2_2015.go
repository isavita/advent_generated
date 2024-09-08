package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Reindeer struct {
	speed, flyTime, restTime, distance, points int
}

func main() {
	reindeer := parseInput("input.txt")
	maxDistance := 0
	maxPoints := 0

	// Simulate the race
	for second := 1; second <= 2503; second++ {
		leadDistance := 0
		for i := range reindeer {
			updateDistance(&reindeer[i], second)
			if reindeer[i].distance > leadDistance {
				leadDistance = reindeer[i].distance
			}
			if reindeer[i].distance > maxDistance {
				maxDistance = reindeer[i].distance
			}
		}

		// Award points to leaders
		for i := range reindeer {
			if reindeer[i].distance == leadDistance {
				reindeer[i].points++
				if reindeer[i].points > maxPoints {
					maxPoints = reindeer[i].points
				}
			}
		}
	}

	fmt.Printf("Part 1: Maximum distance traveled: %d\n", maxDistance)
	fmt.Printf("Part 2: Maximum points scored: %d\n", maxPoints)
}

func parseInput(filename string) []Reindeer {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var reindeer []Reindeer
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		speed, _ := strconv.Atoi(parts[3])
		flyTime, _ := strconv.Atoi(parts[6])
		restTime, _ := strconv.Atoi(parts[13])
		reindeer = append(reindeer, Reindeer{speed, flyTime, restTime, 0, 0})
	}
	return reindeer
}

func updateDistance(r *Reindeer, second int) {
	cycleTime := r.flyTime + r.restTime
	cyclePosition := second % cycleTime
	if cyclePosition <= r.flyTime && cyclePosition != 0 {
		r.distance += r.speed
	}
}
