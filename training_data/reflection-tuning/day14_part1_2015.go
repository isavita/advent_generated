package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Reindeer struct {
	speed    int
	flyTime  int
	restTime int
}

func (r Reindeer) distanceTraveled(totalTime int) int {
	cycleTime := r.flyTime + r.restTime
	fullCycles := totalTime / cycleTime
	remainingTime := totalTime % cycleTime

	distance := fullCycles * r.speed * r.flyTime
	if remainingTime > r.flyTime {
		distance += r.speed * r.flyTime
	} else {
		distance += r.speed * remainingTime
	}

	return distance
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	var reindeers []Reindeer
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		speed, _ := strconv.Atoi(parts[3])
		flyTime, _ := strconv.Atoi(parts[6])
		restTime, _ := strconv.Atoi(parts[13])
		reindeers = append(reindeers, Reindeer{speed, flyTime, restTime})
	}

	maxDistance := 0
	for _, reindeer := range reindeers {
		distance := reindeer.distanceTraveled(2503)
		if distance > maxDistance {
			maxDistance = distance
		}
	}

	fmt.Println(maxDistance)
}
