package main

import (
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	var packages []int
	totalWeight := 0

	for _, line := range lines {
		weight, err := strconv.Atoi(line)
		if err != nil {
			log.Fatalf("Invalid weight in input: %s", line)
		}
		packages = append(packages, weight)
		totalWeight += weight
	}

	targetWeight := totalWeight / 3
	bestQE := math.MaxInt64
	bestLength := math.MaxInt64

	for comb := 1; comb < (1 << len(packages)); comb++ {
		groupWeight, qe, groupLength := 0, 1, 0
		for i := 0; i < len(packages); i++ {
			if comb&(1<<i) != 0 {
				groupWeight += packages[i]
				qe *= packages[i]
				groupLength++
			}
		}
		if groupWeight == targetWeight && groupLength <= bestLength {
			if groupLength < bestLength || qe < bestQE {
				bestLength = groupLength
				bestQE = qe
			}
		}
	}

	fmt.Printf("%d\n", bestQE)
}