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

	targetWeight := totalWeight / 4
	bestQE := math.MaxInt64
	bestLength := math.MaxInt64

	// Generate all combinations and find the best quantum entanglement
	for comb := 1; comb < (1 << uint(len(packages))); comb++ {
		groupWeight, qe, groupLength := 0, 1, 0
		for i := 0; i < len(packages); i++ {
			if comb&(1<<uint(i)) != 0 {
				groupWeight += packages[i]
				qe *= packages[i]
				groupLength++
			}
		}
		if groupWeight == targetWeight && groupLength <= bestLength {
			if groupLength < bestLength || qe < bestQE {
				// Check if the remaining packages can be split into three equal groups
				if canSplit(packages, comb, targetWeight) {
					bestLength = groupLength
					bestQE = qe
				}
			}
		}
	}

	fmt.Printf("%d\n", bestQE)
}

// canSplit checks if the remaining packages (excluding the ones in the first group) can be split into three groups of the target weight.
func canSplit(packages []int, firstGroupComb int, targetWeight int) bool {
	remainingPackages := []int{}
	for i, weight := range packages {
		if firstGroupComb&(1<<uint(i)) == 0 {
			remainingPackages = append(remainingPackages, weight)
		}
	}
	// We now need to find two non-overlapping combinations of the remaining packages that equal the target weight.
	for comb1 := 1; comb1 < (1 << uint(len(remainingPackages))); comb1++ {
		group1Weight := 0
		for i := 0; i < len(remainingPackages); i++ {
			if comb1&(1<<uint(i)) != 0 {
				group1Weight += remainingPackages[i]
			}
		}
		if group1Weight == targetWeight {
			for comb2 := 1; comb2 < (1 << uint(len(remainingPackages))); comb2++ {
				if comb1&comb2 == 0 { // Ensure group 1 and group 2 don't overlap
					group2Weight := 0
					for i := 0; i < len(remainingPackages); i++ {
						if comb2&(1<<uint(i)) != 0 {
							group2Weight += remainingPackages[i]
						}
					}
					if group2Weight == targetWeight {
						return true
					}
				}
			}
		}
	}
	return false
}