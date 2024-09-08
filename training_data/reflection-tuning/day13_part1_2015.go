package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	happiness := make(map[string]map[string]int)
	people := make(map[string]bool)

	// Read and parse input
	file, _ := os.Open("input.txt")
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		person1, person2 := parts[0], strings.TrimRight(parts[10], ".")
		value := 0
		fmt.Sscanf(parts[3], "%d", &value)
		if parts[2] == "lose" {
			value = -value
		}
		if happiness[person1] == nil {
			happiness[person1] = make(map[string]int)
		}
		happiness[person1][person2] = value
		people[person1] = true
	}

	// Convert people map to slice for easier permutation
	peopleSlice := make([]string, 0, len(people))
	for person := range people {
		peopleSlice = append(peopleSlice, person)
	}

	maxHappiness := findMaxHappiness(happiness, peopleSlice, []string{}, make(map[string]bool))
	fmt.Println(maxHappiness)
}

func findMaxHappiness(happiness map[string]map[string]int, people, arrangement []string, used map[string]bool) int {
	if len(arrangement) == len(people) {
		return calculateHappiness(happiness, arrangement)
	}

	maxHappiness := 0
	for _, person := range people {
		if !used[person] {
			used[person] = true
			arrangement = append(arrangement, person)
			happiness := findMaxHappiness(happiness, people, arrangement, used)
			if happiness > maxHappiness {
				maxHappiness = happiness
			}
			arrangement = arrangement[:len(arrangement)-1]
			used[person] = false
		}
	}
	return maxHappiness
}

func calculateHappiness(happiness map[string]map[string]int, arrangement []string) int {
	total := 0
	n := len(arrangement)
	for i := 0; i < n; i++ {
		person := arrangement[i]
		left := arrangement[(i-1+n)%n]
		right := arrangement[(i+1)%n]
		total += happiness[person][left] + happiness[person][right]
	}
	return total
}
