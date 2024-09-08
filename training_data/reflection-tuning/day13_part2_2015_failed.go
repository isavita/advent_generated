package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	happiness := parseInput("input.txt")
	people := make([]string, 0, len(happiness))
	for person := range happiness {
		people = append(people, person)
	}

	maxHappiness := findMaxHappiness(happiness, people)
	fmt.Println("Part 1:", maxHappiness)

	// Part 2: Add yourself
	for _, person := range people {
		happiness["You"][person] = 0
		happiness[person]["You"] = 0
	}
	people = append(people, "You")

	maxHappinessWithYou := findMaxHappiness(happiness, people)
	fmt.Println("Part 2:", maxHappinessWithYou)
}

func parseInput(filename string) map[string]map[string]int {
	file, _ := os.Open(filename)
	defer file.Close()
	scanner := bufio.NewScanner(file)

	happiness := make(map[string]map[string]int)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		person1, person2 := parts[0], parts[10][:len(parts[10])-1]
		value := 0
		fmt.Sscanf(parts[3], "%d", &value)
		if parts[2] == "lose" {
			value = -value
		}

		if happiness[person1] == nil {
			happiness[person1] = make(map[string]int)
		}
		happiness[person1][person2] = value
	}
	return happiness
}

func findMaxHappiness(happiness map[string]map[string]int, people []string) int {
	n := len(people)
	maxHappiness := 0

	var backtrack func(arrangement []int, used int, depth int)
	backtrack = func(arrangement []int, used int, depth int) {
		if depth == n {
			totalHappiness := calculateHappiness(happiness, people, arrangement)
			if totalHappiness > maxHappiness {
				maxHappiness = totalHappiness
			}
			return
		}

		for i := 0; i < n; i++ {
			if used&(1<<i) == 0 {
				arrangement[depth] = i
				backtrack(arrangement, used|(1<<i), depth+1)
			}
		}
	}

	backtrack(make([]int, n), 0, 0)
	return maxHappiness
}

func calculateHappiness(happiness map[string]map[string]int, people []string, arrangement []int) int {
	total := 0
	n := len(arrangement)
	for i := 0; i < n; i++ {
		person := people[arrangement[i]]
		left := people[arrangement[(i-1+n)%n]]
		right := people[arrangement[(i+1)%n]]
		total += happiness[person][left] + happiness[person][right]
	}
	return total
}
